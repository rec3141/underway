"""SIGRID decoding, archive validation, projection and offline publication."""
import io
import gzip
import json
import tarfile
import tempfile
import unittest
import zipfile
from pathlib import Path
from unittest.mock import patch

from dashboard import ice_charts as ic

try:
    import shapefile
    from pyproj import CRS, Transformer
    HAVE_GIS = True
except ImportError:
    HAVE_GIS = False


class IceCodeTests(unittest.TestCase):
    def test_concentration_codes_are_not_percentages_or_missing_ice(self):
        self.assertEqual(ic.concentration("92"), (10, "10/10"))
        self.assertEqual(ic.concentration("91"), (9.5, "9–10/10"))
        self.assertEqual(ic.concentration("81"), (9, "8–10/10"))
        self.assertEqual(ic.concentration("35"), (4, "3–5/10"))
        self.assertEqual(ic.concentration("01"), (0.5, "Open water (<1/10)"))
        self.assertEqual(ic.concentration("02"), (0, "Bergy water"))
        self.assertIsNone(ic.concentration("99")[0])
        self.assertIsNone(ic.concentration("-9")[0])
        self.assertIsNone(ic.concentration("65")[0])

    def test_surface_type_and_codes_preserved(self):
        props = ic.properties({"CT": "92", "SA": "93", "FA": "09", "CN": "95", "CB": "-9", "POLY_TYPE": "I"})
        self.assertEqual(props["CT"], "92")
        self.assertEqual(props["trace_thicker_ice"], "Old ice")
        self.assertEqual(props["CB"], "-9")
        self.assertEqual(props["egg_ct"], "10")
        self.assertEqual(props["egg_sa"], "4•")
        self.assertEqual(props["egg_fa"], "8")
        self.assertEqual(props["egg_cb"], "–")
        self.assertEqual(ic.properties({"POLY_TYPE": "W"})["concentration"], 0)
        self.assertIsNone(ic.properties({"CT": "92", "POLY_TYPE": "N"})["concentration"])
        self.assertEqual(ic.properties({"SA": "95", "FA": "10"})["egg_fa"], "9")

    def test_archive_rejects_paths_and_links(self):
        stream = io.BytesIO()
        with zipfile.ZipFile(stream, "w") as archive:
            archive.writestr("../escaped.shp", b"test")
        with self.assertRaisesRegex(ValueError, "Unsafe"):
            ic._archive_parts(stream.getvalue())
        stream = io.BytesIO()
        with tarfile.open(fileobj=stream, mode="w") as archive:
            member = tarfile.TarInfo("link.shp")
            member.type = tarfile.SYMTYPE
            member.linkname = "/etc/passwd"
            archive.addfile(member)
        with self.assertRaisesRegex(ValueError, "regular"):
            ic._archive_parts(stream.getvalue())

    def test_archive_expansion_bound(self):
        stream = io.BytesIO()
        with zipfile.ZipFile(stream, "w", compression=zipfile.ZIP_DEFLATED) as archive:
            archive.writestr("chart.shp", b"0" * 20)
        with patch.object(ic, "MAX_EXPANDED", 10), self.assertRaisesRegex(ValueError, "size limit"):
            ic._archive_parts(stream.getvalue())

    def test_discovery_uses_only_regional_archives(self):
        html = b'<a href="cis_SGRDREA_20260907T1800Z_pl_a.tar">EA</a><a href="cis_SGRDAWIS50_20260907T1800Z_pl_a.tar">daily</a><a href="https://example.com/other.tar">other</a>'
        with patch.object(ic, "_download", return_value=html):
            rows = ic.available()
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]["date"], "2026-09-07")
        self.assertEqual(rows[0]["valid_time"], "2026-09-07T18:00:00Z")
        self.assertTrue(rows[0]["source_url"].startswith(ic.SOURCE))

    def test_daily_discovery_accepts_only_the_named_product(self):
        html = b'<a href="/prods/WIS36C/20260914180000_WIS36C_0014221325.gif">chart</a>'
        directory = b'<a href="20260914180000_WIS36C_0014221326.pdf">PDF</a>'
        url, day, valid = ic._daily_source("WIS36C", html, directory)
        self.assertEqual(day, "2026-09-14")
        self.assertEqual(valid, "2026-09-14T18:00:00Z")
        self.assertEqual(url, "https://ice-glaces.ec.gc.ca/prods/WIS36C/20260914180000_WIS36C_0014221326.pdf")
        with self.assertRaisesRegex(ValueError, "Unsupported"):
            ic._daily_source("WIS99C", html, directory)
        with self.assertRaisesRegex(ValueError, "does not currently advertise"):
            other = b'<a href="/prods/WIS35C/20260914180000_WIS35C_1.gif">other</a>'
            ic._daily_source("WIS36C", other, directory)
        with self.assertRaisesRegex(ValueError, "georeferenced PDF"):
            ic._daily_source("WIS36C", html, b'<a href="older.pdf">old</a>')

    def test_daily_pdf_neatline_bounds(self):
        info = {"metadata": {"": {"NEATLINE": "POLYGON ((-2 9,-2 -4,7 -4,7 9,-2 9))"}}}
        self.assertEqual(ic._neatline_bounds(info), (-2, -4, 7, 9))
        with self.assertRaisesRegex(ValueError, "neatline"):
            ic._neatline_bounds({})

    def test_daily_import_writes_image_and_metadata_atomically(self):
        advertised = {"product": "WIS36C", "date": "2026-09-14", "region": "Eureka (daily raster)",
                      "valid_time": "2026-09-14T18:00:00Z", "source_url": "https://example.test/chart.pdf"}
        png = b"\x89PNG\r\n\x1a\nchart"
        coordinates = [[-112, 84], [-48, 84], [-48, 72], [-112, 72]]
        with tempfile.TemporaryDirectory() as directory, patch.object(ic, "DB_DIR", Path(directory)), \
                patch.object(ic, "available_daily", return_value=advertised), \
                patch.object(ic, "_download", return_value=b"%PDF-"), \
                patch.object(ic, "_warp_daily", return_value=(png, coordinates, [3600, 3619])):
            entry = ic.import_daily_chart()
            self.assertEqual(entry["kind"], "raster")
            self.assertTrue(entry["ship_area"])
            self.assertEqual(entry["coordinates"], coordinates)
            self.assertEqual(entry["image_size"], [3600, 3619])
            self.assertEqual((ic.chart_dir() / f'{entry["id"]}.png').read_bytes(), png)
            self.assertEqual(json.loads((ic.chart_dir() / f'{entry["id"]}.raster.json').read_text())["product"], "WIS36C")


@unittest.skipUnless(HAVE_GIS, "Install .[ice-charts] for conversion tests")
class IceChartImportTests(unittest.TestCase):
    def make_chart(self, folder):
        path = folder / "cis_SGRDREA_20260907T1800Z_pl_a.shp"
        projection = Transformer.from_crs("EPSG:4326", "EPSG:3857", always_xy=True)
        outer = [projection.transform(*p) for p in [(-90, 70), (-90, 71), (-89, 71), (-89, 70), (-90, 70)]]
        hole = [projection.transform(*p) for p in [(-89.8, 70.2), (-89.2, 70.2), (-89.2, 70.8), (-89.8, 70.8), (-89.8, 70.2)]]
        with shapefile.Writer(str(path)) as writer:
            for field in ("CT", "SA", "FA", "POLY_TYPE"):
                writer.field(field, "C", size=2)
            writer.poly([outer, hole])
            writer.record("92", "93", "08", "I")
            writer.poly([outer])
            writer.record("", "", "", "L")
        path.with_suffix(".prj").write_text(CRS.from_epsg(3857).to_wkt())
        return path

    def test_projected_shapefile_holes_date_publish_and_replace(self):
        with tempfile.TemporaryDirectory() as directory:
            folder = Path(directory)
            path = self.make_chart(folder)
            with patch.object(ic, "DB_DIR", folder / "db"), \
                    patch.object(ic, "SEED_DIR", folder / "seeds"):
                entry = ic.import_chart(file=path, date="2026-09-07", region="Eastern Arctic")
                self.assertEqual(entry["feature_count"], 1)
                self.assertEqual(entry["valid_time"], "2026-09-07T18:00:00Z")
                collection = json.loads(next(ic.chart_dir().glob("*.geojson")).read_text())
                rings = collection["features"][0]["geometry"]["coordinates"]
                self.assertEqual(len(rings), 2)
                self.assertEqual(rings[0][0], [-90.0, 70.0])
                output = ic.publish(folder / "www")
                self.assertEqual(len(output["charts"]), 1)
                url = output["charts"][0]["url"].split("?")[0]
                self.assertTrue((folder / "www" / url).is_file())
                with self.assertRaisesRegex(ValueError, "date"):
                    ic.import_chart(file=path, date="2026-09-08", region="Eastern Arctic")
                path.with_suffix(".prj").unlink()
                with self.assertRaisesRegex(ValueError, ".prj"):
                    ic.import_chart(file=path, date="2026-09-07", region="Eastern Arctic")
                self.assertEqual(ic.publish(folder / "www"), output)

    def test_zip_and_tar_import(self):
        with tempfile.TemporaryDirectory() as directory:
            folder = Path(directory)
            self.make_chart(folder)
            parts = list(folder.iterdir())
            for suffix in ("zip", "tar"):
                archive = folder / f"input.{suffix}"
                if suffix == "zip":
                    with zipfile.ZipFile(archive, "w") as out:
                        for part in parts:
                            out.write(part, "nested/" + part.name)
                else:
                    with tarfile.open(archive, "w") as out:
                        for part in parts:
                            out.add(part, arcname="nested/" + part.name)
                with patch.object(ic, "DB_DIR", folder / "db"):
                    self.assertEqual(ic.import_chart(file=archive, date="2026-09-07", region="Eastern Arctic")["feature_count"], 1)


class PublishTests(unittest.TestCase):
    def test_empty_missing_and_bad_cache_do_not_break_build(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(ic, "DB_DIR", Path(directory)), \
                patch.object(ic, "SEED_DIR", Path(directory) / "seeds"):
            self.assertIsNone(ic.publish(Path(directory) / "www"))
            ic.chart_dir().mkdir()
            (ic.chart_dir() / "broken.geojson").write_text("{")
            with self.assertLogs(ic.log, level="WARNING"):
                self.assertIsNone(ic.publish(Path(directory) / "www"))

    def test_seed_is_published_and_matching_cache_takes_precedence(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            seeds = root / "seeds"
            seeds.mkdir()
            entry = {"id": "eastern-arctic-2026-09-07", "date": "2026-09-07", "region": "Eastern Arctic"}
            seed = {"type": "FeatureCollection", "chart": {**entry, "feature_count": 1},
                    "features": [{"type": "Feature", "properties": {"CT": "92"}, "geometry": None}]}
            (seeds / f'{entry["id"]}.geojson.gz').write_bytes(gzip.compress(json.dumps(seed).encode()))
            with patch.object(ic, "DB_DIR", root / "db"), patch.object(ic, "SEED_DIR", seeds):
                published = ic.publish(root / "www")
                self.assertEqual(published["charts"][0]["feature_count"], 1)
                cache = {**seed, "chart": {**entry, "feature_count": 2}}
                ic.chart_dir().mkdir(parents=True)
                (ic.chart_dir() / f'{entry["id"]}.geojson').write_text(json.dumps(cache))
                published = ic.publish(root / "www")
                self.assertEqual(published["charts"][0]["feature_count"], 2)

    def test_raster_seed_is_validated_and_published(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            seeds = root / "seeds"
            seeds.mkdir()
            entry = {"id": "eureka-daily-raster-2026-09-14", "kind": "raster", "date": "2026-09-14",
                     "region": "Eureka (daily raster)", "coordinates": [[-112, 84], [-47, 84], [-47, 74], [-112, 74]]}
            (seeds / f'{entry["id"]}.raster.json').write_text(json.dumps(entry))
            (seeds / f'{entry["id"]}.png').write_bytes(b"\x89PNG\r\n\x1a\nchart")
            with patch.object(ic, "DB_DIR", root / "db"), patch.object(ic, "SEED_DIR", seeds):
                published = ic.publish(root / "www")
            self.assertEqual(published["charts"][0]["kind"], "raster")
            self.assertTrue((root / "www" / published["charts"][0]["url"].split("?")[0]).is_file())


if __name__ == "__main__":
    unittest.main()
