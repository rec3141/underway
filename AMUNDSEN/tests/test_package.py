"""First-run checks that can also run against an installed wheel from /tmp."""

import json
import tempfile
import unittest
from importlib.resources import files
from pathlib import Path
from unittest.mock import patch

from dashboard import gcal
from dashboard.casts import build_casts
from dashboard.cli import main


class PackageTests(unittest.TestCase):
    def test_site_assets_are_available(self):
        package = files("dashboard")
        for name in ["templates/index.html.j2", "static/plotly.min.js",
                     "static/app.js", "static/tabs.js", "static/chat.js", "static/style.css",
                     "static/geo/bathymetry.geojson", "static/geo/coastline.geojson",
                     "static/geo/land.geojson", "static/geo/minor_islands.geojson",
                     "static/geo/glaciated_areas.geojson"]:
            with self.subTest(name=name):
                self.assertTrue(package.joinpath(name).is_file())

    def test_cli_help(self):
        with self.assertRaises(SystemExit) as result:
            main(["--help"])
        self.assertEqual(result.exception.code, 0)

    def test_first_build_without_casts_writes_empty_index(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / "new-site"
            result = build_casts([], root)
            self.assertEqual(result, {"casts": [], "variables": []})
            self.assertEqual(json.loads((root / "data/casts/index.json").read_text()), result)

    def test_cached_calendar_needs_no_http_extra(self):
        with tempfile.TemporaryDirectory() as tmp:
            with patch.object(gcal, "DB_DIR", Path(tmp)), patch.dict("sys.modules", {"requests": None}):
                result = gcal.import_calendars(fetch=False)
            self.assertEqual(len(result), len(gcal.GCAL))
            self.assertTrue(all(entry["stale"] for entry in result))


if __name__ == "__main__":
    unittest.main()
