"""Cache Canadian Ice Service charts as dated map overlays.

Fetching is an explicit operator action; ordinary builds only publish the local
cache. Weekly SIGRID-3 vectors retain their codes as strings, alongside decoded
display values (WMO/TD-No. 1214). Daily chart images are cropped and warped to
Web Mercator before publication so they line up with the dashboard map.
"""
from __future__ import annotations

import hashlib
import gzip
import io
import json
import logging
import math
import re
import shutil
import subprocess
import tarfile
import tempfile
import urllib.request
import zipfile
from datetime import date as Date, datetime, timezone
from pathlib import Path, PurePosixPath
from urllib.parse import urlparse

from .config import DB_DIR

log = logging.getLogger(__name__)
SOURCE = "https://ice-glaces.ec.gc.ca/prods/sigrids/"
PRODUCT_SOURCE = "https://ice-glaces.ec.gc.ca/cgi-bin/getprod.pl?lang=en&prodid={product}&wrap=1"
PRODUCT_DIRECTORY = "https://ice-glaces.ec.gc.ca/prods/{product}/"
SEED_DIR = Path(__file__).with_name("ice_assets")
ATTRIBUTION = "Canadian Ice Service / ECCC"
LICENCE = "https://open.canada.ca/en/open-government-licence-canada"
REGIONS = {"EA": "Eastern Arctic", "WA": "Western Arctic", "HB": "Hudson Bay", "EC": "East Coast", "GL": "Great Lakes"}
MAX_BYTES = 64 * 1024 * 1024
MAX_EXPANDED = 256 * 1024 * 1024
DAILY_PRODUCTS = {
    "WIS36C": {
        "region": "Eureka (daily raster)",
        "width": 3600,
    },
}
CODES = ("CT", "CA", "CB", "CC", "SA", "SB", "SC", "FA", "FB", "FC", "CN", "CD", "CF")
STAGES = {"00": "Ice free", "80": "No stage of development", "81": "New ice", "82": "Nilas / ice rind (<10 cm)",
          "83": "Young ice (10–30 cm)", "84": "Grey ice (10–15 cm)", "85": "Grey-white ice (15–30 cm)",
          "86": "First-year ice (30–200 cm)", "87": "Thin first-year ice (30–70 cm)",
          "88": "Thin first-year ice, stage 1 (30–50 cm)", "89": "Thin first-year ice, stage 2 (50–70 cm)",
          "91": "Medium first-year ice (70–120 cm)", "93": "Thick first-year ice (>120 cm)",
          "95": "Old ice", "96": "Second-year ice", "97": "Multi-year ice", "98": "Glacier ice", "99": "Unknown"}
FORMS = {"00": "Pancake ice", "01": "Shuga / small ice cake / brash ice", "02": "Ice cake (<20 m)",
         "03": "Small floe (20–100 m)", "04": "Medium floe (100–500 m)", "05": "Big floe (500 m–2 km)",
         "06": "Vast floe (2–10 km)", "07": "Giant floe (>10 km)", "08": "Fast ice",
         "09": "Growlers / floebergs / floebits", "10": "Icebergs", "21": "Level ice", "99": "Unknown",
         **{str(n): f"Strips and patches ({n - 10}/10)" for n in range(11, 21)}}
MISSING = {"", "-9", "-99"}
EGG_FORMS = {**{f"{n:02}": str(n) for n in range(9)}, "09": "8", "10": "9", "99": "X"}
EGG_STAGES = {**{str(n): str(n - 80) for n in range(81, 90)}, "91": "1•", "93": "4•",
              "95": "7•", "96": "8•", "97": "9•", "98": "▲•", "99": "X"}


def egg_concentration(code: str) -> str:
    value, label = concentration(code)
    if code in MISSING:
        return "–"
    if code in {"00", "02"}:
        return "0"
    if code == "01":
        return "<1"
    if code == "91":
        return "9+"
    if code == "92":
        return "10"
    if value is None:
        return "X"
    return label.removesuffix("/10")


def chart_dir() -> Path:
    return DB_DIR / "ice-charts"


def concentration(code: str) -> tuple[float | None, str]:
    """Return display tenths and text; interval midpoints are for colour only."""
    if code in MISSING:
        return None, "Not reported"
    special = {"00": (0, "Ice free"), "01": (0.5, "Open water (<1/10)"),
               "02": (0, "Bergy water"), "91": (9.5, "9–10/10"), "92": (10, "10/10"), "99": (None, "Unknown")}
    if code in special:
        return special[code]
    if re.fullmatch(r"[1-9]0", code):
        return int(code[0]), f"{code[0]}/10"
    if code in {"89", "81", "79", "78", "68", "67", "57", "56", "46", "45", "35", "34", "24", "23", "13", "12"}:
        lo, hi = int(code[0]), 10 if code[1] == "1" else int(code[1])
        return (lo + hi) / 2, f"{lo}–{hi}/10"
    return None, f"Unrecognized code ({code})"


def _label(code: str, labels: dict) -> str:
    return "Not reported" if code in MISSING else labels.get(code, f"Unrecognized code ({code})")


def properties(record: dict) -> dict:
    raw = {str(k).upper(): "" if v is None else str(v).strip() for k, v in record.items()}
    out = {k: raw.get(k, "") for k in CODES}
    kind = raw.get("POLY_TYPE", "").upper()
    value, label = concentration(out["CT"])
    if kind == "W":
        value, label = 0, "Ice free"
    elif kind in {"L", "N", "S"}:
        value, label = None, {"L": "Land", "N": "No data", "S": "Ice shelf / ice of land origin"}[kind]
    out.update(polygon_type=kind, concentration=value, concentration_label=label)
    out["egg_ct"] = egg_concentration(out["CT"])
    out["trace_thicker_ice"] = _label(out["CN"], STAGES) if out["CN"] not in MISSING else None
    for letter in "ABC":
        out[f"egg_c{letter.lower()}"] = egg_concentration(out[f"C{letter}"])
        out[f"egg_s{letter.lower()}"] = EGG_STAGES.get(out[f"S{letter}"], "–" if out[f"S{letter}"] in MISSING else "X")
        form = out[f"F{letter}"]
        out[f"egg_f{letter.lower()}"] = EGG_FORMS.get(form, "–" if form in MISSING else "X")
        out[f"concentration_{letter.lower()}_label"] = concentration(out[f"C{letter}"])[1]
        out[f"stage_{letter.lower()}"] = _label(out[f"S{letter}"], STAGES)
        out[f"form_{letter.lower()}"] = _label(out[f"F{letter}"], FORMS)
    return out


def _download(url: str, limit: int = MAX_BYTES) -> bytes:
    if urlparse(url).scheme != "https":
        raise ValueError("Chart downloads require an HTTPS URL")
    with urllib.request.urlopen(url, timeout=45) as response:
        if urlparse(response.url).scheme != "https":
            raise ValueError("Chart download redirected away from HTTPS")
        data = response.read(limit + 1)
    if len(data) > limit:
        raise ValueError("Chart download exceeds size limit")
    return data


def available() -> list[dict]:
    """List current weekly regional archives advertised by the CIS directory."""
    html = _download(SOURCE, 2 * 1024 * 1024).decode("utf-8")
    names = set(re.findall(r'href="(cis_SGRDR(?:EA|WA|HB|EC|GL)_\d{8}(?:T\d{4}Z)?_pl_[a-z]\.tar)"', html))
    rows = []
    for name in sorted(names, reverse=True):
        match = re.fullmatch(r"cis_SGRDR([A-Z]{2})_(\d{8})(?:T(\d{4})Z)?_pl_[a-z]\.tar", name)
        region, day, time = match.groups()
        rows.append({"date": datetime.strptime(day, "%Y%m%d").date().isoformat(),
                     "region": REGIONS[region], "source_url": SOURCE + name,
                     "valid_time": f"{day[:4]}-{day[4:6]}-{day[6:]}T{time[:2]}:{time[2:]}:00Z" if time else None})
    return sorted(rows, key=lambda row: (row["date"], row["region"]), reverse=True)


def _daily_source(product: str, html: bytes, directory_html: bytes) -> tuple[str, str, str]:
    """Return the vector PDF matching the product page's current chart."""
    if product not in DAILY_PRODUCTS:
        raise ValueError(f"Unsupported daily chart product {product}")
    advertised = re.search(rf'/prods/{re.escape(product)}/(\d{{14}})_{re.escape(product)}_\d+\.gif',
                           html.decode("utf-8", "replace"))
    if not advertised:
        raise ValueError(f"CIS does not currently advertise a {product} daily chart")
    stamp = advertised.group(1)
    pdf = re.search(rf'href="({stamp}_{re.escape(product)}_\d+\.pdf)"',
                    directory_html.decode("utf-8", "replace"), re.IGNORECASE)
    if not pdf:
        raise ValueError(f"CIS does not provide a georeferenced PDF for the current {product} chart")
    valid = datetime.strptime(stamp, "%Y%m%d%H%M%S").replace(tzinfo=timezone.utc)
    return (PRODUCT_DIRECTORY.format(product=product) + pdf.group(1), valid.date().isoformat(),
            valid.isoformat().replace("+00:00", "Z"))


def available_daily(product: str = "WIS36C") -> dict:
    """Describe the latest daily raster advertised for the ship's chart area."""
    page = PRODUCT_SOURCE.format(product=product)
    directory = PRODUCT_DIRECTORY.format(product=product)
    url, day, valid_time = _daily_source(product, _download(page, 2 * 1024 * 1024),
                                         _download(directory, 2 * 1024 * 1024))
    return {"product": product, "date": day, "region": DAILY_PRODUCTS[product]["region"],
            "valid_time": valid_time, "source_url": url}


def _neatline_bounds(info: dict) -> tuple[float, float, float, float]:
    """Read the projected map frame bounds embedded by ArcGIS in a chart PDF."""
    neatline = info.get("metadata", {}).get("", {}).get("NEATLINE", "")
    numbers = [float(value) for value in re.findall(r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[Ee][-+]?\d+)?", neatline)]
    if len(numbers) < 8 or len(numbers) % 2:
        raise ValueError("Daily chart PDF does not contain a usable map neatline")
    xs, ys = numbers[::2], numbers[1::2]
    bounds = min(xs), min(ys), max(xs), max(ys)
    if not all(math.isfinite(value) for value in bounds) or bounds[0] >= bounds[2] or bounds[1] >= bounds[3]:
        raise ValueError("Daily chart PDF contains an invalid map neatline")
    return bounds


def _warp_daily(product: str, document: bytes) -> tuple[bytes, list[list[float]], list[int]]:
    """Render and warp the PDF's embedded map frame into Web Mercator."""
    required = [name for name in ("gdal_translate", "gdalwarp", "gdalinfo") if not shutil.which(name)]
    if required:
        raise ValueError("Daily chart conversion requires GDAL commands: " + ", ".join(required))
    if not document.startswith(b"%PDF-"):
        raise ValueError("Daily chart download is not a PDF document")
    spec = DAILY_PRODUCTS[product]
    with tempfile.TemporaryDirectory() as directory:
        folder = Path(directory)
        source = folder / "source.pdf"
        cropped = folder / "cropped.tif"
        warped = folder / "warped.tif"
        output = folder / "chart.png"
        source.write_bytes(document)
        info_result = subprocess.run(["gdalinfo", "--config", "GDAL_PDF_DPI", "600", "-json", str(source)],
                                     text=True, capture_output=True, check=True)
        info = json.loads(info_result.stdout)
        projection = info.get("coordinateSystem", {}).get("wkt", "")
        if info.get("driverShortName") != "PDF" or "Polar Stereographic" not in projection:
            raise ValueError("Daily chart PDF does not contain the expected projected map")
        xmin, ymin, xmax, ymax = _neatline_bounds(info)
        subprocess.run(["gdal_translate", "-q", "--config", "GDAL_PDF_DPI", "600", "-projwin",
                        str(xmin), str(ymax), str(xmax), str(ymin), str(source), str(cropped)],
                       check=True, capture_output=True)
        subprocess.run(["gdalwarp", "-q", "-overwrite", "-t_srs", "EPSG:3857", "-dstalpha",
                        "-r", "bilinear", "-ts", str(spec["width"]), "0", str(cropped), str(warped)],
                       check=True, capture_output=True)
        subprocess.run(["gdal_translate", "-q", "-of", "PNG", str(warped), str(output)],
                       check=True, capture_output=True)
        data = output.read_bytes()
        warped_info = json.loads(subprocess.run(["gdalinfo", "-json", str(warped)], text=True,
                                                capture_output=True, check=True).stdout)
        image_size = [int(value) for value in warped_info["size"]]
        ring = warped_info["wgs84Extent"]["coordinates"][0]
        if len(ring) < 4:
            raise ValueError("GDAL did not report the warped daily chart extent")
        coordinates = [[float(value) for value in point] for point in (ring[0], ring[3], ring[2], ring[1])]
    if not data.startswith(b"\x89PNG\r\n\x1a\n") or len(data) > MAX_BYTES:
        raise ValueError("GDAL did not produce a valid daily chart PNG")
    if not all(len(point) == 2 and -180 <= point[0] <= 180 and -90 <= point[1] <= 90
               and all(math.isfinite(value) for value in point) for point in coordinates):
        raise ValueError("GDAL reported an invalid daily chart extent")
    return data, coordinates, image_size


def import_daily_chart(product: str = "WIS36C") -> dict:
    """Download and atomically cache the latest daily chart for a fixed product."""
    advertised = available_daily(product)
    document = _download(advertised["source_url"])
    png, coordinates, image_size = _warp_daily(product, document)
    slug = re.sub(r"[^a-z0-9]+", "-", advertised["region"].lower()).strip("-")
    entry = {"id": f"{slug}-{advertised['date']}", "kind": "raster", **advertised,
             "coordinates": coordinates, "image_size": image_size,
             "attribution": ATTRIBUTION, "licence_url": LICENCE, "ship_area": True,
             "imported_at": datetime.now(timezone.utc).isoformat()}
    _atomic_write(chart_dir() / f"{entry['id']}.png", png)
    _atomic_write(chart_dir() / f"{entry['id']}.raster.json",
                  json.dumps(entry, separators=(",", ":"), ensure_ascii=False).encode())
    return entry


def _archive_parts(data: bytes) -> dict[str, bytes]:
    """Read regular members into memory, never extracting archive paths to disk."""
    parts = {}
    total = 0

    def add(name, size, read):
        nonlocal total
        path = PurePosixPath(name)
        if path.is_absolute() or ".." in path.parts or "\\" in name:
            raise ValueError("Unsafe chart archive member path")
        total += size
        if total > MAX_EXPANDED or size > MAX_BYTES:
            raise ValueError("Chart archive exceeds expanded size limit")
        if path.suffix.lower() in {".shp", ".shx", ".dbf", ".prj", ".cpg"}:
            key = str(path).lower()
            if key in parts:
                raise ValueError("Duplicate chart archive member")
            parts[key] = read()

    stream = io.BytesIO(data)
    if zipfile.is_zipfile(stream):
        with zipfile.ZipFile(stream) as archive:
            if len(archive.infolist()) > 1000:
                raise ValueError("Too many chart archive members")
            for item in archive.infolist():
                if (item.external_attr >> 16) & 0o170000 == 0o120000:
                    raise ValueError("Chart archive must not contain links")
                if not item.is_dir():
                    add(item.filename, item.file_size, lambda item=item: archive.read(item))
    else:
        stream.seek(0)
        try:
            with tarfile.open(fileobj=stream) as archive:
                for count, item in enumerate(archive):
                    if count >= 1000:
                        raise ValueError("Too many chart archive members")
                    if not item.isfile() and not item.isdir():
                        raise ValueError("Chart archive must only contain regular files")
                    if item.isfile():
                        add(item.name, item.size, lambda item=item: archive.extractfile(item).read())
        except tarfile.TarError as exc:
            raise ValueError("Input is not a ZIP or TAR shapefile archive") from exc
    return parts


def _convert(parts: dict[str, bytes], day: str) -> tuple[list[dict], str | None]:
    try:
        import shapefile
        from pyproj import CRS, Transformer
    except ImportError as exc:
        raise ValueError('Install conversion dependencies: pip install ".[ice-charts]"') from exc
    shapes = [key for key in parts if key.endswith(".shp")]
    if len(shapes) != 1:
        raise ValueError("Import exactly one polygon shapefile per chart")
    stem = shapes[0][:-4]
    for suffix in (".dbf", ".shx", ".prj"):
        if stem + suffix not in parts:
            raise ValueError(f"Shapefile is missing required {suffix} companion")
    match = re.search(r"_(\d{8})(?:T(\d{4})Z)?_pl_", stem, re.IGNORECASE)
    valid_time = None
    if match:
        stamp = datetime.strptime(match[1], "%Y%m%d").date().isoformat()
        if stamp != day:
            raise ValueError(f"Chart filename date {stamp} does not match requested {day}")
        if match[2]:
            valid_time = f"{stamp}T{match[2][:2]}:{match[2][2:]}:00Z"
    transformer = Transformer.from_crs(CRS.from_wkt(parts[stem + ".prj"].decode("utf-8-sig")), "EPSG:4326", always_xy=True)
    encoding = parts.get(stem + ".cpg", b"utf-8").decode().strip()
    if encoding == "65001":
        encoding = "utf-8"
    features = []

    def coordinates(values):
        if isinstance(values[0], (int, float)):
            lon, lat = transformer.transform(values[0], values[1], errcheck=True)
            if not math.isfinite(lon) or not math.isfinite(lat) or not -180 <= lon <= 180 or not -90 <= lat <= 90:
                raise ValueError("Chart contains invalid transformed coordinates")
            return [round(lon, 6), round(lat, 6)]
        return [coordinates(value) for value in values]

    with shapefile.Reader(shp=io.BytesIO(parts[stem + ".shp"]), shx=io.BytesIO(parts[stem + ".shx"]),
                          dbf=io.BytesIO(parts[stem + ".dbf"]), encoding=encoding) as reader:
        fields = {field[0].upper() for field in reader.fields[1:]}
        if not {"CT", "POLY_TYPE"}.issubset(fields):
            raise ValueError("Shapefile does not have SIGRID-3 CT and POLY_TYPE fields")
        if reader.shapeType not in (5, 15, 25):
            raise ValueError("SIGRID-3 layer must contain polygons")
        for index, item in enumerate(reader.iterShapeRecords()):
            if item.shape.shapeType == 0:
                continue
            props = properties(item.record.as_dict())
            if props["polygon_type"] == "L":
                continue
            geometry = item.shape.__geo_interface__
            if geometry["type"] not in {"Polygon", "MultiPolygon"}:
                raise ValueError("Chart contains a non-polygon geometry")
            features.append({"type": "Feature", "id": index, "properties": props,
                             "geometry": {"type": geometry["type"], "coordinates": coordinates(geometry["coordinates"])}})
    if not features:
        raise ValueError("Chart has no marine polygons")
    return features, valid_time


def _atomic_write(path: Path, data: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(dir=path.parent, delete=False) as f:
        temporary = Path(f.name)
        try:
            f.write(data)
            f.close()
            temporary.chmod(0o644)
            temporary.replace(path)
        finally:
            temporary.unlink(missing_ok=True)


def import_chart(*, date: str, region: str, url: str | None = None, file: Path | None = None,
                 source_url: str | None = None) -> dict:
    """Validate a chart completely before atomically replacing its cached version."""
    day = Date.fromisoformat(date).isoformat()
    if day != date:
        raise ValueError("Date must use YYYY-MM-DD")
    region = region.strip()
    slug = re.sub(r"[^a-z0-9]+", "-", region.lower()).strip("-")
    if not slug or len(region) > 100:
        raise ValueError("A region name of 1–100 characters is required")
    if bool(url) == bool(file):
        raise ValueError("Supply exactly one URL or local file")
    if source_url and urlparse(source_url).scheme != "https":
        raise ValueError("Source attribution URL must use HTTPS")
    if file and Path(file).suffix.lower() == ".shp":
        path = Path(file)
        companions = [p for p in path.parent.iterdir() if p.stem.lower() == path.stem.lower() and p.suffix.lower() in {".shp", ".shx", ".dbf", ".prj", ".cpg"}]
        if any(p.stat().st_size > MAX_BYTES for p in companions) or sum(p.stat().st_size for p in companions) > MAX_EXPANDED:
            raise ValueError("Shapefile exceeds size limit")
        parts = {p.name.lower(): p.read_bytes() for p in companions}
    else:
        if file and Path(file).stat().st_size > MAX_BYTES:
            raise ValueError("Chart file exceeds size limit")
        parts = _archive_parts(_download(url) if url else Path(file).read_bytes())
    features, valid_time = _convert(parts, day)
    entry = {"id": f"{slug}-{day}", "date": day, "region": region, "valid_time": valid_time,
             "source_url": url or source_url or "https://iceweb1.cis.ec.gc.ca/Archive/page1.xhtml",
             "attribution": ATTRIBUTION, "licence_url": LICENCE, "feature_count": len(features),
             "imported_at": datetime.now(timezone.utc).isoformat()}
    collection = {"type": "FeatureCollection", "chart": entry, "features": features}
    data = json.dumps(collection, separators=(",", ":"), ensure_ascii=False, allow_nan=False).encode()
    _atomic_write(chart_dir() / f"{entry['id']}.geojson", data)
    return entry


def publish(root: Path) -> dict | None:
    """Publish local charts without a network request or GIS dependencies.

    Bundled seeds make a fresh deployment useful before its writable cache is
    populated. A cache file with the same name takes precedence.
    """
    entries = []
    charts = {path.name.removesuffix(".gz"): path for path in SEED_DIR.glob("*.geojson.gz")}
    charts.update({path.name: path for path in chart_dir().glob("*.geojson")})
    for name, path in sorted(charts.items()):
        try:
            data = gzip.decompress(path.read_bytes()) if path.suffix == ".gz" else path.read_bytes()
            collection = json.loads(data)
            entry = collection["chart"]
            if collection["type"] != "FeatureCollection" or not collection["features"]:
                raise ValueError("Empty or invalid chart")
            if not re.fullmatch(r"[a-z0-9-]+", entry["id"]) or Path(name).stem != entry["id"]:
                raise ValueError("Invalid cached chart identifier")
            Date.fromisoformat(entry["date"])
            digest = hashlib.sha256(data).hexdigest()[:16]
            destination = root / "data" / "ice-charts" / name
            if not destination.exists() or destination.read_bytes() != data:
                _atomic_write(destination, data)
            entries.append({**entry, "url": f"data/ice-charts/{name}?v={digest}"})
        except (OSError, ValueError, KeyError, TypeError) as exc:
            log.warning("ice chart %s not published: %s", path.name, exc)
    rasters = {path.name: path for path in SEED_DIR.glob("*.raster.json")}
    rasters.update({path.name: path for path in chart_dir().glob("*.raster.json")})
    for name, metadata in sorted(rasters.items()):
        try:
            entry = json.loads(metadata.read_text())
            if entry.get("kind") != "raster" or not re.fullmatch(r"[a-z0-9-]+", entry["id"]):
                raise ValueError("Invalid raster chart metadata")
            Date.fromisoformat(entry["date"])
            coordinates = entry["coordinates"]
            if len(coordinates) != 4 or any(len(point) != 2 or not all(math.isfinite(float(v)) for v in point) for point in coordinates):
                raise ValueError("Invalid raster chart coordinates")
            image_name = f"{entry['id']}.png"
            if name != f"{entry['id']}.raster.json":
                raise ValueError("Invalid raster chart identifier")
            source_image = metadata.with_name(image_name)
            if not source_image.is_file():
                raise ValueError("Raster chart image is missing")
            data = source_image.read_bytes()
            if not data.startswith(b"\x89PNG\r\n\x1a\n") or len(data) > MAX_BYTES:
                raise ValueError("Invalid raster chart image")
            digest = hashlib.sha256(data).hexdigest()[:16]
            destination = root / "data" / "ice-charts" / image_name
            if not destination.exists() or destination.read_bytes() != data:
                _atomic_write(destination, data)
            entries.append({**entry, "url": f"data/ice-charts/{image_name}?v={digest}"})
        except (OSError, ValueError, KeyError, TypeError, json.JSONDecodeError) as exc:
            log.warning("ice chart %s not published: %s", metadata.name, exc)
    return {"charts": sorted(entries, key=lambda row: (row["date"], row["region"]), reverse=True)} if entries else None
