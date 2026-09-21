# Canadian Ice Service chart overlay

The map displays locally cached CIS charts with a selectable region and chart
date. Weekly SIGRID-3 polygons provide concentration colours and clickable
egg-code details. The Eureka daily product is a georeferenced raster for the
ship's current operating area. Chart validity is separate from the camera
capture date, and charts do not classify individual camera images.

## Refresh the ship-area daily chart

Run from `AMUNDSEN/` with the same `UNDERWAY_DB_DIR` used by the dashboard:

```bash
python -m dashboard ice-charts --daily WIS36C
python -m dashboard build --root /path/to/webroot
```

The command discovers the latest Eureka colour chart, downloads its vector PDF,
and reads the projection, affine transform, and map neatline embedded by the CIS
ArcGIS export. It renders the exact map frame at 600 DPI and warps it into a
3600-pixel-wide Web Mercator PNG. Conversion requires the `gdal_translate`,
`gdalwarp`, and `gdalinfo` commands. The importer rejects a PDF without the
expected embedded polar stereographic map. It writes a PNG and a
metadata sidecar under `$UNDERWAY_DB_DIR/ice-charts/`; an incomplete download or
failed conversion does not replace a cached chart. Ordinary dashboard builds
remain offline.

The published 3600-pixel source remains available to capable browsers. Before
uploading it to MapLibre, the browser fits the decoded image to its WebGL maximum
texture size while preserving its aspect ratio and geographic corners. This
keeps the overlay visible on devices limited to 2048-pixel textures.

Fresh deployments include a bundled daily chart covering the ship's area, so
the map has something to draw before anything is fetched.

## Keep the charts current

`underway-ice-charts.timer` runs `python -m dashboard ice-charts --refresh`
four times a day. A refresh reads the Ice Service directory, fetches the
newest weekly chart for each region the ship works in (Eastern Arctic, Hudson
Bay, Western Arctic) and the daily raster for the ship's own area, and leaves
alone anything already cached. The dashboard's own build publishes what is in
the cache on its next pass, so the timer writes no web root.

A region the Ice Service has not posted, or a download that fails, is logged
and skipped: the rest of the run continues and the last good chart stays on
the map. The unit reports failure only when a run cached and confirmed
nothing at all.

Conversion needs the optional dependencies and the GDAL commands:
`pip install ".[ice-charts]"`, plus `gdal_translate`, `gdalwarp` and
`gdalinfo` for the daily raster.

## Import a chart

Run from `AMUNDSEN/` with the same `UNDERWAY_DB_DIR` used by the dashboard build:

```bash
pip install '.[ice-charts]'
python -m dashboard ice-charts --list
python -m dashboard ice-charts \
  --url https://ice-glaces.ec.gc.ca/prods/sigrids/cis_SGRDREA_20260907T1800Z_pl_a.tar \
  --date 2026-09-07 --region 'Eastern Arctic'
python -m dashboard build --root /path/to/webroot
```

Use a URL returned by `--list`: the public directory retains a rolling set of
recent products, so the dated example will eventually expire. Listing downloads
only the directory index. Importing downloads only the specified archive. No
scheduled fetching is installed, and ordinary dashboard builds work offline.
The importer uses HTTPS with a 45-second request timeout, caps archives at 64 MiB
and their expanded content at 256 MiB, and does not extract archive paths.

For older cruise dates, download **Shapefile / SIGRID-3** from the
[CIS archive](https://iceweb1.cis.ec.gc.ca/Archive/page1.xhtml), then import one
chart per invocation. ZIP, TAR, and a local SHP with SHX, DBF, and PRJ companions
are supported. Other vector formats, including E00, must be converted separately.

```bash
python -m dashboard ice-charts \
  --file /path/to/chart.zip --date 2025-08-18 --region 'Eastern Arctic' \
  --source-url https://iceweb1.cis.ec.gc.ca/Archive/page1.xhtml
```

`--date` is the chart valid date, not the download date. CIS filename dates are
checked against it; validity times in filenames (such as `T1800Z`) are retained.
Local files without dated CIS names require the operator to supply the correct
valid date and region. Archives must contain exactly one polygon shapefile.
The `.prj` projection is mandatory and converted with pyproj to WGS84 longitude /
latitude. Holes and multipart polygons are retained; land polygons are omitted.

## Cache and publication

Fresh deployments include six compressed vector seed charts for Eastern Arctic,
Western Arctic, and Hudson Bay dated 2026-08-31 and 2026-09-07, plus the Eureka
daily raster described above. Cached charts with matching identifiers replace
the seeds, so normal imports can update them without changing application code.

`$UNDERWAY_DB_DIR/ice-charts/<region>-<date>.geojson` contains each vector chart
and its source metadata. Daily rasters use a `.png` plus `.raster.json` sidecar.
A successful reimport atomically replaces that region/date; a failed conversion
leaves the existing chart untouched. Builds copy valid charts to
`data/ice-charts/` and include them in the `ice_charts.charts` manifest entry,
with content-based URL cache busting. Corrupt cache entries are logged and
skipped. Publication does not need the conversion tools.

GeoJSON preserves SIGRID-3 codes as strings, including leading zeros and `-9`
missing values. `concentration` is a numeric value in **tenths**, used only for
colour. Concentration interval codes use the interval midpoint; open water
(`01`) uses 0.5, bergy water (`02`) uses 0, `91` uses 9.5, and `92` uses 10.
Human-readable labels preserve these distinctions. Missing/unknown data use
`null`, and no-data polygons are not presented as ice-free water. Raw stage and
form codes are separate from converted `egg_*` symbols; missing values show a
dash and unknown or unsupported egg symbols show `X`. The decoded descriptions
and raw codes remain available for forms without a basic egg-symbol equivalent.
The oval represents the three main SIGRID ice types; extra CN/CD/CF fields are
retained in GeoJSON and raw details, rather than inferred into an expanded five-type egg.
CN trace ice thicker than the main type is also described below the oval; composite
CF codes remain raw.

CIS charts carry Canadian Ice Service / ECCC attribution and an
[Open Government Licence – Canada](https://open.canada.ca/en/open-government-licence-canada)
link. A browser viewing the cached overlay does not contact CIS.

## Verified sources

- [CIS current SIGRID archives](https://ice-glaces.ec.gc.ca/prods/sigrids/): a live
  Eastern Arctic regional TAR dated 2026-09-07 contains Lambert Conformal Conic
  SHP/SHX/DBF/PRJ/XML data, 239 polygons (110 marine, 129 land), and the standard
  CT/CA/CB/CC/SA/SB/SC/FA/FB/FC/CN/CD/CF/POLY_TYPE fields.
- [CIS archive overview](https://www.canada.ca/en/environment-climate-change/services/ice-forecasts-observations/latest-conditions/archive-overview.html).
- [SIGRID-3 specification, WMO/TD-No. 1214](https://globalcryospherewatch.org/wordpress/wp-content/themes/global-cryosphere-watch/files/resources/JCOMM_TR23_SIGRID3.pdf),
  mandatory fields and Appendix 5 code tables.
- [Coast Guard ice navigation guide, tables 10 and 12](https://www.canada.ca/en/canadian-coast-guard/corporate/publications/ice-navigation-in-canadian-waters/chapter-4-navigation-ice-covered-waters.html),
  egg stage and floe-size symbols.

## Verification

```bash
python -m unittest discover -s tests -p test_ice_charts.py -v
```

The conversion tests require `.[ice-charts]`; decoding and offline publication
tests run without them. Tests cover projection, polygon holes, archive formats,
unsafe members, size bounds, missing projections, mismatched dates, preservation
of a good cache after failed imports, concentration special codes, and discovery.

## Map controls

Open **Ice charts** above the map, choose a region, and leave the date on
**Latest on/before map end** to use the most recent cached chart valid at or
before the last visible track observation. An explicit date choice can show a
newer chart, but the status identifies it as after the map end. Charts remain
separate from the ship's camera-derived ice observations and track colours.
For vectors, the concentration legend is in tenths; colour is a summary, and
clicking a polygon shows its decoded ice types and original source values.
Source SIGRID codes are available in an expandable section. The daily raster
retains the printed egg letters and chart colours but is not clickable. The
opacity slider keeps the ship's track and map coastline readable.

The dashboard does not fetch chart data until the overlay is enabled. Each
region/date loads from its published local GeoJSON or PNG; a failed load clears
the previous layer and offers **Retry chart**. Changing a basemap or theme
preserves the selection. An empty cache shows a clear no-charts message.

Additional browser checks, using Node 22+ and Chrome:

```bash
node --test tests/ice-charts.test.cjs tests/map-satellite.test.cjs tests/map-legend.test.cjs
node tests/ice-charts-browser.cjs /usr/bin/google-chrome
# Full dashboard check against the imported 2026-09-07 Eastern Arctic chart:
ICE_CHART_FIXTURE=/path/to/eastern-arctic-2026-09-07.geojson \
  PYTHON=python3 node tests/refresh-browser.cjs /usr/bin/google-chrome
```

The standalone browser test exercises real MapLibre vector and raster
rendering, date selection, cancellation, failure/retry, toggling,
station-click precedence, style changes and safe display of source text. The
full dashboard test also checks mobile and desktop control widths against a
real converted CIS chart.
