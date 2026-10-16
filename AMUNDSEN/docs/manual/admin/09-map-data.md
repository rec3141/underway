# Map data

The map works with nothing but what is committed: a Natural Earth basemap
under `dashboard/static/geo/`. Everything else in this chapter is optional
and lives outside the repository in `UNDERWAY_TILES_DIR`
(`/data/gis/tiles`). When a set exists there, the next build finds it and
the map uses it; when it does not, the map falls back to the bundled layers.

| Directory under `UNDERWAY_TILES_DIR` | What | Built by | Built where |
|---|---|---|---|
| `gebco/` | shaded relief, the map's **Relief** | `tools/rerender-gebco.sh` | ship or grid |
| `gebco-<ramp>/` | the same relief in another colour ramp (`gebco-ibcao`, `gebco-oslo`) | `make_gebco_tiles.sh` with `RAMP=` | ship or grid |
| `survey/` | where the depths came from (surveyed or interpolated) | `tools/make_survey_tiles.sh` | ship or grid |
| `coast/` | a finer coastline and land (vector tiles) | `ogr2ogr` | grid, then `tiles-pull.sh` |
| `names/` | geographic names (vector tiles) | `tools/make_names_tiles.py` | grid, then `tiles-pull.sh` |
| `sea-grid/` | the grid the sea-route distances walk | `tools/make_sea_grid.sh` | ship or grid |

The page server maps `/static/tiles/` onto `UNDERWAY_TILES_DIR` and serves
raster tiles with a week-long cache. None of this is committed: the relief
pyramid alone is a few GB.

The map's **Bathy** button cycles through the relief sets present on disk.
A renderer stages into `<name>.new` and keeps what it replaced as
`<name>.old`; neither is offered.

## The bundled basemap

`dashboard/static/geo/*.geojson` is Natural Earth 10 m (coastline, land,
islands, glaciers, depth bands) clipped to the western Arctic. Regenerate it
with `ogr2ogr` if the ship works somewhere else. Without a `gebco/` set the
map draws these depth bands; without a `coast/` set it draws this coastline.

The **Places** layer (settlements of Nunavut, the NWT, Labrador, northern
Québec, Ontario and Manitoba, and Greenland) is from GeoNames (CC BY 4.0)
with a curated list of Inuit, Greenlandic and older names. To refresh it,
download `CA.zip` and `GL.zip` from geonames.org into `/data/gis/geonames/`
and run `tools/make_communities.py`.

## GEBCO relief

`tools/make_gebco_tiles.sh` turns the GEBCO 2024 GeoTIFF release
(`gebco_2024_sub_ice_topo_geotiff.zip`, 4.4 GB from BODC/CEDA) into a
shaded-relief Web Mercator tile pyramid. The ship's pyramid is the globe at
zooms 0–8 (610 m per pixel, about what GEBCO's 15″ grid supports), with the
western Arctic and Labrador Sea at zoom 9 on top. `tools/rerender-gebco.sh`
builds both into a staging directory and swaps it in:

```sh
tools/rerender-gebco.sh gebco_2024_sub_ice_topo_geotiff.zip        # -> $UNDERWAY_TILES_DIR/gebco
```

That is equivalent to two runs into one directory:

```sh
tools/make_gebco_tiles.sh gebco_2024_sub_ice_topo_geotiff.zip \
    "$UNDERWAY_TILES_DIR/gebco" -180 -90 180 90 0-8      # the globe, about 6 min on 32 cores
tools/make_gebco_tiles.sh gebco_2024_sub_ice_topo_geotiff.zip \
    "$UNDERWAY_TILES_DIR/gebco" -150 45 -15 86 9-9       # the Arctic box at z9
```

| Setting (environment) | Default | What |
|---|---|---|
| `RAMP` | `tools/ramps/lapaz.txt` | the depth colour ramp (a `gdaldem` colour file); others are in `tools/ramps/` |
| `LAND`, `LAND_LAYER`, `LAND_BBOX` | | OSM land polygons (layer, default `land`) and their box: the shore inside the box comes from them instead of GEBCO's zero contour, so a strait they keep open stays open |
| `ARCTIC_BBOX` | `-150 45 -15 86` | `rerender-gebco.sh`: the zoom-9 box |
| `GDAL_PYTHON` | `/usr/bin/python3` | the interpreter with GDAL's Python bindings |
| `TMPDIR` | `/tmp` | room for intermediate rasters: tens of GB |

Needs GDAL 3.4 or later with Python bindings (`gdal-bin python3-gdal` on
Ubuntu). The previous pyramid stays as `gebco.old` until the next run.

The build reads the directory and gives each run of zooms to the map as its
own source, the boxed run with its bounds, so the map never asks for a tile
that is not there.

A relief in another ramp goes beside it as `gebco-<ramp>/`:

```sh
RAMP=tools/ramps/oslo.txt tools/rerender-gebco.sh gebco_2024_sub_ice_topo_geotiff.zip \
    "$UNDERWAY_TILES_DIR/gebco-oslo"
```

### The survey chart

`tools/make_survey_tiles.sh` draws GEBCO's Type Identifier grid (where each
depth came from) as a flat tile pyramid in `survey/`:

```sh
tools/make_survey_tiles.sh GEBCO_2024_TID.nc "$UNDERWAY_TILES_DIR/survey"   # default box, zooms 2-8
```

## Sea-route distances

A waypoint's box gives the distance from the ship by sea and the depth
there. Both come from `dashboard/searoute.py`
(`GET /api/searoute?to=lat,lon[&from=lat,lon]`), which reads a grid built
once from the same GEBCO release:

```sh
tools/make_sea_grid.sh gebco_2024_sub_ice_topo_geotiff.zip \
    "$UNDERWAY_TILES_DIR/sea-grid"          # the western Arctic and Labrador Sea, about 4 min
```

| Setting | Default | What |
|---|---|---|
| `LAND`, `LAND_LAYER` | | the shore polygons the map draws (reprojected to EPSG:3413 first: GDAL does not reproject while rasterising); land then comes from them rather than GEBCO |
| `TID` | | GEBCO's Type Identifier grid: each cell also records whether its depth was surveyed |
| `GDAL_PYTHON` | `/usr/bin/python3` | the interpreter with GDAL and numpy |
| `UNDERWAY_SEA_GRID` | `$UNDERWAY_TILES_DIR/sea-grid` | where the server reads it |

The grid is `elevation.npy`, `water.npy`, `source.npy` and `grid.json` on a
polar stereographic plane (EPSG:3413) at 250 m: about 31,000 × 26,000 cells,
1.7 GB, memory-mapped. A cell is water when the lowest GEBCO sample in it is
below sea level, so a channel about a cell wide stays open. The server picks
up a new grid without a restart.

The route itself needs `scikit-fmm` (`pip install -e '.[routing]'`, built
from source). Without it, or without the grid, the box shows the distance by
air only.

How the route is chosen, for when someone asks:

- it is the fast-marching solution over the water, so open water gives a
  straight line within a few tens of metres of the great circle;
- surveyed water counts as faster by a third, tapering over 2 km, so routes
  follow ship tracks where they go the same way (for one to three percent
  more distance);
- water shallower than 100 m and water within 5 km of land are slowed, which
  holds a route about 5 km off a headland. These are survey margins, not
  keel margins: GEBCO is least trustworthy in the shallows and near shore.

A route is an estimate for planning, not a track to steer; the depth is
GEBCO's for that cell, not a sounding.

## A finer coastline

The Natural Earth coastline is generalised to about a kilometre. A vector
tile set cut from the OpenStreetMap land polygons and coastline
(osmdata.openstreetmap.de, ODbL) replaces it when `coast/` exists. It is built
on grid, where the 1.3 GB source and GDAL live:

```sh
ogr2ogr -t_srs EPSG:3857 -clipdst <box in metres> -nlt MULTILINESTRING coast.gpkg coastlines-split-4326/lines.shp -nln coast
ogr2ogr -update -t_srs EPSG:3857 -clipdst <box in metres> -nlt MULTIPOLYGON coast.gpkg land-polygons-split-4326/land_polygons.shp -nln land
ogr2ogr -f MVT coast coast.gpkg -dsco MINZOOM=0 -dsco MAXZOOM=10 -dsco COMPRESS=NO
```

The shoreline must come from OSM's coastline lines, not from the outline of
the land polygons: the polygons are split into a grid, and their outlines
include every cut. The map draws the `coast` layer as the shoreline and,
without a relief raster, the `land` layer as land. Glaciers and depth bands
stay Natural Earth. The build reads the set's `metadata.json` for zooms,
bounds and layers.

## Geographic names

`tools/make_names_tiles.py` cuts the geographic names of the map's area as
vector tiles: every official name in the Canadian Geographical Names Database
(Open Government Licence – Canada) and GeoNames' Greenland dump (CC BY 4.0),
less settlements (the **Places** layer has them), roads, parks and reserves.
Built on grid:

```sh
mkdir -p /data/gis/names/src && cd /data/gis/names/src
curl -O https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_cgn_toponyme/prov_csv_eng/cgn_canada_csv_eng.zip
curl -O https://download.geonames.org/export/dump/GL.zip
tools/make_names_tiles.py /data/gis/names/src /data/gis/tiles/names     # about 30 s, 34,000 tiles, 140 MB
```

When `names/` exists the map shows a **Names** pill. Known gaps: no names for
the Alaskan part of the box, and Greenland names are GeoNames' primary form,
often the Danish one.

The label fonts are in `dashboard/static/geo/glyphs/` (Open Sans Regular and
Italic, built with `fontnik` for the four Latin ranges 0–1023).

## Copying tile sets from grid

Grid cannot reach the ship, so the ship pulls:

```sh
tools/tiles-pull.sh            # every set grid has (each directory with a metadata.json)
tools/tiles-pull.sh names      # one set
tools/tiles-pull.sh status     # what is here and on grid
```

Each set lands whole (copied to a sibling directory, then swapped by one
rename), so a build never reads a half-copied set. Grid's tiles directory is
`/data/gis/tiles` unless `UNDERWAY_TILES_REMOTE` (host:path) says otherwise.
Raster pyramids (`gebco*`, `survey`) are not pulled: render them where they
are used, or copy them with rsync.

The public web copy draws grid's own tiles ([Publishing to the web](public-web.html#map-tiles)).
