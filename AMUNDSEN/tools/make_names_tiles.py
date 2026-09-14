#!/usr/bin/env python3
"""Cut the geographic names layer: bays, sounds, straits, islands, capes,
lakes, rivers, glaciers and mountains as vector tiles the map labels.

    make_names_tiles.py [SRC_DIR] [OUT_DIR]     default /data/gis/names/src  /data/gis/tiles/names

Sources, both open and both refreshed by re-downloading into SRC_DIR:

- the Canadian Geographical Names Database, whole-country CSV
  (https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_cgn_toponyme/prov_csv_eng/cgn_canada_csv_eng.zip,
  Open Government Licence - Canada): every official name with its generic
  term, the language it is in, and the map scale it belongs at;
- GeoNames' Greenland dump (https://download.geonames.org/export/dump/GL.zip,
  CC BY 4.0), the same source the settlements layer uses.

Settlements are left out (the Places layer has them); so are roads, parks,
reserves and the like. What remains inside the map's box (150 W to 15 W,
45 N to 86 N) is some 280,000 names, far too many to label at once, so each
gets a *band*: the zoom at which it first appears. CGNDB's "relevance at
scale" gives the band directly (a 1:30,000,000 name shows from zoom 2, a
1:50,000 one from zoom 11); a floor by generic term keeps a cove or a creek
from ever showing far out, whatever the scale field says. Greenland has no
scale field: the band comes from the feature code, promoted a step or two
for names with many alternates (the well-known ones). A feature CGNDB
names in two languages is one label on two lines, the Inuktitut first.

The tiles are one layer per band (z2 ... z12), each cut only from its band's
zoom up, so a far-out tile holds a few dozen names and a close-in one every
name there is. The map draws one symbol layer per band (see mapStyle in
app.js) and MapLibre's collision engine thins whatever still overlaps.
Water names are set in italic, land names upright.

Every feature carries: n (the label, with a newline between two names),
w (1 for water, 0 for land), k (the generic term).

Needs ogr2ogr (GDAL >= 3.1 for the MVT writer's CONF option); on grid it is
in the routing micromamba env. Like the coast tiles the result lives under
UNDERWAY_TILES_DIR on the ship, copied with rsync; the build reads its
metadata.json and the server maps /static/tiles/ onto the directory.
"""

import csv
import json
import math
import os
import re
import shutil
import subprocess
import sys
import tempfile
import zipfile
from collections import defaultdict
from pathlib import Path

SRC_DEFAULT, OUT_DEFAULT = "/data/gis/names/src", "/data/gis/tiles/names"
OGR2OGR = os.environ.get("OGR2OGR") or shutil.which("ogr2ogr") or str(Path.home() / "micromamba/envs/routing/bin/ogr2ogr")

BOX = (-150.0, 45.0, -15.0, 86.0)                 # lon0, lat0, lon1, lat1: the coast tiles' box
BANDS = range(2, 13)                              # a name's band is the zoom it first shows at
MAXZOOM = 10                                      # tiles stop here; the map overzooms them

# ---------------------------------------------------------------- CGNDB
# concise codes kept, and whether they are water
CGN_CODES = {"LAKE": 1, "RIV": 1, "BAY": 1, "CHAN": 1, "RIVF": 1, "SEA": 1, "SEAF": 1,
             "ISL": 0, "MTN": 0, "CAPE": 0, "SHL": 0, "VALL": 0, "BCH": 0, "CLF": 0, "PLN": 0, "GLAC": 0, "GEOG": 0}
# the zoom floor by generic term (English and French), whatever the scale field says
FLOORS = [
    (2, r"\b(sea|ocean|mer|oc[ée]an|gulf|golfe)\b"),
    (4, r"\b(strait|d[ée]troit|sound|channel|chenal|passage|basin|bassin|peninsula|presqu'?[îi]le|p[ée]ninsule|"
        r"islands|[îi]les|archipelago|archipel|region|r[ée]gion|plateau|highlands|lowlands|mountains|monts|range|cha[îi]ne)\b"),
    (5, r"\b(bay|baie|island|[îi]le|fiord|fjord|inlet|ice ?cap|icefield|calotte)\b"),
    (7, r"\b(lake|lac|river|rivi[èe]re|fleuve|glacier|mountain|mont|peak|pic|cape|cap|head|plain|plaine|arm|bras|reach)\b"),
    (9, r"\b(point|pointe|cove|anse|harbour|havre|creek|ruisseau|pond|[ée]tang|rock|rocher|rocks|rochers|islet|[îi]lot|"
        r"shoal|haut-fond|reef|r[ée]cif|hill|colline|valley|vall[ée]e|cliff|falaise|beach|plage|narrows|rapids|rapides|"
        r"brook|bight|lagoon|lagune|ledge|bank|banc|spit|flèche)\b"),
]
FLOORS = [(z, re.compile(p, re.I)) for z, p in FLOORS]
FRENCH = re.compile(r"^(Baie|D[ée]troit|Mer|[ÎI]le|[ÎI]les|Lac|Rivi[èe]re|Golfe|Chenal|Cap|Pointe|Anse|Havre|Presqu'[îi]le|P[ée]ninsule|Bassin|Passage|Fleuve|Mont|Monts|Glacier|Archipel|Rocher|Ruisseau|Plage|Colline|Vall[ée]e|Falaise|Plaine)\b")


def floor_of(generic: str, default: int = 8) -> int:
    for z, rx in FLOORS:
        if rx.search(generic):
            return z
    return default


def band_of_scale(scale: int) -> int:
    """The zoom at which a name relevant at 1:scale first shows: the map's
    scale at 70 N is about 1.9e8 / 2^zoom, and a name comes in one zoom
    before its own scale so the collision engine has something to thin."""
    if not scale:
        return BANDS[-1]
    return max(BANDS[0], min(BANDS[-1], round(math.log2(1.9e8 / scale)) - 1))


def in_box(lat: float, lon: float) -> bool:
    return BOX[1] <= lat <= BOX[3] and BOX[0] <= lon <= BOX[2]


def read_cgndb(path: Path):
    """One feature per toponymic feature id: its names (Inuktitut and other
    Indigenous-language names first), the band, water or land, the generic."""
    feats: dict[str, dict] = {}
    with open(path, encoding="utf-8-sig", newline="") as f:
        for r in csv.DictReader(f):
            code = r["Concise Code"]
            if code not in CGN_CODES:
                continue
            lat, lon = float(r["Latitude"]), float(r["Longitude"])
            if not in_box(lat, lon):
                continue
            generic = r["Generic Term"]
            scale = int(r["Relevance at Scale"] or 0)
            # the scale field is trusted outright for the few names at 1:10M and
            # coarser (Hudson Bay, Baffin Island); below that the generic's floor
            # holds, since some provinces tag a cove or a rock at 1:7.5M
            band = band_of_scale(scale) if scale >= 10_000_000 else max(band_of_scale(scale), floor_of(generic))
            indigenous = r["Language"] not in ("Undetermined", "Uncoded languages", "English", "French", "")
            fid = r["Toponymic Feature ID"] or r["CGNDB ID"]
            ft = feats.setdefault(fid, {"names": [], "lat": lat, "lon": lon, "band": band, "w": CGN_CODES[code], "k": generic})
            ft["names"].append((0 if indigenous else 1, r["Geographical Name"]))
            ft["band"] = min(ft["band"], band)
    for ft in feats.values():
        seen, names = set(), []
        for _, n in sorted(ft["names"], key=lambda x: (x[0], 1 if FRENCH.search(x[1]) else 0, x[1])):
            if n.lower() not in seen:
                seen.add(n.lower()); names.append(n)
        # two lines only for an Indigenous-language name over the English one;
        # an English/French pair shows the English (the French where there is no other)
        ft["n"] = "\n".join(names[:2]) if ft["names"] and min(ft["names"])[0] == 0 and len(names) > 1 else names[0]
    return list(feats.values())


# ---------------------------------------------------------------- GeoNames (Greenland)
# feature code -> (zoom floor, water)
GN_CODES = {
    "SEA": (2, 1), "OCN": (2, 1), "GULF": (2, 1),
    "AREA": (5, 0), "RGN": (5, 0), "CST": (5, 0), "PEN": (5, 0), "ISLS": (5, 0), "PLAT": (5, 0), "MTS": (5, 0),
    # GeoNames files every little ikerasak (strait) of the Greenland coast as a channel: they come in with the bays
    "STRT": (6, 1), "SD": (6, 1), "CHN": (6, 1), "CHNM": (6, 1), "CHNL": (6, 1),
    "BAY": (6, 1), "FJD": (6, 1), "FJDS": (6, 1), "INLT": (7, 1), "LK": (6, 1), "LKS": (6, 1),
    "ISL": (6, 0), "GLCR": (6, 0), "UPLD": (6, 0), "CAPE": (6, 0),
    "STM": (8, 1), "MT": (8, 0), "PK": (8, 0), "PT": (8, 0), "NTK": (8, 0), "VAL": (8, 0), "HDLD": (8, 0), "PROM": (8, 0), "PLN": (8, 0),
    "COVE": (9, 1), "HBR": (9, 1), "RK": (9, 0), "RKS": (9, 0), "HLL": (9, 0), "CLF": (9, 0), "RDGE": (9, 0), "GRGE": (9, 0),
    "SLP": (9, 0), "RVN": (9, 0), "ISLT": (9, 0),
}
# the generic words either side of the strait: what is left of a name is its
# specific, and a Greenland feature whose specific CGNDB already names nearby
# (Nares Sund beside Nares Strait) is left to CGNDB
GENERIC = re.compile(r"\b(Sund|Sundet|Stræde|Strædet|Strait|Sound|Bugt|Bugten|Bay|Fjord|Fjorden|Bassin|Basin|Hav|Sea|Kanal|Channel|Bredning|Land|Halvø|Ø|Øer|Island|Islands|Kap|Cape|Gletscher|Glacier|Isbræ)\b", re.I)
specific = lambda n: GENERIC.sub("", n).replace("  ", " ").strip().lower()
# GeoNames has no scale field, so the Greenland names the wider world knows
# are promoted by hand to the band they belong at
PROMOTE = {"Nares Stræde": 4, "Kennedy Kanal": 5, "Robeson Kanal": 5, "Smith Sund": 5, "Kane Bassin": 5, "Melville Bugt": 3, "Disko Bugt": 4, "Scoresby Sund": 4, "Kangerlussuaq": 5, "Uummannaq Fjord": 5, "Inglefield Bredning": 5,
           "Independence Fjord": 4, "Kong Oscar Fjord": 5, "Kejser Franz Joseph Fjord": 5, "Peary Land": 4, "Nuussuaq": 5,
           "Wolstenholme Fjord": 6, "Qeqertarsuaq": 5, "Hall Bredning": 5, "Danmark Fjord": 5, "Nordostrundingen": 5, "Kap Morris Jesup": 4,
           "Kap Farvel": 4, "Kap York": 5, "Kap Alexander": 5, "Sermersuaq": 5, "Jakobshavn Isbræ": 5, "Petermann Gletscher": 5,
           "Humboldt Gletscher": 5, "Steensby Land": 5, "Washington Land": 5, "Inglefield Land": 5, "Prudhoe Land": 5, "Hayes Halvø": 5}
GN_NAMES = {"SEA": "Sea", "OCN": "Ocean", "GULF": "Gulf", "STRT": "Strait", "SD": "Sound", "CHN": "Channel", "CHNM": "Channel", "CHNL": "Channel",
            "PEN": "Peninsula", "ISLS": "Islands", "AREA": "Area", "CST": "Coast", "RGN": "Region", "PLAT": "Plateau", "MTS": "Mountains",
            "CAPE": "Cape", "BAY": "Bay", "FJD": "Fjord", "FJDS": "Fjords", "INLT": "Inlet", "LK": "Lake", "LKS": "Lakes", "ISL": "Island",
            "GLCR": "Glacier", "UPLD": "Upland", "STM": "River", "MT": "Mountain", "PK": "Peak", "PT": "Point", "NTK": "Nunatak", "VAL": "Valley",
            "HDLD": "Headland", "PROM": "Promontory", "PLN": "Plain", "COVE": "Cove", "HBR": "Harbour", "RK": "Rock", "RKS": "Rocks",
            "HLL": "Hill", "CLF": "Cliff", "RDGE": "Ridge", "GRGE": "Gorge", "SLP": "Slope", "RVN": "Ravine", "ISLT": "Islet"}


def read_geonames(path: Path, taken=None):
    """Greenland's names; a feature CGNDB already names in the same place
    (Nares Strait and Nares Stræde, Smith Sound and Smith Sund) is left to
    CGNDB, whose scale field ranks it."""
    feats = []
    taken = taken or {}
    with open(path, encoding="utf-8", newline="") as f:
        for row in csv.reader(f, delimiter="\t", quoting=csv.QUOTE_NONE):
            fcode = row[7]
            if fcode not in GN_CODES:
                continue
            lat, lon = float(row[4]), float(row[5])
            if not in_box(lat, lon):
                continue
            floor, water = GN_CODES[fcode]
            names = [row[1], row[2], *row[3].split(",")]
            keys = {n.lower() for n in names} | {specific(n) for n in names if len(specific(n)) >= 4}
            if any(abs(lat - la) < 3 and abs(lon - lo) < 6 for k in keys for la, lo in taken.get(k, ())):
                continue
            band = PROMOTE.get(row[1], floor)
            feats.append({"n": row[1], "lat": lat, "lon": lon, "band": band, "w": water, "k": GN_NAMES[fcode]})
    return feats


# ---------------------------------------------------------------- the tiles

def write_band_files(feats, work: Path) -> dict:
    counts = defaultdict(int)
    handles = {z: open(work / f"z{z}.geojsonl", "w", encoding="utf-8") for z in BANDS}
    try:
        for ft in feats:
            z = ft["band"]
            handles[z].write(json.dumps({"type": "Feature", "geometry": {"type": "Point", "coordinates": [round(ft["lon"], 5), round(ft["lat"], 5)]},
                                         "properties": {"n": ft["n"], "w": ft["w"], "k": ft["k"]}}, ensure_ascii=False) + "\n")
            counts[z] += 1
    finally:
        for h in handles.values():
            h.close()
    return counts


def run(*args):
    subprocess.run([str(a) for a in args], check=True)


def main(argv=None):
    argv = sys.argv[1:] if argv is None else argv
    SRC = Path(argv[0] if len(argv) > 0 else SRC_DEFAULT)
    OUT = Path(argv[1] if len(argv) > 1 else OUT_DEFAULT)
    cgn = next(SRC.glob("cgn_canada_csv_eng.csv"), None)
    if cgn is None:
        z = SRC / "cgn_canada_csv_eng.zip"
        if not z.is_file():
            sys.exit(f"no CGNDB csv or zip under {SRC}; see the docstring for the download")
        zipfile.ZipFile(z).extractall(SRC); cgn = SRC / "cgn_canada_csv_eng.csv"
    gl = SRC / "GL.txt"
    if not gl.is_file():
        z = SRC / "GL.zip"
        if not z.is_file():
            sys.exit(f"no GL.txt or GL.zip under {SRC}; see the docstring for the download")
        zipfile.ZipFile(z).extract("GL.txt", SRC)
    feats = read_cgndb(cgn)
    taken = defaultdict(list)
    for ft in feats:
        if ft["band"] <= 7:
            for n in ft["n"].split("\n"):
                taken[n.lower()].append((ft["lat"], ft["lon"]))
                if ft["w"] and len(specific(n)) >= 4:
                    taken[specific(n)].append((ft["lat"], ft["lon"]))
    feats += read_geonames(gl, taken)
    print(f"{len(feats)} names in the box")
    with tempfile.TemporaryDirectory(prefix="names.") as tmp:
        work = Path(tmp)
        counts = write_band_files(feats, work)
        print("  by band:", " ".join(f"z{z}:{counts[z]}" for z in BANDS))
        gpkg = work / "names.gpkg"
        bands = [z for z in BANDS if counts[z]]                       # ogr2ogr cannot open an empty file
        for z in bands:
            run(OGR2OGR, "-q", "-f", "GPKG", *(["-update"] if gpkg.exists() else []), gpkg, work / f"z{z}.geojsonl", "-nln", f"z{z}", "-a_srs", "EPSG:4326")
        conf = {f"z{z}": {"minzoom": min(z, MAXZOOM), "maxzoom": MAXZOOM} for z in bands}
        out_tmp = work / "names"
        run(OGR2OGR, "-q", "-f", "MVT", out_tmp, gpkg, "-dsco", f"MINZOOM={BANDS[0]}", "-dsco", f"MAXZOOM={MAXZOOM}", "-dsco", "COMPRESS=NO",
            "-dsco", f"CONF={json.dumps(conf)}", "-dsco", "NAME=names", "-dsco", "DESCRIPTION=geographic names, CGNDB and GeoNames")
        if OUT.exists():
            shutil.rmtree(OUT)
        OUT.parent.mkdir(parents=True, exist_ok=True)
        shutil.move(str(out_tmp), str(OUT))
    n = sum(1 for _ in OUT.rglob("*.pbf"))
    print(f"wrote {n} tiles to {OUT}")


if __name__ == "__main__":
    main()
