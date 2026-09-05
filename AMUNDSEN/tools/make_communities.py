#!/usr/bin/env python3
"""Build static/geo/communities.geojson from the GeoNames country dumps.

    make_communities.py [GEONAMES_DIR]      default /data/gis/geonames

Reads CA.zip and GL.zip (https://download.geonames.org/export/dump/, CC BY 4.0;
refresh by re-downloading the zips and re-running) and keeps the populated
places of the Amundsen's working area: Nunavut, the Northwest Territories,
Labrador and the northern shores of Quebec, Ontario and Manitoba, and all of
Greenland. Each feature carries the name, up to three Latin-script alternate
names from a curated list (Inuit, Greenlandic and older colonial names),
population, region and country. The output is small (a few hundred places)
and is served locally with the other geo layers.
"""

import json
import re
import sys
import zipfile
from pathlib import Path

SRC = Path(sys.argv[1] if len(sys.argv) > 1 else "/data/gis/geonames")
OUT = Path(__file__).resolve().parents[1] / "dashboard" / "static" / "geo" / "communities.geojson"

ADMIN_CA = {"01": "AB", "02": "BC", "03": "MB", "04": "NB", "05": "NL", "07": "NS", "08": "ON", "09": "PE", "10": "QC",
            "11": "SK", "12": "YT", "13": "NT", "14": "NU"}
ADMIN_GL: dict[str, str] = {}                              # filled from the ADM1 records in GL.txt
# GeoNames' alternate-name column carries no language tags and is mostly
# transliteration noise, so the Inuit and Greenlandic (and older Danish) names
# shown beside the official ones come from this list instead.
ALT_NAMES = {
    "Pond Inlet": ["Mittimatalik"], "Arctic Bay": ["Ikpiarjuk"], "Resolute": ["Qausuittuq"], "Grise Fiord": ["Aujuittuq"],
    "Clyde River": ["Kangiqtugaapik"], "Pangnirtung": ["Panniqtuuq"], "Kimmirut": ["Lake Harbour"], "Kinngait": ["Cape Dorset"],
    "Cape Dorset": ["Kinngait"], "Sanirajak": ["Hall Beach"], "Hall Beach": ["Sanirajak"], "Igloolik": ["Iglulik"], "Naujaat": ["Repulse Bay"],
    "Coral Harbour": ["Salliq"], "Rankin Inlet": ["Kangiqliniq"], "Chesterfield Inlet": ["Igluligaarjuk"], "Baker Lake": ["Qamani'tuaq"],
    "Whale Cove": ["Tikirarjuaq"], "Gjoa Haven": ["Uqsuqtuuq"], "Cambridge Bay": ["Iqaluktuuttiaq"], "Kugluktuk": ["Coppermine"],
    "Kugaaruk": ["Pelly Bay"], "Taloyoak": ["Spence Bay"], "Iqaluit": ["Frobisher Bay"], "Nain": ["Nunainguk"], "Hopedale": ["Agvituk"],
    "Rigolet": ["Tikigâkkik"], "Kuujjuaq": ["Fort Chimo"], "Kangiqsualujjuaq": ["George River"], "Kuujjuarapik": ["Great Whale River"],
    "Inukjuak": ["Port Harrison"], "Puvirnituq": ["Povungnituk"], "Salluit": ["Sugluk"], "Kangiqsujuaq": ["Wakeham Bay"],
    "Nuuk": ["Godthåb"], "Ilulissat": ["Jakobshavn"], "Qaanaaq": ["Thule"], "Aasiaat": ["Egedesminde"], "Sisimiut": ["Holsteinsborg"],
    "Qeqertarsuaq": ["Godhavn"], "Maniitsoq": ["Sukkertoppen"], "Paamiut": ["Frederikshåb"], "Qaqortoq": ["Julianehåb"],
    "Tasiilaq": ["Ammassalik"], "Ittoqqortoormiit": ["Scoresbysund"], "Upernavik": [], "Uummannaq": [], "Kangerlussuaq": ["Søndre Strømfjord"],
    "Narsaq": [], "Nanortalik": [], "Kullorsuaq": [], "Savissivik": [], "Siorapaluk": [], "Pituffik": ["Thule Air Base"],
}
SKIP_CODES = {"PPLQ", "PPLH", "PPLW", "PPLX", "PPLCH"}       # abandoned, historical, destroyed, sections of places
LATIN = re.compile(r"^[A-Za-zÀ-ÿʼ'’\-\. ]+$")
# alternates worth showing: capitalised words, not airport codes or
# transliterations (Cyrillic/pinyin romanisations are lower-case or carry 'j/ʹ)
def good_alt(a: str, name: str) -> bool:
    if not LATIN.match(a) or a.lower() == name.lower() or len(a) < 4:
        return False
    if a.isupper() or not a[0].isupper() or "'j" in a.lower() or "ʹ" in a:
        return False
    words = a.split()
    return all(w[0].isupper() or w in ("de", "la", "du", "des", "of", "the", "and", "og") for w in words)


def keep_ca(lat, lon, admin):
    if admin in ("14", "13"):                                  # Nunavut, NWT: everything
        return True
    if admin == "05":                                          # Labrador (not the island)
        return lat >= 51.0 and lon <= -55.0
    if admin in ("10", "08", "03"):                            # Hudson Bay / Ungava / Hudson Strait shores
        return lat >= 55.0
    return False


def rows(zip_path, txt):
    with zipfile.ZipFile(zip_path) as z, z.open(txt) as f:
        for line in f:
            yield line.decode("utf-8").rstrip("\n").split("\t")


features = []
for cc, zp, keep, admin_map in (("CA", SRC / "CA.zip", keep_ca, ADMIN_CA), ("GL", SRC / "GL.zip", lambda *a: True, dict(ADMIN_GL))):
    if not zp.is_file():
        print(f"missing {zp}", file=sys.stderr)
        continue
    # region names from the ADM1 records themselves (Greenland's codes are numeric ids)
    for r in rows(zp, f"{cc}.txt"):
        if r[7] == "ADM1" and r[10] not in admin_map:
            admin_map[r[10]] = r[1].replace(" Municipality", "").replace("Kommunia", "").strip()
    for r in rows(zp, f"{cc}.txt"):
        if r[6] != "P" or r[7] in SKIP_CODES:
            continue
        lat, lon = float(r[4]), float(r[5])
        if not keep(lat, lon, r[10]):
            continue
        name = r[1]
        alt = ALT_NAMES.get(name, [])
        pop = int(r[14]) if r[14].isdigit() else 0
        features.append({"type": "Feature", "geometry": {"type": "Point", "coordinates": [round(lon, 4), round(lat, 4)]},
                         "properties": {"name": name, "alt": alt, "pop": pop, "region": admin_map.get(r[10], r[10]), "cc": cc,
                                        "code": r[7]}})

features.sort(key=lambda f: -f["properties"]["pop"])
OUT.write_text(json.dumps({"type": "FeatureCollection",
                           "attribution": "GeoNames (geonames.org), CC BY 4.0",
                           "features": features}, ensure_ascii=False, separators=(",", ":")))
print(f"{len(features)} communities -> {OUT} ({OUT.stat().st_size // 1024} kB)")
by = {}
for f in features:
    by[f["properties"]["region"]] = by.get(f["properties"]["region"], 0) + 1
print(" by region:", dict(sorted(by.items(), key=lambda x: -x[1])))
