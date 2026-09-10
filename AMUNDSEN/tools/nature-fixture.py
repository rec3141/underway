#!/usr/bin/env python3
"""A stand-in for the natural half of the history layer, to see the Nature
tab before grid publishes the real thing.

    tools/nature-fixture.py --live /data/underway/www/data/history --out /some/scratch/data/history

The out directory gets every file of the live publish (symlinked, so nothing
is copied), plus what the design (db/history/NATURE-DESIGN.md, section 6)
says the publish will add: ``subjects.json``, ``observations.json``, a
``subject/<slug>`` page per subject, three natural topics in ``index.json``
with ``domain: nature``, and observations on the timeline. The rows are the
design's own worked examples and a few more in their shape, marked
``fixture: true`` in each file; none of it is research, and nothing here
touches the live publish or the database.
"""

from __future__ import annotations

import argparse
import json
import re
import unicodedata
from pathlib import Path


def slugify(text: str, limit: int = 80) -> str:
    s = unicodedata.normalize("NFKD", str(text or ""))
    s = "".join(ch for ch in s if not unicodedata.combining(ch))
    s = re.sub(r"[^a-z0-9]+", "-", s.lower()).strip("-")
    return s[:limit]


TOPICS = [
    {"slug": "muskox-and-caribou", "title": "Muskox and caribou", "summary": "The two land herbivores of the islands: Sverdrup's hunting returns, Tener's survey of 1961, the crashes and recoveries since.", "status": "drafted", "domain": "nature"},
    {"slug": "the-magnetic-pole", "title": "The Magnetic Pole", "summary": "The pole's wander from James Clark Ross standing on it in 1831 to the observatory's readings this year; declination, dip and the storms.", "status": "drafted", "domain": "nature"},
    {"slug": "the-weather-and-the-climate", "title": "The weather and the climate", "summary": "The winters as the expeditions recorded them, thermometer by thermometer, and the station records since 1947.", "status": "drafted", "domain": "nature"},
]

SUBJECTS = [
    # kind, domain, rank, parent, english, french, inuktitut, kalaallisut, also, unit, status, backbone, note, topic
    {"name": "Mammalia", "kind": "taxon", "domain": "biology", "rank": "class", "parent": "", "english": "mammals", "french": "mammifères", "note": "The mammals, land and sea."},
    {"name": "Ovibos moschatus", "kind": "taxon", "domain": "biology", "rank": "species", "parent": "Mammalia", "english": "muskox", "french": "bœuf musqué", "inuktitut": "umingmak", "kalaallisut": "umimmak", "also": "musk-ox; musk ox", "status": "COSEWIC not at risk (2006)", "backbone_id": "2441106", "topic": "muskox-and-caribou",
     "note": "The bearded one, umingmak: a goat-antelope of the tundra that stands its ground in a ring, which fed every wintering party from Parry's to Sverdrup's and was hunted close to nothing on the islands before the 1917 protection. The record here runs from Parry's first at Melville Island in 1820 to the aerial counts."},
    {"name": "Rangifer tarandus", "kind": "taxon", "domain": "biology", "rank": "species", "parent": "Mammalia", "english": "caribou", "french": "caribou", "inuktitut": "tuktu", "kalaallisut": "tuttu", "backbone_id": "5220114", "topic": "muskox-and-caribou", "note": "The Peary caribou of the islands, small and pale, and the barren-ground herds to the south."},
    {"name": "Ursus maritimus", "kind": "taxon", "domain": "biology", "rank": "species", "parent": "Mammalia", "english": "polar bear", "french": "ours blanc", "inuktitut": "nanuq", "kalaallisut": "nanoq", "status": "COSEWIC special concern (2018)", "backbone_id": "2433451", "note": "The bear. Encounters, dens and the Lancaster Sound population."},
    {"name": "Odobenus rosmarus", "kind": "taxon", "domain": "biology", "rank": "species", "parent": "Mammalia", "english": "walrus", "french": "morse", "inuktitut": "aiviq", "kalaallisut": "aaveq", "status": "COSEWIC special concern (2017)", "backbone_id": "2433786", "note": "The walrus, at the haul-outs and the ice edge; a haul-out is a sensitive site."},
    {"name": "Balaena mysticetus", "kind": "taxon", "domain": "biology", "rank": "species", "parent": "Mammalia", "english": "bowhead whale", "french": "baleine boréale", "inuktitut": "arviq", "kalaallisut": "arfivik", "status": "COSEWIC special concern (2009)", "backbone_id": "2440546", "note": "The bowhead, whose catch by the whalers is the first census of it."},
    {"name": "Cetacea", "kind": "taxon", "domain": "biology", "rank": "order", "parent": "Mammalia", "english": "whales", "french": "cétacés", "note": "A narrative's 'a whale' is entered here, not as a guessed species."},
    {"name": "Saxifraga oppositifolia", "kind": "taxon", "domain": "biology", "rank": "species", "parent": "", "english": "purple saxifrage", "french": "saxifrage à feuilles opposées", "inuktitut": "aupilattunnguat", "backbone_id": "5372487", "note": "The first flower of the Arctic spring, and Nunavut's floral emblem."},
    {"name": "Eureka Sound Group", "kind": "unit", "domain": "geology", "rank": "group", "parent": "", "english": "Eureka Sound Group", "backbone_id": "WEBLEX 4197", "note": "The Late Cretaceous to Eocene sands, silts and coals of the Sverdrup Basin's last fill, with the forest beds Schei found in 1901 and the Geodetic Hills forest inside them."},
    {"name": "Geodetic Hills forest", "kind": "fossil", "domain": "geology", "parent": "Eureka Sound Group", "english": "the Geodetic Hills fossil forest", "note": "An Eocene forest of dawn redwood on Axel Heiberg Island, its stumps standing where they grew, in the beds Schei first described from Stenkul Fiord."},
    {"name": "air temperature", "kind": "weather", "domain": "meteorology", "unit": "C", "english": "air temperature", "french": "température de l'air", "topic": "the-weather-and-the-climate", "note": "The reading of the thermometer in the shade, entered in the unit the observer used: Fahrenheit for the nineteenth-century expeditions, Celsius for the stations."},
    {"name": "aurora", "kind": "sky", "domain": "astronomy", "english": "aurora", "french": "aurore boréale", "inuktitut": "aqsarniit", "kalaallisut": "arsarnerit", "note": "The northern lights, which the islands see less of than the south does: they lie inside the auroral oval, under the polar cap."},
    {"name": "North Magnetic Pole", "kind": "field", "domain": "geomagnetism", "english": "the North Magnetic Pole", "french": "pôle nord magnétique", "topic": "the-magnetic-pole", "note": "Where the dip needle stands vertical. Ross found it on the Boothia coast in 1831; it has walked north and west since, out of the islands and across the Arctic Ocean towards Siberia."},
    {"name": "dip", "kind": "field", "domain": "geomagnetism", "unit": "deg", "english": "magnetic dip", "french": "inclinaison magnétique", "topic": "the-magnetic-pole", "note": "The angle the needle makes with the horizontal; 90 degrees at the pole."},
    {"name": "sounding", "kind": "water", "domain": "hydrography", "unit": "fathoms", "english": "sounding", "french": "sondage", "note": "A depth by the lead line, in the unit the leadsman called; the multibeam's metres come later."},
    {"name": "ice shelf", "kind": "ice", "domain": "glaciology", "english": "ice shelf", "french": "plate-forme de glace", "note": "Floating ice attached to the land: the Ellesmere shelves, Ward Hunt and Milne among them, and the ice islands that calve from them."},
    {"name": "sikuliaq", "kind": "ice", "domain": "sea-ice", "english": "new ice", "inuktitut": "sikuliaq", "kalaallisut": "sikuliaq", "note": "Young ice, just formed, that will bear a fox but not a person; the first word in a vocabulary that has a name for every stage of the ice."},
    {"name": "parhelion", "kind": "weather", "domain": "meteorology", "english": "sun dog", "french": "parhélie", "also": "mock sun; sun dog", "note": "A bright spot beside the sun, from ice crystals in the air; Parry's men drew them all winter."},
]

# lat, lon, place, date_text, date_start, value, unit, count, qualifier, method, observer, vessel, detail, bibkey-pattern, topic, event flag
OBSERVATIONS = [
    ("Odobenus rosmarus", 74.53, -82.40, "Dundas Harbour", "16 August 1924", "1924-08-16", None, "", "about 200", "", "sighting", "A. H. Joy", "", "A walrus haul-out of about 200 animals on the point at Dundas Harbour, counted by Joy from the boat.", "rcmp|joy|patrol", "", 1),
    ("dip", 70.09, -96.77, "Cape Adelaide", "1 June 1831", "1831-06-01", 89.983, "deg", "", "", "instrument", "James Clark Ross", "Victory", "The dip needle at 89 degrees 59 minutes, the Victory's party standing on the Magnetic Pole at Cape Adelaide, Boothia.", "ross|victory", "the-magnetic-pole", 0),
    ("North Magnetic Pole", 70.09, -96.77, "Cape Adelaide", "1 June 1831", "1831-06-01", None, "", "", "the pole's first fix", "instrument", "James Clark Ross", "Victory", "The North Magnetic Pole fixed by Ross at Cape Adelaide, 70 deg 5 min N, 96 deg 46 min W, the flag raised over it.", "ross|victory", "the-magnetic-pole", 0),
    ("North Magnetic Pole", 70.5, -95.5, "", "1904", "1904-05", None, "", "", "Amundsen's fix, north of Ross's", "instrument", "Roald Amundsen", "Gjøa", "Amundsen's magnetic party put the pole some thirty miles north of Ross's position of 1831, by the observations of the Gjoa Haven winters.", "amundsen|gjoa", "the-magnetic-pole", 0),
    ("North Magnetic Pole", 74.9, -101.0, "Allen Lake, Prince of Wales Island", "1947", "1947", None, "", "", "the Dominion Observatory's fix", "instrument", "Dominion Observatory", "", "The pole located by the Dominion Observatory's survey at Allen Lake on Prince of Wales Island, two hundred and fifty miles from Ross's.", "observatory|nrcan|geomagnetic", "the-magnetic-pole", 0),
    ("North Magnetic Pole", 86.5, 162.9, "", "2025", "2025", None, "", "", "the model's position", "instrument", "NRCan Geomagnetic Laboratory", "", "The pole's modelled position for 2025, far across the Arctic Ocean towards Siberia, moving some forty kilometres a year.", "observatory|nrcan|geomagnetic", "the-magnetic-pole", 0),
    ("sounding", 82.0, -61.5, "off Cape Union", "28 August 1871", "1871-08-28", 1000, "fathoms", "", "no bottom at", "sounding", "Polaris", "Polaris", "No bottom at 1,000 fathoms, the Polaris off Cape Union in Robeson Channel, by Davis's narrative.", "polaris|davis|hall", "", 0),
    ("air temperature", 81.73, -64.75, "Fort Conger", "4 February 1882", "1882-02-04", -58.5, "F", "", "coldest of the winter", "instrument", "Greely's party", "", "Minus 58.5 F, the coldest of the winter at Fort Conger, from the meteorological register in Greely's appendix.", "greely", "the-weather-and-the-climate", 0),
    ("air temperature", 74.78, -110.85, "Winter Harbour", "15 February 1820", "1820-02-15", -55, "F", "", "coldest of the winter", "instrument", "Parry's ships", "Hecla", "Minus 55 F at Winter Harbour, Melville Island, the coldest reading of Parry's first wintering.", "parry", "the-weather-and-the-climate", 0),
    ("air temperature", 82.5, -62.3, "Alert", "February 1979", "1979-02-11", -50.0, "C", "", "monthly minimum", "station-record", "Alert station", "", "Minus 50.0 C, the monthly minimum at Alert, from the station's daily record.", "eccc|climate|alert", "the-weather-and-the-climate", 0),
    ("Eureka Sound Group", 77.5, -85.5, "Stenkul Fiord", "June 1901", "1901-06", None, "", "", "", "sample", "Per Schei", "Fram", "A Tertiary forest bed with stumps in place at Stenkul Fiord, described by Schei in the geology of New Land.", "sverdrup|schei", "", 0),
    ("aurora", 74.78, -110.85, "Winter Harbour", "15 January 1820", "1820-01-15", None, "", "", "three arcs through the zenith", "sighting", "Parry's officers", "Hecla", "An aurora in three arcs through the zenith over Winter Harbour, entered in Parry's journal.", "parry", "", 0),
    ("parhelion", 74.78, -110.85, "Winter Harbour", "3 March 1820", "1820-03-03", None, "", "", "with a halo, all afternoon", "sighting", "Parry's officers", "Hecla", "Parhelia and a halo about the sun all afternoon, drawn by the officers of the Hecla.", "parry", "", 0),
    ("ice shelf", 83.1, -74.2, "Ward Hunt Island", "August 1962", "1962-08", 4, "km", "", "front back from the 1906 position", "aerial-survey", "G. Hattersley-Smith", "", "The Ward Hunt ice shelf front four kilometres back from where Peary's 1906 sledge journey put it, by the survey of 1962.", "hattersley|ice-shelf|shelf", "", 0),
    ("Ovibos moschatus", 79.0, -91.0, "De To Kratere", "October 1900", "1900-10", None, "", "a herd of 11", "", "hunt", "Gunnar Isachsen", "Fram", "Eleven muskoxen shot at De To Kratere, which fed the dogs that got Isachsen home.", "sverdrup", "muskox-and-caribou", 1),
    ("Ovibos moschatus", 75.0, -110.0, "Melville Island", "June 1820", "1820-06", None, "", "3", "the first of the record", "hunt", "Parry's hunting parties", "Hecla", "Three muskoxen shot on Melville Island in June, the first of the record, from Parry's game returns.", "parry", "muskox-and-caribou", 0),
    ("Ovibos moschatus", 78.7, -89.5, "Axel Heiberg and Ellesmere", "1961", "1961-07", 8500, "animals", "", "estimated", "aerial-survey", "J. S. Tener", "", "Tener's aerial survey of 1961 put the Queen Elizabeth Islands' muskoxen at some 8,500.", "tener", "muskox-and-caribou", 0),
    ("Rangifer tarandus", 76.4, -98.5, "Bathurst Island", "1961", "1961-07", 3000, "animals", "", "estimated", "aerial-survey", "J. S. Tener", "", "Peary caribou on the Bathurst Island group, from the same survey.", "tener", "muskox-and-caribou", 0),
    ("Ursus maritimus", 74.2, -80.5, "Lancaster Sound", "12 September 1819", "1819-09-12", None, "", "2", "", "sighting", "W. E. Parry", "Hecla", "Two bears on the floe off the port bow in Lancaster Sound, one shot.", "parry", "", 0),
    ("Balaena mysticetus", 74.0, -80.0, "Lancaster Sound", "1820", "1820-08", 18, "whales", "", "the Dundee ships' catch", "catch-record", "the Dundee fleet", "", "Eighteen bowheads taken by the Dundee whalers in Lancaster Sound in the 1820 season.", "whal", "", 0),
    ("Saxifraga oppositifolia", 74.72, -94.98, "Resolute", "12 June 1955", "1955-06-12", None, "", "present", "first flower of the season", "sighting", "the station's observer", "", "Purple saxifrage in flower at Resolute on 12 June, the first of the season.", "resolute|station", "", 0),
    ("sikuliaq", 68.63, -95.88, "Uqsuqtuuq (Gjoa Haven)", "October", "1903-10-12", None, "", "", "new ice bearing", "testimony", "Netsilingmiut hunters", "", "The new ice, sikuliaq, first bore a hunter across the bay in the second week of October, by the Netsilik account given to Amundsen.", "amundsen|gjoa|netsilik", "", 0),
    ("Cetacea", 76.104, -92.41, "", "11 September 2026", "2026-09-11", None, "", "1", "", "sighting", "the bridge watch", "CCGS Amundsen", "A whale surfacing beside the ship, seen from the bridge; species not made out.", "", "", 0),
]


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--live", required=True, type=Path, help="the live publish, data/history")
    ap.add_argument("--out", required=True, type=Path, help="where the stand-in goes")
    a = ap.parse_args()
    live, out = a.live.resolve(), a.out
    out.mkdir(parents=True, exist_ok=True)
    (out / "pages").mkdir(exist_ok=True)
    for f in live.iterdir():
        if f.name in ("index.json", "timeline.json", "pages"):
            continue
        dst = out / f.name
        if not dst.exists():
            dst.symlink_to(f)
    for f in (live / "pages").iterdir():
        dst = out / "pages" / f.name
        if not dst.exists():
            dst.symlink_to(f)
    # the bibkeys the live bibliography has, so the source links are real
    bib = re.findall(r"@\w+\s*\{\s*([^,\s]+)\s*,", (live / "references.bib").read_text()) if (live / "references.bib").is_file() else []
    def bibkey(pat):
        if not pat:
            return ""
        for p in pat.split("|"):
            k = next((b for b in bib if p in b.lower()), "")
            if k:
                return k
        return ""
    # the people and places the live publish knows, for the links
    people = {p["name"] for p in json.loads((live / "people.json").read_text()).get("people", [])} if (live / "people.json").is_file() else set()
    index = json.loads((live / "index.json").read_text())
    index["topics"] = [t for t in index["topics"] if t["slug"] not in {x["slug"] for x in TOPICS}] + [{**t, "artifacts": 0, "pages": 0} for t in TOPICS]
    subjects = []
    for s in SUBJECTS:
        row = {"kind": "", "domain": "", "rank": "", "parent": "", "english": "", "french": "", "inuktitut": "", "kalaallisut": "", "also": "", "unit": "", "status": "", "backbone_id": "", "note": "", "bibkey": "", "topic": "", **s}
        row["page"] = f"subject/{slugify(row['name'])}"
        subjects.append(row)
        index["pages"].append({"slug": row["page"], "kind": "subject", "title": row["name"], "topic": row["topic"], "summary": row["note"][:160], "status": ""})
    observations = []
    for i, o in enumerate(OBSERVATIONS, 1):
        subject, lat, lon, place, date_text, date_start, value, unit, count, qualifier, method, observer, vessel, detail, pat, topic, ev = o
        observations.append({"id": f"fx-{i:03d}", "subject": subject, "date_text": date_text, "date_start": date_start, "date_end": "", "lat": lat, "lon": lon, "place": place,
                             "depth": None, "height": None, "value": value, "unit": unit, "count": count, "qualifier": qualifier, "stage": "", "sex": "", "behaviour": "",
                             "method": method, "instrument": "", "observer": observer, "vessel": vessel, "detail": detail, "confidence": "certain" if value is not None or count else "probable",
                             "artifact_id": "", "event_id": None, "sensitive": 1 if subject == "Odobenus rosmarus" else 0, "origin": "research",
                             "bibkey": bibkey(pat), "pages": "", "topic": topic or next((s["topic"] for s in subjects if s["name"] == subject), "")})
    # the timeline gains observations as a kind
    tl = json.loads((live / "timeline.json").read_text())
    tl["timeline"] = [r for r in tl["timeline"] if r.get("entity_kind") != "observation"] + [
        {"entity_kind": "observation", "entity_id": o["id"], "topic": o["topic"] or "muskox-and-caribou", "date": o["date_start"], "precision": {10: "day", 7: "month"}.get(len(o["date_start"]), "year"),
         "qualifier": o["qualifier"], "role": "", "span": "", "label": f"{o['subject']}: {o['count'] or (str(o['value']) + ' ' + o['unit']).strip() or o['qualifier']}", "lat": o["lat"], "lon": o["lon"], "place": o["place"], "bibkey": o["bibkey"]}
        for o in observations if o["date_start"]]
    (out / "index.json").write_text(json.dumps(index, ensure_ascii=False))
    (out / "timeline.json").write_text(json.dumps(tl, ensure_ascii=False))
    (out / "subjects.json").write_text(json.dumps({"fixture": True, "subjects": subjects}, ensure_ascii=False))
    (out / "observations.json").write_text(json.dumps({"fixture": True, "observations": observations}, ensure_ascii=False))
    # one page per subject: the body is the row's note, as the design says; the
    # backlinks are the pages that would mention it
    for s in subjects:
        obs_people = sorted({o["observer"] for o in observations if o["subject"] == s["name"] and o["observer"] in people})
        doc = {"slug": s["page"], "kind": "subject", "title": s["name"], "topic": s["topic"], "summary": s["note"][:160], "ref": s["name"], "status": "", "people": obs_people,
               "html": s["note"] + (f"\n\nIt sits under [{s['parent']}](subject/{slugify(s['parent'])})." if s["parent"] else ""),
               "backlinks": [p["slug"] for p in index["pages"] if p["kind"] == "page" and p["topic"] in ("ross-parry", "sverdrup-bernier-peary")][:4] + [x["page"] for x in subjects if x["parent"] == s["name"]]}
        (out / "pages" / f"{s['page'].replace('/', '__')}.json").write_text(json.dumps(doc, ensure_ascii=False))
    print(f"{len(subjects)} subjects, {len(observations)} observations, {len(TOPICS)} topics into {out}")


if __name__ == "__main__":
    main()
