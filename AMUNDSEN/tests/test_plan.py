"""The cruise plan KMZ: tracks and stations out, communities left out."""
import io
import json
import tempfile
import unittest
import zipfile
from pathlib import Path
from unittest.mock import patch

from dashboard import plan

KML = """<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2"><Document><name>2026 Leg 3 test plan</name>
<Folder><name>2026 Leg 3</name>
  <Folder><name>Communities</name><Placemark><name>Eureka</name><Point><coordinates>-85.94,79.99,0</coordinates></Point></Placemark></Folder>
  <Placemark><name>Plan A Cruise Track Leg 3</name><LineString><coordinates>-94.8,74.7,0 -93.0,76.0,0 -91.5,77.5,0</coordinates></LineString></Placemark>
  <Placemark><name>Altern Cruise Track Leg 3</name><LineString><coordinates>-94.8,74.7,0 -92.0,76.5,0</coordinates></LineString></Placemark>
  <Folder><name>MAZE &amp; QEI Survey</name>
    <Placemark><name>CardS-3</name><description>&lt;b&gt;CTD&lt;/b&gt; and nets</description><Point><coordinates>-91.9,76.9,0</coordinates></Point></Placemark>
  </Folder>
  <Folder><name>RetroSeep</name><Folder><name>Sites</name>
    <Placemark><name>RS-1</name><Point><coordinates>-88.0,78.2,0</coordinates></Point></Placemark>
  </Folder></Folder>
</Folder></Document></kml>"""


class PlanTests(unittest.TestCase):
    def test_parse_kml_and_kmz(self):
        p = plan.parse(KML.encode())
        self.assertEqual(p["name"], "2026 Leg 3 test plan")
        self.assertEqual([(t["name"], t["alternate"], len(t["coords"])) for t in p["tracks"]],
                         [("Plan A Cruise Track Leg 3", False, 3), ("Altern Cruise Track Leg 3", True, 2)])
        self.assertEqual([(s["name"], s["group"]) for s in p["stations"]], [("CardS-3", "MAZE & QEI Survey"), ("RS-1", "RetroSeep / Sites")])
        self.assertEqual(p["stations"][0]["desc"], "CTD  and nets")           # tags stripped
        self.assertEqual(p["groups"], ["MAZE & QEI Survey", "RetroSeep / Sites"])
        buf = io.BytesIO()
        with zipfile.ZipFile(buf, "w") as z:
            z.writestr("doc.kml", KML)
        self.assertEqual(plan.parse(buf.getvalue())["stations"][1]["lat"], 78.2)

    def test_save_load_publish(self):
        with tempfile.TemporaryDirectory() as d, patch.object(plan, "DB_DIR", Path(d)):
            self.assertIsNone(plan.load())
            with self.assertRaises(ValueError):
                plan.save(b"<kml xmlns='http://www.opengis.net/kml/2.2'><Document/></kml>")
            plan.save(KML.encode())
            self.assertTrue(plan.plan_path().is_file())
            info = plan.publish(Path(d) / "www")
            self.assertEqual((info["n_stations"], info["n_tracks"]), (2, 2))
            self.assertTrue(info["file"].startswith("data/plan.json?v="))
            self.assertEqual(json.loads((Path(d) / "www" / "data" / "plan.json").read_text())["name"], "2026 Leg 3 test plan")


if __name__ == "__main__":
    unittest.main()
