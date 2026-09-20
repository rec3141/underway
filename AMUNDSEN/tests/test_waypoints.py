"""Waypoints round-trip through the server, keep their id, and are validated."""
import functools
import http.client
import json
import tempfile
import threading
import unittest
from pathlib import Path
from unittest.mock import patch

from dashboard import waypoints
from dashboard.serve import Handler, ThreadingHTTPServer

MARK = {"name": "Polynya edge", "lat": 78.5, "lon": -74.25}


class WaypointTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        base = Path(self.tmp.name)
        patcher = patch.object(waypoints, "DB_DIR", base / "db")
        patcher.start()
        self.addCleanup(patcher.stop)
        web = base / "web"; web.mkdir()
        self.server = ThreadingHTTPServer(("127.0.0.1", 0), functools.partial(Handler, directory=str(web)))
        threading.Thread(target=self.server.serve_forever, daemon=True).start()
        self.addCleanup(self.server.server_close)
        self.addCleanup(self.server.shutdown)

    def call(self, method, path, payload=None):
        conn = http.client.HTTPConnection(*self.server.server_address, timeout=5)
        conn.request(method, path, json.dumps(payload) if payload is not None else None, {"Content-Type": "application/json"})
        response = conn.getresponse()
        result = response.status, json.loads(response.read())
        conn.close()
        return result

    def test_round_trip(self):
        self.assertEqual(self.call("GET", "/api/waypoints"), (200, {"waypoints": []}))
        status, body = self.call("POST", "/api/waypoints", {"waypoint": MARK, "name": "Eric"})
        self.assertEqual(status, 200, body)
        kept = body["waypoint"]
        self.assertTrue(kept["id"].startswith("wp:"))
        self.assertEqual((kept["name"], kept["lat"], kept["by"]), ("Polynya edge", 78.5, "Eric"))
        status, listed = self.call("GET", "/api/waypoints")
        self.assertEqual([w["id"] for w in listed["waypoints"]], [kept["id"]])
        # renaming the same waypoint replaces it and keeps when it was made
        status, again = self.call("POST", "/api/waypoints", {"waypoint": {**MARK, "id": kept["id"], "name": "Polynya"}})
        self.assertEqual(again["waypoint"]["created_utc"], kept["created_utc"])
        status, listed = self.call("GET", "/api/waypoints")
        self.assertEqual([w["name"] for w in listed["waypoints"]], ["Polynya"])
        self.assertEqual(self.call("POST", "/api/waypoints/delete", {"id": kept["id"]}), (200, {"ok": True, "removed": True}))
        self.assertEqual(self.call("POST", "/api/waypoints/delete", {"id": kept["id"]}), (200, {"ok": True, "removed": False}))
        self.assertEqual(self.call("GET", "/api/waypoints"), (200, {"waypoints": []}))

    def test_rejects_bad_records(self):
        for bad in ({**MARK, "name": ""}, {**MARK, "lat": 100}, {**MARK, "lat": "north"}, {"name": "No position"},
                    {**MARK, "id": "../x"}, {**MARK, "note": "x" * 501}, "not an object"):
            status, body = self.call("POST", "/api/waypoints", {"waypoint": bad})
            self.assertEqual(status, 400, bad)
            self.assertIn("error", body)
        self.assertEqual(self.call("POST", "/api/waypoints/delete", {"id": "wp:../x"})[0], 400)
        self.assertEqual(self.call("GET", "/api/waypoints"), (200, {"waypoints": []}))

    def test_name_and_position_are_tidied(self):
        kept = waypoints.save({**MARK, "name": "  Ice edge  ", "lat": 78.50000004}, who="  Ada  ")["id"]
        row = waypoints.listing()[0]
        self.assertEqual((row["name"], row["by"], row["lat"], row["id"]), ("Ice edge", "Ada", 78.5, kept))
