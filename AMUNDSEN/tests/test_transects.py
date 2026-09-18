"""Shared transects round-trip through the server and are validated on the way in."""
import functools
import http.client
import json
import tempfile
import threading
import unittest
from pathlib import Path
from unittest.mock import patch

from dashboard import transects
from dashboard.serve import Handler, ThreadingHTTPServer

TRANSECT = {"id": "trs:0123abcd0123abcd", "kind": "TRS", "cast": "", "label": "Nares Strait", "variable": "Temperature",
            "members": ["2026_LEG_03:CTD_001", "2026_LEG_03:CTD_002"], "legs": ["2026_LEG_03"], "leg": "2026_LEG_03",
            "stations": ["NS-1", "NS-2"], "station": "NS-1 → NS-2", "time": "2026-09-01T10:00", "time_end": "2026-09-01T16:00",
            "track": [[81.1, -64.2], [81.4, -63.9]]}


class TransectTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        base = Path(self.tmp.name)
        patcher = patch.object(transects, "DB_DIR", base / "db")
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
        self.assertEqual(self.call("GET", "/api/transects"), (200, {"transects": []}))
        status, body = self.call("POST", "/api/transects", {"transect": {**TRANSECT, "extra": "dropped"}, "name": "Eric"})
        self.assertEqual(status, 200, body)
        self.assertTrue(body["transect"]["shared"])
        self.assertEqual(body["transect"]["by"], "Eric")
        self.assertNotIn("extra", body["transect"])
        status, listed = self.call("GET", "/api/transects")
        self.assertEqual([t["id"] for t in listed["transects"]], [TRANSECT["id"]])
        self.assertEqual(listed["transects"][0]["track"], TRANSECT["track"])
        # saving again replaces rather than duplicates
        self.call("POST", "/api/transects", {"transect": {**TRANSECT, "label": "Nares Strait (north)"}})
        status, listed = self.call("GET", "/api/transects")
        self.assertEqual([t["label"] for t in listed["transects"]], ["Nares Strait (north)"])
        self.assertEqual(self.call("POST", "/api/transects/delete", {"id": TRANSECT["id"]}), (200, {"ok": True, "removed": True}))
        self.assertEqual(self.call("POST", "/api/transects/delete", {"id": TRANSECT["id"]}), (200, {"ok": True, "removed": False}))
        self.assertEqual(self.call("GET", "/api/transects"), (200, {"transects": []}))

    def test_rejects_bad_records(self):
        for bad in ({**TRANSECT, "id": "cast:1"}, {**TRANSECT, "label": ""}, {**TRANSECT, "members": []},
                    {**TRANSECT, "track": [[1, 2, 3]]}, {**TRANSECT, "members": "CTD_001"}, "not an object"):
            status, body = self.call("POST", "/api/transects", {"transect": bad})
            self.assertEqual(status, 400, bad)
            self.assertIn("error", body)
        self.assertEqual(self.call("POST", "/api/transects/delete", {"id": "../x"})[0], 400)
        self.assertEqual(self.call("GET", "/api/transects"), (200, {"transects": []}))
