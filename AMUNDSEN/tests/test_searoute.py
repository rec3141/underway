"""The sea route walks round land, keeps off the shallows, and says when it cannot."""
import functools
import http.client
import json
import math
import tempfile
import threading
import unittest
from pathlib import Path
from unittest.mock import patch

import numpy as np

from dashboard import searoute
from dashboard.serve import Handler, ThreadingHTTPServer

HAVE_FMM = True
try:
    import skfmm                                              # noqa: F401
except ImportError:
    HAVE_FMM = False


def write_grid(directory, water, elev=None, metres=2000.0, centre=(80.0, -90.0)):
    """A grid file set laid out like tools/make_sea_grid.sh writes one, centred on a point."""
    directory.mkdir(parents=True, exist_ok=True)
    rows, cols = water.shape
    x, y = searoute.forward(*centre)
    head = {"crs": "EPSG:3413", "x0": x - cols / 2 * metres, "y0": y + rows / 2 * metres,
            "metres": metres, "rows": rows, "cols": cols, "source": "test"}
    (directory / "grid.json").write_text(json.dumps(head))
    np.save(directory / "water.npy", np.packbits(water, axis=1))
    np.save(directory / "elevation.npy", (np.where(water, -500, 300) if elev is None else elev).astype(np.int16))
    return head


class ProjectionTests(unittest.TestCase):
    def test_round_trip_and_scale(self):
        for lat, lon in [(79.0, -86.0), (70.0, -45.0), (45.0, -150.0), (86.0, -15.0)]:
            lat2, lon2 = searoute.inverse(*searoute.forward(lat, lon))
            self.assertAlmostEqual(lat, lat2, places=6)
            self.assertAlmostEqual(lon, lon2, places=6)
        # true scale at the standard parallel, and the plane stretches away from it
        self.assertAlmostEqual(float(np.interp(math.hypot(*searoute.forward(70.0, -45.0)), searoute._RHO, searoute._K)), 1.0, places=3)
        self.assertGreater(float(np.interp(math.hypot(*searoute.forward(45.0, -45.0)), searoute._RHO, searoute._K)), 1.1)

    def test_matches_the_grid_the_tool_builds(self):
        # the published corner of EPSG:3413 for a known point, to the metre
        x, y = searoute.forward(79.0, -86.0)
        self.assertAlmostEqual(x, -784073.31, places=1)
        self.assertAlmostEqual(y, -901973.17, places=1)


@unittest.skipUnless(HAVE_FMM, "scikit-fmm is not installed")
class SeaRouteTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.dir = Path(self.tmp.name) / "sea-grid"
        self.addCleanup(searoute._route_cached.cache_clear)
        patcher = patch.object(searoute, "GRID_DIR", self.dir)
        patcher.start()
        self.addCleanup(patcher.stop)
        searoute._grid_key = None

    def at(self, grid, i, j):
        """The latitude and longitude at a cell of the test grid."""
        return searoute.inverse(grid["x0"] + (j + .5) * grid["metres"], grid["y0"] - (i + .5) * grid["metres"])

    def test_open_water_is_a_straight_line(self):
        head = write_grid(self.dir, np.ones((200, 200), dtype=bool))
        a, b = self.at(head, 160, 40), self.at(head, 40, 160)
        r = searoute.route(*a, *b)
        self.assertIsNone(r["reason"])
        self.assertAlmostEqual(r["sea_km"], r["air_km"], delta=0.02 * r["air_km"])
        self.assertLessEqual(len(r["path"]), 3)               # a line, not a staircase

    def test_route_goes_round_a_wall(self):
        water = np.ones((200, 200), dtype=bool)
        water[60:, 100] = False                               # a wall open at the top
        head = write_grid(self.dir, water)
        a, b = self.at(head, 150, 60), self.at(head, 150, 140)
        r = searoute.route(*a, *b)
        self.assertIsNone(r["reason"])
        self.assertGreater(r["sea_km"], 1.3 * r["air_km"])
        self.assertGreater(len(r["path"]), 2)

    def test_shallow_water_is_avoided_when_deep_water_is_near(self):
        water = np.ones((160, 160), dtype=bool)
        elev = np.full((160, 160), -800, dtype=np.int16)
        elev[70:90, :] = -8                                   # a shallow bar across the middle
        head = write_grid(self.dir, water, elev)
        a, b = self.at(head, 120, 80), self.at(head, 40, 80)
        r = searoute.route(*a, *b)
        self.assertIsNone(r["reason"])
        # it crosses the bar somewhere, but not by the straight line: the route is longer
        self.assertGreater(r["sea_km"], r["air_km"])
        self.assertLess(r["sea_km"], 2.0 * r["air_km"])

    def test_a_point_on_land_snaps_and_deep_land_does_not(self):
        water = np.ones((200, 200), dtype=bool)
        water[:, 120:] = False
        head = write_grid(self.dir, water)
        near = searoute.route(*self.at(head, 100, 60), *self.at(head, 100, 122))
        self.assertIsNotNone(near["sea_km"])
        far = searoute.route(*self.at(head, 100, 60), *self.at(head, 100, 190))
        self.assertIsNone(far["sea_km"])
        self.assertEqual(far["reason"], "the point is on land")

    def test_no_route_and_off_the_grid(self):
        water = np.ones((120, 120), dtype=bool)
        water[:, 60] = False                                  # a wall right across
        head = write_grid(self.dir, water)
        r = searoute.route(*self.at(head, 60, 20), *self.at(head, 60, 100))
        self.assertEqual(r["reason"], "no sea route within the charted area")
        self.assertEqual(searoute.route(*self.at(head, 60, 20), 10.0, 100.0)["reason"], "outside the charted area")

    def test_depth_comes_from_the_grid(self):
        elev = np.full((120, 120), -640, dtype=np.int16)
        elev[30, 30] = -12
        head = write_grid(self.dir, np.ones((120, 120), dtype=bool), elev)
        self.assertEqual(searoute.place(*self.at(head, 30, 30))["elev_m"], -12)
        self.assertEqual(searoute.place(*self.at(head, 60, 60))["elev_m"], -640)
        self.assertIsNone(searoute.place(10.0, 100.0)["elev_m"])

    def test_bad_coordinates(self):
        for bad in ((91.0, 0.0, 0.0, 0.0), (float("nan"), 0.0, 0.0, 0.0)):
            with self.assertRaises(ValueError):
                searoute.route(*bad)


class WithoutAGridTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        patcher = patch.object(searoute, "GRID_DIR", Path(self.tmp.name) / "missing")
        patcher.start()
        self.addCleanup(patcher.stop)
        searoute._grid_key = None
        self.addCleanup(searoute._route_cached.cache_clear)

    def test_only_air(self):
        r = searoute.route(72.0, -99.0, 72.0, -97.0)
        self.assertIsNone(r["sea_km"])
        self.assertIn("no sea grid", r["reason"])
        self.assertAlmostEqual(r["air_km"], 68.9, delta=0.5)
        self.assertIsNone(searoute.place(72.0, -99.0)["elev_m"])


class SeaRouteEndpointTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        base = Path(self.tmp.name)
        write_grid(base / "sea-grid", np.ones((120, 120), dtype=bool))
        self.addCleanup(searoute._route_cached.cache_clear)
        patcher = patch.object(searoute, "GRID_DIR", base / "sea-grid")
        patcher.start()
        self.addCleanup(patcher.stop)
        searoute._grid_key = None
        web = base / "web"; web.mkdir()
        self.server = ThreadingHTTPServer(("127.0.0.1", 0), functools.partial(Handler, directory=str(web)))
        threading.Thread(target=self.server.serve_forever, daemon=True).start()
        self.addCleanup(self.server.server_close)
        self.addCleanup(self.server.shutdown)

    def get(self, path):
        conn = http.client.HTTPConnection(*self.server.server_address, timeout=10)
        conn.request("GET", path)
        response = conn.getresponse()
        result = response.status, json.loads(response.read())
        conn.close()
        return result

    def test_endpoint(self):
        status, body = self.get("/api/searoute?from=80.1,-90.1&to=80.0,-89.9")
        self.assertEqual(status, 200)
        self.assertGreater(body["air_km"], 0)
        self.assertIn("elev_m", body)
        self.assertIsNotNone(self.get("/api/searoute?to=80.0,-90.0")[1]["elev_m"])
        self.assertEqual(self.get("/api/searoute?from=71&to=72,-97")[0], 400)
        self.assertEqual(self.get("/api/searoute")[0], 400)
