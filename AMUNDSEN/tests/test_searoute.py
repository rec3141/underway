"""The sea route walks round land, snaps a shore click to water, and says when it cannot."""
import functools
import http.client
import json
import tempfile
import threading
import unittest
from pathlib import Path
from unittest.mock import patch

import numpy as np

from dashboard import searoute
from dashboard.serve import Handler, ThreadingHTTPServer


def write_mask(path, water, lon0=-100.0, lat0=70.0, dlon=0.1, dlat=0.1, elev=None):
    extra = {} if elev is None else {"elev": elev}
    np.savez_compressed(path, water=np.packbits(water, axis=1), shape=np.array(water.shape),
                        lon0=lon0, lat0=lat0, dlon=dlon, dlat=dlat, **extra)


class SeaRouteTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.path = Path(self.tmp.name) / "mask.npz"
        self.addCleanup(searoute._route_cached.cache_clear)
        patcher = patch.object(searoute, "MASK_PATH", self.path)
        patcher.start()
        self.addCleanup(patcher.stop)
        searoute._mask_key = None

    def wall(self):
        # 40 x 40 cells of water with a north-south wall down the middle, open at the top
        water = np.ones((40, 40), dtype=bool)
        water[:34, 20] = False
        return water

    def test_route_goes_round_the_wall(self):
        write_mask(self.path, self.wall())
        r = searoute.route(71.0, -99.0, 71.0, -96.5)     # either side of the wall, low down
        self.assertIsNone(r["reason"])
        self.assertGreater(r["sea_km"], 1.4 * r["air_km"])
        lats = [p[0] for p in r["path"]]
        self.assertGreater(max(lats), 73.3)               # it went over the top of the wall
        self.assertEqual(r["path"][0], [71.0, -99.0])
        self.assertEqual(r["path"][-1], [71.0, -96.5])

    def test_open_water_is_close_to_air(self):
        write_mask(self.path, np.ones((40, 40), dtype=bool))
        r = searoute.route(70.5, -99.5, 73.5, -96.5)
        self.assertLess(r["sea_km"], 1.06 * r["air_km"])
        self.assertGreaterEqual(r["sea_km"], r["air_km"] - 0.01)

    def test_shore_click_snaps_and_deep_land_does_not(self):
        water = np.ones((40, 40), dtype=bool)
        water[:, 25:] = False                             # land east of column 25
        write_mask(self.path, water)
        near = searoute.route(72.0, -99.0, 72.0, -97.4)   # a cell into the land: snaps to the shore
        self.assertIsNotNone(near["sea_km"])
        far = searoute.route(72.0, -99.0, 72.0, -96.2)    # deep inland
        self.assertIsNone(far["sea_km"])
        self.assertEqual(far["reason"], "the point is on land")

    def test_without_a_mask_only_air(self):
        r = searoute.route(72.0, -99.0, 72.0, -97.0)
        self.assertIsNone(r["sea_km"])
        self.assertIn("no sea mask", r["reason"])
        self.assertAlmostEqual(r["air_km"], 68.9, delta=0.5)

    def test_outside_the_grid(self):
        write_mask(self.path, np.ones((40, 40), dtype=bool))
        r = searoute.route(72.0, -99.0, 60.0, -50.0)
        self.assertEqual(r["reason"], "outside the charted area")

    def test_pooling_keeps_a_strait_open(self):
        water = np.zeros((400, 400), dtype=bool)
        water[:, :150] = True; water[:, 250:] = True       # two seas
        water[200, 150:250] = True                        # joined by a one-cell strait
        write_mask(self.path, water, dlon=0.01, dlat=0.01)
        with patch.object(searoute, "MAX_CELLS", 10_000):
            r = searoute.route(71.0, -99.5, 71.0, -96.5)
        self.assertIsNotNone(r["sea_km"], r["reason"])

    def test_elevation_comes_with_the_route_and_on_its_own(self):
        water = np.ones((40, 40), dtype=bool)
        elev = np.full((40, 40), -350, dtype=np.int16)
        elev[20:, :] = 240                                # land across the north half
        write_mask(self.path, water, elev=elev)
        self.assertEqual(searoute.place(71.0, -99.0)["elev_m"], -350)
        self.assertEqual(searoute.place(73.0, -99.0)["elev_m"], 240)
        self.assertIsNone(searoute.place(10.0, 10.0)["elev_m"])       # outside the grid
        self.assertEqual(searoute.route(71.0, -99.0, 73.0, -97.0)["elev_m"], 240)

    def test_a_grid_without_elevations_still_routes(self):
        write_mask(self.path, np.ones((40, 40), dtype=bool))
        r = searoute.route(71.0, -99.0, 72.0, -97.0)
        self.assertIsNotNone(r["sea_km"])
        self.assertIsNone(r["elev_m"])
        self.assertIsNone(searoute.place(71.0, -99.0)["elev_m"])

    def test_bad_coordinates(self):
        with self.assertRaises(ValueError):
            searoute.route(91.0, 0.0, 0.0, 0.0)
        with self.assertRaises(ValueError):
            searoute.route(float("nan"), 0.0, 0.0, 0.0)


class SeaRouteEndpointTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        base = Path(self.tmp.name)
        self.path = base / "mask.npz"
        write_mask(self.path, np.ones((40, 40), dtype=bool))
        self.addCleanup(searoute._route_cached.cache_clear)
        patcher = patch.object(searoute, "MASK_PATH", self.path)
        patcher.start()
        self.addCleanup(patcher.stop)
        searoute._mask_key = None
        web = base / "web"; web.mkdir()
        self.server = ThreadingHTTPServer(("127.0.0.1", 0), functools.partial(Handler, directory=str(web)))
        threading.Thread(target=self.server.serve_forever, daemon=True).start()
        self.addCleanup(self.server.server_close)
        self.addCleanup(self.server.shutdown)

    def get(self, path):
        conn = http.client.HTTPConnection(*self.server.server_address, timeout=5)
        conn.request("GET", path)
        response = conn.getresponse()
        result = response.status, json.loads(response.read())
        conn.close()
        return result

    def test_endpoint(self):
        status, body = self.get("/api/searoute?from=71,-99&to=72,-97")
        self.assertEqual(status, 200)
        self.assertGreater(body["sea_km"], 0)
        self.assertIsNone(body["reason"])
        self.assertEqual(self.get("/api/searoute?to=72,-97")[1]["elev_m"], None)   # the point alone, with no ship
        self.assertEqual(self.get("/api/searoute?from=71&to=72,-97")[0], 400)
        self.assertEqual(self.get("/api/searoute")[0], 400)
        self.assertEqual(self.get("/api/searoute?from=71,x&to=72,-97")[0], 400)
        self.assertEqual(self.get("/api/searoute?from=91,0&to=72,-97")[0], 400)
