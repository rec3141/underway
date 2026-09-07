"""Isolated tests of the live CTD listener against a synthetic Seasave
TCP/IP Out server. No production endpoints are used."""
import functools
import http.client
import json
import socket
import tempfile
import threading
import time
import unittest
from unittest.mock import patch

from pathlib import Path

from dashboard import live as live_mod
from dashboard.live import LiveCTD, xml_columns

SEASAVE_HEADER = """<?xml version="1.0"?>
<SBE_ConvertedDataSettings>
<SecondsBetweenUpdates>1.000000</SecondsBetweenUpdates>
<FieldDefinition><CalcID>18</CalcID><UnitID>31</UnitID><Ordinal>0</Ordinal><Units>salt water, m</Units><FullName>Depth [salt water, m]</FullName><Tag>Field0</Tag><Digits>3</Digits></FieldDefinition>
<FieldDefinition><CalcID>29</CalcID><UnitID>-1</UnitID><Ordinal>0</Ordinal><Units></Units><FullName>Fluorescence, Seapoint</FullName><Tag>Field1</Tag><Digits>4</Digits></FieldDefinition>
<FieldDefinition><CalcID>29</CalcID><UnitID>-1</UnitID><Ordinal>1</Ordinal><Units></Units><FullName>Fluorescence, Seapoint, 2</FullName><Tag>Field2</Tag><Digits>4</Digits></FieldDefinition>
<FieldDefinition><CalcID>81</CalcID><UnitID>6</UnitID><Ordinal>0</Ordinal><Units>ITS-90, deg C</Units><FullName>Temperature [ITS-90, deg C]</FullName><Tag>Field3</Tag><Digits>4</Digits></FieldDefinition>
<FieldDefinition><CalcID>81</CalcID><UnitID>6</UnitID><Ordinal>1</Ordinal><Units>ITS-90, deg C</Units><FullName>Temperature, 2 [ITS-90, deg C]</FullName><Tag>Field4</Tag><Digits>4</Digits></FieldDefinition>
<FieldDefinition><CalcID>8</CalcID><UnitID>-1</UnitID><Ordinal>0</Ordinal><Units></Units><FullName>Bottles Fired</FullName><Tag>Field5</Tag><Digits>0</Digits></FieldDefinition>
</SBE_ConvertedDataSettings>
"""
SCAN = "<SBE_ConvertedData><Field0>{d}</Field0><Field1>0.12</Field1><Field2>0.13</Field2><Field3>{t}</Field3><Field4>{t2}</Field4><Field5>0</Field5></SBE_ConvertedData>\n"


class FakeSeasave:
    """Accepts a connection, sends the field list in pieces, then whatever
    scans the test queues."""

    def __init__(self):
        self.srv = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.srv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.srv.bind(("127.0.0.1", 0)); self.srv.listen(2); self.srv.settimeout(5)
        self.addr = f"127.0.0.1:{self.srv.getsockname()[1]}"
        self.conn = None
        self.ready = threading.Event()
        threading.Thread(target=self._serve, daemon=True).start()

    def _serve(self):
        try:
            self.conn, _ = self.srv.accept()
        except OSError:
            return
        for k in range(0, len(SEASAVE_HEADER), 500):
            self.conn.sendall(SEASAVE_HEADER[k:k + 500].encode()); time.sleep(0.01)
        self.ready.set()

    def scan(self, d, t=3.0, t2="NaN"):
        self.ready.wait(3)
        self.conn.sendall(SCAN.format(d=d, t=t, t2=t2).encode())

    def close(self):
        if self.conn:
            self.conn.close()
        self.srv.close()


def wait_for(live, predicate, timeout=4):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if predicate(live.status()):
            return
        time.sleep(0.03)
    raise AssertionError("listener did not reach the expected state")


class LiveTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(); self.addCleanup(self.tmp.cleanup)
        p = patch.object(live_mod, "DB_DIR", Path(self.tmp.name)); p.start(); self.addCleanup(p.stop)
        self.seasave = FakeSeasave()
        self.addCleanup(self.seasave.close)
        self.live = LiveCTD(self.seasave.addr)
        self.addCleanup(self.live.close)
        wait_for(self.live, lambda s: s["fields"])

    def start_cast(self):
        self.seasave.scan(12.0)
        wait_for(self.live, lambda s: s["current"] is not None)

    def test_xml_columns(self):
        cols, tags, names = xml_columns(SEASAVE_HEADER)
        self.assertEqual(cols, ["depth_m", "fluorescence", "fluorescence_2", "temperature", "temperature_2", "bottles"])
        self.assertEqual(tags, ["Field0", "Field1", "Field2", "Field3", "Field4", "Field5"])
        self.assertEqual(names[0], "Depth [salt water, m]")
        cols, _, _ = xml_columns(SEASAVE_HEADER.replace("Depth [salt water, m]", "Depth, NMEA [salt water, m]"))
        self.assertEqual(cols[0], "bottom_depth_m")                            # the echosounder over NMEA is not the package depth

    def test_field_list_names_the_columns_and_scans_make_a_cast(self):
        st = self.live.status()
        self.assertEqual((st["tcp"], st["tcp_state"]), (self.seasave.addr, "connected"))
        self.assertEqual(st["columns"], ["depth_m", "fluorescence", "fluorescence_2", "temperature", "temperature_2", "bottles"])
        self.assertEqual(len(st["fields"]), 6)
        self.assertIsNone(st["current"])
        self.seasave.scan(0.4, 4.1)                                # at the surface: no cast yet
        wait_for(self.live, lambda s: s["packets"] == 1)
        self.assertIsNone(self.live.status()["current"])
        for d, t in ((3.0, 3.9), (12.5, 2.2), (15.0, 1.8)):
            self.seasave.scan(d, t); time.sleep(0.6)               # KEEP_HZ thins to 2 a second
        wait_for(self.live, lambda s: s["current"] and s["current"]["max_p"] == 15.0)
        c = self.live.status()["current"]
        self.assertEqual((c["pressure_col"], c["depth_like"]), ("depth_m", True))
        self.assertEqual(c["cols"]["temperature"][0], 3.9)
        self.assertIsNone(c["cols"]["temperature_2"][0])          # NaN keeps its column as a gap
        self.assertTrue(self.live.status()["raw"][-1].startswith("<SBE_ConvertedData>"))
        json.dumps(self.live.status(), allow_nan=False)

    def test_source_change_ends_the_cast_and_keeps_it(self):
        self.start_cast()
        old = self.live.status()["current"]
        self.live.configure("")
        st = self.live.status()
        self.assertEqual((st["tcp"], st["tcp_state"], st["fields"], st["columns"]), ("", "off", [], []))
        self.assertIsNone(st["current"])
        self.assertEqual(st["last"]["cols"], old["cols"])
        self.assertEqual(st["last"]["end_reason"], "configuration changed")
        self.live.configure(None)                                  # None leaves the source alone
        self.assertEqual(self.live.status()["tcp"], "")

    def test_invalid_source_does_not_mutate(self):
        for tcp in ["garbage", "host:", ":1", "a b:5", 5, ["x:1"]]:
            with self.subTest(tcp=tcp), self.assertRaises(ValueError):
                self.live.configure(tcp)
        self.assertEqual(self.live.status()["tcp"], self.seasave.addr)
        self.start_cast()

    def test_unreachable_source_keeps_connecting(self):
        with socket.socket() as s:
            s.bind(("127.0.0.1", 0)); free = s.getsockname()[1]
        self.live.configure(f"127.0.0.1:{free}")
        wait_for(self.live, lambda s: s["tcp_state"].startswith("connecting ("), timeout=8)
        self.live.close()
        self.assertFalse(self.live._thread.is_alive())

    def test_casts_survive_a_restart(self):
        self.start_cast()
        n = self.live.status()["current"]["n"]
        self.live.close()                                          # the server stops mid-cast
        again = LiveCTD("")
        self.addCleanup(again.close)
        st = again.status()
        self.assertEqual((st["current"]["n"], st["current"]["pressure_col"]), (n, "depth_m"))

    def test_status_is_a_snapshot(self):
        self.start_cast()
        snapshot = self.live.status()
        snapshot["current"]["cols"]["depth_m"].append(999)
        self.assertNotIn(999, self.live.status()["current"]["cols"]["depth_m"])

    def test_http_configuration_errors_and_retry(self):
        from dashboard.serve import Handler, ThreadingHTTPServer
        with tempfile.TemporaryDirectory() as root, patch("dashboard.serve.LIVE", self.live):
            server = ThreadingHTTPServer(("127.0.0.1", 0), functools.partial(Handler, directory=root))
            thread = threading.Thread(target=server.serve_forever, daemon=True)
            thread.start()
            try:
                for payload, expected in [({"tcp": 1.5}, 400), ({"tcp": "nonsense"}, 400), ({"tcp": self.seasave.addr}, 200), ({}, 200)]:
                    conn = http.client.HTTPConnection("127.0.0.1", server.server_port, timeout=3)
                    try:
                        conn.request("POST", "/api/live", json.dumps(payload), {"Content-Type": "application/json"})
                        response = conn.getresponse()
                        body = json.loads(response.read())
                        self.assertEqual(response.status, expected)
                        self.assertIn("error" if expected == 400 else "tcp", body)
                    finally:
                        conn.close()
            finally:
                server.shutdown()
                server.server_close()
                thread.join(timeout=3)


if __name__ == "__main__":
    unittest.main()
