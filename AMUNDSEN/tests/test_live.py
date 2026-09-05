"""Isolated synthetic UDP regression tests. No production endpoints are used."""
import json
import functools
import http.client
import socket
import tempfile
import threading
import time
import unittest
from unittest.mock import patch

from dashboard.live import LiveCTD


def free_port():
    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


class LiveTests(unittest.TestCase):
    def setUp(self):
        self.live = LiveCTD(free_port(), "scan,pressure,temperature")
        self.addCleanup(self.live.close)
        self.sender = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.addCleanup(self.sender.close)

    def send_until(self, text, predicate):
        deadline = time.monotonic() + 3
        while time.monotonic() < deadline:
            self.sender.sendto(text.encode(), ("127.0.0.1", self.live.port))
            time.sleep(0.03)
            if predicate(self.live.status()):
                return
        self.fail("UDP receiver did not reach expected state")

    def start_cast(self):
        self.send_until("1,12,3", lambda s: s["current"] is not None)

    def test_schema_change_retains_old_cast_and_receiver(self):
        self.start_cast()
        old = self.live.status()["current"]
        self.live.configure(self.live.port, "scan,temperature,depth")
        self.send_until("2,4,25", lambda s: s["current"] is not None)
        status = self.live.status()
        self.assertEqual(status["last"]["cols"], old["cols"])
        self.assertEqual(status["last"]["pressure_col"], "pressure")
        self.assertEqual(status["last"]["end_reason"], "configuration changed")
        self.assertEqual(status["current"]["pressure_col"], "depth")
        self.assertEqual(status["current"]["max_p"], 25)
        self.assertTrue(self.live._thread.is_alive())

    def test_busy_port_is_transactional(self):
        self.start_cast()
        port, sock = self.live.port, self.live.sock
        with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as busy:
            busy.bind(("", 0))
            with self.assertRaises(OSError):
                self.live.configure(busy.getsockname()[1], "scan,depth")
        self.assertEqual(self.live.port, port)
        self.assertIs(self.live.sock, sock)
        self.assertEqual(self.live.columns, ["scan", "pressure", "temperature"])
        self.send_until("2,30,4", lambda s: s["current"]["max_p"] == 30)

    def test_invalid_configuration_does_not_mutate(self):
        for port in [-1, 65536, 1.5, True, "garbage", "1.5"]:
            with self.subTest(port=port), self.assertRaises(ValueError):
                self.live.configure(port)
        for columns in ["", "scan,,pressure", "pressure,Pressure", ["pressure"]]:
            with self.subTest(columns=columns), self.assertRaises(ValueError):
                self.live.configure(self.live.port, columns)
        self.start_cast()

    def test_malformed_and_nonfinite_scans_are_ignored(self):
        for line in ["1,1e999,4", "1,bad,40", "1,,40", "status: 1 50"]:
            before = self.live.status()["packets"]
            self.send_until(line, lambda s: s["packets"] > before)
            self.assertIsNone(self.live.status()["current"])
        self.start_cast()
        json.dumps(self.live.status(), allow_nan=False)

    def test_noop_and_status_snapshot(self):
        self.start_cast()
        self.live.configure(self.live.port, "scan,pressure,temperature")
        snapshot = self.live.status()
        self.assertIsNone(snapshot["last"])
        snapshot["current"]["cols"]["pressure"].append(999)
        self.assertNotIn(999, self.live.status()["current"]["cols"]["pressure"])

    def test_port_switch_disable_and_close(self):
        self.start_cast()
        self.live.configure(free_port())
        self.send_until("1,9,3", lambda s: s["current"] is not None)
        self.live.configure(0)
        self.assertIsNone(self.live.sock)
        self.assertIsNone(self.live.status()["current"])
        self.live.close()
        self.assertFalse(self.live._thread.is_alive())

    def test_http_configuration_errors_and_retry(self):
        from dashboard.serve import Handler, ThreadingHTTPServer
        with tempfile.TemporaryDirectory() as root, patch("dashboard.serve.LIVE", self.live):
            server = ThreadingHTTPServer(("127.0.0.1", 0), functools.partial(Handler, directory=root))
            thread = threading.Thread(target=server.serve_forever, daemon=True)
            thread.start()
            try:
                for payload, expected in [({"port": 1.5}, 400), ({"columns": ""}, 400),
                                          ({"port": str(self.live.port), "columns": "scan,depth"}, 200)]:
                    conn = http.client.HTTPConnection("127.0.0.1", server.server_port, timeout=3)
                    try:
                        conn.request("POST", "/api/live", json.dumps(payload), {"Content-Type": "application/json"})
                        response = conn.getresponse()
                        body = json.loads(response.read())
                        self.assertEqual(response.status, expected)
                        if expected == 400:
                            self.assertIn("error", body)
                    finally:
                        conn.close()
            finally:
                server.shutdown()
                server.server_close()
                thread.join(timeout=3)


if __name__ == "__main__":
    unittest.main()
