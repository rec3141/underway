"""The photo import: the share's folders, when and where a photograph was
taken, the contact sheets and the model's answer, and the job that writes
the journal."""

import base64
import functools
import http.client
import io
import json
import shutil
import subprocess
import tempfile
import threading
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import patch

from PIL import Image

from dashboard import nature, photos
from dashboard.serve import Handler, ThreadingHTTPServer

EXIFTOOL = shutil.which("exiftool")


def jpeg(path: Path, colour=(200, 40, 40), size=(640, 480), **exif):
    Image.new("RGB", size, colour).save(path, "JPEG")
    if exif and EXIFTOOL:
        subprocess.run([EXIFTOOL, "-overwrite_original", "-q", *[f"-{k}={v}" for k, v in exif.items()], str(path)], check=True)


class ShareDir(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(); self.addCleanup(self.tmp.cleanup)
        root = Path(self.tmp.name)
        self.share = root / "share"; (self.share / "2026" / "2026_LEG_03" / "Pictures" / "Eric").mkdir(parents=True)
        (self.share / "2026" / "2026_LEG_03" / "Coring").mkdir()
        for name, val in (("SHARE_ROOT", self.share), ("THUMB_DIR", root / "thumbs"), ("JOBS_DIR", root / "imports"), ("IMPORTED", root / "imported.json"), ("WATCHES", root / "watches.json")):
            p = patch.object(photos, name, val); p.start(); self.addCleanup(p.stop)
        d = root / "_journal"
        for name, val in (("JOURNAL_DIR", d), ("IMG_DIR", d / "img"), ("FILE", d / "journal.jsonl")):
            p = patch.object(nature, name, val); p.start(); self.addCleanup(p.stop)
        self.pics = self.share / "2026" / "2026_LEG_03" / "Pictures"
        photos._JOBS.clear()


class ShareTests(ShareDir):
    def test_the_browser_opens_on_the_newest_legs_pictures_and_lists_a_folder(self):
        jpeg(self.pics / "Eric" / "b.jpg"); jpeg(self.pics / "Eric" / "a.JPG"); (self.pics / "Eric" / "notes.txt").write_text("x")
        jpeg(self.pics / "top.jpg"); (self.pics / ".hidden.jpg").write_bytes(b"")
        self.assertEqual(photos.start_path(), "2026/2026_LEG_03/Pictures")
        l = photos.listing("2026/2026_LEG_03/Pictures")
        self.assertEqual(l["path"], "2026/2026_LEG_03/Pictures"); self.assertEqual(l["parent"], "2026/2026_LEG_03")
        self.assertEqual(l["folders"], [{"name": "Eric", "images": 2}])
        self.assertEqual([f["name"] for f in l["files"]], ["top.jpg"])
        self.assertEqual(photos.listing("")["path"], ""); self.assertIsNone(photos.listing("")["parent"])
        for bad in ("../", "2026/../../etc", "/etc", "2026/2026_LEG_03/Pictures/top.jpg", "nowhere"):
            with self.assertRaises(ValueError):
                photos.listing(bad)

    def test_a_thumbnail_is_small_upright_and_cached(self):
        jpeg(self.pics / "wide.jpg", size=(1600, 400))
        data = photos.thumb("2026/2026_LEG_03/Pictures/wide.jpg", 200)
        with Image.open(io.BytesIO(data)) as im:
            self.assertEqual(im.size, (200, 50))
        self.assertEqual(len(list(photos.THUMB_DIR.glob("*.jpg"))), 1)
        self.assertEqual(photos.thumb("2026/2026_LEG_03/Pictures/wide.jpg", 200), data)
        with self.assertRaises(ValueError):
            photos.thumb("2026/2026_LEG_03/Pictures/../../../share")


class WhenAndWhereTests(ShareDir):
    def test_the_time_is_read_in_the_cameras_zone_or_the_one_named(self):
        ex = {"taken": "2026-09-03T12:20:26", "offset": "+02:00"}
        self.assertEqual(photos.taken_utc(ex).isoformat(), "2026-09-03T10:20:26+00:00")           # the camera said CEST
        self.assertEqual(photos.taken_utc(ex, "ship").isoformat(), "2026-09-03T16:20:26+00:00")   # overridden: the clock was on ship time (EDT)
        self.assertEqual(photos.taken_utc(ex, "utc").isoformat(), "2026-09-03T12:20:26+00:00")
        self.assertEqual(photos.taken_utc(ex, "-05:00").isoformat(), "2026-09-03T17:20:26+00:00")
        self.assertEqual(photos.taken_utc({"taken": "2026-09-03T12:20:26", "offset": None}).isoformat(), "2026-09-03T16:20:26+00:00")   # no zone written: ship time
        self.assertIsNone(photos.taken_utc({"taken": None}))

    @unittest.skipUnless(EXIFTOOL, "exiftool writes the EXIF the test reads")
    def test_exif_is_read_from_the_file(self):
        p = self.pics / "cam.jpg"
        jpeg(p, DateTimeOriginal="2026:09:03 12:20:26", OffsetTimeOriginal="+02:00", Model="DC-G110", Make="Panasonic")
        ex = photos.exif_of(p)
        self.assertEqual((ex["taken"], ex["offset"], ex["lat"], ex["model"]), ("2026-09-03T12:20:26", "+02:00", None, "Panasonic DC-G110"))
        g = self.pics / "gps.jpg"
        jpeg(g, DateTimeOriginal="2026:09:04 08:00:00", GPSLatitude="76.104", GPSLatitudeRef="N", GPSLongitude="92.41", GPSLongitudeRef="W")
        ex = photos.exif_of(g)
        self.assertAlmostEqual(ex["lat"], 76.104, 3); self.assertAlmostEqual(ex["lon"], -92.41, 3); self.assertIsNone(ex["offset"])
        jpeg(self.pics / "bare.jpg")
        self.assertIsNone(photos.exif_of(self.pics / "bare.jpg")["taken"])

    def test_the_track_places_a_moment_between_fixes_or_not_at_all(self):
        t = datetime(2026, 9, 3, 12, 0, tzinfo=timezone.utc).timestamp()
        track = photos.Track([(t, 76.0, -92.0), (t + 600, 76.1, -92.2), (t + 7200, 77.0, -93.0)])
        self.assertEqual(track.at(datetime.fromtimestamp(t + 300, timezone.utc)), (76.05, -92.1))      # halfway between two fixes
        self.assertEqual(track.at(datetime.fromtimestamp(t + 1200, timezone.utc)), (76.1, -92.2))      # the nearer fix alone: the next is over 15 min away
        self.assertIsNone(track.at(datetime.fromtimestamp(t + 4000, timezone.utc)))                     # nothing within 15 min
        self.assertEqual(track.at(datetime.fromtimestamp(t - 100, timezone.utc)), (76.0, -92.0))
        self.assertIsNone(photos.Track([]).at(datetime.now(timezone.utc)))


class SheetTests(ShareDir):
    def test_a_sheet_numbers_its_tiles_and_the_answer_is_read_leniently(self):
        paths = []
        for i in range(4):
            paths.append(self.pics / f"p{i}.jpg"); jpeg(paths[-1], colour=(i * 60, 100, 100))
        url = photos.make_sheet(paths, side=200)
        self.assertTrue(url.startswith("data:image/jpeg;base64,"))
        with Image.open(io.BytesIO(base64.b64decode(url.split(",", 1)[1]))) as im:
            self.assertEqual(im.size, (400, 400))                                # four tiles: two by two
            r, g, b = im.getpixel((10, 10)); self.assertTrue(r > 240 and g > 190 and b < 30, (r, g, b))   # the number's yellow box at the corner (JPEG rounds a little)
        rows = photos.parse_tags('Here you are:\n```json\n[{"n": 2, "caption": "A red square.", "tags": "red, square", "subject": "the ship", "kind": "Ship"}, '
                                 '{"n": 1, "caption": "Fog.", "tags": ["fog", "grey"], "subject": "fog", "kind": "weather"}]\n```', 3)
        self.assertEqual(rows[0]["subject"], "fog"); self.assertEqual(rows[1]["tags"], ["red", "square"]); self.assertEqual(rows[1]["kind"], "ship"); self.assertEqual(rows[2], {})
        self.assertEqual(photos.parse_tags("no json here", 2), [{}, {}])

    def test_the_models_words_find_a_published_subject(self):
        names = photos.subject_names(Path("/nowhere"))
        self.assertEqual(names, {})
        root = Path(self.tmp.name) / "www"; (root / "data" / "history").mkdir(parents=True)
        (root / "data" / "history" / "subjects.json").write_text(json.dumps({"subjects": [
            {"name": "Ursus maritimus", "english": "polar bear", "inuktitut": "nanuq", "also": "ice bear; white bear"},
            {"name": "sea ice", "english": "sea ice"}, {"name": "Larus hyperboreus", "english": "glaucous gull"}]}))
        names = photos.subject_names(root)
        self.assertEqual(photos.match_subject("Polar bear", [], names), "Ursus maritimus")
        self.assertEqual(photos.match_subject("polar bears", [], names), "Ursus maritimus")
        self.assertEqual(photos.match_subject("the ship", ["ice bear"], names), "Ursus maritimus")
        self.assertEqual(photos.match_subject("iceberg", ["sea ice", "blue"], names), "sea ice")
        self.assertIsNone(photos.match_subject("helicopter", ["deck", "people"], names))


class ImportTests(ShareDir):
    @unittest.skipUnless(EXIFTOOL, "exiftool writes the EXIF the import reads")
    def test_photographs_from_the_share_become_journal_lines(self):
        root = Path(self.tmp.name) / "www"; (root / "data" / "history").mkdir(parents=True)
        (root / "data" / "history" / "subjects.json").write_text(json.dumps({"subjects": [{"name": "Ursus maritimus", "english": "polar bear"}]}))
        d = self.pics / "Eric"
        jpeg(d / "bear.jpg", DateTimeOriginal="2026:09:03 12:20:26", OffsetTimeOriginal="+02:00")            # 10:20 UTC: on the track
        jpeg(d / "gps.jpg", DateTimeOriginal="2026:09:03 09:00:00", GPSLatitude="76.5", GPSLatitudeRef="N", GPSLongitude="90.25", GPSLongitudeRef="W")   # 13:00 UTC by ship time, placed by its own GPS
        jpeg(d / "lost.jpg", DateTimeOriginal="2026:09:01 12:00:00")                                          # no fix near that time
        jpeg(d / "bare.jpg")                                                                                  # no time at all
        (d / "sub").mkdir(); jpeg(d / "sub" / "deep.jpg", DateTimeOriginal="2026:09:03 12:30:00", OffsetTimeOriginal="+02:00")
        t0 = datetime(2026, 9, 3, 10, 0, tzinfo=timezone.utc).timestamp()
        fixes = [(t0 + 60 * k, 76.0 + 0.001 * k, -92.0 - 0.002 * k) for k in range(60)]
        sheets = []
        def tagger(url, n):
            sheets.append(n)
            first = len(sheets) == 1                                                                 # the bear is the first tile of the first sheet
            return [{"caption": f"Photo {i + 1}.", "tags": ["ice", "bear"], "subject": "polar bear" if first and i == 0 else "the ship", "kind": "wildlife" if first and i == 0 else "ship"} for i in range(n)]
        with patch.object(photos, "TRACK_SOURCE", lambda a, b: [f for f in fixes if a <= f[0] <= b]), patch.object(photos, "TAGGER", tagger), patch.object(photos, "SHEET_N", 2):
            with self.assertRaises(ValueError):
                photos.start(root, {"folders": ["2026/2026_LEG_03/Pictures/Eric"], "name": ""})
            with self.assertRaises(ValueError):
                photos.start(root, {"files": ["2026/2026_LEG_03/Pictures/Eric/bear.jpg"], "name": "Eric"})       # single files are not taken
            with self.assertRaises(ValueError):
                photos.start(root, {"folder": "2026/2026_LEG_03/Coring", "name": "Eric"})                        # nothing in it
            with self.assertRaises(ValueError):
                photos.start(root, {"folder": "2026/2026_LEG_03/Pictures/Eric", "name": "Eric", "licence": "mine"})
            spec = {"folder": "2026/2026_LEG_03/Pictures/Eric", "name": "Eric Collins", "org": "UM", "email": "e@example.org", "licence": "cc-by-4.0", "clock": "exif"}
            job = {"id": "20260910-000000-abcdef", "status": "queued", "started": "", "finished": None, "form": {k: spec[k] for k in ("name", "org", "email", "licence", "clock")},
                   "who": "Eric", "total": 0, "done": 0, "stage": "", "error": "", "items": [{"file": f, "status": "queued"} for f in photos._files_of(spec)]}
            job["total"] = len(job["items"])
            self.assertEqual([i["file"] for i in job["items"]], ["2026/2026_LEG_03/Pictures/Eric/bare.jpg", "2026/2026_LEG_03/Pictures/Eric/bear.jpg", "2026/2026_LEG_03/Pictures/Eric/gps.jpg",
                                                                 "2026/2026_LEG_03/Pictures/Eric/lost.jpg", "2026/2026_LEG_03/Pictures/Eric/sub/deep.jpg"])
            photos.run(job, root)
        by = {i["file"].split("/")[-1]: i for i in job["items"]}
        self.assertEqual(job["status"], "done"); self.assertEqual(job["done"], 3)
        self.assertEqual(sheets, [2, 1])                                                             # three placed photographs, two to a sheet
        self.assertEqual(by["bare.jpg"]["status"], "skipped"); self.assertIn("no time", by["bare.jpg"]["error"])
        self.assertEqual(by["lost.jpg"]["status"], "skipped"); self.assertIn("no position", by["lost.jpg"]["error"])
        bear = by["bear.jpg"]
        self.assertEqual((bear["status"], bear["date"], bear["position"]), ("imported", "2026-09-03T10:20Z", "the ship's track at that moment"))
        self.assertAlmostEqual(bear["lat"], 76.0 + 0.001 * 20.43, 3)                                # interpolated along the track
        self.assertEqual(bear["subject_page"], "Ursus maritimus"); self.assertEqual(bear["id"], "amundsen-2026-09-03-001")
        self.assertEqual((by["gps.jpg"]["lat"], by["gps.jpg"]["lon"], by["gps.jpg"]["position"]), (76.5, -90.25, "the camera's GPS"))
        self.assertEqual(by["gps.jpg"]["date"], "2026-09-03T13:00Z")
        self.assertIsNone(by["deep.jpg"]["subject_page"])                                            # "the ship" is no published subject: left as written
        rows = {e["id"]: e for e in nature.entries()}
        self.assertEqual(len(rows), 3)
        line = rows[bear["id"]]
        self.assertEqual((line["subject"], line["method"], line["observer"], line["licence"], line["lat"]), ("Ursus maritimus", "camera", "Eric Collins", "cc-by-4.0", bear["lat"]))
        self.assertIn("Photo 1. Photograph by Eric Collins (UM), bear.jpg", line["detail"]); self.assertIn("Tags: ice, bear.", line["detail"])
        self.assertTrue((nature.IMG_DIR / f"{bear['id']}.jpg").is_file())
        self.assertEqual(rows[by["deep.jpg"]["id"]]["subject"], "the ship")
        saved = json.loads((photos.JOBS_DIR / f"{job['id']}.json").read_text())
        self.assertEqual(saved["form"]["email"], "e@example.org")
        self.assertNotIn("email", photos.public(saved)["form"])                                      # the page never sees it
        self.assertEqual(photos.jobs()[0]["imported"], 3)
        reg = photos.imported()
        self.assertEqual({f for f, v in reg.items() if not v.startswith("skipped")}, {by[k]["file"] for k in ("bear.jpg", "gps.jpg", "deep.jpg")})   # the registry: what the journal has
        self.assertTrue(reg[by["bare.jpg"]["file"]].startswith("skipped: no time"))                    # and what could not be placed, so it is not read again
        with self.assertRaises(ValueError) as cm:
            photos.start(root, spec)                                                                 # nothing new in the folder
        self.assertIn("already", str(cm.exception))
        jpeg(d / "new.jpg", DateTimeOriginal="2026:09:03 12:40:00", OffsetTimeOriginal="+02:00")
        with patch.object(photos, "TRACK_SOURCE", lambda a, b: [f for f in fixes if a <= f[0] <= b]), patch.object(photos, "TAGGER", tagger), patch.object(photos, "run", lambda j, r: None):
            j2 = photos.start(root, {**spec, "watch": True})
        self.assertEqual(([i["file"].split("/")[-1] for i in j2["items"]], j2["known"], j2["watch"]), (["new.jpg"], 5, True))   # only the new one, the rest passed over
        self.assertEqual([w["path"] for w in photos.watches()], ["2026/2026_LEG_03/Pictures/Eric"])              # and the folder is watched under that form
        self.assertEqual(photos.watches()[0]["form"]["email"], "e@example.org"); self.assertNotIn("email", photos.watches_public()[0]["form"])
        photos._JOBS.clear()

    def test_a_model_that_does_not_answer_still_lets_the_photographs_in(self):
        root = Path(self.tmp.name) / "www"; (root / "data").mkdir(parents=True)
        d = self.pics / "Eric"; jpeg(d / "one.jpg")
        t = datetime(2026, 9, 3, 10, 0, tzinfo=timezone.utc)
        job = {"id": "20260910-000001-abcdef", "status": "queued", "started": "", "finished": None, "form": {"name": "Eric", "org": "", "email": "", "licence": "attribution", "clock": "exif"},
               "who": "Eric", "total": 1, "done": 0, "stage": "", "error": "", "items": [{"file": "2026/2026_LEG_03/Pictures/Eric/one.jpg", "status": "queued"}]}
        def boom(url, n):
            raise RuntimeError("model offline")
        with patch.object(photos, "exif_many", lambda ps: {p: {"taken": "2026-09-03T06:00:00", "offset": None, "lat": None, "lon": None, "model": ""} for p in ps}), \
             patch.object(photos, "TRACK_SOURCE", lambda a, b: [(t.timestamp(), 76.0, -92.0)]), patch.object(photos, "TAGGER", boom):
            photos.run(job, root)
        it = job["items"][0]
        self.assertEqual((job["status"], it["status"], it["subject_page"]), ("done", "imported", None))
        self.assertIn("did not answer", job["error"])
        self.assertEqual(nature.entries()[0]["subject"], "photograph")


class WatchTests(ShareDir):
    def test_a_watched_folder_is_imported_for_what_is_new_and_settled(self):
        root = Path(self.tmp.name) / "www"; (root / "data").mkdir(parents=True)
        d = self.pics / "Eric"; jpeg(d / "old.jpg"); jpeg(d / "fresh.jpg")
        import os, time as _t
        os.utime(d / "old.jpg", (_t.time() - 3600, _t.time() - 3600))                                # settled an hour ago; fresh.jpg is seconds old
        form = {"name": "Eric", "org": "", "email": "e@example.org", "licence": "attribution", "clock": "ship"}
        self.assertIsNone(photos.watch_scan(root))                                                    # nothing watched
        photos.watch_add("2026/2026_LEG_03/Pictures/Eric/", form, "Eric")
        started = []
        with patch.object(photos, "run", lambda j, r: started.append(j)):
            j = photos.watch_scan(root)
        self.assertEqual([i["file"].split("/")[-1] for i in j["items"]], ["old.jpg"])                # the settled one now, the fresh one next time
        self.assertTrue(j["from_watch"]); self.assertEqual(j["who"], "Eric (watched folder)")
        self.assertEqual(photos.watches()[0]["last_job"], j["id"]); self.assertIsNotNone(photos.watches()[0]["checked"])
        photos._JOBS.clear()
        photos._register("2026/2026_LEG_03/Pictures/Eric/old.jpg", "amundsen-2026-09-03-001")
        os.utime(d / "fresh.jpg", (_t.time() - 3600, _t.time() - 3600))
        with patch.object(photos, "run", lambda j, r: started.append(j)):
            j = photos.watch_scan(root)
        self.assertEqual([i["file"].split("/")[-1] for i in j["items"]], ["fresh.jpg"])
        photos._JOBS.clear(); photos._register("2026/2026_LEG_03/Pictures/Eric/fresh.jpg", "amundsen-2026-09-03-002")
        self.assertIsNone(photos.watch_scan(root))                                                    # all in: nothing to do
        self.assertTrue(photos.watch_remove("2026/2026_LEG_03/Pictures/Eric")); self.assertFalse(photos.watch_remove("2026/2026_LEG_03/Pictures/Eric"))
        self.assertEqual(photos.watches(), [])
        photos.watch_add("2026/2026_LEG_03/Pictures/Gone", form)
        (self.pics / "Gone").mkdir(); (self.pics / "Gone").rmdir()
        self.assertIsNone(photos.watch_scan(root)); self.assertIn("no such folder", photos.watches()[0]["error"])   # a folder that went away is noted, not fatal


class RouteTests(ShareDir):
    def setUp(self):
        super().setUp()
        self.web = Path(self.tmp.name) / "web"; self.web.mkdir(); (self.web / "index.html").write_text("dashboard")
        self.server = ThreadingHTTPServer(("127.0.0.1", 0), functools.partial(Handler, directory=str(self.web)))
        threading.Thread(target=self.server.serve_forever, daemon=True).start()
        self.addCleanup(self.server.server_close); self.addCleanup(self.server.shutdown)

    def call(self, method, path, body=None):
        c = http.client.HTTPConnection("127.0.0.1", self.server.server_address[1], timeout=5)
        c.request(method, path, body=json.dumps(body) if body is not None else None, headers={"Content-Type": "application/json"} if body is not None else {})
        r = c.getresponse(); data = r.read(); ctype = r.getheader("Content-Type"); c.close()
        return r.status, data, ctype

    def test_the_share_is_browsed_thumbnailed_and_imported_over_the_api(self):
        jpeg(self.pics / "Eric" / "a.jpg", size=(300, 200))
        status, data, _ = self.call("GET", "/api/nature/share")
        self.assertEqual(status, 200); j = json.loads(data)
        self.assertEqual((j["path"], j["start"], j["folders"][0]["name"]), ("2026/2026_LEG_03/Pictures", "2026/2026_LEG_03/Pictures", "Eric"))
        status, data, _ = self.call("GET", "/api/nature/share?path=2026%2F..%2F..%2Fetc")
        self.assertEqual(status, 404)
        status, data, ctype = self.call("GET", "/api/nature/share/thumb?path=2026%2F2026_LEG_03%2FPictures%2FEric%2Fa.jpg")
        self.assertEqual((status, ctype), (200, "image/jpeg"))
        with Image.open(io.BytesIO(data)) as im:
            self.assertEqual(im.size, (200, 133))
        status, data, _ = self.call("GET", "/api/nature/import")
        self.assertEqual(status, 200); self.assertEqual(json.loads(data)["jobs"], []); self.assertIn("cc-by-4.0", json.loads(data)["licences"]); self.assertEqual(json.loads(data)["watches"], [])
        status, data, _ = self.call("POST", "/api/nature/import", {"folder": "2026/2026_LEG_03/Pictures/Eric", "name": ""})
        self.assertEqual(status, 400); self.assertIn("name", json.loads(data)["error"])
        status, data, _ = self.call("POST", "/api/nature/import", {"files": ["2026/2026_LEG_03/Pictures/Eric/a.jpg"], "name": "Eric"})
        self.assertEqual(status, 400); self.assertIn("folder", json.loads(data)["error"])
        started = threading.Event()
        def hold(j, root):
            started.wait(2); j["status"] = "done"; photos._save(j)
        with patch.object(photos, "run", hold):
            status, data, _ = self.call("POST", "/api/nature/import", {"folder": "2026/2026_LEG_03/Pictures/Eric", "name": "Eric", "email": "e@example.org", "watch": True})
            self.assertEqual(status, 200, data); job = json.loads(data)["job"]
            self.assertEqual((job["total"], job["status"], job["watch"]), (1, "queued", True)); self.assertNotIn("email", job["form"])
            status, data, _ = self.call("POST", "/api/nature/import", {"folder": "2026/2026_LEG_03/Pictures/Eric", "name": "Eric"})
            self.assertEqual(status, 400); self.assertIn("already running", json.loads(data)["error"])
            started.set()
        status, data, _ = self.call("GET", "/api/nature/import")
        self.assertEqual([w["path"] for w in json.loads(data)["watches"]], ["2026/2026_LEG_03/Pictures/Eric"])
        status, data, _ = self.call("POST", "/api/nature/watch", {"path": "2026/2026_LEG_03/Pictures/Eric", "stop": True})
        self.assertEqual(status, 200); self.assertEqual(json.loads(data)["watches"], []); self.assertTrue(json.loads(data)["stopped"])
        status, data, _ = self.call("GET", f"/api/nature/import?job={job['id']}")
        self.assertEqual(status, 200); self.assertEqual(json.loads(data)["id"], job["id"])
        status, data, _ = self.call("GET", "/api/nature/import?job=nope")
        self.assertEqual(status, 404)
