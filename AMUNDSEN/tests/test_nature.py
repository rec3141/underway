"""The Nature tab's ship side: the journal, its routes, Doc's slice of the
record, and the stand-in publish the tab is tried against."""

import base64
import functools
import http.client
import json
import subprocess
import sys
import tempfile
import threading
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import patch

from dashboard import chat, chatbot, nature
from dashboard.serve import Handler, ThreadingHTTPServer

PNG = base64.b64decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==")
LINE = {"subject": "Balaena mysticetus", "date": "2026-09-11T14:22Z", "lat": 76.104, "lon": -92.41, "count": "1", "method": "sighting",
        "observer": "the bridge watch", "vessel": "CCGS Amundsen", "detail": "A bowhead surfacing beside the ship, seen from the bridge.", "origin": "ship"}


class JournalDir(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(); self.addCleanup(self.tmp.cleanup)
        d = Path(self.tmp.name) / "_journal"
        for name, val in (("JOURNAL_DIR", d), ("IMG_DIR", d / "img"), ("FILE", d / "journal.jsonl")):
            p = patch.object(nature, name, val); p.start(); self.addCleanup(p.stop)
        self.dir = d


class JournalTests(JournalDir):
    def test_lines_get_the_days_next_id_and_read_back_newest_first(self):
        a = nature.append(LINE, "Eric")
        self.assertEqual(a["id"], "amundsen-2026-09-11-001")
        self.assertEqual(a["kind"], "observation"); self.assertEqual(a["origin"], "ship")
        self.assertEqual(set(a), {"kind", "id", "subject", "date", "lat", "lon", "count", "method", "observer", "vessel", "detail", "origin", "sensitive"})   # the writer's fields, nothing else
        self.assertIn(" amundsen-2026-09-11-001 Eric", (self.dir / "journal.log").read_text())
        b = nature.append({**LINE, "date": "2026-09-11T15:00Z", "subject": "aurora", "qualifier": "first of the season", "count": ""})
        self.assertEqual(b["id"], "amundsen-2026-09-11-002")
        self.assertNotIn("count", b)                                   # empty fields are not written
        rows = nature.entries()
        self.assertEqual([r["id"] for r in rows], ["amundsen-2026-09-11-002", "amundsen-2026-09-11-001"])
        self.assertEqual(len(nature.FILE.read_text().splitlines()), 2)

    def test_a_corrected_line_keeps_its_id_and_replaces_the_row(self):
        a = nature.append(LINE)
        c = nature.append({**LINE, "id": a["id"], "count": "2"})
        self.assertEqual(c["id"], a["id"])
        rows = nature.entries()
        self.assertEqual(len(rows), 1); self.assertEqual(rows[0]["count"], "2")
        self.assertEqual(len(nature.FILE.read_text().splitlines()), 2)   # append-only: both lines stand in the file
        p = nature.append({**LINE, "image": "data:image/png;base64," + base64.b64encode(PNG).decode()})
        c2 = nature.append({**LINE, "id": p["id"], "observer": "the keeper"})
        self.assertEqual(c2["artifact_file"], p["artifact_file"])           # a correction without a new photograph keeps the one it has
        self.assertEqual(nature.append({**LINE, "id": p["id"], "image": "data:image/png;base64," + base64.b64encode(PNG).decode()})["artifact_file"], p["artifact_file"])
        with self.assertRaises(nature.Refused):
            nature.append({**LINE, "id": "amundsen-2026-09-11-099"})    # not a line the journal has

    def test_what_the_journal_refuses(self):
        for bad, why in [({**LINE, "subject": ""}, "subject"), ({**LINE, "date": "yesterday"}, "date"), ({**LINE, "lat": 91}, "globe"),
                         ({**LINE, "value": 3.5, "unit": ""}, "unit"), ({**LINE, "detail": ""}, "detail"), ({**LINE, "method": "guess"}, "method"),
                         ({**LINE, "value": "warm", "unit": "C"}, "number")]:
            with self.assertRaises(nature.Refused, msg=why) as cm:
                nature.append(bad)
            self.assertIn(why, str(cm.exception))
        self.assertFalse(nature.FILE.exists())

    def test_a_photograph_is_saved_beside_the_line(self):
        data = "data:image/png;base64," + base64.b64encode(PNG).decode()
        a = nature.append({**LINE, "image": data})
        self.assertEqual(a["artifact_file"], f"_journal/img/{a['id']}.png")
        b = nature.append({**LINE, "image": data, "licence": "cc-by-4.0"}); self.assertEqual(b["licence"], "cc-by-4.0")
        self.assertEqual((nature.IMG_DIR / f"{a['id']}.png").read_bytes(), PNG)
        with self.assertRaises(nature.Refused):
            nature.append({**LINE, "image": "data:image/gif;base64,AAAA"})


class JournalRouteTests(JournalDir):
    def setUp(self):
        super().setUp()
        self.web = Path(self.tmp.name) / "web"; self.web.mkdir()
        (self.web / "index.html").write_text("dashboard")
        p = patch("dashboard.serve.JOURNAL_IMG", self.dir / "img"); p.start(); self.addCleanup(p.stop)
        self.server = ThreadingHTTPServer(("127.0.0.1", 0), functools.partial(Handler, directory=str(self.web)))
        threading.Thread(target=self.server.serve_forever, daemon=True).start()
        self.addCleanup(self.server.server_close); self.addCleanup(self.server.shutdown)

    def call(self, method, path, body=None):
        c = http.client.HTTPConnection("127.0.0.1", self.server.server_address[1], timeout=5)
        c.request(method, path, body=json.dumps(body) if body is not None else None, headers={"Content-Type": "application/json"} if body is not None else {})
        r = c.getresponse(); data = r.read(); c.close()
        return r.status, data

    def test_the_form_writes_a_line_and_the_tab_reads_it_back(self):
        status, data = self.call("POST", "/api/nature/journal", {**LINE, "name": "Eric", "image": "data:image/png;base64," + base64.b64encode(PNG).decode()})
        self.assertEqual(status, 200, data)
        entry = json.loads(data)["entry"]
        self.assertEqual(entry["id"], "amundsen-2026-09-11-001")
        status, data = self.call("GET", "/api/nature/journal")
        self.assertEqual(status, 200)
        self.assertEqual([e["id"] for e in json.loads(data)["entries"]], [entry["id"]])
        status, data = self.call("GET", f"/journal/{entry['id']}.png")     # the photograph is served from the journal
        self.assertEqual(status, 200); self.assertEqual(data, PNG)
        status, data = self.call("GET", "/journal/../journal.jsonl")
        self.assertNotEqual(status, 200)

    def test_a_refused_line_is_a_400_with_the_reason(self):
        status, data = self.call("POST", "/api/nature/journal", {**LINE, "date": "soon"})
        self.assertEqual(status, 400); self.assertIn("date", json.loads(data)["error"])
        status, data = self.call("POST", "/api/nature/journal", [1, 2])
        self.assertEqual(status, 400)


class DocTests(JournalDir):
    def test_docs_slice_of_the_record(self):
        root = Path(self.tmp.name) / "www"; h = root / "data" / "history"; h.mkdir(parents=True)
        (h / "subjects.json").write_text(json.dumps({"subjects": [
            {"name": "Ovibos moschatus", "english": "muskox", "inuktitut": "umingmak", "page": "subject/ovibos-moschatus", "note": "The bearded one."},
            {"name": "air temperature", "unit": "C", "page": "subject/air-temperature"}]}))
        (h / "observations.json").write_text(json.dumps({"observations": [
            {"id": "1", "subject": "Ovibos moschatus", "date_start": "1900-10", "date_text": "October 1900", "lat": 79.0, "lon": -91.0, "place": "De To Kratere", "count": "a herd of 11", "observer": "Gunnar Isachsen", "detail": "Eleven shot."},
            {"id": "2", "subject": "air temperature", "date_start": "1882-02-04", "lat": 81.73, "lon": -64.75, "place": "Fort Conger", "value": -58.5, "unit": "F", "observer": "Greely's party"},
            {"id": "3", "subject": "Ovibos moschatus", "date_start": "1820-06-12", "lat": 75.0, "lon": -110.0, "place": "Melville Island", "count": "3", "observer": "Parry's hunting parties"}]}))
        now = datetime(2026, 6, 12, tzinfo=timezone.utc)
        self.assertEqual(chatbot.nature_lines(Path(self.tmp.name) / "nowhere", 78.5, -90.0, now=now), [])   # nothing published, nothing journalled
        nature.append(LINE)
        lines = chatbot.nature_lines(root, 78.5, -90.0, "", now=now)
        text = "\n".join(lines)
        self.assertIn("NATURAL RECORD near the ship", text)
        self.assertIn("muskox (Ovibos moschatus), a herd of 11, October 1900, by Gunnar Isachsen, De To Kratere", text)
        self.assertNotIn("Fort Conger", lines[0])                         # 700 km off: not near
        self.assertIn("on this date in other years", text); self.assertIn("Melville Island", text)
        self.assertIn("THE SHIP'S JOURNAL", text); self.assertIn("Balaena mysticetus, 1, 2026-09-11, by the bridge watch", text)
        self.assertIn("not yet on grid", text)
        subj = "\n".join(chatbot.nature_lines(root, None, None, "subject/ovibos-moschatus", now=now))
        self.assertIn("THE PAGE OPEN ON THE NATURE TAB: Ovibos moschatus (english: muskox; inuktitut: umingmak). The bearded one.", subj)
        self.assertIn("Its record:", subj); self.assertIn("Parry's hunting parties", subj); self.assertNotIn("Greely", subj)
        one = "\n".join(chatbot.nature_lines(root, None, None, "observation/2", now=now))
        self.assertIn("THE OBSERVATION OPEN", one); self.assertIn("-58.5 F", one)
        only = chatbot.nature_lines(Path(self.tmp.name) / "nowhere", 78.5, -90.0, now=now)   # no publish yet: the journal alone reaches Doc
        self.assertEqual(len(only), 2); self.assertTrue(all("journal" in x for x in only))


    def test_doc_reads_the_wiki_and_has_a_room_of_his_own(self):
        root = Path(self.tmp.name) / "www"; h = root / "data" / "history"
        (h / "pages").mkdir(parents=True); (h / "excerpts").mkdir()
        (root / "data" / "manifest.json").write_text(json.dumps({"data_range": {"end": "2026-06-12T00:00:00"}, "latest": {"lat": 78.5, "lon": -90.0}, "legs": [], "windows": []}))
        (h / "index.json").write_text(json.dumps({"pages": [], "topics": [{"slug": "muskox-and-caribou", "domain": "nature"}, {"slug": "sverdrup-fram", "domain": "history"}]}))
        (h / "subjects.json").write_text(json.dumps({"subjects": [{"name": "Ovibos moschatus", "english": "muskox", "page": "subject/ovibos-moschatus", "note": "The bearded one."}]}))
        (h / "observations.json").write_text(json.dumps({"observations": [
            {"id": "1", "subject": "Ovibos moschatus", "date_start": "1900-10", "lat": 79.0, "lon": -91.0, "place": "De To Kratere", "count": "a herd of 11", "observer": "Gunnar Isachsen"}]}))
        pad = lambda seed: " ".join(f"{seed}{i}" for i in range(300))          # pages of a modest length, each its own words
        (h / "pages" / "subject__ovibos-moschatus.json").write_text(json.dumps({"slug": "subject/ovibos-moschatus", "kind": "subject", "title": "Ovibos moschatus", "topic": "muskox-and-caribou", "html": "<p>The muskox, in a word.</p>"}))
        (h / "excerpts" / "subject__ovibos-moschatus.txt").write_text("Ovibos moschatus, the muskox, umingmak. Isachsen counted a herd of eleven muskox in October 1900; "
                                                                       "Isachsen shot the herd for the dogs, the muskox meat lasting to October; Isachsen wrote of 1900 later. " + pad("ox"))
        (h / "pages" / "sverdrup-the-sledge-journeys.json").write_text(json.dumps({"slug": "sverdrup-the-sledge-journeys", "kind": "page", "title": "The sledge journeys", "topic": "sverdrup-fram",
                                                                                   "html": "<p>Isachsen and the muskox herd of eleven, shot for the dogs in October 1900. The herd fed the dogs; the muskox were gone by October, Isachsen says, and 1900 closed. " + pad("sledge") + "</p>"}))
        for i in range(10):
            (h / "pages" / f"filler-{i}.json").write_text(json.dumps({"slug": f"filler-{i}", "kind": "page", "title": f"Filler {i}", "topic": "sverdrup-fram", "html": "<p>" + pad(f"f{i}w") + "</p>"}))
        chatbot._wiki_cache.update(stamp=None, pages=[])
        pages = chatbot.wiki_pages(root)
        self.assertEqual({p["slug"]: p["_domain"] for p in pages if not p["slug"].startswith("filler")}, {"subject/ovibos-moschatus": "nature", "sverdrup-the-sledge-journeys": "history"})
        self.assertIn("herd of eleven", next(p["_text"] for p in pages if p["kind"] == "subject"))   # a subject page reads as the publish's excerpt of it
        q = "Isachsen muskox herd October 1900"
        self.assertEqual(chatbot.wiki_excerpts(root, q, prefer="nature")[0]["slug"], "subject/ovibos-moschatus")      # Doc's half first
        self.assertEqual(chatbot.wiki_excerpts(root, q, prefer="history")[0]["slug"], "sverdrup-the-sledge-journeys")   # Ada's half first
        crew = chatbot.Crew(root, lambda *a, **k: None, lambda *a, **k: [])
        ctx = crew.context("environment", q, "subject/ovibos-moschatus")
        self.assertIn("NATURAL RECORD near the ship", ctx)
        self.assertIn("WIKI EXCERPTS", ctx); self.assertIn("### [1] Ovibos moschatus", ctx)     # the page open on the tab is excerpt 1
        self.assertNotIn("THE PAGE OPEN ON THE NATURE TAB", ctx)                                # and is not repeated after it
        self.assertEqual(crew._pages[0]["slug"], "subject/ovibos-moschatus")
        self.assertNotIn("WIKI EXCERPTS", crew.context("schedule", q))                          # the Cap'n reads no pages
        with patch.object(chat, "bots", lambda: chatbot.PERSONAS):
            self.assertTrue(chatbot.Crew.own_room("doc", "dm:eric|@doc")); self.assertTrue(chatbot.Crew.own_room("ada", "ada"))
            self.assertFalse(chatbot.Crew.own_room("doc", "crew")); self.assertFalse(chatbot.Crew.own_room("doc", "dm:eric|@ada")); self.assertFalse(chatbot.Crew.own_room("ada", "dm:eric|@ada"))


class FixtureTests(unittest.TestCase):
    def test_the_stand_in_has_the_designs_shape(self):
        tmp = tempfile.TemporaryDirectory(); self.addCleanup(tmp.cleanup)
        live = Path(tmp.name) / "live"; (live / "pages").mkdir(parents=True)
        (live / "index.json").write_text(json.dumps({"topics": [{"slug": "ross-parry", "title": "Ross and Parry", "summary": "", "status": "good", "artifacts": 1, "pages": 1}],
                                                     "pages": [{"slug": "parry-overview", "kind": "page", "title": "Parry", "topic": "ross-parry", "summary": "", "status": "good"}], "wanted": []}))
        (live / "timeline.json").write_text(json.dumps({"timeline": [{"entity_kind": "event", "entity_id": 1, "topic": "ross-parry", "date": "1820-01-15", "precision": "day", "label": "x"}]}))
        (live / "references.bib").write_text("@book{parry1821,\n  title = {Journal},\n}\n@book{sverdrup1904,\n  title = {New Land},\n}\n")
        (live / "people.json").write_text(json.dumps({"people": [{"name": "Gunnar Isachsen", "page": "person/gunnar-isachsen"}]}))
        (live / "pages" / "parry-overview.json").write_text("{}")
        out = Path(tmp.name) / "out"
        r = subprocess.run([sys.executable, str(Path(__file__).resolve().parents[1] / "tools" / "nature-fixture.py"), "--live", str(live), "--out", str(out)], capture_output=True, text=True)
        self.assertEqual(r.returncode, 0, r.stderr)
        subjects = json.loads((out / "subjects.json").read_text())["subjects"]
        obs = json.loads((out / "observations.json").read_text())["observations"]
        names = {s["name"] for s in subjects}
        self.assertTrue(all(o["subject"] in names for o in obs), [o["subject"] for o in obs if o["subject"] not in names])
        self.assertTrue(all(s["parent"] in names for s in subjects if s["parent"]))
        for s in subjects:
            self.assertTrue((out / "pages" / (s["page"].replace("/", "__") + ".json")).is_file(), s["page"])
        index = json.loads((out / "index.json").read_text())
        self.assertEqual(sum(1 for t in index["topics"] if t.get("domain") == "nature"), 3)
        self.assertTrue(any(p["slug"] == "parry-overview" for p in index["pages"]))     # the live pages are kept
        self.assertTrue((out / "pages" / "parry-overview.json").is_symlink())
        self.assertEqual(next(o for o in obs if o["place"] == "Winter Harbour" and o["unit"] == "F")["bibkey"], "parry1821")
        tl = json.loads((out / "timeline.json").read_text())["timeline"]
        self.assertTrue(any(r["entity_kind"] == "observation" for r in tl) and any(r["entity_kind"] == "event" for r in tl))
        self.assertIn("Gunnar Isachsen", json.loads((out / "pages" / "subject__ovibos-moschatus.json").read_text())["people"])


if __name__ == "__main__":
    unittest.main()
