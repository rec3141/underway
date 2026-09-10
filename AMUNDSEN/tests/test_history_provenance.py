"""The provenance figures counted from a history database on publish."""
import json
import sqlite3
import tempfile
import unittest
from pathlib import Path

from dashboard import history


class ProvenanceTests(unittest.TestCase):
    @unittest.skipUnless(history.AVAILABLE, "the arctic-history project is not installed here")
    def test_counts_from_a_small_database(self):
        tmp = tempfile.TemporaryDirectory(); self.addCleanup(tmp.cleanup)
        root = Path(tmp.name); db = root / "h.sqlite"
        c = sqlite3.connect(db)
        c.executescript("""
          CREATE TABLE topics (slug TEXT); INSERT INTO topics VALUES ('a'), ('b');
          CREATE TABLE pages (slug TEXT, kind TEXT, status TEXT);
          INSERT INTO pages VALUES ('p1','page','good'), ('p2','page','draft'), ('x','artifact','');
          CREATE TABLE page_history (slug TEXT); INSERT INTO page_history VALUES ('p1'), ('p1'), ('p2');
          CREATE TABLE artifacts (id TEXT, type TEXT, local_file TEXT, source_url TEXT, waypoints TEXT);
          INSERT INTO artifacts VALUES ('1','image','f.jpg','http://x',''), ('2','quote','','',''), ('3','image','','http://y',''),
                                       ('4','track','','','[{"lat":1,"lon":2},{"lat":3,"lon":4}]'), ('5','track','','','');
          CREATE TABLE sources (bibkey TEXT, primary_src INTEGER, local_file TEXT, language TEXT);
          INSERT INTO sources VALUES ('k1',1,'a.pdf','en'), ('k2',0,'','fr'), ('k3',0,'','en');
          CREATE TABLE worklog (who TEXT, at TEXT);
          INSERT INTO worklog VALUES ('run 1, grid','2026-09-08T13:00:00+00:00'), ('review, grid','2026-09-09T10:00:00+00:00'), ('run 1, grid','2026-09-09T11:00:00+00:00');
          CREATE TABLE requests (status TEXT, kind TEXT); INSERT INTO requests VALUES ('approved','download'), ('denied','access'), ('done','download');
          CREATE TABLE keywords (artifact_id TEXT, source TEXT); INSERT INTO keywords VALUES ('1','llm:gemma4'), ('1','llm:gemma4'), ('2','crew');
          CREATE TABLE ontology (model TEXT); INSERT INTO ontology VALUES ('gemma4:26b');
          CREATE TABLE faces (kind TEXT, person TEXT, method TEXT);
          INSERT INTO faces VALUES ('person','John Rae','yunet-2023mar; single-face-single-name'), ('person','','yunet-2023mar; unidentified'),
                                   ('dog','','yolo11x:dog; unidentified'), ('person','','yunet-2023mar; rejected by the operator: wrong man');
        """)
        c.commit(); c.close()
        (root / "PROVENANCE.md").write_text("# How it was made\n\n## The short version\n\nBy agents.\n")
        out = history.provenance(root, db)
        self.assertEqual(out["text"], "# How it was made\n\n## The short version\n\nBy agents.\n")
        f = json.loads((root / "data" / "history" / "provenance.json").read_text())
        self.assertEqual(f, out)
        self.assertEqual((out["counts"]["topics"], out["counts"]["pages"], out["counts"]["pages_draft"], out["counts"]["page_versions"]), (2, 2, 1, 3))
        self.assertEqual((out["counts"]["artifacts"], out["counts"]["artifacts_by_type"], out["counts"]["artifacts_local"], out["counts"]["artifacts_linked"]), (5, {"image": 2, "track": 2, "quote": 1}, 1, 2))
        self.assertEqual(out["counts"]["waypoints"], 2)
        self.assertEqual((out["counts"]["sources"], out["counts"]["sources_primary"], out["counts"]["sources_local"], out["counts"]["languages"]), (3, 1, 1, 2))
        self.assertEqual(out["counts"]["people"], 0)                                     # a table the database lacks counts as nothing
        self.assertEqual((out["work"]["worklog"], out["work"]["runs"], out["work"]["review_entries"], out["work"]["first"][:10], out["work"]["last"][:10]), (3, 2, 1, "2026-09-08", "2026-09-09"))
        self.assertEqual(out["work"]["requests"], {"approved": 1, "denied": 1, "done": 1})
        self.assertEqual((out["machine"]["keywords"], out["machine"]["keyword_artifacts"], out["machine"]["keyword_models"], out["machine"]["ontology_model"]), (2, 1, ["gemma4"], "gemma4:26b"))
        self.assertEqual((out["machine"]["faces"], out["machine"]["faces_identified"], out["machine"]["animals_detected"], out["machine"]["detectors"], out["machine"]["rejected_by_operator"]), (3, 1, 1, ["yolo11x", "yunet-2023mar"], 1))
        self.assertIsNone(history.provenance(root, root / "missing.sqlite"))
