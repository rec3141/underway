"""The historian sees a generated page's row fields again, now that the
published body is the row's prose alone."""
import json
import tempfile
import unittest
from pathlib import Path

from dashboard import chatbot


class RowFactsTests(unittest.TestCase):
    def test_facts_from_the_published_rows(self):
        tmp = tempfile.TemporaryDirectory(); self.addCleanup(tmp.cleanup)
        root = Path(tmp.name); h = root / "data" / "history"; (h / "pages").mkdir(parents=True)
        (h / "artifacts.json").write_text(json.dumps({"artifacts": [
            {"id": "f-1", "page": "artifact/f-1", "type": "map", "date_text": "1859", "creator": "M'Clintock", "lat": 69.1, "lon": -98.0,
             "credit": "LAC", "bibkey": "mcclintock1859", "pages": "pl. 3", "people": ["John Rae"]}]}))
        (h / "places.json").write_text(json.dumps({"places": [{"name": "Gjoa Haven", "page": "place/gjoa-haven", "kind": "community", "inuktitut": "Uqsuqtuuq", "lat": 68.6, "lon": -95.9}]}))
        (h / "people.json").write_text(json.dumps({"people": [{"name": "John Rae", "page": "person/john-rae", "role": "surgeon", "born": "1813", "died": "1893", "indigenous": False}]}))
        (h / "vessels.json").write_text(json.dumps({"vessels": [{"name": "Fox", "page": "vessel/fox", "kind": "yacht", "kind_label": "yacht", "kind_note": "steam auxiliary", "lost": "", "built": "1855"}]}))
        (h / "events.json").write_text(json.dumps({"events": [{"id": 7, "date_start": "1859-05-06", "place": "Point Victory", "people": ["Hobson"], "bibkey": "mcclintock1859"}]}))
        facts = chatbot.row_facts(root)
        self.assertEqual(facts["artifact/f-1"], "kind: map; date: 1859; creator: M'Clintock; position: 69.100, -98.000; credit: LAC; source: mcclintock1859; pages: pl. 3; people: John Rae")
        self.assertEqual(facts["place/gjoa-haven"], "kind: community; Inuktitut name: Uqsuqtuuq; position: 68.600, -95.900")
        self.assertEqual(facts["person/john-rae"], "role: surgeon; lived: 1813–1893")
        self.assertEqual(facts["vessel/fox"], "kind: yacht, steam auxiliary; built: 1855")
        self.assertEqual(facts["event/7"], "date: 1859-05-06; place: Point Victory; people: Hobson; source: mcclintock1859")
        # the facts ride on the page's text for matching and for the excerpt
        (h / "index.json").write_text("{}")
        (h / "pages" / "artifact__f-1.json").write_text(json.dumps({"slug": "artifact/f-1", "kind": "artifact", "title": "Chart", "html": "<p>The chart.</p>"}))
        (h / "pages" / "the-search.json").write_text(json.dumps({"slug": "the-search", "kind": "page", "title": "The search", "html": "<p>Prose.</p>"}))
        chatbot._wiki_cache.update(stamp=None)
        pages = {p["slug"]: p for p in chatbot.wiki_pages(root)}
        self.assertEqual(pages["artifact/f-1"]["_text"], "The chart.\nkind: map; date: 1859; creator: M'Clintock; position: 69.100, -98.000; credit: LAC; source: mcclintock1859; pages: pl. 3; people: John Rae")
        self.assertIn("mcclintock1859", pages["artifact/f-1"]["_words"])
        self.assertEqual(pages["the-search"]["_text"].strip(), "Prose.")
