"""Ada's answers: what becomes a link, what loses its brackets, and the
chips chosen from the pages she cites."""
import json
from pathlib import Path

import pytest

from dashboard import chat


PAGES = [{"slug": "sverdrup-four-winters", "title": "Four winters on a new coast", "kind": "page"},
         {"slug": "person/sverre-hassel", "title": "Sverre Hassel", "kind": "person"}]


def test_numbers_and_titles_become_links_and_the_rest_lose_their_brackets():
    text = ("Hassel drove the dogs [2]. The sledge left in March [1901-03-20/1901-05-12] and the "
            "journal [Four winters on a new coast] has the dates [see the chart].")
    out, refs = chat.link_citations(text, PAGES)
    assert "[1](#history/person/sverre-hassel)" in out            # the first citation is number 1
    assert "[2](#history/sverdrup-four-winters)" in out           # a title in brackets resolves to its page
    assert "1901-03-20 to 1901-05-12" in out and "[1901" not in out   # a date loses its brackets
    assert "see the chart" in out and "[see the chart]" not in out    # so does anything unresolved
    assert [r["slug"] for r in refs] == ["person/sverre-hassel", "sverdrup-four-winters"]


def test_chips_come_from_the_cited_pages_in_order_and_fit_the_paragraphs(tmp_path, monkeypatch):
    hist = tmp_path / "data" / "history"
    (hist / "pages").mkdir(parents=True)
    arts = [{"id": "a-1", "type": "image", "title": "The Fram in the ice", "thumb": "data/history/thumbs/a-1.jpg", "page": "artifact/a-1", "date_text": "1899", "people": []},
            {"id": "a-2", "type": "quote", "title": "Sverdrup on the dogs", "description": 'He wrote: "the dogs were the whole of it, and without them we should have gone nowhere at all" (p. 12)', "page": "artifact/a-2", "date_text": "1901", "people": ["Sverre Hassel"]},
            {"id": "a-3", "type": "text", "title": "A report", "page": "artifact/a-3", "people": []},
            {"id": "a-4", "type": "image", "title": "No thumbnail yet", "page": "artifact/a-4", "people": []}]
    (hist / "artifacts.json").write_text(json.dumps({"artifacts": arts}))
    (hist / "pages" / "sverdrup-four-winters.json").write_text(json.dumps({"html": "See [the ship](artifact/a-1), [a report](artifact/a-3) and [the words](artifact/a-2)."}))
    monkeypatch.setattr(chat, "ROOT", tmp_path)
    chat._art_cache.update(stamp=None, by_id={})
    three = "one\n\ntwo\n\nthree"
    chips = chat.answer_chips(PAGES, three)
    assert [c["slug"] for c in chips] == ["artifact/a-1", "artifact/a-2"]     # the picture first, the text skipped, the quote from the person's own list
    assert chips[0]["thumb"] == "data/history/thumbs/a-1.jpg" and chips[1]["quote"].startswith("the dogs were the whole of it")
    assert len(chat.answer_chips(PAGES, "one paragraph only")) == 1          # one chip for one paragraph
    assert chat.answer_chips([], three) == []


def test_quote_of_prefers_the_quoted_passage():
    assert chat.quote_of('As Parry put it, "a long sentence of at least forty characters, quoted here in full" and more.') == "a long sentence of at least forty characters, quoted here in full"
    assert chat.quote_of("x" * 300).endswith("…") and len(chat.quote_of("x" * 300)) <= 240
