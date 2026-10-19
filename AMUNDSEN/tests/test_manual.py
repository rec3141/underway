"""The manual renders into the web root, and only when its sources change."""
import shutil
from pathlib import Path

import pytest

from dashboard import manual


def source(root: Path) -> Path:
    src = root / "src"
    (src / "user").mkdir(parents=True)
    (src / "admin").mkdir()
    (src / "img").mkdir()
    (src / "manual.css").write_text("body{}")
    (src / "img" / "01-map.webp").write_bytes(b"RIFF")
    (src / "user" / "01-welcome.md").write_text("# Welcome\n\nHello.\n\n## First steps\n\n![The map](img/01-map.webp)\n")
    (src / "user" / "02-map.md").write_text("# The map\n\n!!! tip\n    Zoom in.\n\n| a | b |\n|---|---|\n| 1 | 2 |\n")
    (src / "admin" / "01-overview.md").write_text("# How it works\n\n```sh\nunderway build\n```\n")
    (src / "STYLE.md").write_text("# not a chapter\n")
    return src


def test_parts_chapters_and_the_print_page(tmp_path):
    src = source(tmp_path)
    www = tmp_path / "www"
    assert manual.publish(www, src) == {"user": 2, "admin": 1}
    out = www / "manual"
    page = (out / "user" / "welcome.html").read_text()
    assert "<h1>Welcome</h1>" in page and 'src="../img/01-map.webp"' in page        # pictures are one folder up
    assert 'href="map.html">The map →' in page                                      # the next chapter
    assert (out / "img" / "01-map.webp").is_file()
    assert 'class="admonition tip"' in (out / "user" / "map.html").read_text()
    assert not (out / "user" / "STYLE.html").exists()
    printed = (out / "print.html").read_text()
    assert printed.index("Welcome") < printed.index("The map") < printed.index("How it works")
    assert 'src="img/01-map.webp"' in printed and 'id="user-welcome-first-steps"' in printed
    assert "User manual" in (out / "index.html").read_text()


def test_unchanged_sources_are_not_rendered_again(tmp_path):
    src = source(tmp_path)
    www = tmp_path / "www"
    manual.publish(www, src)
    assert manual.publish(www, src) is None
    (src / "user" / "02-map.md").write_text("# The map\n\nChanged.\n")
    assert manual.publish(www, src) == {"user": 2, "admin": 1}
    assert "Changed." in (www / "manual" / "user" / "map.html").read_text()


def test_links_between_chapters_stay_inside_the_print_page(tmp_path):
    src = source(tmp_path)
    (src / "user" / "02-map.md").write_text("# The map\n\nSee [the welcome](welcome.html#first-steps) and [how](../admin/overview.html).\n")
    printed = manual._print(manual.chapters(src), src)
    assert 'href="#user-welcome-first-steps"' in printed and 'href="#admin-overview"' in printed
    user_only = manual._book(manual.chapters(src), src, ("user",), pdf=True)
    assert '<a class="away">how</a>' in user_only                       # a chapter not in this PDF


def test_without_weasyprint_there_is_no_pdf_and_no_link(tmp_path, monkeypatch):
    src = source(tmp_path)
    (src / "manual-pdf.css").write_text("@page { size: letter; }")
    monkeypatch.setattr(manual, "_weasyprint", lambda: None)
    www = tmp_path / "www"
    assert manual.publish(www, src) == {"user": 2, "admin": 1}
    assert not list((www / "manual").glob("*.pdf"))
    assert ".pdf" not in (www / "manual" / "index.html").read_text() + (www / "manual" / "print.html").read_text()


def test_the_pdfs(tmp_path):
    pytest.importorskip("weasyprint")
    from PIL import Image
    src = source(tmp_path)
    Image.new("RGB", (40, 30), "steelblue").save(src / "img" / "01-map.webp", "WEBP")
    shutil.copy2(manual.SRC / "manual-pdf.css", src / "manual-pdf.css")
    www = tmp_path / "www"
    n = manual.publish(www, src)
    out = www / "manual"
    assert set(n["pdf"]) == {"underway-manual.pdf", "underway-user-manual.pdf", "underway-admin-manual.pdf"}
    for name in n["pdf"]:
        assert (out / name).read_bytes().startswith(b"%PDF")
    assert n["pdf"]["underway-manual.pdf"] > n["pdf"]["underway-user-manual.pdf"]
    assert 'href="underway-manual.pdf"' in (out / "index.html").read_text()
    assert 'href="underway-manual.pdf">Download PDF' in (out / "print.html").read_text()
    assert 'href="../underway-user-manual.pdf"' in (out / "user" / "index.html").read_text()
    book = manual._book(manual.chapters(src), src, ("user",), pdf=True)
    assert "<figcaption>The map</figcaption>" in book and "How it works" not in book
    assert '<table class="short">' in book                               # kept on one page
    assert manual.publish(www, src) is None                              # unchanged: not typeset again
