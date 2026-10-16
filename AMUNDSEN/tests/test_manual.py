"""The manual renders into the web root, and only when its sources change."""
from pathlib import Path

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
