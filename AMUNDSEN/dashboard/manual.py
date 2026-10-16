"""The manual: ``docs/manual/`` rendered into the web root at ``manual/``.

Two parts, each a folder of chapters named ``NN-topic.md`` and read in that
order; a chapter's first line is its ``# Title``:

    docs/manual/user/    the user manual: every tab, slowly, with screenshots
    docs/manual/admin/   the administrator manual: how it runs and how to run it
    docs/manual/img/     the pictures both refer to (``img/name.webp``)

``publish(root)`` writes ``manual/index.html`` (the two parts), a page per
chapter with the part's contents beside it, and ``manual/print.html``: the
whole manual as one document with a title page, contents and a page break
before each chapter, for printing or saving as a PDF. It is called by every
full build and does nothing when the sources have not changed since the last
time (a stamp of their contents in ``manual/.stamp``).
"""

from __future__ import annotations

import hashlib
import html
import logging
import re
import shutil
from dataclasses import dataclass
from pathlib import Path

log = logging.getLogger(__name__)

SRC = Path(__file__).resolve().parents[1] / "docs" / "manual"
PARTS = (("user", "User manual", "For everyone aboard and ashore: what the dashboard shows and how to use it, step by step."),
         ("admin", "Administrator manual", "For whoever installs and runs it: how it works, where everything is, what to check."))
EXTENSIONS = ["tables", "fenced_code", "attr_list", "admonition", "def_list", "toc", "sane_lists"]
TITLE = "CCGS Amundsen underway dashboard"


@dataclass
class Chapter:
    part: str
    slug: str
    title: str
    body: str           # rendered HTML, without the title
    toc: list           # [(level, id, text)] of its ## and ### headings


def _render(text: str, prefix: str = "") -> tuple[str, list]:
    import markdown
    md = markdown.Markdown(extensions=EXTENSIONS, extension_configs={"toc": {"permalink": False, "toc_depth": "2-3"}})
    out = md.convert(text)
    if prefix:            # one document holds every chapter: ids must not collide
        out = re.sub(r'id="([^"]+)"', lambda m: f'id="{prefix}-{m[1]}"', out)
        out = re.sub(r'href="#([^"]+)"', lambda m: f'href="#{prefix}-{m[1]}"', out)
    toc = [(t["level"], (f"{prefix}-" if prefix else "") + t["id"], t["name"]) for t in _flatten(md.toc_tokens)]
    return out, toc


def _flatten(tokens):
    for t in tokens:
        yield t
        yield from _flatten(t.get("children", []))


def chapters(src: Path = SRC) -> dict[str, list[Chapter]]:
    """Every part's chapters, in order."""
    parts = {}
    for key, _, _ in PARTS:
        out = []
        for path in sorted((src / key).glob("[0-9][0-9]-*.md")):
            text = path.read_text(encoding="utf-8")
            first, _, rest = text.partition("\n")
            title = first.lstrip("#").strip() if first.startswith("# ") else path.stem[3:].replace("-", " ").capitalize()
            body = rest if first.startswith("# ") else text
            slug = path.stem[3:]
            rendered, toc = _render(body)
            out.append(Chapter(key, slug, title, rendered, toc))
        parts[key] = out
    return parts


def _stamp(src: Path) -> str:
    h = hashlib.sha1(Path(__file__).read_bytes())
    for p in sorted(src.rglob("*")):
        if p.is_file():
            h.update(str(p.relative_to(src)).encode())
            h.update(p.read_bytes())
    return h.hexdigest()


def _page(title: str, body: str, css: str = "manual.css", extra_head: str = "") -> str:
    return (f'<!doctype html><html lang="en"><head><meta charset="utf-8">'
            f'<meta name="viewport" content="width=device-width, initial-scale=1">'
            f'<title>{html.escape(title)}</title><link rel="stylesheet" href="{css}">{extra_head}</head>'
            f'<body>{body}</body></html>')


def _chapter_page(part: tuple, chs: list[Chapter], i: int) -> str:
    key, name, _ = part
    ch = chs[i]
    nav = "".join(
        f'<li class="{"on" if j == i else ""}"><a href="{c.slug}.html">{html.escape(c.title)}</a>'
        + (("<ul>" + "".join(f'<li><a href="#{hid}">{html.escape(text)}</a></li>' for level, hid, text in c.toc if level == 2) + "</ul>") if j == i and c.toc else "")
        + "</li>" for j, c in enumerate(chs))
    prev = f'<a class="prev" href="{chs[i - 1].slug}.html">← {html.escape(chs[i - 1].title)}</a>' if i else "<span></span>"
    nxt = f'<a class="next" href="{chs[i + 1].slug}.html">{html.escape(chs[i + 1].title)} →</a>' if i + 1 < len(chs) else "<span></span>"
    body = (f'<header class="top"><a href="../index.html">Manual</a> › <a href="index.html">{html.escape(name)}</a>'
            f'<span class="sp"></span><a href="../print.html">Print the whole manual</a> · <a href="../../">Dashboard</a></header>'
            f'<div class="layout"><nav class="side"><h2>{html.escape(name)}</h2><ol>{nav}</ol></nav>'
            f'<main><article><h1>{html.escape(ch.title)}</h1>{_up(ch.body)}</article>'
            f'<footer class="pager">{prev}{nxt}</footer></main></div>')
    return _page(f"{ch.title} · {name}", body, css="../manual.css")


def _up(body: str) -> str:
    """A chapter page sits in its part's folder: its pictures are one up."""
    return re.sub(r'(src|href)="img/', r'\1="../img/', body)


def _part_index(part: tuple, chs: list[Chapter]) -> str:
    key, name, blurb = part
    items = "".join(f'<li><a href="{c.slug}.html">{html.escape(c.title)}</a></li>' for c in chs)
    body = (f'<header class="top"><a href="../index.html">Manual</a> › {html.escape(name)}'
            f'<span class="sp"></span><a href="../print.html">Print the whole manual</a> · <a href="../../">Dashboard</a></header>'
            f'<main class="solo"><h1>{html.escape(name)}</h1><p class="lede">{html.escape(blurb)}</p><ol class="contents">{items}</ol></main>')
    return _page(name, body, css="../manual.css")


def _landing(parts: dict) -> str:
    cards = "".join(
        f'<a class="card" href="{key}/index.html"><h2>{html.escape(name)}</h2><p>{html.escape(blurb)}</p>'
        f'<span>{len(parts[key])} chapters</span></a>' for key, name, blurb in PARTS if parts.get(key))
    body = (f'<header class="top"><span>{html.escape(TITLE)}</span><span class="sp"></span><a href="../">Dashboard</a></header>'
            f'<main class="solo"><h1>Manual</h1><div class="cards">{cards}</div>'
            f'<p><a href="print.html">The whole manual on one page</a>, for printing or saving as a PDF.</p></main>')
    return _page("Manual · " + TITLE, body)


def _print(parts: dict) -> str:
    """Every chapter of both parts as one document, chapter ids prefixed so
    links within a chapter still work."""
    cover = (f'<section class="cover"><h1>{html.escape(TITLE)}</h1><p>User and administrator manual</p></section>')
    toc, blocks = [], []
    for key, name, blurb in PARTS:
        chs = parts.get(key) or []
        if not chs:
            continue
        toc.append(f'<li><a href="#part-{key}">{html.escape(name)}</a><ol>'
                   + "".join(f'<li><a href="#{key}-{c.slug}">{html.escape(c.title)}</a></li>' for c in chs) + "</ol></li>")
        blocks.append(f'<section class="part" id="part-{key}"><h1>{html.escape(name)}</h1><p class="lede">{html.escape(blurb)}</p></section>')
        for c in chs:
            src = (SRC / key).glob(f"[0-9][0-9]-{c.slug}.md")
            text = next(src).read_text(encoding="utf-8").partition("\n")[2]
            rendered, _ = _render(text, prefix=f"{key}-{c.slug}")
            blocks.append(f'<section class="chapter" id="{key}-{c.slug}"><h1>{html.escape(c.title)}</h1>{rendered}</section>')
    body = (f'<header class="top noprint"><a href="index.html">Manual</a><span class="sp"></span>'
            f'<a href="#" onclick="window.print();return false">Print</a> · <a href="../">Dashboard</a></header>'
            f'<main class="print">{cover}<section class="toc"><h1>Contents</h1><ol>{"".join(toc)}</ol></section>{"".join(blocks)}</main>')
    return _page("Manual (print) · " + TITLE, body)


def publish(root: Path, src: Path = SRC, force: bool = False) -> dict | None:
    """Render the manual into ``root/manual``; None when there is no source,
    or nothing changed since the last time."""
    if not src.is_dir():
        return None
    out = Path(root) / "manual"
    stamp = _stamp(src)
    if not force and (out / ".stamp").is_file() and (out / ".stamp").read_text() == stamp:
        return None
    try:
        import markdown  # noqa: F401
    except ImportError:
        log.warning("manual not built: the markdown package is missing (pip install markdown)")
        return None
    parts = chapters(src)
    stage = out.with_name("manual.new")
    shutil.rmtree(stage, ignore_errors=True)
    stage.mkdir(parents=True)
    shutil.copy2(src / "manual.css", stage / "manual.css")
    if (src / "img").is_dir():
        shutil.copytree(src / "img", stage / "img")
    for part in PARTS:
        key = part[0]
        chs = parts.get(key) or []
        if not chs:
            continue
        d = stage / key
        d.mkdir()
        (d / "index.html").write_text(_part_index(part, chs), encoding="utf-8")
        for i in range(len(chs)):
            (d / f"{chs[i].slug}.html").write_text(_chapter_page(part, chs, i), encoding="utf-8")
    (stage / "index.html").write_text(_landing(parts), encoding="utf-8")
    (stage / "print.html").write_text(_print(parts), encoding="utf-8")
    (stage / ".stamp").write_text(stamp)
    old = out.with_name("manual.old")
    shutil.rmtree(old, ignore_errors=True)
    if out.exists():
        out.rename(old)
    stage.rename(out)
    shutil.rmtree(old, ignore_errors=True)
    n = {k: len(v) for k, v in parts.items()}
    log.info("manual: %s chapters", n)
    return n
