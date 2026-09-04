"""Command-line entry point.

    python -m underway legs                      list legs found on the shares
    python -m underway build --root DIR          ingest new files from every leg, rebuild DIR
    python -m underway serve --root DIR [--port N]
    python -m underway gcal-push                 push queued calendar items, refresh the feeds
"""

from __future__ import annotations

import argparse
import logging
import sys
from pathlib import Path

from .build import build
from .legs import RootsUnavailable, discover
from .serve import serve

DEFAULT_TITLE = "CCGS Amundsen — Underway"


def main(argv: list[str] | None = None) -> int:
    p = argparse.ArgumentParser(prog="underway", description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("-v", "--verbose", action="store_true")
    sub = p.add_subparsers(dest="cmd", required=True)

    sub.add_parser("legs", help="list discovered legs")

    b = sub.add_parser("build", help="ingest new files and regenerate the dashboard")
    b.add_argument("--root", required=True, type=Path, help="web root to write into")
    b.add_argument("--title", default=DEFAULT_TITLE)
    b.add_argument("--link", action="append", default=[], metavar="LABEL|URL", help="footer link; repeatable")

    s = sub.add_parser("serve", help="serve the web root")
    s.add_argument("--root", required=True, type=Path)
    s.add_argument("--port", type=int, default=8042)
    s.add_argument("--bind", default="0.0.0.0")

    sub.add_parser("gcal-push", help="push queued Google Calendar items and refresh the feed cache")

    a = p.parse_args(argv)
    logging.basicConfig(level=logging.DEBUG if a.verbose else logging.INFO,
                        format="%(asctime)s %(levelname)s %(name)s: %(message)s", datefmt="%H:%M:%S")

    if a.cmd == "legs":
        try:
            found = discover()
        except RootsUnavailable as e:
            logging.error("%s", e)
            return 2
        for l in found:
            print(f"{'*' if l.live else ' '} {l.id:<13} {l.files:3d} files {l.bytes/1e6:6.0f} MB "
                  f"{l.first_date}..{l.last_date}  stations={'yes' if l.stations else 'no'}")
        return 0

    if a.cmd == "serve":
        serve(a.root, a.port, a.bind)
        return 0

    if a.cmd == "gcal-push":
        from .gcal import push
        push()
        return 0

    links = []
    for s_ in a.link:
        label, _, url = s_.partition("|")
        if url:
            links.append({"label": label.strip(), "url": url.strip()})
    try:
        r = build(a.root, a.title, links)
    except RootsUnavailable as e:
        # exit non-zero so the systemd timer surfaces it instead of quietly
        # republishing an empty dashboard over a good one
        logging.error("%s", e)
        return 2
    if r["unresolved"]:
        logging.warning("unresolved variables: %s", ", ".join(r["unresolved"]))
    return 0


if __name__ == "__main__":
    sys.exit(main())
