"""Command-line entry point.

    python -m dashboard legs                      list legs found on the shares
    python -m dashboard build --root DIR          ingest new files from every leg, rebuild DIR
    python -m dashboard serve --root DIR [--port N]
    python -m dashboard gcal-push                 push queued calendar items, refresh the feeds
    python -m dashboard alerts                    send due schedule alerts (email, Telegram)
    python -m dashboard telegram-bot              answer the Telegram bot's commands as they arrive
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
    p = argparse.ArgumentParser(prog="dashboard", description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("-v", "--verbose", action="store_true")
    sub = p.add_subparsers(dest="cmd", required=True)

    sub.add_parser("legs", help="list discovered legs")

    b = sub.add_parser("build", help="ingest new files and regenerate the dashboard")
    b.add_argument("--root", required=True, type=Path, help="web root to write into")
    b.add_argument("--tracks-only", action="store_true", help="rebuild track windows while preserving the ship manifest and page")
    b.add_argument("--title", default=DEFAULT_TITLE)
    b.add_argument("--link", action="append", default=[], metavar="LABEL|URL", help="footer link; repeatable")

    s = sub.add_parser("serve", help="serve the web root")
    s.add_argument("--root", required=True, type=Path)
    s.add_argument("--port", type=int, default=8042)
    s.add_argument("--bind", default="0.0.0.0")

    sub.add_parser("gcal-push", help="push queued Google Calendar items and refresh the feed cache")
    sub.add_parser("alerts", help="send due schedule alerts (email and Telegram)")
    sub.add_parser("telegram-bot", help="answer the Telegram bot's commands as they arrive (runs until stopped)")
    sub.add_parser("codex-bot", help="run the dedicated single-session Codex Telegram bot")
    st = sub.add_parser("satellite", help="render recent Sentinel imagery around the ship when due")
    st.add_argument("--force", action="store_true", help="render now regardless of age and distance")
    st.add_argument("--backfill", metavar="YYYY-MM-DD", help="fill the archive from this day on: a picture a day per sensor of the box round the ship")
    st.add_argument("--within-km", type=float, default=500.0, help="the backfilled box reaches this far from the ship (default 500)")

    ic = sub.add_parser("ice-charts", help="cache public CIS vector or daily raster charts")
    source = ic.add_mutually_exclusive_group(required=True)
    source.add_argument("--list", action="store_true", help="list current regional chart download URLs")
    source.add_argument("--refresh", action="store_true",
                        help="cache every advertised chart not already held (what the timer runs)")
    source.add_argument("--daily", choices=("WIS36C",), metavar="PRODUCT",
                        help="download and georeference the latest daily chart (WIS36C: ship area)")
    source.add_argument("--url", help="HTTPS URL of one ZIP or TAR chart archive")
    source.add_argument("--file", type=Path, help="local ZIP, TAR, or SHP (with companions)")
    ic.add_argument("--date", help="chart valid date, YYYY-MM-DD")
    ic.add_argument("--region", help="region name, e.g. Eastern Arctic")
    ic.add_argument("--source-url", help="HTTPS provenance URL for a local import")

    a = p.parse_args(argv)
    logging.basicConfig(level=logging.DEBUG if a.verbose else logging.INFO,
                        format="%(asctime)s %(levelname)s %(name)s: %(message)s", datefmt="%H:%M:%S")

    if a.cmd == "ice-charts":
        import json
        from .ice_charts import available, import_chart, import_daily_chart, refresh
        if not a.list and not a.daily and not a.refresh and (not a.date or not a.region):
            p.error("ice-charts imports require --date and --region")
        try:
            result = (available() if a.list else refresh() if a.refresh else
                      import_daily_chart(a.daily) if a.daily else
                      import_chart(date=a.date, region=a.region, url=a.url, file=a.file,
                                   source_url=a.source_url))
        except Exception as exc:
            logging.error("Ice chart import failed: %s", exc)
            return 2
        print(json.dumps(result, indent=2, ensure_ascii=False))
        # a refresh reports trouble only when it got nothing at all: one region
        # the Ice Service has not posted must not fail every run of the timer
        if a.refresh and result["failed"] and not (result["added"] or result["kept"]):
            return 2
        return 0

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

    if a.cmd == "alerts":
        from .alerts import run
        run()
        return 0

    if a.cmd == "telegram-bot":
        from .alerts import bot_loop
        bot_loop()
        return 0

    if a.cmd == "codex-bot":
        from .telegram_codex import bot_loop
        bot_loop()
        return 0

    if a.cmd == "satellite":
        from .satellite import backfill, refresh
        if a.backfill:
            from datetime import datetime, timezone
            backfill(datetime.strptime(a.backfill, "%Y-%m-%d").replace(tzinfo=timezone.utc), within_km=a.within_km)
            return 0
        refresh(force=a.force)
        return 0

    links = []
    for s_ in a.link:
        label, _, url = s_.partition("|")
        if url:
            links.append({"label": label.strip(), "url": url.strip()})
    try:
        r = build(a.root, a.title, links, tracks_only=a.tracks_only)
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
