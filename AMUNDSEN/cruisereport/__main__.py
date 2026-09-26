"""``python -m cruisereport serve [--port N] [--bind ADDR]``, or ``cruise-report serve``."""

import argparse
import logging


def main(argv=None):
    from .server import serve

    ap = argparse.ArgumentParser(prog="cruise-report")
    sub = ap.add_subparsers(dest="cmd", required=True)
    s = sub.add_parser("serve", help="serve the report page")
    s.add_argument("--port", type=int, default=8044)
    s.add_argument("--bind", default="0.0.0.0")
    args = ap.parse_args(argv)
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(name)s: %(message)s")
    if args.cmd == "serve":
        serve(args.port, args.bind)


if __name__ == "__main__":
    main()
