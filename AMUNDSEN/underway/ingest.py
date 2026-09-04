"""Incremental ingest of ACSD_YYYYMMDD.csv files into a local SQLite store.

The store is wide (one column per observed quantity) but schema-free: columns
are created as new header entries appear, so files from legs with different
instrument sets coexist. Rows carry the file they came from, so a changed file
is reloaded by deleting and re-inserting only its own rows.
"""

from __future__ import annotations

import csv
import io
import logging
import os
import sqlite3
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd

from .config import FILE_PATTERN, canonical_key, display_name

log = logging.getLogger(__name__)

SCHEMA = """
CREATE TABLE IF NOT EXISTS files (
    name     TEXT PRIMARY KEY,
    path     TEXT NOT NULL,
    size     INTEGER NOT NULL,
    mtime    REAL NOT NULL,
    rows     INTEGER NOT NULL,
    loaded   REAL NOT NULL
);
CREATE TABLE IF NOT EXISTS columns (
    col      TEXT PRIMARY KEY,      -- identifier used in obs
    key      TEXT NOT NULL UNIQUE,  -- canonical lookup key
    display  TEXT NOT NULL,         -- "Instrument — Variable" as written in the file
    first_seen TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS obs (
    t        INTEGER NOT NULL,      -- seconds since epoch, UTC
    src      TEXT NOT NULL          -- file name
);
CREATE INDEX IF NOT EXISTS obs_t   ON obs (t);
CREATE INDEX IF NOT EXISTS obs_src ON obs (src);
"""


@dataclass
class SourceFile:
    name: str
    path: Path
    size: int
    mtime: float


def discover(indirs: list[Path]) -> list[SourceFile]:
    """List ACSD files across all input directories.

    A date present in more than one directory resolves to the earliest
    directory given, so the order of --indir decides precedence.
    """
    seen: dict[str, SourceFile] = {}
    for d in indirs:
        if not d.is_dir():
            log.warning("input directory missing: %s", d)
            continue
        for p in sorted(d.iterdir()):
            if not FILE_PATTERN.match(p.name) or p.name in seen:
                continue
            st = p.stat()
            seen[p.name] = SourceFile(p.name, p, st.st_size, st.st_mtime)
    return sorted(seen.values(), key=lambda f: f.name)


# ---------------------------------------------------------------- parsing

def _read_text(path: Path) -> str:
    raw = path.read_bytes()
    for enc in ("utf-8", "cp1252", "latin-1"):
        try:
            return raw.decode(enc)
        except UnicodeDecodeError:
            continue
    return raw.decode("latin-1", errors="replace")


def parse_file(path: Path) -> tuple[pd.DataFrame, dict[str, str]]:
    """Return (frame indexed by UTC time, {canonical key: display name}).

    Header is two lines: variable names, then the instrument that produced
    each. Values are ';' separated; the first column is the timestamp.
    """
    text = _read_text(path)
    rdr = csv.reader(io.StringIO(text), delimiter=";")
    variables = [c.strip() for c in next(rdr)]
    instruments = [c.strip() for c in next(rdr)]
    if len(instruments) < len(variables):
        instruments += [""] * (len(variables) - len(instruments))

    keys = [canonical_key(i, v) for v, i in zip(variables, instruments)]
    names = {k: display_name(i, v) for k, v, i in zip(keys, variables, instruments)}

    body = pd.read_csv(
        io.StringIO(text), sep=";", skiprows=2, header=None, names=keys,
        usecols=range(len(keys)), engine="c", na_values=["NaN", "nan", "", "NA"],
        dtype=str, skipinitialspace=True,
    )
    if body.empty:
        return body, names

    time_key = keys[0]
    t = pd.to_datetime(body[time_key].str.strip(), format="%Y/%m/%d %H:%M:%S",
                       errors="coerce", utc=True)
    body = body.drop(columns=[time_key])
    body = body.apply(pd.to_numeric, errors="coerce")
    body.index = t
    body = body[~body.index.isna()]
    body = body[~body.index.duplicated(keep="last")].sort_index()
    names.pop(time_key, None)
    return body, names


# ---------------------------------------------------------------- store

class Store:
    def __init__(self, db_path: Path):
        db_path.parent.mkdir(parents=True, exist_ok=True)
        self.db_path = db_path
        self.conn = sqlite3.connect(db_path)
        self.conn.executescript(SCHEMA)
        self.conn.execute("PRAGMA journal_mode=WAL")
        self.conn.execute("PRAGMA synchronous=NORMAL")

    # -- columns ---------------------------------------------------------
    def column_map(self) -> dict[str, str]:
        """canonical key -> obs column identifier"""
        return dict(self.conn.execute("SELECT key, col FROM columns"))

    def display_map(self) -> dict[str, str]:
        """canonical key -> display name"""
        return dict(self.conn.execute("SELECT key, display FROM columns"))

    def ensure_columns(self, names: dict[str, str], src: str) -> dict[str, str]:
        cmap = self.column_map()
        for key, disp in names.items():
            if key in cmap:
                continue
            col = f"c{len(cmap):03d}"
            self.conn.execute("ALTER TABLE obs ADD COLUMN %s REAL" % col)
            self.conn.execute(
                "INSERT INTO columns (col, key, display, first_seen) VALUES (?,?,?,?)",
                (col, key, disp, src))
            cmap[key] = col
            log.debug("new column %s <- %s", col, disp)
        return cmap

    # -- files -----------------------------------------------------------
    def known_files(self) -> dict[str, tuple[int, float]]:
        return {n: (s, m) for n, s, m in
                self.conn.execute("SELECT name, size, mtime FROM files")}

    def load_file(self, f: SourceFile) -> int:
        frame, names = parse_file(f.path)
        cmap = self.ensure_columns(names, f.name)
        cur = self.conn.cursor()
        cur.execute("DELETE FROM obs WHERE src = ?", (f.name,))
        if frame.empty:
            n = 0
        else:
            cols = [cmap[k] for k in frame.columns]
            # resolution-independent: pandas may hold the index in ns or µs
            t = frame.index.as_unit("s").asi8.astype(np.int64)
            vals = frame.to_numpy(dtype=float)
            rows = [(int(ti), f.name, *[None if np.isnan(v) else float(v) for v in row])
                    for ti, row in zip(t, vals)]
            placeholders = ",".join("?" * (2 + len(cols)))
            cur.executemany(
                f"INSERT INTO obs (t, src, {','.join(cols)}) VALUES ({placeholders})", rows)
            n = len(rows)
        cur.execute(
            "INSERT OR REPLACE INTO files (name, path, size, mtime, rows, loaded) "
            "VALUES (?,?,?,?,?,strftime('%s','now'))",
            (f.name, str(f.path), f.size, f.mtime, n))
        self.conn.commit()
        return n

    def drop_missing(self, present: set[str]) -> list[str]:
        """Remove rows for files no longer offered by any input directory."""
        gone = [n for (n,) in self.conn.execute("SELECT name FROM files") if n not in present]
        for n in gone:
            self.conn.execute("DELETE FROM obs WHERE src = ?", (n,))
            self.conn.execute("DELETE FROM files WHERE name = ?", (n,))
        if gone:
            self.conn.commit()
        return gone

    # -- read ------------------------------------------------------------
    def frame(self, since_epoch: int | None = None) -> pd.DataFrame:
        """All observations (optionally from a time) as a wide frame keyed by
        canonical column key, indexed by UTC time."""
        cmap = self.column_map()
        if not cmap:
            return pd.DataFrame()
        inv = {c: k for k, c in cmap.items()}
        cols = ", ".join(["t", *cmap.values()])
        where = f"WHERE t >= {int(since_epoch)}" if since_epoch else ""
        df = pd.read_sql_query(f"SELECT {cols} FROM obs {where} ORDER BY t", self.conn)
        df.index = pd.to_datetime(df.pop("t"), unit="s", utc=True)
        # SQLite NULLs arrive as None in object columns; make them float NaN
        return df.rename(columns=inv).astype("float64")

    def frame_columns(self, keys: list[str], since_epoch: int | None = None) -> pd.DataFrame:
        """Like frame(), restricted to the given canonical keys."""
        cmap = self.column_map()
        keys = [k for k in keys if k in cmap]
        if not keys:
            return pd.DataFrame()
        cols = ", ".join(["t", *[cmap[k] for k in keys]])
        where = f"WHERE t >= {int(since_epoch)}" if since_epoch else ""
        df = pd.read_sql_query(f"SELECT {cols} FROM obs {where} ORDER BY t", self.conn)
        df.index = pd.to_datetime(df.pop("t"), unit="s", utc=True)
        return df.rename(columns={cmap[k]: k for k in keys}).astype("float64")

    def time_range(self) -> tuple[int, int] | None:
        row = self.conn.execute("SELECT MIN(t), MAX(t) FROM obs").fetchone()
        return None if row[0] is None else (row[0], row[1])

    def close(self) -> None:
        self.conn.close()


# ---------------------------------------------------------------- driver

def sync(indirs: list[Path], db_path: Path) -> dict:
    """Bring the store in line with the input directories. Returns a summary."""
    store = Store(db_path)
    files = discover(indirs)
    known = store.known_files()
    changed = [f for f in files if known.get(f.name) != (f.size, f.mtime)]
    rows = 0
    for f in changed:
        n = store.load_file(f)
        rows += n
        log.info("loaded %s (%d rows)", f.name, n)
    gone = store.drop_missing({f.name for f in files})
    for n in gone:
        log.info("dropped %s (no longer in any input directory)", n)
    summary = {
        "files_total": len(files), "files_loaded": len(changed),
        "rows_loaded": rows, "files_dropped": gone,
        "columns": len(store.column_map()),
    }
    store.close()
    return summary
