"""Turn raw observations into the analysis frame the dashboard is built from:
a coalesced ship position, distance and elapsed time, resolved variables, and
the surprise score.
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass

import numpy as np
import pandas as pd

from .config import (LINE_WARMING, POSITION_CANDIDATES, SURPRISE, SURPRISE_FEATURES, SURPRISE_SCALES, VARIABLES,
                     Variable)
from .surprise import surprise_scores

log = logging.getLogger(__name__)

EARTH_RADIUS_KM = 6371.0088
# Consecutive samples further apart than this are a break in the record (a
# port call, a season boundary), not a leg the ship travelled.
DISTANCE_BREAK = pd.Timedelta(hours=1)


@dataclass
class Resolution:
    variable: Variable
    key: str | None
    display: str | None

    @property
    def resolved(self) -> bool:
        return self.variable.derived or self.key is not None


def _first_match(patterns: tuple[str, ...], keys: list[str]) -> str | None:
    for pat in patterns:
        rx = re.compile(pat, re.IGNORECASE)
        for k in keys:
            if rx.search(k):
                return k
    return None


def resolve_variables(keys: list[str], display: dict[str, str]) -> list[Resolution]:
    out = []
    for v in VARIABLES:
        k = None if v.derived else v.resolve(keys)
        out.append(Resolution(v, k, display.get(k) if k else None))
        if not v.derived and k is None:
            log.warning("unresolved variable: %s (tried %s)", v.name, ", ".join(v.candidates))
    return out


def resolve_position(keys: list[str]) -> list[tuple[str, str]]:
    pairs = []
    for lat_pat, lon_pat in POSITION_CANDIDATES:
        lat, lon = _first_match((lat_pat,), keys), _first_match((lon_pat,), keys)
        if lat and lon and (lat, lon) not in pairs:
            pairs.append((lat, lon))
    return pairs


def resolve_features(keys: list[str]) -> list[str]:
    feats = []
    for pats in SURPRISE_FEATURES:
        k = _first_match(pats, keys)
        if k and k not in feats:
            feats.append(k)
    return feats


def needed_keys(keys: list[str], display: dict[str, str]) -> tuple[list[str], list[Resolution], list[tuple[str, str]], list[str]]:
    """Everything the build must read from the store."""
    res = resolve_variables(keys, display)
    pos = resolve_position(keys)
    feats = resolve_features(keys)
    want = {r.key for r in res if r.key} | {k for pair in pos for k in pair} | set(feats)
    want |= {k for k in (_first_match(pats, keys) for pats in LINE_WARMING) if k}
    return sorted(want), res, pos, feats


# ---------------------------------------------------------------- geometry

def haversine_km(lat1, lon1, lat2, lon2):
    lat1, lon1, lat2, lon2 = map(np.radians, (lat1, lon1, lat2, lon2))
    a = np.sin((lat2 - lat1) / 2) ** 2 + np.cos(lat1) * np.cos(lat2) * np.sin((lon2 - lon1) / 2) ** 2
    return 2 * EARTH_RADIUS_KM * np.arcsin(np.sqrt(a))


def coalesce_position(df: pd.DataFrame, pairs: list[tuple[str, str]]) -> tuple[pd.Series, pd.Series, str | None]:
    lat = pd.Series(np.nan, index=df.index)
    lon = pd.Series(np.nan, index=df.index)
    used = None
    for lat_k, lon_k in pairs:
        ok = lat.isna() & df[lat_k].notna() & df[lon_k].notna()
        # reject obvious junk fixes
        ok &= df[lat_k].abs().le(90) & df[lon_k].abs().le(180) & ~(df[lat_k].eq(0) & df[lon_k].eq(0))
        if ok.any():
            lat[ok] = df.loc[ok, lat_k]
            lon[ok] = df.loc[ok, lon_k]
            used = used or lat_k.split(" — ")[0]
    return _drop_position_spikes(df.index, lat, lon) + (used,)


# A fix that would need the ship to exceed this speed to reach it is a receiver
# glitch, not a position; drawing it puts a spoke across the map.
MAX_PLAUSIBLE_KN = 40.0


def _drop_position_spikes(t: pd.DatetimeIndex, lat: pd.Series, lon: pd.Series) -> tuple[pd.Series, pd.Series]:
    good = lat.notna() & lon.notna()
    idx = np.flatnonzero(good.to_numpy())
    if idx.size < 3:
        return lat, lon
    la, lo = lat.to_numpy(float)[idx], lon.to_numpy(float)[idx]
    ts = t.as_unit("s").asi8.astype(float)[idx]
    dt_h = np.diff(ts) / 3600.0
    with np.errstate(divide="ignore", invalid="ignore"):
        kn = haversine_km(la[:-1], lo[:-1], la[1:], lo[1:]) / dt_h / 1.852
    # a segment is "fast" only when it is also short in time: across a real
    # gap the ship may legitimately be anywhere
    fast = (kn > MAX_PLAUSIBLE_KN) & (dt_h > 0) & (dt_h <= DISTANCE_BREAK.total_seconds() / 3600.0)
    # A spike is implausible from both neighbours (out and back). A step —
    # implausible from the previous fix only, with the track continuing from
    # the new place — is a logging discontinuity: the fixes after it are good,
    # so they are kept and the line is broken at the step when drawn.
    spike = np.zeros(len(idx), dtype=bool)
    spike[1:-1] = fast[:-1] & fast[1:]
    bad = np.zeros(len(lat), dtype=bool)
    bad[idx[spike]] = True
    steps = int(fast.sum() - 2 * spike.sum())
    if bad.any() or steps > 0:
        log.info("position: dropped %d spike fixes; %d discontinuities kept as breaks (> %.0f kn)",
                 int(bad.sum()), max(steps, 0), MAX_PLAUSIBLE_KN)
        lat = lat.copy(); lon = lon.copy()
        lat[bad] = np.nan; lon[bad] = np.nan
    return lat, lon


def cumulative_distance_km(t: pd.DatetimeIndex, lat: pd.Series, lon: pd.Series) -> pd.Series:
    """Distance along track, ignoring jumps across breaks in the record."""
    good = lat.notna() & lon.notna()
    la, lo = lat[good].to_numpy(), lon[good].to_numpy()
    tt = t[good]
    step = np.zeros(len(la))
    if len(la) > 1:
        d = haversine_km(la[:-1], lo[:-1], la[1:], lo[1:])
        dt = np.diff(tt.as_unit("s").asi8).astype(float)
        d[dt > DISTANCE_BREAK.total_seconds()] = 0.0
        # a discontinuity in the log is not distance the ship travelled
        with np.errstate(divide="ignore", invalid="ignore"):
            d[(d / (dt / 3600.0) / 1.852) > MAX_PLAUSIBLE_KN] = 0.0
        d[~np.isfinite(d)] = 0.0
        step[1:] = d
    out = pd.Series(np.nan, index=t)
    out[good] = np.cumsum(step)
    return out.ffill()


# ---------------------------------------------------------------- assembly

@dataclass
class Analysis:
    frame: pd.DataFrame                 # raw cadence; columns = variable names + lat/lon/dist_km
    resolutions: list[Resolution]
    position_source: str | None
    surprise_features: list[str]
    surprise_note: str


def build_analysis(df: pd.DataFrame, res: list[Resolution], pos_pairs: list[tuple[str, str]],
                   feats: list[str], display: dict[str, str]) -> Analysis:
    if df.empty:
        raise SystemExit("no observations in store")

    lat, lon, pos_src = coalesce_position(df, pos_pairs)
    out = pd.DataFrame(index=df.index)
    out["lat"], out["lon"] = lat, lon
    out["dist_km"] = cumulative_distance_km(df.index, lat, lon)

    for r in res:
        if r.key:
            out[r.variable.name] = df[r.key]

    sal_t, hull_t = (_first_match(pats, list(df.columns)) for pats in LINE_WARMING)
    if sal_t and hull_t:
        out["TSG line warming (°C)"] = df[sal_t] - df[hull_t]

    # elapsed time is filled per window at build time; keep a placeholder so
    # the column set is stable
    out["Time elapsed (h)"] = np.nan
    out["Distance travelled (km)"] = out["dist_km"]

    # surprise on minute medians of the feature set
    note = ""
    scored = False
    if len(feats) >= 2:
        minute = df[feats].resample("1min").median()
        minute = minute.dropna(how="all")
        sc = surprise_scores(minute, SURPRISE)
        if sc is not None:
            # broadcast the minute scores back onto the raw cadence
            m = df.index.floor("1min")
            for col in sc.columns:
                out[col] = sc[col].reindex(m).to_numpy()
            scored = True
            note = ("each minute against exponentially weighted history at half-lives "
                    + ", ".join(l for l, _ in SURPRISE_SCALES) + " using "
                    + ", ".join(display.get(f, f) for f in feats))
        else:
            note = "not enough data to score"
    else:
        note = "fewer than two feature columns available"
    if not scored:
        for v in VARIABLES:
            if v.name.startswith("Surprise"):
                out[v.name] = np.nan

    return Analysis(out, res, pos_src, feats, note)
