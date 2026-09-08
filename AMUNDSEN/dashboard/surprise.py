"""Surprise: how unusual each minute is against its own recent past.

Minute-median features, robustly scaled over the record, are compared with
exponentially weighted estimates of their mean and covariance built from the
minutes *before* each one, at several half-lives spaced roughly log-evenly
from a quarter of an hour to two days (``SURPRISE_SCALES``). The squared
Mahalanobis distance at each half-life becomes an upper-tail chi-square
p-value, reported as −log10 p and capped; the combined score is the mean over
half-lives.

The short half-lives fire the moment a front is crossed — every time, since
their reference is only the last hour or so of water — and settle again
within a few half-lives; the long ones stay raised while the ship is in
water unlike the last day or two. The mean across scales therefore spikes at
a crossing and fades roughly logarithmically afterwards.
"""

from __future__ import annotations

import logging

import numpy as np
import pandas as pd
from scipy.stats import chi2

from .config import SURPRISE_NAME, SURPRISE_SCALES, SurpriseConfig, surprise_scale_name

log = logging.getLogger(__name__)


def _winsor(x: np.ndarray, p: tuple[float, float], bounds: tuple[float, float] | None = None) -> tuple[np.ndarray, tuple[float, float] | None]:
    """``x`` clipped to its ``p`` quantiles (or to ``bounds`` given), and the
    bounds used; too few values to set bounds from leave it as it is."""
    if bounds is None:
        f = np.isfinite(x)
        if f.sum() < 10:
            return x, None
        lo, hi = np.quantile(x[f], p)
        bounds = (float(lo), float(hi))
    return np.clip(x, bounds[0], bounds[1]), bounds


def _prep(name: str, x: np.ndarray, cfg: SurpriseConfig, bounds=None) -> tuple[np.ndarray, tuple[float, float] | None]:
    # fluorescence is log-normal: a bloom would otherwise own the scale
    if "fluor" in name.lower():
        x = np.log10(np.clip(x, cfg.log_floor, None))
    return _winsor(x, cfg.winsor, bounds)


def standardize(minute: pd.DataFrame, cfg: SurpriseConfig, stats: dict | None = None):
    """The features robustly scaled on the full minute grid: (feats, grid, Z,
    stats). ``stats`` (the features, their winsor bounds, median and IQR)
    describe a whole record; given, they are applied as they are, so a part
    of the record is scaled exactly as the whole was. None when there is not
    enough data."""
    if stats is None:
        if minute.shape[1] < 2 or minute.empty:
            log.info("surprise: fewer than 2 features; skipping")
            return None
        cover = minute.notna().mean()
        feats = list(cover[cover >= cfg.min_feature_cover].index)      # features with (almost) no data at all
        if len(feats) < 2:
            log.info("surprise: not enough well-covered features (%s); skipping",
                     ", ".join(f"{k}={v:.2f}" for k, v in cover.items()))
            return None
    else:
        feats = list(stats["feats"])
        if minute.empty or any(f not in minute.columns for f in feats):
            return None
    # the full minute grid, so time keeps passing through the gaps and the
    # memory of a previous leg has faded by the time the next one starts
    grid = minute[feats].asfreq("1min")
    cols, bounds = [], []
    for i, f in enumerate(feats):
        x, b = _prep(f, grid[f].to_numpy(float), cfg, stats["bounds"][i] if stats else None)
        cols.append(x); bounds.append(b)
    X = np.column_stack(cols)
    if stats is None:
        med = np.nanmedian(X, axis=0)
        q75, q25 = np.nanpercentile(X, [75, 25], axis=0)
        iqr = q75 - q25
        iqr[~np.isfinite(iqr) | (iqr == 0)] = 1.0
        stats = {"feats": feats, "bounds": bounds, "med": med.tolist(), "iqr": iqr.tolist()}
    Z = (X - np.asarray(stats["med"], float)) / np.asarray(stats["iqr"], float)
    return feats, grid, Z, stats


def surprise_scores(minute: pd.DataFrame, cfg: SurpriseConfig) -> pd.DataFrame | None:
    """``minute`` has a UTC DatetimeIndex at 1-minute resolution (gaps allowed)
    and one column per candidate feature. Returns a frame on the full minute
    grid with one column per scale plus the combined ``SURPRISE_NAME``, or
    None when there is not enough data."""
    return score_minutes(minute, cfg)[0]


def score_minutes(minute: pd.DataFrame, cfg: SurpriseConfig, stats: dict | None = None) -> tuple[pd.DataFrame | None, dict | None]:
    """The scores (as ``surprise_scores``) and the scaling statistics they
    were made with; with ``stats`` given, the scaling of an earlier, fuller
    record is applied instead of one from this data."""
    std = standardize(minute, cfg, stats)
    if std is None:
        return None, None
    feats, grid, Z, stats = std
    n, p = Z.shape
    zf = pd.DataFrame(Z, index=grid.index, columns=feats)
    eye = np.eye(p) * cfg.ridge

    out = pd.DataFrame(index=grid.index)
    total = np.zeros(n)
    count = np.zeros(n)
    for label, half in SURPRISE_SCALES:
        ew = zf.ewm(halflife=half, ignore_na=False, min_periods=max(10, half // 2))
        # the reference for a minute is the history up to the minute before it
        mu = ew.mean().shift(1).to_numpy()
        cov = ew.cov().to_numpy().reshape(n, p, p)
        cov = np.concatenate([np.full((1, p, p), np.nan), cov[:-1]])
        d = Z - mu
        S = cov + eye
        # a minute is scored on whichever features it has (and a reference
        # for), so a sensor that is missing for a leg does not silence the rest
        have = np.isfinite(d) & np.isfinite(np.diagonal(S, axis1=1, axis2=2))
        d2 = np.full(n, np.nan)
        dof = np.zeros(n)
        for pattern in np.unique(have, axis=0):
            k = int(pattern.sum())
            if k < 2:
                continue
            rows = np.flatnonzero((have == pattern).all(axis=1))
            Sk = S[np.ix_(rows, np.flatnonzero(pattern), np.flatnonzero(pattern))]
            dk = d[np.ix_(rows, np.flatnonzero(pattern))]
            good = np.isfinite(Sk).all(axis=(1, 2))
            if not good.any():
                continue
            sol = np.linalg.solve(Sk[good], dk[good][:, :, None])[:, :, 0]
            d2[rows[good]] = (dk[good] * sol).sum(axis=1)
            dof[rows[good]] = k
        with np.errstate(invalid="ignore"):
            s = np.minimum(-np.log10(np.maximum(chi2.sf(d2, np.maximum(dof, 1)), 1e-300)), cfg.cap)
        s[~np.isfinite(d2)] = np.nan
        out[surprise_scale_name(label)] = s
        fin = np.isfinite(s)
        total[fin] += s[fin]
        count += fin
    with np.errstate(invalid="ignore"):
        comb = total / count
    comb[count == 0] = np.nan
    out[SURPRISE_NAME] = comb
    log.info("surprise: %d features (%s), scales %s, %d scored minutes",
             p, ", ".join(feats), ", ".join(l for l, _ in SURPRISE_SCALES), int((count > 0).sum()))
    return out, stats
