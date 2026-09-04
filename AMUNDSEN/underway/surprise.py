"""Surprise score: how unusual each minute looks against the recent past.

A model is fitted on the most recent ``learn_hours`` of minute-median
features. Three statistics are computed for every minute — Hotelling's T² in
the retained PCA subspace, the squared reconstruction residual Q, and a
shrinkage Mahalanobis distance — each converted to an upper-tail empirical
p-value against the learning set, and the score is the sum of their −log10 p.
Larger means more surprising.
"""

from __future__ import annotations

import logging

import numpy as np
import pandas as pd

from .config import SurpriseConfig

log = logging.getLogger(__name__)


def _winsor(x: np.ndarray, p: tuple[float, float]) -> np.ndarray:
    f = np.isfinite(x)
    if f.sum() < 10:
        return x
    lo, hi = np.quantile(x[f], p)
    return np.clip(x, lo, hi)


def _shrink_cov(X: np.ndarray) -> np.ndarray:
    """Schäfer–Strimmer shrinkage of the sample covariance toward its diagonal.

    Keeps the estimate well-conditioned when the learning window is short
    relative to the number of features, or when features are nearly collinear.
    """
    n, p = X.shape
    Xc = X - X.mean(axis=0)
    S = Xc.T @ Xc / (n - 1)
    if n < 4 or p < 2:
        return S + np.eye(p) * 1e-9
    # variance of each covariance entry, for the optimal shrinkage intensity
    W = np.einsum("ni,nj->nij", Xc, Xc)
    var_s = W.var(axis=0, ddof=1) * n / (n - 1) ** 2
    off = ~np.eye(p, dtype=bool)
    denom = (S[off] ** 2).sum()
    lam = 1.0 if denom == 0 else float(np.clip(var_s[off].sum() / denom, 0.0, 1.0))
    T = np.diag(np.diag(S))
    return lam * T + (1 - lam) * S


def _ecdf_upper_p(train: np.ndarray, x: np.ndarray) -> np.ndarray:
    """P(train >= x) with a +1/n floor so the most extreme value is not p=0."""
    tr = np.sort(train[np.isfinite(train)])
    n = tr.size
    if n == 0:
        return np.full_like(x, np.nan, dtype=float)
    ranks = np.searchsorted(tr, x, side="left")      # number of train values < x
    return 1.0 - ranks / n + 1.0 / n


def surprise_score(minute: pd.DataFrame, cfg: SurpriseConfig) -> pd.Series | None:
    """``minute`` has a UTC DatetimeIndex at 1-minute resolution and one column
    per candidate feature. Returns a Series aligned to that index, or None when
    there is not enough data to fit a model."""
    if minute.shape[1] < 2 or minute.empty:
        log.info("surprise: fewer than 2 features; skipping")
        return None

    t_end = minute.index.max()
    learn_mask = minute.index >= t_end - pd.Timedelta(hours=cfg.learn_hours)
    learn = minute.loc[learn_mask]

    cover = learn.notna().mean()
    feats = list(cover[cover >= cfg.min_feature_cover].index)
    if len(feats) < 2:
        log.info("surprise: not enough well-covered features (%s); skipping",
                 ", ".join(f"{k}={v:.2f}" for k, v in cover.items()))
        return None

    X_all = np.column_stack([_winsor(minute[f].to_numpy(float), cfg.winsor) for f in feats])
    X_learn = X_all[learn_mask]

    med = np.nanmedian(X_learn, axis=0)
    q75, q25 = np.nanpercentile(X_learn, [75, 25], axis=0)
    iqr = q75 - q25
    iqr[~np.isfinite(iqr) | (iqr == 0)] = 1.0
    Z_all = (X_all - med) / iqr
    Z_learn = Z_all[learn_mask]

    good_learn = np.isfinite(Z_learn).all(axis=1)
    Zl = Z_learn[good_learn]
    if Zl.shape[0] < 10:
        log.info("surprise: only %d complete learning rows; skipping", Zl.shape[0])
        return None

    # PCA on the (already robust-scaled) learning rows
    _, s, Vt = np.linalg.svd(Zl, full_matrices=False)
    eig = s ** 2 / max(1, Zl.shape[0] - 1)
    vr = eig / eig.sum()
    k = int(np.searchsorted(np.cumsum(vr), cfg.pca_variance) + 1)
    k = max(1, min(k, len(vr) - 1)) if len(vr) > 1 else 1
    P = Vt[:k].T                      # p × k loadings
    lam = eig[:k]

    def stats(Z: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
        scores = Z @ P
        t2 = (scores ** 2 / lam).sum(axis=1)
        resid = Z - scores @ P.T
        q = (resid ** 2).sum(axis=1)
        return t2, q

    S = _shrink_cov(Zl)
    Sinv = np.linalg.pinv(S)
    mu = Zl.mean(axis=0)

    def maha(Z: np.ndarray) -> np.ndarray:
        d = Z - mu
        return np.einsum("ij,jk,ik->i", d, Sinv, d)

    t2_l, q_l = stats(Zl)
    md_l = maha(Zl)

    good_all = np.isfinite(Z_all).all(axis=1)
    out = np.full(Z_all.shape[0], np.nan)
    if good_all.any():
        Za = Z_all[good_all]
        t2_a, q_a = stats(Za)
        md_a = maha(Za)
        p = np.column_stack([
            _ecdf_upper_p(t2_l, t2_a),
            _ecdf_upper_p(q_l, q_a),
            _ecdf_upper_p(md_l, md_a),
        ])
        score = np.nansum(-np.log10(p), axis=1)
        score[~np.isfinite(score)] = np.nan
        out[good_all] = score

    log.info("surprise: %d features (%s), k=%d, learn rows=%d",
             len(feats), ", ".join(feats), k, Zl.shape[0])
    return pd.Series(out, index=minute.index, name="surprise")
