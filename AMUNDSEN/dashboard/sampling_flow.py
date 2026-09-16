"""Hypothetical historical flow-through picks; replay never sends notifications."""

from datetime import datetime, timezone
import time

import numpy as np
from scipy.spatial import cKDTree

from .config import LOW_FLOW_V, SURPRISE_NAME
from .sampling import ALGORITHMS, number, novelty_distribution


def score_window(data, manifest, model, cfg):
    """Rank healthy historical observations and fill a requested target budget."""
    algorithm = cfg['algorithm']
    if algorithm not in ALGORITHMS:
        raise ValueError('Select a supported algorithm')
    count, spacing = cfg.get('count', 6), cfg.get('spacing', 15)
    result = {'targets': [], 'distribution': {'values': [], 'baseline_values': []},
              'requested_count': count, 'available_count': 0, 'warnings': []}
    variables = data.get('vars', {})
    raw = [data.get('t', []), variables.get('SST (°C)', []),
           variables.get('Salinity (PSU)', []), variables.get('TSG flow (V)', []),
           data.get('leg', [])]
    n = min(map(len, raw))
    if not n:
        return result
    stamps, temperature, salinity, flow, legs = [
        np.array([number(value) for value in values[:n]], dtype=float) for values in raw]
    stamps /= 1000
    selected = [i for i, leg in enumerate(manifest.get('legs', [])) if leg['id'] in cfg['legs']]
    pump = np.zeros(n, dtype=bool)
    for i, value in enumerate(data.get('pump_low', [])[:n]):
        pump[i] = bool(value)
    good = (np.isfinite(stamps) & (stamps >= 0) & (stamps <= time.time() + 30)
            & np.isfinite(temperature) & (temperature >= -3) & (temperature <= 40)
            & np.isfinite(salinity) & (salinity >= 0) & (salinity <= 45)
            & np.isfinite(flow) & (flow >= LOW_FLOW_V) & ~pump & np.isin(legs, selected))
    indices = np.flatnonzero(good)
    indices = indices[np.argsort(stamps[indices], kind='stable')]
    if not len(indices):
        return result
    reference = np.asarray(model['ref'], dtype=float)
    sampled = np.asarray(model['sampled'], dtype=float).reshape(-1, 2)
    med, scale = np.asarray(model['med'], dtype=float), np.asarray(model['scale'], dtype=float)
    if not len(reference):
        return result
    if (reference.ndim != 2 or reference.shape[1] != 2 or not np.isfinite(reference).all()
            or not np.isfinite(sampled).all() or not np.isfinite(med).all()
            or not np.isfinite(scale).all() or (scale <= 0).any()):
        raise ValueError('Flow scoring needs finite temperature/salinity reference properties')
    z = (np.column_stack([temperature[indices], salinity[indices]]) - med) / scale
    anchor = sampled if len(sampled) else np.median(reference, axis=0)[None, :]
    tree = cKDTree(anchor)
    coverage = tree.query(z)[0]
    coverage_limit = max(.25, float(np.percentile(tree.query(reference)[0], 90)))
    k = min(10, len(reference))
    reference_tree = cKDTree(reference)
    distances = reference_tree.query(z, k=k)[0]
    rarity = distances[:, -1] if k > 1 else distances
    reference_distances = reference_tree.query(reference, k=k)[0]
    rarity_limit = max(.1, float(np.percentile(reference_distances[:, -1] if k > 1 else reference_distances, 90)))
    surprise_values = variables.get(SURPRISE_NAME) or []
    surprise = np.array([number(surprise_values[i]) if i < len(surprise_values) else None for i in indices], dtype=float)
    gradient = np.zeros(len(indices))
    times = stamps[indices]
    start = 0
    for j in range(1, len(indices)):
        i, previous = indices[j], indices[j - 1]
        continuous_flow = np.isfinite(flow[previous:i + 1]) & (flow[previous:i + 1] >= LOW_FLOW_V) & ~pump[previous:i + 1]
        if not continuous_flow.all() or legs[i] != legs[previous] or times[j] - times[j - 1] > 180:
            start = j
        before = np.searchsorted(times, times[j] - 60, side='right') - 1
        if before >= start and 60 <= times[j] - times[before] <= 180:
            gradient[j] = np.linalg.norm(z[j] - z[before])
    scores = {
        'coverage': (coverage, coverage_limit),
        'rarity': (rarity, rarity_limit),
        'gradient': (gradient, .5),
        'surprise': (surprise, 3.),
        'hybrid': (.6 * np.minimum(coverage / coverage_limit, 2)
                   + .25 * np.minimum(rarity / rarity_limit, 2)
                   + .15 * np.minimum(gradient / .5, 2), 1.),
    }
    score, threshold = scores[algorithm]
    fallback = ~np.isfinite(score)
    score = np.where(fallback, coverage, score)
    distribution = novelty_distribution(reference, model.get('bottle_sampled', np.empty((0, 2))), z)
    result.update(distribution=distribution, available_count=len(np.unique(times)))
    if distribution.get('warning'):
        result['warnings'].append(distribution['warning'])
    if result['available_count'] < count:
        result['warnings'].append(f"Only {result['available_count']} distinct healthy observations are available for {count} requested targets.")
    available = np.isfinite(score)
    spaced = available.copy()
    for _ in range(min(count, len(indices))):
        eligible = available & spaced
        relaxed = not eligible.any()
        if relaxed:
            eligible = available
        if not eligible.any():
            break
        novel = eligible & (coverage > 1e-8)
        if novel.any():
            eligible = novel
        scored = eligible & ~fallback
        if scored.any():
            eligible = scored
        j = int(np.argmax(np.where(eligible, score, -np.inf)))
        i = indices[j]
        stamp = datetime.fromtimestamp(float(times[j]), timezone.utc).isoformat()
        reason = f'Hypothetical historical pick: {algorithm} score {score[j]:.2f}. Not sent.'
        if fallback[j]:
            reason += ' Coverage fallback: requested score unavailable.'
        if relaxed:
            reason += ' Spacing relaxed to fill the target budget.'
        result['targets'].append({'time': stamp, 'score': round(float(score[j]), 4),
                       'rank': len(result['targets']) + 1, 'z': distribution['values'][j],
                       'threshold': round(float(threshold), 4), 'algorithm': algorithm,
                       'reason': reason, 'spacing_relaxed': relaxed, 'fallback': bool(fallback[j]),
                       'label': 'Hypothetical historical pick', 'historical': True,
                       'temperature': float(temperature[i]), 'salinity': float(salinity[i]),
                       'leg': manifest['legs'][int(legs[i])]['id']})
        available &= times != times[j]
        spaced &= np.abs(times - times[j]) >= spacing * 60
    if any(p['spacing_relaxed'] for p in result['targets']):
        result['warnings'].append('Time spacing was relaxed to show the requested number of distinct targets.')
    if any(p['fallback'] for p in result['targets']):
        result['warnings'].append('Coverage fills targets where the requested method score is unavailable.')
    return result
