"""Water-property coverage and reproducible reference embeddings for live sampling.

Sampling distances use robustly scaled temperature and practical salinity,
which are comparable across the rosette and flow-through instruments. Archived
optical and oxygen properties additionally inform the reference embeddings.
"""
from __future__ import annotations

import csv
import io
import json
from pathlib import Path

import numpy as np
from scipy.spatial import cKDTree

ALGORITHMS = ('coverage', 'rarity', 'gradient', 'surprise', 'hybrid')
FEATURES = ('Temperature', 'Salinity', 'Fluorescence', 'Oxygen', 'Transmission')
MAX_REFERENCE = 3000


def number(value):
    try:
        value = float(value)
        return value if np.isfinite(value) else None
    except (ValueError, TypeError, OverflowError):
        return None


def depth_from_pressure(p, lat=70):
    """UNESCO (1983) depth in metres, matching the main cast viewer."""
    p = np.asarray(p, dtype=float)
    x = np.sin(np.deg2rad(lat if lat is not None else 70)) ** 2
    g = 9.780318 * (1 + (5.2788e-3 + 2.36e-5 * x) * x) + 1.092e-6 * p
    return (((-1.82e-15 * p + 2.279e-10) * p - 2.2512e-5) * p + 9.72659) * p / g


def pressure_from_depth(depth, lat=70):
    p = np.asarray(depth, dtype=float).copy()
    for _ in range(5):
        p += (np.asarray(depth) - depth_from_pressure(p, lat)) * 1.02
    return p


def parse_samples(tsv):
    if not isinstance(tsv, str) or len(tsv.encode()) > 2_000_000:
        raise ValueError('TSV must be text smaller than 2 MB')
    if not tsv.strip():
        return []
    reader = csv.DictReader(io.StringIO(tsv.lstrip('\ufeff')), delimiter='\t')
    reader.fieldnames = [s.strip().lower() for s in (reader.fieldnames or [])]
    names = reader.fieldnames
    if len(names) != len(set(names)) or not ({'cast', 'source'} & set(names)):
        raise ValueError('TSV needs cast and bottle (or pressure); flow rows need source, time, temperature, salinity')
    rows = []
    for line, row in enumerate(reader, 2):
        if None in row:
            raise ValueError(f'Line {line}: too many tab-separated columns')
        row = {k: (v or '').strip() for k, v in row.items()}
        if not any(row.values()):
            continue
        if 'sampled' in names:
            marker = row['sampled']
            if marker not in ('', '0', '1'):
                raise ValueError(f'Line {line}: sampled must be 1, 0, or blank')
            if marker != '1':
                continue
        source = row.get('source', '').lower() or 'ctd'
        if source not in ('ctd', 'flow'):
            raise ValueError(f'Line {line}: source must be ctd or flow')
        row.update(source=source, line=line)
        if source == 'ctd':
            if not row.get('cast') or not (row.get('bottle') or row.get('pressure')):
                raise ValueError(f'Line {line}: give cast and bottle or pressure in dbar')
            if row.get('pressure') and (number(row['pressure']) is None or float(row['pressure']) < 0):
                raise ValueError(f'Line {line}: invalid pressure')
            if row.get('bottle') and (not row['bottle'].isdigit() or not 1 <= int(row['bottle']) <= 100):
                raise ValueError(f'Line {line}: invalid bottle number')
        else:
            import pandas as pd
            try:
                stamp = pd.Timestamp(row.get('time', ''))
                if pd.isna(stamp) or stamp.tzinfo is None:
                    raise ValueError()
            except (ValueError, TypeError):
                raise ValueError(f'Line {line}: flow time must be ISO 8601 with timezone') from None
            if not valid_ts(number(row.get('temperature')), number(row.get('salinity'))):
                raise ValueError(f'Line {line}: flow needs finite temperature (°C) and salinity (PSU)')
        rows.append(row)
        if len(rows) > 10000:
            raise ValueError('At most 10,000 sampled bottles or flow samples')
    return rows


def valid_ts(t, s):
    return t is not None and s is not None and -3 <= t <= 40 and 0 <= s <= 45


def safe_read(root, relative):
    path = (root / relative).resolve()
    if not path.is_relative_to(root.resolve()):
        raise ValueError('Data path must stay inside the dashboard root')
    return json.loads(path.read_text())


def read_reference(root, legs):
    root = Path(root)
    index = safe_read(root, 'data/casts/index.json')
    return [safe_read(root, c['file']) for c in index['casts']
            if c['leg'] in legs and c.get('kind') in ('CTD', 'TM')]


def bottle_template(root, legs):
    """List every archived rosette firing in selected legs for sample marking."""
    output = io.StringIO()
    writer = csv.writer(output, delimiter='\t', lineterminator='\n')
    writer.writerow(('leg', 'cast', 'bottle', 'sampled'))
    for cast in read_reference(root, legs):
        for bottle in cast.get('bottles', []):
            writer.writerow((cast['leg'], cast['id'], bottle['bottle'], ''))
    return output.getvalue()


def property_arrays(cast):
    """Canonical calibrated archive values; incompatible units remain missing."""
    n = len(cast.get('p', []))
    out = {}
    units = cast.get('units', {})
    for name in FEATURES:
        values = cast.get('vars', {}).get(name, [None] * n)
        a = np.array([number(x) for x in values], dtype=float)
        if len(a) != n:
            a = np.full(n, np.nan)
        unit = units.get(name, '')
        if name == 'Oxygen':
            if unit == 'mL/L':
                a *= 44.6596
            elif unit not in ('µM', 'μM', 'umol/L', 'µmol/L'):
                a[:] = np.nan
        if name == 'Fluorescence' and unit not in ('µg/L', 'μg/L', 'ug/L'):
            a[:] = np.nan
        if name == 'Transmission' and unit != '%':
            a[:] = np.nan
        bounds = {'Temperature': (-3, 40), 'Salinity': (0, 45), 'Fluorescence': (0, 200),
                  'Oxygen': (0, 1000), 'Transmission': (0, 110)}[name]
        a[(a < bounds[0]) | (a > bounds[1])] = np.nan
        out[name] = a
    return out


CANONICAL_UNITS = {'Temperature': '°C', 'Salinity': 'PSU', 'Fluorescence': 'µg/L',
                   'Oxygen': 'µM', 'Transmission': '%'}


def variable_arrays(cast):
    """All archive channels, preserving units and missing sensor values."""
    canonical = property_arrays(cast)
    arrays, descriptors = {}, []
    n = len(cast.get('p', []))
    for name, values in cast.get('vars', {}).items():
        unit = CANONICAL_UNITS[name] if name in canonical else cast.get('units', {}).get(name, '')
        key = f'{name} [{unit}]' if unit else name
        a = canonical[name] if name in canonical else np.array([number(v) for v in values], float)
        if len(a) != n:
            a = np.full(n, np.nan)
        arrays[key] = a
        descriptors.append({'key': key, 'name': name, 'unit': unit})
    return arrays, descriptors


def variable_catalog(casts):
    out = {}
    for cast in casts:
        for item in variable_arrays(cast)[1]:
            out[item['key']] = item
    return sorted(out.values(), key=lambda x: x['name'].casefold())


def interpolate_level(cast, pressure):
    ps = np.asarray(cast.get('p', []), dtype=float)
    def values_at(arrays):
        result = {}
        for name, a in arrays.items():
            good = np.isfinite(ps) & np.isfinite(a)
            xp, ix = np.unique(ps[good], return_index=True)
            vals = a[good][ix]
            if len(xp) and xp[0] <= pressure <= xp[-1]:
                near = np.searchsorted(xp, pressure)
                # Interpolation cannot bridge more than five dbar of missing data.
                if near == 0 or near == len(xp) or xp[near] == pressure or xp[near] - xp[near - 1] <= 5:
                    result[name] = float(np.interp(pressure, xp, vals))
        return result
    return {'cast': cast['id'], 'leg': cast['leg'], 'pressure': pressure, 'source': 'ctd',
            **values_at(property_arrays(cast)), 'properties': values_at(variable_arrays(cast)[0])}


def collected_points(casts):
    """Recorded bottle firings, grouped at each distinct sampled pressure."""
    out = {}
    for cast in casts:
        for bottle in cast.get('bottles', []):
            p = number(bottle.get('p'))
            if p is None and number(bottle.get('depth_m')) is not None:
                p = float(pressure_from_depth(float(bottle['depth_m']), cast.get('lat')))
            if p is None:
                continue
            key = (cast['id'], round(p, 2))
            if key not in out:
                point = interpolate_level(cast, p)
                if not valid_ts(point.get('Temperature'), point.get('Salinity')):
                    continue
                out[key] = {**point, 'collected': True, 'sampled': False, 'bottles': []}
            out[key]['bottles'].append(bottle['bottle'])
    return list(out.values())


def resolve_samples(rows, casts):
    samples, unmatched, seen = [], [], set()
    for row in rows:
        if row['source'] == 'flow':
            key = ('flow', row['time'])
            if key not in seen:
                samples.append({'cast': 'flow', 'leg': row.get('leg', ''), 'pressure': 0.,
                                'Temperature': float(row['temperature']), 'Salinity': float(row['salinity']),
                                'time': row['time'], 'source': 'flow',
                                'properties': {'Temperature [°C]': float(row['temperature']), 'Salinity [PSU]': float(row['salinity'])}})
                seen.add(key)
            continue
        identifier = row['cast']
        def matches(c):
            return (not row.get('leg') or c['leg'] == row['leg']) and (
                c['id'] == identifier or str(c.get('cast', '')).lstrip('0') == identifier.lstrip('0'))
        options = [c for c in casts if matches(c)]
        prefix = f"Line {row['line']} ({identifier})"
        if len(options) != 1:
            unmatched.append(prefix + (': ambiguous cast; include leg or full cast ID' if options else ': cast not in selected legs'))
            continue
        c = options[0]
        p = number(row.get('pressure'))
        if p is None:
            bottles = [b for b in c.get('bottles', []) if str(b['bottle']) == str(int(row['bottle']))]
            if len(bottles) == 1:
                p = number(bottles[0].get('p'))
                if p is None and number(bottles[0].get('depth_m')) is not None:
                    p = float(pressure_from_depth(float(bottles[0]['depth_m']), c.get('lat')))
        if p is None:
            unmatched.append(prefix + ': bottle has no usable firing pressure/depth')
            continue
        sample = interpolate_level(c, p)
        if not valid_ts(sample.get('Temperature'), sample.get('Salinity')):
            unmatched.append(prefix + ': pressure outside valid temperature/salinity profile')
            continue
        key = (c['id'], round(p, 2))
        if key not in seen:
            samples.append(sample)
            seen.add(key)
    return samples, unmatched


def reference_points(casts):
    """Deterministic equal-per-cast thinning bounds model and browser work."""
    groups = []
    budget = max(2, MAX_REFERENCE // max(1, len(casts)))
    for c in casts:
        props = property_arrays(c)
        channels = variable_arrays(c)[0]
        p = np.asarray(c.get('p', []), dtype=float)
        good = np.flatnonzero(np.isfinite(p) & (p >= 0) & np.isfinite(props['Temperature']) & np.isfinite(props['Salinity']))
        if len(good) > budget:
            good = good[np.linspace(0, len(good)-1, budget, dtype=int)]
        groups.extend({'cast': c['id'], 'leg': c['leg'], 'pressure': float(p[i]), 'reference': True,
                       'properties': {name: float(a[i]) if np.isfinite(a[i]) else None for name, a in channels.items()},
                       **{name: float(a[i]) if np.isfinite(a[i]) else None for name, a in props.items()}}
                      for i in good)
    return groups[:MAX_REFERENCE]


def robust_scale(points, features):
    x = np.array([[p.get(f) for f in features] for p in points], dtype=float)
    if 'Fluorescence' in features:
        i = features.index('Fluorescence')
        x[:, i] = np.log10(np.maximum(x[:, i], .01))
    med = np.nanmedian(x, axis=0)
    q25, q75 = np.nanpercentile(x, [25, 75], axis=0)
    scale = q75 - q25
    scale[scale < 1e-6] = 1
    return np.nan_to_num((x-med)/scale), med, scale


def rank_candidates(reference, sampled, candidates, pressure, algorithm, count, min_spacing, surprise=None):
    """Fill the target budget, preferring spacing before distinct-level fill."""
    if algorithm not in ALGORITHMS:
        raise ValueError('Unknown sampling algorithm')
    if not len(candidates) or not len(reference):
        return []
    candidates = np.asarray(candidates, float)
    pressure = np.asarray(pressure, float)
    if not np.isfinite(candidates).all() or not np.isfinite(pressure).all():
        raise ValueError('Candidates must contain finite properties and pressure')
    tree = cKDTree(reference)
    k = min(10, len(reference))
    distance = tree.query(candidates, k=k)[0]
    rarity = distance[:, -1] if k > 1 else distance
    anchor = np.asarray(sampled) if len(sampled) else np.median(reference, axis=0)[None, :]
    coverage = cKDTree(anchor).query(candidates)[0]
    gradient = np.zeros(len(candidates))
    if len(candidates) > 1:
        order = np.argsort(pressure)
        delta = np.linalg.norm(np.diff(candidates[order], axis=0), axis=1) / np.maximum(np.diff(pressure[order]), 1)
        gradient[order[1:]] = delta
        gradient[order[:-1]] = np.maximum(gradient[order[:-1]], delta)
    ss = np.asarray(surprise if surprise is not None else np.full(len(candidates), np.nan))
    def norm(a):
        return np.clip(a / max(float(np.nanpercentile(a, 90)), 1e-6), 0, 2)
    available = np.ones(len(candidates), bool)
    spaced = available.copy()
    out = []
    for _ in range(min(count, len(candidates))):
        values = {'coverage': coverage, 'rarity': rarity, 'gradient': gradient, 'surprise': ss}
        score = .6 * norm(coverage) + .25 * norm(rarity) + .15 * norm(gradient) if algorithm == 'hybrid' else values[algorithm]
        fallback = ~np.isfinite(score)
        score = np.where(fallback, coverage, score)
        eligible = available & spaced & np.isfinite(score)
        relaxed = not eligible.any()
        if relaxed:
            eligible = available & np.isfinite(score)
        if not eligible.any():
            break
        novel = eligible & (coverage > 1e-8)
        if novel.any():
            eligible = novel
        scored = eligible & ~fallback
        if scored.any():
            eligible = scored
        i = int(np.argmax(np.where(eligible, score, -np.inf)))
        reasons = {'coverage': 'Distance from sampled water; greedy diversity selection',
                   'rarity': 'Sparse water type among selected reference casts',
                   'gradient': 'Strong vertical change in water properties',
                   'surprise': 'Existing multiscale temporal surprise (−log10 p)',
                   'hybrid': '60% coverage + 25% rarity + 15% vertical gradient'}
        reason = reasons[algorithm]
        if fallback[i]:
            reason += '; coverage fallback because the requested score is unavailable'
        if relaxed:
            reason += '; spacing relaxed to fill the target budget'
        out.append({'index': i, 'rank': len(out) + 1, 'score': round(float(score[i]), 4),
                    'reason': reason, 'spacing_relaxed': relaxed, 'fallback': bool(fallback[i])})
        spaced &= abs(pressure - pressure[i]) >= min_spacing
        available &= pressure != pressure[i]
        coverage = np.minimum(coverage, np.linalg.norm(candidates-candidates[i], axis=1))
    return out


def novelty_distribution(reference, bottles, candidates):
    """Standardize nearest-bottle novelty against leave-one-out bottle novelty.

    A bottle's own point is excluded when calculating its baseline distance.
    With fewer than two located bottles, reference levels supply the baseline.
    """
    reference = np.asarray(reference, float).reshape(-1, 2)
    bottles = np.asarray(bottles, float).reshape(-1, 2)
    candidates = np.asarray(candidates, float).reshape(-1, 2)
    warning = ''
    baseline = bottles
    if len(baseline) < 2:
        baseline = reference
        warning = 'Fewer than two valid archived bottles; novelty z-scores use reference levels as the baseline.'
    if not len(baseline):
        return {'values': [], 'baseline_values': [], 'warning': warning, 'label': 'Novelty z-score'}
    tree = cKDTree(baseline)
    distances = tree.query(baseline, k=2)[0][:, 1] if len(baseline) > 1 else np.zeros(1)
    mean, scale = float(np.mean(distances)), float(np.std(distances))
    if scale <= 1e-12:
        scale = 1.
        warning = (warning + ' ' if warning else '') + 'Baseline novelty has no spread; a unit scale is used.'
    values = (tree.query(candidates)[0] - mean) / scale if len(candidates) else np.array([])
    return {'values': np.round(values, 6).tolist(), 'baseline_values': np.round((distances - mean) / scale, 6).tolist(),
            'mean': mean, 'std': scale, 'bottle_count': len(bottles), 'warning': warning,
            'label': 'Nearest-bottle T/S distance, standardized against leave-one-out archived bottle distances'}


def embed(points, samples, collected=None):
    """Fit both genuine algorithms on one bounded, seeded reference matrix."""
    from threadpoolctl import threadpool_limits
    features = [f for f in FEATURES if sum(number(p.get(f)) is not None for p in points) >= .8 * len(points)]
    result = {name: {'points': [], 'error': ''} for name in ('tsne', 'umap')}
    if len(points) < 5 or len(features) < 2:
        for entry in result.values():
            entry['error'] = 'At least five valid reference levels and two properties are required'
        return result, features
    # Sampled levels join the visualization explicitly, including levels lost
    # from the bounded reference thinning. Scaling is fitted on references only.
    plotted = []
    known = {}
    for group in (points, samples, collected or []):
        for point in group:
            key = (point['cast'], round(point['pressure'], 2), point.get('time') if point.get('source') == 'flow' else None)
            sampled = group is samples
            if key in known:
                target = plotted[known[key]]
                target['sampled'] = target.get('sampled', False) or sampled
                target['collected'] = target.get('collected', False) or point.get('collected', False)
                target['bottles'] = sorted(set(target.get('bottles', []) + point.get('bottles', [])))
            else:
                known[key] = len(plotted)
                plotted.append({**point, 'sampled': sampled})
    _, med, scale = robust_scale(points, features)
    x = np.array([[p.get(f) for f in features] for p in plotted], dtype=float)
    if 'Fluorescence' in features:
        x[:, features.index('Fluorescence')] = np.log10(np.maximum(x[:, features.index('Fluorescence')], .01))
    z = np.nan_to_num((x-med)/scale)
    for name in result:
        try:
            if name == 'tsne':
                from sklearn.manifold import TSNE
                model = TSNE(n_components=2, perplexity=min(30., (len(z)-1)/3), random_state=42,
                             init='pca', learning_rate='auto', max_iter=750, n_jobs=1)
            else:
                from umap import UMAP
                model = UMAP(n_components=2, n_neighbors=min(15, len(z)-1), random_state=42,
                             n_jobs=1, init='random', n_epochs=200)
            with threadpool_limits(limits=1):
                xy = model.fit_transform(z)
            result[name]['points'] = [{**{k: p.get(k) for k in ('cast', 'leg', 'pressure', 'sampled', 'source', 'Temperature', 'Salinity', 'properties', 'collected', 'bottles', 'reference')},
                                       'x': round(float(v[0]), 5), 'y': round(float(v[1]), 5)} for p, v in zip(plotted, xy)]
        except ImportError:
            result[name]['error'] = f'Install sampling dependencies to enable {name}'
        except Exception as exc:
            result[name]['error'] = f'{name} could not fit: {type(exc).__name__}'
    return result, features
