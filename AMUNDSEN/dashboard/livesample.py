"""Persistent experimental sampling adviser, independently served or embedded.

The standalone server reads the main server's live API, so a sampling experiment
shares its CTD listener. Settings, sample inventory and notification deduplication
live outside the published web root. No browser needs to remain open for alerts.
"""
from __future__ import annotations

import argparse
import copy
import gzip
import hashlib
import json
import logging
import os
from pathlib import Path
import threading
import time
import urllib.request
from datetime import datetime, timezone
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import parse_qs, urlsplit

import numpy as np
import pandas as pd
from scipy.spatial import cKDTree

from . import sampling as S
from .config import DB_DIR, WEBROOT, SURPRISE, SURPRISE_NAME, LOW_FLOW_V

log = logging.getLogger(__name__)
DEFAULTS = {'legs': [], 'algorithm': 'coverage', 'count': 6, 'min_spacing': 10., 'telegram': False}
SOAK_DEPTH = 15.
SOAK_RETURN = 5.
DESCENT_CONFIRMATION = 2.
DIRECT_CAST_DEPTH = 30.


def atomic_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix('.tmp')
    tmp.write_text(json.dumps(value, allow_nan=False))
    tmp.replace(path)


def live_candidates(live):
    """Two-dbar medians from the observed downcast, with real observation times."""
    c = live.get('current')
    age = S.number(live.get('last_packet_age_s'))
    if not c or age is None or not 0 <= age <= 30 or c.get('ended'):
        return [], 'No fresh cast in the water'
    if cast_segment(c)['soak_pending']:
        return [], 'Soak cycle excluded; waiting for the surface return and real downcast'
    return candidates_from_cast(c, require_fresh=True)


def cast_segment(c):
    """Locate the real downcast after a deck soak and surface return."""
    cols = c.get('cols', {})
    pressure_col = c.get('pressure_col')
    positions = cols.get(pressure_col, [])
    valid = [(i, S.number(value)) for i, value in enumerate(positions)]
    valid = [(i, value) for i, value in valid if value is not None]
    direct = bool(valid) and max(value for _, value in valid) >= DIRECT_CAST_DEPTH
    deep = next((k for k, (_, value) in enumerate(valid) if value >= SOAK_DEPTH), None)
    if deep is None:
        return {'soak_detected': False, 'soak_pending': not direct, 'start_index': 0, 'start_time': None}
    returned = next((k for k in range(deep + 1, len(valid)) if valid[k][1] <= SOAK_RETURN), None)
    if returned is None:
        return {'soak_detected': False, 'soak_pending': not direct, 'start_index': 0, 'start_time': None}
    minimum = valid[returned][1]
    valley = returned
    confirmed = None
    for k in range(returned + 1, len(valid)):
        value = valid[k][1]
        if value < minimum:
            minimum, valley = value, k
        elif value <= minimum + .5:
            valley = k
        elif value >= minimum + DESCENT_CONFIRMATION:
            confirmed = k
            break
    if confirmed is None:
        return {'soak_detected': True, 'soak_pending': True, 'start_index': len(positions), 'start_time': None}
    start_index = valid[valley][0] + 1
    times = c.get('t', [])
    start_time = S.number(times[start_index]) if start_index < len(times) else None
    return {'soak_detected': True, 'soak_pending': False, 'start_index': start_index, 'start_time': start_time}


def candidates_from_cast(c, require_fresh=False):
    """Extract observed downcast bins, including completed casts for display."""
    cols = c.get('cols', {})
    pressure_col = c.get('pressure_col')
    if not all(k in cols for k in (pressure_col, 'temperature', 'salinity')):
        return [], 'Live ranking needs temperature, salinity and package pressure/depth'
    segment = cast_segment(c)
    start = segment['start_index']
    frame = pd.DataFrame({k: cols[k][start:] for k in (pressure_col, 'temperature', 'salinity')})
    frame['time'] = c['t'][start:]
    frame = frame.apply(pd.to_numeric, errors='coerce').dropna()
    frame = frame[frame.temperature.between(-3, 40) & frame.salinity.between(0, 45) & frame[pressure_col].ge(2)]
    if len(frame) < 3:
        return [], 'Soak cycle excluded; waiting for the real downcast' if segment['soak_detected'] else 'Waiting for valid downcast levels'
    if require_fresh and not -30 <= time.time() - float(frame.time.iloc[-1]) <= 30:
        return [], 'No fresh valid temperature/salinity scan; alerts paused'
    lat_values = [v for v in cols.get('lat', []) if S.number(v) is not None]
    lat = lat_values[-1] if lat_values else 70
    pos = frame[pressure_col]
    frame = frame.iloc[:int(np.argmax(pos.to_numpy())) + 1]
    frame = frame[frame[pressure_col] >= frame[pressure_col].cummax() - 2]
    frame['pressure'] = S.pressure_from_depth(frame[pressure_col].to_numpy(), lat) if c.get('depth_like') else frame[pressure_col]
    frame['bin'] = (frame.pressure // 2).astype(int)
    binned = frame.groupby('bin', sort=True).median(numeric_only=True)
    result = []
    for _, row in binned.iterrows():
        p = float(row['pressure'])
        result.append({'pressure': p, 'depth_m': float(S.depth_from_pressure(p, lat)),
                       'position': float(row[pressure_col]), 'unit': 'm' if c.get('depth_like') else 'dbar',
                       'Temperature': float(row.temperature), 'Salinity': float(row.salinity), 'time': float(row.time)})
    return result, ''


def cast_surprise(points):
    """Use the existing scorer on actual minute medians, without inventing time."""
    from .surprise import surprise_scores
    if not points:
        return np.array([])
    frame = pd.DataFrame(points)
    frame.index = pd.to_datetime(frame.pop('time'), unit='s', utc=True)
    minute = frame[['Temperature', 'Salinity']].resample('1min').median()
    if len(minute) < 11:
        return np.full(len(points), np.nan)
    scores = surprise_scores(minute, SURPRISE)
    if scores is None:
        return np.full(len(points), np.nan)
    return scores[SURPRISE_NAME].reindex(frame.index.floor('min')).to_numpy()


class SampleService:
    def __init__(self, root, state_dir=None, live_provider=None, start=True):
        self.root = Path(root)
        self.state_dir = Path(state_dir or DB_DIR / 'livesample')
        self.lock = threading.RLock()
        self.stop_event = threading.Event()
        self.wake = threading.Event()
        self.live_provider = live_provider or (lambda: {})
        self.config = copy.deepcopy(DEFAULTS)
        self.tsv = ''
        self.revision = 0
        self.model_key = None
        self.model = None
        self.profile_hashes = {}
        self.plan = []
        self.plan_cast = None
        self.notices = {}
        try:
            saved = json.loads((self.state_dir / 'settings.json').read_text())
            self.config.update(saved['config'])
            self.tsv = saved.get('tsv', '')
            S.parse_samples(self.tsv)
        except (OSError, ValueError, KeyError):
            try:
                manifest = S.safe_read(self.root, 'data/manifest.json')
                self.config['legs'] = [manifest['live']] if manifest.get('live') else [x['id'] for x in manifest['legs'][-1:]]
            except (OSError, ValueError, KeyError):
                pass
        try:
            self.notices = json.loads((self.state_dir / 'notices.json').read_text())
        except (OSError, ValueError):
            pass
        self.result = {'status': 'Preparing selected casts', 'error': '', 'warnings': [],
                       'recommendations': [], 'embeddings': {}, 'reference_count': 0, 'sample_count': 0}
        self.thread = None
        if start:
            self.thread = threading.Thread(target=self._loop, name='live-sampling', daemon=True)
            self.thread.start()

    def status(self):
        with self.lock:
            result = {**copy.deepcopy(self.result), 'config': copy.deepcopy(self.config)}
        try:
            result['live'] = self.live_provider()
        except Exception:
            result['live'] = {'tcp_state': 'unavailable'}
        return result

    def template(self, legs):
        manifest = S.safe_read(self.root, 'data/manifest.json')
        available = {leg['id'] for leg in manifest.get('legs', [])}
        if not legs or len(legs) != len(set(legs)) or any(leg not in available for leg in legs):
            raise ValueError('Select valid reference legs for the bottle template')
        return S.bottle_template(self.root, legs)

    def update(self, payload):
        if not isinstance(payload, dict) or set(payload) - {*DEFAULTS, 'tsv'}:
            raise ValueError('Unknown sampling settings')
        with self.lock:
            cfg = {**self.config, **{k: v for k, v in payload.items() if k in DEFAULTS}}
            manifest = S.safe_read(self.root, 'data/manifest.json')
            available = {x['id'] for x in manifest['legs']}
            if not isinstance(cfg['legs'], list) or not all(isinstance(x, str) and x in available for x in cfg['legs']):
                raise ValueError('Select valid legs')
            if not cfg['legs']:
                raise ValueError('Select at least one leg')
            cfg['legs'] = sorted(set(cfg['legs']))
            if cfg['algorithm'] not in S.ALGORITHMS:
                raise ValueError('Select a supported algorithm')
            if type(cfg['count']) is not int or not 1 <= cfg['count'] <= 24:
                raise ValueError('Bottle budget must be an integer from 1 to 24')
            if isinstance(cfg['min_spacing'], bool) or S.number(cfg['min_spacing']) is None or not 2 <= float(cfg['min_spacing']) <= 500:
                raise ValueError('Minimum spacing must be 2–500 dbar')
            cfg['min_spacing'] = float(cfg['min_spacing'])
            if type(cfg['telegram']) is not bool:
                raise ValueError('Telegram setting must be true or false')
            if cfg['telegram']:
                from .alerts import telegram_token, ops_targets
                if not telegram_token() or not ops_targets()[2]:
                    raise ValueError('Underwaybot owner Telegram token/chat is not configured on this server')
            tsv = payload.get('tsv', self.tsv)
            S.parse_samples(tsv)
            atomic_json(self.state_dir / 'settings.json', {'config': cfg, 'tsv': tsv})
            self.config, self.tsv = cfg, tsv
            self.revision += 1
            self.result = {**self.result, 'status': 'Updating sampling model', 'recommendations': [], 'error': ''}
            self.plan = []
            self.wake.set()
        return {'ok': True}

    def _model(self, cfg, tsv):
        index = self.root / 'data/casts/index.json'
        # Builds rewrite unchanged profiles. File stats cache content hashes;
        # only changed bytes require the reference embeddings to be refitted.
        raw = index.read_bytes()
        meta = json.loads(raw)
        selected = [c for c in meta['casts'] if c['leg'] in cfg['legs'] and c.get('kind') in ('CTD', 'TM')]
        stamps = []
        for c in selected:
            path = (self.root / c['file']).resolve()
            if not path.is_relative_to(self.root.resolve()):
                raise ValueError('Cast path outside dashboard root')
            st = path.stat()
            signature = (st.st_size, st.st_mtime_ns)
            cached = self.profile_hashes.get(path)
            if cached is None or cached[:2] != signature:
                cached = (*signature, hashlib.sha256(path.read_bytes()).hexdigest())
                self.profile_hashes[path] = cached
            stamps.append((c['id'], cached[2]))
        key = hashlib.sha256(json.dumps([cfg['legs'], stamps, tsv], sort_keys=True).encode()).hexdigest()
        if key == self.model_key and self.model is not None:
            return self.model
        casts = S.read_reference(self.root, cfg['legs'])
        points = S.reference_points(casts)
        samples, unmatched = S.resolve_samples(S.parse_samples(tsv), casts)
        if len(points) < 5:
            raise ValueError('Selected legs need at least five valid rosette temperature/salinity levels')
        ref, med, scale = S.robust_scale(points, ['Temperature', 'Salinity'])
        sampled = np.array([[p['Temperature'], p['Salinity']] for p in samples], float).reshape(-1, 2)
        sampled = (sampled - med) / scale
        collected = S.collected_points(casts)
        bottle_properties = [[p['Temperature'], p['Salinity']] for p in collected for _ in p.get('bottles', [None])]
        bottle_sampled = (np.asarray(bottle_properties, float).reshape(-1, 2) - med) / scale
        embeddings, features = S.embed(points, samples, collected)
        model = {'points': points, 'samples': samples, 'unmatched': unmatched, 'ref': ref, 'sampled': sampled,
                 'med': med, 'scale': scale, 'bottle_sampled': bottle_sampled,
                 'embeddings': embeddings, 'features': features, 'casts': len(casts),
                 'variables': S.variable_catalog(casts), 'collected_count': len(collected),
                 'unlocated_bottles': sum(len(c.get('bottles', [])) for c in casts) - sum(len(p['bottles']) for p in collected)}
        with self.lock:
            if cfg['legs'] == self.config['legs'] and tsv == self.tsv:
                self.model_key, self.model = key, model
        return model

    def flow_history(self, window, algorithm=None, count=None, spacing=None):
        """Rank a published window with independent controls; never sends alerts."""
        from .sampling_flow import score_window
        with self.lock:
            model, cfg, revision = self.model, copy.deepcopy(self.config), self.revision
        cfg['algorithm'] = cfg['algorithm'] if algorithm is None else algorithm
        if cfg['algorithm'] not in S.ALGORITHMS:
            raise ValueError('Select a supported algorithm')
        count = cfg['count'] if count is None else count
        if isinstance(count, bool) or not isinstance(count, (str, int)) or not str(count).isdigit() or not 1 <= int(count) <= 24:
            raise ValueError('Target count must be an integer from 1 to 24')
        cfg['count'] = int(count)
        spacing = 15 if spacing is None else spacing
        if isinstance(spacing, bool) or S.number(spacing) is None or not 1 <= float(spacing) <= 1440:
            raise ValueError('Time spacing must be 1–1440 minutes')
        cfg['spacing'] = float(spacing)
        with self.lock:
            if model is None or self.result.get('status') == 'Updating sampling model':
                return {'targets': [], 'window': window, 'algorithm': cfg['algorithm'],
                        'revision': revision, 'error': 'Reference model is updating'}
        manifest = S.safe_read(self.root, 'data/manifest.json')
        entry = next((w for w in manifest['windows'] if w['label'] == window), None)
        if entry is None:
            raise ValueError('Select a published timespan')
        data = S.safe_read(self.root, entry['file'])
        replay = score_window(data, manifest, model, cfg)
        with self.lock:
            if revision != self.revision:
                return {'targets': [], 'error': 'Settings changed; refresh the chart', 'revision': self.revision}
        return {**replay, 'window': window, 'algorithm': cfg['algorithm'], 'spacing': cfg['spacing'],
                'revision': revision, 'historical': True}

    def _flow(self, model, cfg, now):
        try:
            data = S.safe_read(self.root, 'data/w-1h.json')
        except (OSError, ValueError):
            return {'status': 'Flow-through record unavailable', 'recommendation': False}
        vs = data.get('vars', {})
        ts, ss = vs.get('SST (°C)', []), vs.get('Salinity (PSU)', [])
        valid = [i for i in range(min(len(ts), len(ss), len(data.get('t', [])))) if S.valid_ts(S.number(ts[i]), S.number(ss[i]))]
        if not valid:
            return {'status': 'No valid flow-through temperature/salinity', 'recommendation': False}
        newest = valid[-1]
        newest_stamp = data['t'][newest] / 1000
        base = {'time': datetime.fromtimestamp(newest_stamp, timezone.utc).isoformat(), 'temperature': ts[newest], 'salinity': ss[newest], 'recommendation': False}
        if not -30 <= now-newest_stamp <= 180:
            return {**base, 'status': 'Flow-through data stale; alerts paused'}
        flows = vs.get('TSG flow (V)', [])
        pump = data.get('pump_low', [])
        if newest < len(pump) and pump[newest]:
            return {**base, 'status': 'Intake flow low or unknown; alerts paused'}
        complete = [j for j in valid if j < len(flows) and S.number(flows[j]) is not None]
        if not complete:
            return {**base, 'status': 'Intake flow low or unknown; alerts paused'}
        i = complete[-1]
        stamp = data['t'][i] / 1000
        base = {'time': datetime.fromtimestamp(stamp, timezone.utc).isoformat(), 'temperature': ts[i], 'salinity': ss[i], 'recommendation': False}
        if now-stamp > 180 or flows[i] < LOW_FLOW_V or (i < len(pump) and pump[i]):
            return {**base, 'status': 'Intake flow low or unknown; alerts paused'}
        # A previous leg's terminal window must not drive current recommendations.
        manifest = S.safe_read(self.root, 'data/manifest.json')
        if manifest.get('live') not in cfg['legs']:
            return {**base, 'status': 'Select the live leg to enable flow-through recommendations'}
        z = (np.array([ts[i], ss[i]]) - model['med'])/model['scale']
        ref, samples = model['ref'], model['sampled']
        anchor = samples if len(samples) else np.median(ref, axis=0)[None, :]
        tree = cKDTree(anchor)
        coverage = float(tree.query(z)[0])
        cov_limit = max(.25, float(np.percentile(tree.query(ref)[0], 90)))
        k = min(10, len(ref))
        rt = cKDTree(ref)
        rarity = float(np.atleast_1d(rt.query(z, k=k)[0])[-1])
        rarity_limit = max(.1, float(np.percentile(rt.query(ref, k=k)[0][:, -1], 90)))
        surprise_values = vs.get(SURPRISE_NAME) or []
        surprise = S.number(surprise_values[i]) if i < len(surprise_values) else None
        previous = next((j for j in reversed(valid[:-1]) if 60000 <= data['t'][i]-data['t'][j] <= 180000
                         and j < len(flows) and S.number(flows[j]) is not None and flows[j] >= LOW_FLOW_V
                         and not (j < len(pump) and pump[j])), None)
        gradient = 0.
        if previous is not None:
            gradient = float(np.linalg.norm((np.array([ts[i]-ts[previous], ss[i]-ss[previous]]))/model['scale']))
        scores = {'coverage': (coverage, cov_limit), 'rarity': (rarity, rarity_limit),
                  'gradient': (gradient, .5), 'surprise': (surprise or 0., 3.),
                  'hybrid': (.6 * min(coverage/cov_limit, 2) + .25 * min(rarity/rarity_limit, 2) + .15 * min(gradient/.5, 2), 1.)}
        score, threshold = scores[cfg['algorithm']]
        recommend = score >= threshold and (not len(samples) or coverage > 1e-8)
        return {**base, 'status': 'Sample flow-through water now' if recommend else 'Current water below sampling threshold',
                'score': round(score, 4), 'threshold': round(threshold, 4), 'surprise': surprise,
                'recommendation': recommend, 'reason': f"{cfg['algorithm']} score {score:.2f}; threshold {threshold:.2f}",
                'features': ['Temperature', 'Salinity'], 'flow_v': flows[i]}

    def compute(self):
        with self.lock:
            cfg, tsv, revision = copy.deepcopy(self.config), self.tsv, self.revision
        model = self._model(cfg, tsv)
        live = self.live_provider()
        points, live_error = live_candidates(live)
        operational = bool(points)
        c = live.get('current') or {}
        completed = not c and bool(live.get('last'))
        if completed:
            c = live['last']
            points, live_error = candidates_from_cast(c)
            live_error = live_error or 'Last completed cast targets'
        elif c and not points:
            # A stalled feed must silence operational advice, but the chart still
            # shows a complete display-only plan for the observed part of the cast.
            points, display_error = candidates_from_cast(c)
            if points:
                live_error = 'Stale cast targets for display only; alerts paused'
            elif display_error:
                live_error = display_error
        segment = cast_segment(c) if c else {'soak_detected': False, 'soak_pending': False, 'start_index': 0, 'start_time': None}
        warnings = ['Experimental priorities use temperature and practical salinity; confirm bottle capacity and ship operations.',
                    'Reference embeddings use calibrated archive properties; missing optional values use reference medians. Distances on these plots are not sampling scores.',
                    'Reference levels are thinned equally per cast, up to 3,000 levels; MVP raw/nominal sensors are excluded.']
        if model.get('unlocated_bottles'):
            warnings.append(f"{model['unlocated_bottles']} archived bottle firings lack valid matching profile data and cannot be placed on the embeddings.")
        if not model['samples']:
            warnings.append('No matched sampled inventory: coverage starts at the reference median until samples are supplied.')
        if model['unmatched']:
            warnings.append('Some TSV rows could not be matched; review unmatched rows before relying on coverage.')
        recs = []
        distribution = {'values': [], 'baseline_values': []}
        if points:
            z = (np.array([[p['Temperature'], p['Salinity']] for p in points]) - model['med'])/model['scale']
            distribution = S.novelty_distribution(model['ref'], model.get('bottle_sampled', np.empty((0, 2))), z)
            if distribution.get('warning'):
                warnings.append(distribution['warning'])
            surprise = cast_surprise(points) if cfg['algorithm'] == 'surprise' else None
            if surprise is not None and not np.isfinite(surprise).any():
                warnings.append('Temporal surprise needs at least 11 observed minutes; coverage fills the target budget while it is unavailable.')
            ranked = S.rank_candidates(model['ref'], model['sampled'], z, np.array([p['pressure'] for p in points]),
                                       cfg['algorithm'], cfg['count'], cfg['min_spacing'], surprise)
            recs = [{**points[r['index']], 'z': distribution['values'][r['index']],
                     **{k: v for k, v in r.items() if k != 'index'}} for r in ranked]
            if any(r.get('spacing_relaxed') for r in recs):
                warnings.append('Depth spacing was relaxed to show the requested number of distinct observed targets.')
        if len(points) < cfg['count']:
            warnings.append(f"Only {len(points)} valid observed bins are available for {cfg['count']} requested targets.")
        flow = self._flow(model, cfg, time.time())
        with self.lock:
            if revision != self.revision:
                return
            if c.get('started') != self.plan_cast:
                self.plan, self.plan_cast = [], c.get('started')
            if c.get('direction') == 'up' and points and self.plan and not completed:
                # The full downcast plan remains visible throughout the upcast.
                recs = copy.deepcopy(self.plan)
            elif points:
                self.plan = recs
            self.result = {'status': live_error or ('Upcast: full downcast target plan' if c.get('direction') == 'up' else 'Downcast targets update as water is observed'),
                           'error': '', 'warnings': warnings, 'recommendations': recs, 'flow': flow,
                           'distributions': {'ctd': distribution}, 'requested_count': cfg['count'], 'available_count': len(points),
                           'features': model['features'], 'scoring_features': ['Temperature', 'Salinity'],
                           'reference_count': len(model['points']), 'sample_count': len(model['samples']),
                           'cast_count': model['casts'], 'variables': model.get('variables', []),
                           'collected_count': model.get('collected_count', 0), 'unmatched': model['unmatched'], 'embeddings': model['embeddings'],
                           'phase': c.get('direction'), 'cast_segment': segment, 'updated': datetime.now(timezone.utc).isoformat()}
        if cfg['telegram']:
            self._notify(cfg, live, recs if operational else [], flow, expected_revision=revision)

    def _notify(self, cfg, live, recs, flow, expected_revision=None):
        from .alerts import Telegram, telegram_token, ops_targets
        with self.lock:
            revision = self.revision if expected_revision is None else expected_revision
            if revision != self.revision:
                return
            notices = self.notices.copy()
            sample_count = self.result.get('sample_count', 0)
        token, chat = telegram_token(), ops_targets()[2]
        if not token or not chat:
            with self.lock:
                if revision == self.revision:
                    self.result['warnings'].append('Telegram owner/token unavailable; no notifications sent')
            return
        now = time.time()
        c = live.get('current') or {}
        messages = []
        if recs and c.get('direction') in ('down', 'hold'):
            depths = sorted(round(r['pressure']/cfg['min_spacing']) for r in recs)
            key = f"ctd:{c.get('started')}:{cfg['algorithm']}:{depths}"
            if key != notices.get('ctd_key') and now-notices.get('ctd_at', 0) >= 120:
                text = '🧪 Underway live sampling · upcast targets\n' + ', '.join(f"{r['depth_m']:.0f} m ({r['pressure']:.0f} dbar)" for r in sorted(recs, key=lambda x: -x['pressure']))
                text += f"\nMethod: {cfg['algorithm']} · T/S coverage · {sample_count} recorded samples\nObserved downcast only; confirm operational suitability.\nhttp://underway.local/livesample.html"
                messages.append(('ctd', key, text))
        if flow.get('recommendation') and now-notices.get('flow_at', 0) >= 900:
            key = flow['time']
            if key != notices.get('flow_key'):
                text = f"🧪 Underway live sampling · flow-through\nSample now: {flow['temperature']:.3f} °C, {flow['salinity']:.3f} PSU\n{flow['reason']}\n{flow['time']}\nhttp://underway.local/livesample.html"
                messages.append(('flow', key, text))
        for kind, key, text in messages:
            with self.lock:
                if revision != self.revision:
                    return
            try:
                Telegram(token).send(chat, text)
                with self.lock:
                    self.notices.update({kind+'_key': key, kind+'_at': now})
                    atomic_json(self.state_dir / 'notices.json', self.notices)
            except Exception:
                log.warning('Live sampling Telegram delivery failed')
                with self.lock:
                    if revision == self.revision:
                        self.result['warnings'].append('Telegram delivery failed; retrying on the next evaluation')

    def _loop(self):
        while not self.stop_event.is_set():
            self.wake.clear()
            try:
                self.compute()
            except Exception as exc:
                log.exception('Live sampling evaluation failed')
                with self.lock:
                    self.result.update(error=str(exc)[:240], status='Sampling unavailable', recommendations=[], flow={'recommendation': False, 'status': 'Sampling unavailable'})
            self.wake.wait(10)

    def close(self):
        self.stop_event.set()
        self.wake.set()
        if self.thread:
            self.thread.join(timeout=3)


class Handler(SimpleHTTPRequestHandler):
    def __init__(self, *args, service, **kwargs):
        self.service = service
        super().__init__(*args, **kwargs)

    def json_response(self, code, payload):
        body = json.dumps(payload, allow_nan=False).encode()
        encodings = {}
        for item in self.headers.get('Accept-Encoding', '').lower().split(','):
            coding, *params = item.strip().split(';')
            quality = next((v.strip()[2:] for v in params if v.strip().startswith('q=')), '1')
            try:
                encodings[coding] = float(quality)
            except ValueError:
                encodings[coding] = 0
        compressed = len(body) > 1024 and encodings.get('gzip', encodings.get('*', 0)) > 0
        if compressed:
            body = gzip.compress(body, compresslevel=3, mtime=0)
        self.send_response(code)
        self.send_header('Vary', 'Accept-Encoding')
        if compressed:
            self.send_header('Content-Encoding', 'gzip')
        self.send_header('Content-Type', 'application/json; charset=utf-8')
        self.send_header('Content-Length', str(len(body)))
        self.send_header('Cache-Control', 'no-store')
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        path = urlsplit(self.path).path
        if path == '/api/livesample/template':
            try:
                query = parse_qs(urlsplit(self.path).query, keep_blank_values=True)
                body = self.service.template(query.get('leg', [])).encode('utf-8')
            except ValueError as exc:
                return self.json_response(400, {'error': str(exc)})
            except OSError:
                return self.json_response(503, {'error': 'Archived bottle template unavailable'})
            self.send_response(200)
            self.send_header('Content-Type', 'text/tab-separated-values; charset=utf-8')
            self.send_header('Content-Disposition', 'attachment; filename="selected-leg-bottles.tsv"')
            self.send_header('Content-Length', str(len(body)))
            self.send_header('Cache-Control', 'no-store')
            self.end_headers()
            self.wfile.write(body)
            return
        if path == '/api/livesample/flow':
            try:
                query = parse_qs(urlsplit(self.path).query, keep_blank_values=True)
                window = query.get('window', ['6h'])[0]
                options = {name: query[name][0] for name in ('algorithm', 'count', 'spacing') if name in query}
                return self.json_response(200, self.service.flow_history(window, **options))
            except ValueError as exc:
                return self.json_response(400, {'error': str(exc)})
            except OSError:
                return self.json_response(503, {'error': 'Flow-through history unavailable'})
        if path == '/api/livesample':
            return self.json_response(200, self.service.status())
        files = {'/livesample.html': 'livesample.html', '/static/livesample.js': 'livesample.js', '/static/livesample.css': 'livesample.css'}
        if path in files:
            p = Path(__file__).parent / 'static' / files[path]
            body = p.read_bytes()
            self.send_response(200)
            self.send_header('Content-Type', self.guess_type(str(p)))
            self.send_header('Content-Length', str(len(body)))
            self.send_header('Cache-Control', 'no-cache')
            self.end_headers()
            self.wfile.write(body)
            return
        return super().do_GET()

    def do_POST(self):
        if urlsplit(self.path).path != '/api/livesample':
            return self.json_response(404, {'error': 'Unknown endpoint'})
        try:
            if self.headers.get('Sec-Fetch-Site') == 'cross-site' or self.headers.get('Transfer-Encoding'):
                raise ValueError('Use the live sampling page on this site')
            origin = self.headers.get('Origin')
            if origin and urlsplit(origin).netloc != self.headers.get('Host'):
                raise ValueError('Invalid origin')
            if self.headers.get_content_type() != 'application/json':
                raise ValueError('Use application/json')
            length = int(self.headers.get('Content-Length', '0'))
            if not 0 < length <= 2_100_000:
                raise ValueError('Settings and TSV must be smaller than 2 MB')
            self.connection.settimeout(10)
            payload = json.loads(self.rfile.read(length))
            return self.json_response(200, self.service.update(payload))
        except (ValueError, UnicodeError) as exc:
            return self.json_response(400, {'error': str(exc)})
        except OSError:
            return self.json_response(503, {'error': 'Could not read reference data or save settings'})


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=WEBROOT)
    parser.add_argument('--state-dir', type=Path, default=DB_DIR / 'livesample')
    parser.add_argument('--port', type=int, default=8043)
    parser.add_argument('--live-url', default='http://127.0.0.1:8042/api/live')
    args = parser.parse_args()
    logging.basicConfig(level=logging.INFO)
    def live():
        with urllib.request.urlopen(args.live_url, timeout=5) as response:
            return json.load(response)
    service = SampleService(args.root, args.state_dir, live)
    server = ThreadingHTTPServer(('127.0.0.1', args.port), partial(Handler, service=service, directory=str(args.root)))
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()
        service.close()


if __name__ == '__main__':
    main()
