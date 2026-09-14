"""Independent HTTP probes and durable outage/recovery delivery to ops channels."""
import json
import logging
import os
import time
import urllib.request
from concurrent.futures import ThreadPoolExecutor

from .config import DB_DIR

log = logging.getLogger(__name__)


def targets():
    configured = os.environ.get('UNDERWAY_UPTIME_URLS', '').split()
    return configured or [f'http://127.0.0.1:{os.environ.get("UNDERWAY_PORT", "8042")}/api/health',
                          'http://127.0.0.1/underway/api/health',
                          f'http://{os.environ.get("UNDERWAY_MDNS_NAME", "underway.local")}/api/health']


def probe(url):
    start = time.monotonic()
    try:
        # Local network health must not be routed through an environment proxy.
        opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
        with opener.open(url, timeout=5) as response:
            if response.status != 200 or json.loads(response.read(4096)).get('ok') is not True:
                raise ValueError('Health response is not ready')
        return {'ok': True, 'ms': round((time.monotonic() - start) * 1000)}
    except Exception as e:
        return {'ok': False, 'ms': round((time.monotonic() - start) * 1000), 'error': str(e)[:240]}


def update(state, results, now):
    pending = state.setdefault('pending', [])
    checks = state.setdefault('checks', {})
    for url, result in results.items():
        previous = checks.setdefault(url, {'failures': 0, 'down': False})
        previous.update(last_check=now, result=result)
        previous['failures'] = 0 if result['ok'] else previous['failures'] + 1
        if not result['ok'] and previous['failures'] == 1:
            previous['since'] = now
        text = None
        if previous['failures'] >= 2 and not previous['down']:
            previous['down'] = True
            text = f'Underway unavailable: {url}\nFailed two consecutive checks. {result.get("error", "")}\nFirst failure: {previous["since"]}'
        elif result['ok'] and previous['down']:
            previous['down'] = False
            text = f'Underway recovered: {url}\nResponse {result["ms"]} ms; unavailable since {previous["since"]}.'
        if text:
            log.warning('%s', text)
            pending.append({'text': text, 'sent': []})
            state.setdefault('events', []).append({'at': now, 'url': url, 'status': 'down' if previous['down'] else 'recovered'})
            state['events'] = state['events'][-100:]
    state['checked_at'] = now


def deliver(state):
    from .alerts import ops_targets, send_email
    to, cfg, _ = ops_targets()
    channels = {}
    if to and cfg:
        channels['email'] = lambda text: send_email(cfg, to, text.splitlines()[0], text)
    pending = state.get('pending', [])
    for name, send in channels.items():
        items = [item for item in pending if name not in item['sent']]
        if not items:
            continue
        try:
            send('\n\n'.join(item['text'] for item in items[:8]))
            for item in items[:8]:
                item['sent'].append(name)
        except Exception as e:
            log.warning('Uptime %s delivery failed: %s', name, type(e).__name__)
    state['pending'] = [item for item in pending if not channels or any(name not in item['sent'] for name in channels)]


def save(state, path):
    temporary = path.with_suffix('.tmp')
    temporary.write_text(json.dumps(state, indent=2))
    temporary.replace(path)


def run():
    from datetime import datetime, timezone
    DB_DIR.mkdir(parents=True, exist_ok=True)
    path = DB_DIR / 'uptime.json'
    try:
        state = json.loads(path.read_text())
    except (OSError, ValueError):
        state = {}
    urls = targets()
    with ThreadPoolExecutor(max_workers=min(8, len(urls))) as pool:
        results = dict(zip(urls, pool.map(probe, urls)))
    update(state, results, datetime.now(timezone.utc).isoformat(timespec='seconds'))
    save(state, path)  # Preserve notices even if the process stops during delivery.
    deliver(state)
    save(state, path)
    log.info('Uptime: %s', json.dumps(results))


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    run()
