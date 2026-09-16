#!/usr/bin/env python3
"""Install/remove only the live sampling route in the local Caddy admin API."""
import argparse
import json
from pathlib import Path
import urllib.error
import urllib.request

URL = 'http://127.0.0.1:2019/config/apps/http/servers/srv0/routes'
IDENTIFIER = 'underway-live-sampling'
ROUTE = {'@id': IDENTIFIER, 'match': [{'host': ['underway.local'], 'path': [
    '/livesample.html', '/static/livesample.*', '/api/livesample', '/api/livesample/*']}],
    'handle': [{'handler': 'reverse_proxy', 'upstreams': [{'dial': '127.0.0.1:8043'}]}], 'terminal': True}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--remove', action='store_true')
    parser.add_argument('--backup', type=Path)
    args = parser.parse_args()
    if not args.remove:
        # An absent adviser must not acquire a route that would produce 502s.
        with urllib.request.urlopen('http://127.0.0.1:8043/livesample.html', timeout=5) as r:
            if r.status != 200:
                raise RuntimeError('Sampling page is not ready')
    for attempt in range(3):
        with urllib.request.urlopen(URL, timeout=5) as r:
            etag, raw = r.headers.get('Etag'), r.read()
        routes = json.loads(raw)
        keep = [r for r in routes if r.get('@id') != IDENTIFIER]
        desired = keep if args.remove else [ROUTE, *keep]
        if desired == routes:
            print('Sampling route already configured' if not args.remove else 'Sampling route absent')
            return
        if args.backup and not args.backup.exists():
            args.backup.parent.mkdir(parents=True, exist_ok=True)
            args.backup.write_bytes(raw)
        if etag:
            request = urllib.request.Request(URL, data=json.dumps(desired).encode(), method='PATCH',
                                             headers={'Content-Type': 'application/json', 'If-Match': etag})
        elif args.remove:
            request = urllib.request.Request('http://127.0.0.1:2019/id/' + IDENTIFIER, method='DELETE')
        else:
            if any(r.get('@id') == IDENTIFIER for r in routes):
                raise RuntimeError('Existing sampling route differs; remove it before replacing it')
            # Insert one array element atomically; concurrent changes to other
            # routes remain intact even when this Caddy has no ETag support.
            request = urllib.request.Request(URL + '/0', data=json.dumps(ROUTE).encode(), method='PUT',
                                             headers={'Content-Type': 'application/json'})
        try:
            with urllib.request.urlopen(request, timeout=10) as response:
                response.read()
            print('Sampling route removed' if args.remove else 'Sampling route installed')
            return
        except urllib.error.HTTPError as exc:
            if exc.code != 412 or attempt == 2:
                raise


if __name__ == '__main__':
    import fcntl
    lock_path = Path.home() / '.cache/underway-livesample-route.lock'
    lock_path.parent.mkdir(parents=True, exist_ok=True)
    with lock_path.open('w') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        main()
