"""Daily page views and distinct client IP counts, retained for 90 days."""
import ipaddress
import json
import sqlite3
from datetime import datetime, timedelta, timezone

from .config import DB_DIR

PAGES = {'underway', 'casts', 'stations', 'calendar', 'sources', 'wiki', 'photos'}


def client_ip(peer, forwarded=''):
    """Only the local reverse proxy may supply a forwarded client address."""
    address = ipaddress.ip_address(peer)
    if address.is_loopback and forwarded:
        try:
            address = ipaddress.ip_address(forwarded.split(',')[-1].strip())
        except ValueError:
            pass
    return str(getattr(address, 'ipv4_mapped', None) or address)


def record(page, ip=None):
    if page not in PAGES:
        raise ValueError('Unknown page')
    DB_DIR.mkdir(parents=True, exist_ok=True)
    now = datetime.now(timezone.utc)
    with sqlite3.connect(DB_DIR / 'usage.sqlite', timeout=1) as db:
        db.execute('CREATE TABLE IF NOT EXISTS views (day TEXT, page TEXT, n INTEGER, PRIMARY KEY(day,page))')
        db.execute('INSERT INTO views VALUES (?,?,1) ON CONFLICT(day,page) DO UPDATE SET n=n+1', (now.date().isoformat(), page))
        db.execute('DELETE FROM views WHERE day < ?', ((now - timedelta(days=90)).date().isoformat(),))
        db.execute('CREATE TABLE IF NOT EXISTS client_ips (day TEXT, ip TEXT, PRIMARY KEY(day,ip))')
        db.execute('CREATE TABLE IF NOT EXISTS usage_meta (key TEXT PRIMARY KEY, value TEXT)')
        if ip:
            address = client_ip(ip)
            db.execute('INSERT OR IGNORE INTO client_ips VALUES (?,?)', (now.date().isoformat(), address))
            db.execute("INSERT OR IGNORE INTO usage_meta VALUES ('ips_since',?)", (now.isoformat(),))
        db.execute('DELETE FROM client_ips WHERE day < ?', ((now - timedelta(days=89)).date().isoformat(),))


def report():
    if not (DB_DIR / 'usage.sqlite').exists():
        return []
    with sqlite3.connect(DB_DIR / 'usage.sqlite') as db:
        return [dict(zip(('day', 'page', 'views'), row)) for row in db.execute('SELECT day,page,n FROM views ORDER BY day DESC,page')]


def report_ips():
    result = dict(today=0, week=0, total=0, since=None)
    if not (DB_DIR / 'usage.sqlite').exists():
        return result
    now = datetime.now(timezone.utc).date()
    with sqlite3.connect(DB_DIR / 'usage.sqlite') as db:
        tables = {row[0] for row in db.execute("SELECT name FROM sqlite_master WHERE type='table'")}
        if 'client_ips' not in tables:
            return result  # Existing page-view history predates IP counting.
        for key, days in (('today', 1), ('week', 7), ('total', 90)):
            result[key] = db.execute('SELECT COUNT(DISTINCT ip) FROM client_ips WHERE day >= ? AND day <= ?',
                                     ((now - timedelta(days=days-1)).isoformat(), now.isoformat())).fetchone()[0]
        if 'usage_meta' in tables:
            row = db.execute("SELECT value FROM usage_meta WHERE key='ips_since'").fetchone()
            result['since'] = row[0] if row else None
    return result


if __name__ == '__main__':
    print(json.dumps(report(), indent=2))
