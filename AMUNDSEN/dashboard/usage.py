"""Daily page-view counts, without visitor identifiers or browsing histories."""
import json
import sqlite3
from datetime import datetime, timedelta, timezone

from .config import DB_DIR

PAGES = {'underway', 'casts', 'stations', 'calendar', 'sources', 'wiki', 'photos'}


def record(page):
    if page not in PAGES:
        raise ValueError('Unknown page')
    DB_DIR.mkdir(parents=True, exist_ok=True)
    now = datetime.now(timezone.utc)
    with sqlite3.connect(DB_DIR / 'usage.sqlite', timeout=1) as db:
        db.execute('CREATE TABLE IF NOT EXISTS views (day TEXT, page TEXT, n INTEGER, PRIMARY KEY(day,page))')
        db.execute('INSERT INTO views VALUES (?,?,1) ON CONFLICT(day,page) DO UPDATE SET n=n+1', (now.date().isoformat(), page))
        db.execute('DELETE FROM views WHERE day < ?', ((now - timedelta(days=90)).date().isoformat(),))


def report():
    if not (DB_DIR / 'usage.sqlite').exists():
        return []
    with sqlite3.connect(DB_DIR / 'usage.sqlite') as db:
        return [dict(zip(('day', 'page', 'views'), row)) for row in db.execute('SELECT day,page,n FROM views ORDER BY day DESC,page')]


if __name__ == '__main__':
    print(json.dumps(report(), indent=2))
