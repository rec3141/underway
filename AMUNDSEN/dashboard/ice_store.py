"""Camera product store. No model work in the HTTP process; IDs never become paths."""
import json
import os
from pathlib import Path
import sqlite3
import time

ROOT=Path(os.environ.get('UNDERWAY_ICE_ROOT','/data/underway_server/ice'))
TYPES=['grease ice','nilas','thin ice floe','icy bits','brash ice','thick ice floe']

def connect(root=ROOT,create=True):
    root=Path(root)
    if create:root.mkdir(parents=True,exist_ok=True)
    db=sqlite3.connect(str(root/'ice.sqlite') if create else f'file:{root}/ice.sqlite?mode=ro',uri=not create,timeout=20)
    db.row_factory=sqlite3.Row
    if create:
        db.execute('PRAGMA journal_mode=WAL')
        db.executescript('''CREATE TABLE IF NOT EXISTS meta(key TEXT PRIMARY KEY,value TEXT);
        CREATE TABLE IF NOT EXISTS photos(id TEXT PRIMARY KEY,file TEXT UNIQUE,t REAL,leg TEXT,status TEXT DEFAULT 'pending',ice REAL,types TEXT,detail TEXT,attempts INTEGER DEFAULT 0,retry_after REAL DEFAULT 0);
        CREATE INDEX IF NOT EXISTS photos_time ON photos(t);
        CREATE TABLE IF NOT EXISTS audits(id TEXT PRIMARY KEY,detail TEXT);
        CREATE TABLE IF NOT EXISTS telemetry(t REAL,cpu REAL,gpu REAL,average REAL);''')
    return db

def track(start,end,root=ROOT):
    if not (Path(root)/'ice.sqlite').exists():return dict(photos=[],types=TYPES,status='not configured')
    with connect(root,False) as db:
        rows=db.execute('SELECT id,t,leg,status,ice,types FROM photos WHERE t>=? AND t<=? ORDER BY t',(start,end)).fetchall()
    return dict(types=TYPES,photos=[dict(id=r['id'],time=r['t']*1000,leg=r['leg'],status=r['status'],ice=r['ice'],types=json.loads(r['types']) if r['types'] else None) for r in rows])

def detail(identifier,root=ROOT):
    with connect(root,False) as db:
        row=db.execute('SELECT file,status,detail FROM photos WHERE id=?',(identifier,)).fetchone()
        return dict(row) if row else None

def photo_path(identifier,kind,root=ROOT):
    import re
    if not re.fullmatch('[a-f0-9]{20}',identifier) or kind not in {'source','roi','slice'}:raise ValueError('Invalid photo')
    return Path(root)/'images'/f'{identifier}-{kind}.jpg'

def complete(db,identifier,status,values,record):
    db.execute('UPDATE photos SET status=?,ice=?,types=?,detail=? WHERE id=?',
               (status,sum(values),json.dumps(values),json.dumps(record),identifier));db.commit()
