"""Dedicated operator-only Telegram bot with one durable Codex session."""

from __future__ import annotations

import fcntl
import json
import logging
import os
from pathlib import Path
import shutil
import signal
import sqlite3
import subprocess
import tempfile
import threading
import time
import uuid

from .config import DB_DIR

log = logging.getLogger(__name__)
USAGE = ("Send a message to continue your Codex session. Replies include the quoted text. "
         "The same session is resumed after later messages and bot restarts.")
_guard = threading.Lock()
_worker = None


def connect():
    DB_DIR.mkdir(parents=True, exist_ok=True)
    db = sqlite3.connect(DB_DIR / 'codex_bot.sqlite', timeout=10)
    db.row_factory = sqlite3.Row
    db.executescript('''
        CREATE TABLE IF NOT EXISTS sessions (
            chat TEXT, name TEXT, thread TEXT NOT NULL, PRIMARY KEY(chat, name));
        CREATE TABLE IF NOT EXISTS jobs (
            id INTEGER PRIMARY KEY, chat TEXT NOT NULL, name TEXT NOT NULL,
            prompt TEXT NOT NULL, status TEXT NOT NULL DEFAULT 'queued',
            replies TEXT NOT NULL DEFAULT '[]', delivered INTEGER NOT NULL DEFAULT 0);
        CREATE TABLE IF NOT EXISTS metadata (key TEXT PRIMARY KEY, value TEXT NOT NULL);
    ''')
    return db


def submit(msg, update_id, tg):
    """Authorize before accepting ordinary messages on the dedicated bot."""
    chat = str((msg.get('chat') or {}).get('id', ''))
    sender = str((msg.get('from') or {}).get('id', ''))
    owner = os.environ.get('UNDERWAY_CODEX_TELEGRAM_ID') or os.environ.get('TELEGRAM_ID', '')
    if not owner or sender != owner or chat != owner or msg.get('chat', {}).get('type') != 'private':
        return 'Codex is available only to the configured operator in a private chat.'
    message = (msg.get('text') or msg.get('caption') or '').strip()
    if message.split('@')[0] in ('/start', '/help'):
        return USAGE
    name = 'main'
    quoted = msg.get('reply_to_message') or {}
    context = quoted.get('text') or quoted.get('caption') or ''
    prompt = (f'Quoted Telegram message:\n{context}\n\nOperator request:\n{message}'
              if context and message else message or context)
    if not prompt:
        return USAGE
    if len(prompt) > 20000:
        return 'That message is too long. Please keep it below 20,000 characters.'
    with connect() as db:
        # Preserve the latest old session, but not the old bot's queue or IDs.
        legacy = DB_DIR / 'telegram_codex.sqlite'
        if not db.execute('SELECT 1 FROM sessions WHERE chat=?', (chat,)).fetchone() and legacy.exists():
            with sqlite3.connect(f'{legacy.as_uri()}?mode=ro', uri=True) as old:
                previous = old.execute('''SELECT s.thread FROM sessions s
                    LEFT JOIN jobs j ON j.chat=s.chat AND j.name=s.name
                    WHERE s.chat=? GROUP BY s.name ORDER BY MAX(j.id) DESC LIMIT 1''', (chat,)).fetchone()
            if previous:
                db.execute('INSERT INTO sessions VALUES (?, ?, ?)', (chat, name, previous[0]))
        if db.execute('SELECT 1 FROM jobs WHERE id=?', (update_id,)).fetchone():
            return 'Codex: this message was already queued.'
        if db.execute("SELECT count(*) FROM jobs WHERE status IN ('queued', 'running')").fetchone()[0] >= 20:
            return 'Codex has 20 pending messages. Please try again after they finish.'
        db.execute('INSERT INTO jobs(id, chat, name, prompt) VALUES (?, ?, ?, ?)',
                   (update_id, chat, name, prompt))
    start(tg)
    return 'Codex: queued. I will reply here when it finishes.'


def command(thread=None):
    binary = os.environ.get('UNDERWAY_CODEX_BIN') or shutil.which('codex') or str(Path.home() / '.local/bin/codex')
    cwd = Path(os.environ.get('UNDERWAY_CODEX_CWD') or '/data/dev/underway')
    args = [binary, 'exec', '-C', str(cwd), '-s', 'workspace-write', '-c', 'approval_policy="never"',
            '-c', 'sandbox_workspace_write.network_access=true',
            '-c', 'sandbox_workspace_write.writable_roots=' + json.dumps([str(cwd / '.git')])]
    if thread:
        args += ['resume', thread]
    return args + ['--json', '-']


def execute(job):
    """Save the thread ID as soon as Codex emits it, even if the turn fails."""
    with connect() as db:
        session = db.execute('SELECT thread FROM sessions WHERE chat=? AND name=?',
                             (job['chat'], job['name'])).fetchone()
    cwd = Path(os.environ.get('UNDERWAY_CODEX_CWD') or '/data/dev/underway')
    timeout = int(os.environ.get('UNDERWAY_CODEX_TIMEOUT', '1800'))
    answers, failed = [], False
    with tempfile.TemporaryFile(mode='w+b') as source, tempfile.TemporaryFile(mode='w+b') as output, tempfile.TemporaryFile() as errors:
        source.write(job['prompt'].encode()); source.seek(0)
        proc = subprocess.Popen(command(session['thread'] if session else None), cwd=cwd,
                                stdin=source, stdout=output, stderr=errors, start_new_session=True)
        # Separate file descriptions prevent our reader moving the child's write offset.
        with open(f'/proc/self/fd/{output.fileno()}', 'rb') as reader:
            started = time.monotonic()
            try:
                while True:
                    exited = proc.poll() is not None
                    while True:
                        position = reader.tell()
                        line = reader.readline()
                        if not line or not line.endswith(b'\n'):
                            reader.seek(position)
                            break
                        try:
                            event = json.loads(line)
                        except (ValueError, UnicodeError):
                            continue
                        if event.get('type') == 'thread.started':
                            thread = str(uuid.UUID(event['thread_id']))
                            with connect() as db:
                                db.execute('INSERT OR REPLACE INTO sessions VALUES (?, ?, ?)',
                                           (job['chat'], job['name'], thread))
                        elif event.get('type') == 'item.completed':
                            item = event.get('item') or {}
                            if item.get('type') == 'agent_message' and item.get('text'):
                                answers.append(item['text'])
                        elif event.get('type') in ('turn.failed', 'error'):
                            failed = True
                    if exited:
                        break
                    if time.monotonic() - started > timeout or os.fstat(output.fileno()).st_size > 16 * 1024 * 1024:
                        raise TimeoutError('Codex exceeded its time or output limit')
                    time.sleep(0.2)
            finally:
                if proc.poll() is None:
                    os.killpg(proc.pid, signal.SIGKILL)
                proc.wait()
    answer = '\n\n'.join(answers)[-24000:]
    if failed or proc.returncode:
        return (answer + '\n\n' if answer else '') + 'Codex could not finish this turn. Send another message to resume the saved session. Check the Codex bot service journal for setup issues.'
    return answer or 'Codex finished without a text response.'


def finish(db, job, answer):
    prefix = 'Codex\n'
    # 1,800 code points also fit Telegram's limit when every character is astral.
    chunks = [prefix + answer[i:i + 1800] for i in range(0, len(answer), 1800)]
    db.execute("UPDATE jobs SET status='done', replies=? WHERE id=?", (json.dumps(chunks), job['id']))


def work(tg):
    DB_DIR.mkdir(parents=True, exist_ok=True)
    with (DB_DIR / 'codex_bot.lock').open('a') as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            return
        with connect() as db:
            for job in db.execute("SELECT * FROM jobs WHERE status='running'").fetchall():
                finish(db, job, 'The bot restarted during this turn. It was not replayed automatically. Send another message to continue.')
        while True:
            try:
                with connect() as db:
                    pending = db.execute("SELECT * FROM jobs WHERE status='done' ORDER BY id").fetchall()
                for job in pending:
                    try:
                        for i, reply in enumerate(json.loads(job['replies'])):
                            if i < job['delivered']:
                                continue
                            tg.send(job['chat'], reply)
                            with connect() as db:
                                db.execute('UPDATE jobs SET delivered=? WHERE id=?', (i + 1, job['id']))
                        with connect() as db:
                            db.execute("UPDATE jobs SET status='sent' WHERE id=?", (job['id'],))
                    except Exception:
                        log.warning('Codex reply delivery failed; will retry', exc_info=True)
                with connect() as db:
                    job = db.execute("SELECT * FROM jobs WHERE status='queued' ORDER BY id LIMIT 1").fetchone()
                    if job:
                        db.execute("UPDATE jobs SET status='running' WHERE id=?", (job['id'],))
                if job:
                    try:
                        answer = execute(job)
                    except Exception:
                        log.exception('Telegram Codex job %s failed', job['id'])
                        answer = 'Codex could not finish this turn. Check its login, workspace permissions and service journal, then send another message to resume.'
                    with connect() as db:
                        finish(db, job, answer)
                else:
                    time.sleep(2)
            except Exception:
                log.exception('Telegram Codex worker failed; retrying')
                time.sleep(5)


def start(tg):
    global _worker
    with _guard:
        if _worker is None or not _worker.is_alive():
            _worker = threading.Thread(target=work, args=(tg,), name='telegram-codex', daemon=True)
            _worker.start()


def handle(tg, wait=0):
    with connect() as db:
        row = db.execute("SELECT value FROM metadata WHERE key='offset'").fetchone()
    count = 0
    for update in tg.updates(int(row[0]) if row else 0, wait):
        msg = update.get('message') or {}
        reply = submit(msg, update['update_id'], tg) if msg.get('chat') else None
        with connect() as db:
            db.execute("INSERT OR REPLACE INTO metadata VALUES ('offset', ?)", (str(update['update_id'] + 1),))
        if reply:
            try:
                tg.send(str(msg['chat']['id']), reply)
            except Exception:
                log.warning('Codex acknowledgement delivery failed')
        count += 1
    return count


def bot_loop():
    from .alerts import Telegram
    token = os.environ.get('TELEGRAM_KEY_CODEX', '')
    if not token:
        raise RuntimeError('TELEGRAM_KEY_CODEX is not configured')
    tg = Telegram(token)
    log.info('Codex bot: @%s', tg.me())
    start(tg)
    while True:
        try:
            handle(tg, wait=25)
        except Exception:
            log.warning('Codex bot polling failed; retrying')
            time.sleep(10)
