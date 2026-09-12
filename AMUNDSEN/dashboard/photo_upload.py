"""Explicit LAN photo uploads, always into a newly created share subfolder.

Original bytes are validated, not re-encoded. Upload capabilities and manifests
stay on local disk; the browser cannot name a write target after batch creation.
"""
import hashlib
import json
import re
import secrets
import shutil
import sqlite3
import tempfile
import threading
import time
from pathlib import Path

from PIL import Image

from . import photos

MAX_FILE = 64 * 1024 * 1024
MAX_BATCH = 2 * 1024 * 1024 * 1024
MAX_FILES = 300
TTL = 24 * 3600
FORMATS = {'.jpg': 'JPEG', '.jpeg': 'JPEG', '.png': 'PNG', '.webp': 'WEBP'}
_locks = [threading.Lock() for _ in range(32)]
_slots = threading.BoundedSemaphore(4)


def _db():
    photos.PHOTOS_DIR.mkdir(parents=True, exist_ok=True)
    db = sqlite3.connect(photos.PHOTOS_DIR / 'uploads.sqlite', timeout=20)
    db.execute('CREATE TABLE IF NOT EXISTS batches (id TEXT PRIMARY KEY, created REAL, manifest TEXT)')
    return db


def _name(value, limit=100):
    return re.sub(r'[^\w .()-]', '_', str(value))[:limit].strip(' .') or 'photos'


def create(spec):
    if not isinstance(spec, dict):
        raise ValueError('Expected an upload batch')
    parent = photos._safe(spec.get('parent', ''))
    if not parent.is_dir():
        raise ValueError('Choose an existing folder on the share')
    files = spec.get('files')
    if not isinstance(files, list) or not 1 <= len(files) <= MAX_FILES:
        raise ValueError(f'Choose 1–{MAX_FILES} photos')
    entries = []
    for i, item in enumerate(files):
        if not isinstance(item, dict):
            raise ValueError('Invalid photo')
        name, size = item.get('name', ''), item.get('size')
        if not isinstance(name, str) or '/' in name or '\\' in name or '\x00' in name:
            raise ValueError('Invalid photo filename')
        ext = Path(name).suffix.lower()
        if ext not in FORMATS:
            raise ValueError('Use JPEG, PNG or WebP photos; HEIC and videos are not supported yet')
        if type(size) is not int or not 0 < size <= MAX_FILE:
            raise ValueError('Each photo must be between 1 byte and 64 MiB')
        entries.append(dict(name=f'{i + 1:03d}-{_name(Path(name).stem)}{ext}', size=size))
    if sum(f['size'] for f in entries) > MAX_BATCH:
        raise ValueError('A batch must be at most 2 GiB')
    identifier = secrets.token_hex(24)
    label = spec.get('label') or 'Phone photos'
    if not isinstance(label, str) or not 1 <= len(label) <= 60 or label != label.strip(' .') or label in ('.','..') or any(c in label for c in '/\\\x00<>:"|?*') or any(ord(c)<32 for c in label):
        raise ValueError('Use a folder name of 1–60 characters, without path separators or special characters')
    folder = parent / label
    try:
        folder.mkdir()  # exclusive creation; never adopt an existing directory
    except FileExistsError as e:
        raise ValueError('That folder already exists. Choose another name to create a new folder.') from e
    # A watch on the parent belongs to somebody else: it must not silently
    # publish this user's photos under that person's name/licence.
    (folder / photos.UPLOAD_MARKER).write_text('Import this folder explicitly to choose credit and licence.\n')
    batch = dict(id=identifier, path=photos.rel_of(folder), files=entries)
    with _db() as db:
        db.execute('DELETE FROM batches WHERE created < ?', (time.time() - TTL,))
        db.execute('INSERT INTO batches VALUES (?,?,?)', (identifier, time.time(), json.dumps(batch)))
    return batch


def receive(identifier, index, stream, length):
    if not re.fullmatch(r'[a-f0-9]{48}', identifier or ''):
        raise ValueError('Invalid upload batch')
    if type(index) is not int or index < 0:
        raise ValueError('Invalid photo index')
    if not 0 < length <= MAX_FILE:
        raise ValueError('Photo must be at most 64 MiB')
    if not _slots.acquire(blocking=False):
        raise ValueError('Uploads busy; please retry shortly')
    try:
        with _locks[int(identifier[:2], 16) % len(_locks)]:
            with _db() as db:
                row = db.execute('SELECT created,manifest FROM batches WHERE id=?', (identifier,)).fetchone()
            if not row or row[0] < time.time() - TTL:
                raise ValueError('Upload batch expired; start a new batch')
            batch = json.loads(row[1])
            if index >= len(batch['files']) or batch['files'][index]['size'] != length:
                raise ValueError('Photo does not match the upload batch')
            entry = batch['files'][index]
            folder = photos._safe(batch['path'])
            if not folder.is_dir() or folder != photos.SHARE_ROOT.resolve() / batch['path']:
                raise ValueError('Upload folder is no longer available')
            target = folder / entry['name']
            # Stage on local disk first: an interrupted phone connection never
            # leaves a half-photo visible to the journal importer.
            with tempfile.TemporaryFile() as staged:
                digest = hashlib.sha256()
                remaining = length
                while remaining:
                    chunk = stream.read(min(1024 * 1024, remaining))
                    if not chunk:
                        raise ValueError('Upload interrupted; retry this photo')
                    staged.write(chunk)
                    digest.update(chunk)
                    remaining -= len(chunk)
                staged.seek(0)
                try:
                    with Image.open(staged) as im:
                        if im.format != FORMATS[target.suffix] or im.width * im.height > 80_000_000:
                            raise ValueError('Unsupported image or image larger than 80 megapixels')
                        im.verify()
                except (OSError, SyntaxError, Image.DecompressionBombError) as e:
                    raise ValueError('This file is not a valid supported photo') from e
                staged.seek(0)
                if target.exists() or target.is_symlink():
                    if target.is_symlink() or not target.is_file():
                        raise ValueError('Destination already exists; it was not overwritten')
                    with target.open('rb') as saved:
                        same = hashlib.file_digest(saved, 'sha256').digest() == digest.digest()
                    if not same:
                        raise ValueError('Destination already exists with different contents; it was not overwritten')
                    return dict(ok=True, name=entry['name'], already_saved=True)
                partial = folder / f'.upload-{secrets.token_hex(12)}.part'
                try:
                    with partial.open('xb') as out:
                        shutil.copyfileobj(staged, out, 1024 * 1024)
                    # This capability-owned directory and per-batch lock make
                    # publication serial; retries compare bytes, never replace.
                    if target.exists() or target.is_symlink():
                        raise ValueError('Destination already exists; it was not overwritten')
                    partial.rename(target)
                finally:
                    partial.unlink(missing_ok=True)
            return dict(ok=True, name=entry['name'], already_saved=False)
    finally:
        _slots.release()
