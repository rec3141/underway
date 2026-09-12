import functools
import http.client
import io
import json
import threading

import pytest
from PIL import Image

from dashboard import photos, photo_upload as upload
from dashboard.serve import Handler, ThreadingHTTPServer


@pytest.fixture
def share(tmp_path, monkeypatch):
    root = tmp_path / 'share'
    (root / 'Pictures').mkdir(parents=True)
    monkeypatch.setattr(photos, 'SHARE_ROOT', root)
    monkeypatch.setattr(photos, 'PHOTOS_DIR', tmp_path / 'local')
    return root


def picture(colour='red'):
    out = io.BytesIO()
    Image.new('RGB', (20, 20), colour).save(out, 'JPEG', exif=b'Exif\x00\x00original-metadata')
    return out.getvalue()


def batch(data, **extra):
    return upload.create(dict(parent='Pictures', files=[dict(name='phone.jpg', size=len(data))], **extra))


def test_new_subfolders_preserve_originals_and_retry(share):
    data = picture()
    a, b = batch(data, label='My trip'), batch(data, label='My trip')
    assert a['path'] != b['path']
    assert (share / a['path']).parent == share / 'Pictures'
    target = share / a['path'] / a['files'][0]['name']
    assert upload.receive(a['id'], 0, io.BytesIO(data), len(data))['already_saved'] is False
    assert target.read_bytes() == data
    assert upload.receive(a['id'], 0, io.BytesIO(data), len(data))['already_saved'] is True
    target.write_bytes(b'existing different content')
    with pytest.raises(ValueError, match='not overwritten'):
        upload.receive(a['id'], 0, io.BytesIO(data), len(data))
    assert target.read_bytes() == b'existing different content'


def test_duplicate_names_get_unique_targets(share):
    data = picture()
    a = upload.create(dict(parent='Pictures', files=[dict(name='same.jpg', size=len(data))] * 2))
    assert a['files'][0]['name'] != a['files'][1]['name']
    for i in range(2):
        upload.receive(a['id'], i, io.BytesIO(data), len(data))
    assert len(list((share / a['path']).glob('*.jpg'))) == 2
    assert photos.folder_files('Pictures') == []
    assert photos._files_of({'folder':'Pictures'}) == []
    assert len(photos.folder_files(a['path'])) == 2
    assert len(photos._files_of({'folder':a['path']})) == 2


@pytest.mark.parametrize('name', ['../x.jpg', '/x.jpg', 'a\\b.jpg', 'photo.heic', 'script.html'])
def test_invalid_names_leave_share_untouched(share, name):
    with pytest.raises(ValueError):
        upload.create(dict(parent='Pictures', files=[dict(name=name, size=10)]))
    assert not list((share / 'Pictures').iterdir())


def test_bad_destinations_and_limits(share, tmp_path):
    (share / 'escape').symlink_to(tmp_path)
    for parent in ('../', 'escape', 'missing'):
        with pytest.raises(ValueError):
            upload.create(dict(parent=parent, files=[dict(name='a.jpg', size=10)]))
    for files in ([], [dict(name='a.jpg', size=0)], [dict(name='a.jpg', size=upload.MAX_FILE + 1)],
                  [dict(name='a.jpg', size=1)] * 301, [dict(name='a.jpg', size=upload.MAX_FILE)] * 33):
        with pytest.raises(ValueError):
            upload.create(dict(parent='Pictures', files=files))


def test_bad_or_interrupted_images_never_publish(share):
    data = picture()
    a = batch(data)
    for content in (data[:10], b'x' * len(data)):
        with pytest.raises(ValueError):
            upload.receive(a['id'], 0, io.BytesIO(content), len(data))
        assert [p.name for p in (share / a['path']).iterdir()] == [photos.UPLOAD_MARKER]
    for identifier, index, size in [('bad', 0, len(data)), (a['id'], -1, len(data)),
                                     (a['id'], 1, len(data)), (a['id'], 0, len(data) + 1)]:
        with pytest.raises(ValueError):
            upload.receive(identifier, index, io.BytesIO(data), size)


def test_replaced_batch_folder_is_refused(share):
    data = picture()
    a = batch(data)
    folder = share / a['path']
    (folder / photos.UPLOAD_MARKER).unlink()
    folder.rmdir()
    folder.symlink_to(share / 'Pictures')
    with pytest.raises(ValueError, match='no longer available'):
        upload.receive(a['id'], 0, io.BytesIO(data), len(data))
    assert not (share / 'Pictures' / a['files'][0]['name']).exists()


def test_expired_batch_and_share_write_failure(share, monkeypatch):
    data = picture()
    a = batch(data)
    with upload._db() as db:
        db.execute('UPDATE batches SET created=0 WHERE id=?', (a['id'],))
    with pytest.raises(ValueError, match='expired'):
        upload.receive(a['id'], 0, io.BytesIO(data), len(data))
    b = batch(data)
    def fail(*args):
        raise OSError('Share disconnected')
    monkeypatch.setattr(upload.shutil, 'copyfileobj', fail)
    with pytest.raises(OSError):
        upload.receive(b['id'], 0, io.BytesIO(data), len(data))
    assert [p.name for p in (share / b['path']).iterdir()] == [photos.UPLOAD_MARKER]


def test_upload_routes_and_cross_site_refusal(share, tmp_path):
    server = ThreadingHTTPServer(('127.0.0.1', 0), functools.partial(Handler, directory=str(tmp_path)))
    threading.Thread(target=server.serve_forever, daemon=True).start()
    def post(path, data, headers):
        c = http.client.HTTPConnection('127.0.0.1', server.server_port, timeout=5)
        try:
            c.request('POST', path, data, headers)
            r = c.getresponse()
            return r.status, json.loads(r.read())
        finally:
            c.close()
    try:
        data = picture()
        spec = json.dumps(dict(parent='Pictures', files=[dict(name='a.jpg', size=len(data))]))
        headers = {'Content-Type': 'application/json', 'X-Photo-Upload': '1'}
        for bad in ({}, {**headers, 'Origin': 'https://evil.example'}, {**headers, 'Sec-Fetch-Site': 'cross-site'}):
            assert post('/api/nature/upload', spec, bad)[0] == 400
        assert not list((share / 'Pictures').iterdir())
        status, a = post('/api/nature/upload', spec, headers)
        assert status == 200
        status, result = post('/api/nature/upload/file?index=0', data, {**headers, 'X-Upload-ID': a['id']})
        assert status == 200 and result['ok']
        assert (share / a['path'] / a['files'][0]['name']).read_bytes() == data
    finally:
        server.shutdown()
        server.server_close()
