import json

from dashboard import casts
from dashboard.legs import Leg


def test_cast_sheet_is_published_and_linked_in_index_and_profile(tmp_path, monkeypatch):
    leg = Leg('2026_LEG_03', 2026, 3)
    source = tmp_path / 'mirror' / 'Rosette' / leg.id / 'Logs' / 'RosetteSheet_014.xlsx'
    source.parent.mkdir(parents=True)
    source.write_bytes(b'sheet fixture')
    cast = casts.Cast(f'{leg.id}:CTD_014', leg.id, 'CTD', '014', None, 70, -80)
    monkeypatch.setattr(casts, 'DATA_ROOT', tmp_path / 'mirror')
    monkeypatch.setattr(casts, 'rosette_casts', lambda _: [cast])
    monkeypatch.setattr(casts, 'mvp_casts', lambda _: [])
    root = tmp_path / 'www'
    row = casts.build_casts([leg], root)['casts'][0]
    assert (root / row['log_url']).read_bytes() == source.read_bytes()
    assert json.loads((root / row['file']).read_text())['log_url'] == row['log_url']
