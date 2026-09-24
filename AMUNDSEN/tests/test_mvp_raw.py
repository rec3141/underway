"""Raw AML profiles supplement converted MVP dips without duplicating them."""
from unittest.mock import patch

import pytest

from dashboard import casts
from dashboard.legs import Leg


HEADER = """SER1 Type: 30, AML_CTD_SV
SER1 FID: M
SER1 Num5Vin: 3
LAT ( ddmm.mmmmmmm,N): 7552.9387300,N
LON (dddmm.mmmmmmm,E): 08315.3587800,W
Time (hh|mm|ss.s): 23:53:59.0
Date (dd/mm/yyyy): 23/09/2026
Bottom Depth (m): 442.6
<END_OF_HEADER>
"""


@pytest.fixture
def source(tmp_path):
    leg = Leg('2026_LEG_03', 2026, 3)
    tow = tmp_path / 'data' / 'MVP' / leg.id / '2026_03_002'
    tow.mkdir(parents=True)
    with patch.object(casts, 'DATA_ROOT', tmp_path / 'data'), patch.object(casts, 'DB_DIR', tmp_path / 'db'), patch.object(casts, 'LOCAL_MIRROR', True):
        yield leg, tow


def raw_file(tow, stem='MVP_2026-09-23_235353', header=HEADER):
    path = tow / (stem + '.raw')
    rows = [f'M {p} 1445.755 26.619 .256 3575 1361 166' for p in range(1, 13)]
    path.write_text(header + '\n'.join(rows) + '\nZ1 11.40 12.41 123.1 116.2\nM 5 1499 99 99 9999 9999 9999\n')
    path.with_suffix('.log').write_text('EVENT: DWNBO,23:53:59\nEVENT: DWNB1,23:55:45\n')
    return path


def test_raw_channels_positions_and_downcast(source):
    leg, tow = source
    raw_file(tow)
    result = casts.mvp_casts(leg)
    assert len(result) == 1
    cast = result[0]
    assert cast.station == '2026_03_002'
    assert cast.time == '2026-09-23T23:53:59'
    assert cast.lat == pytest.approx(75 + 52.93873 / 60)
    assert cast.lon == pytest.approx(-(83 + 15.35878 / 60))
    assert cast.bottom_m == 442.6
    profile = cast.profiles[0]
    assert len(profile['p']) == 12
    assert profile['vars']['Temperature'] == [.256] * 12
    assert profile['vars']['Sound velocity'] == [1445.755] * 12
    assert profile['vars']['Conductivity'] == [26.619] * 12
    assert profile['vars']['Dissolved oxygen'] == [round(1361 * 5 / 4095, 4)] * 12
    assert 'Salinity' not in profile['vars']
    assert 'Sigma-t' not in profile['vars']
    assert casts.mvp_casts(leg)[0].profiles == cast.profiles


def test_converted_profile_supersedes_raw_without_duplicate(source):
    leg, tow = source
    raw_file(tow)
    assert len(casts.mvp_casts(leg)[0].profiles) == 1
    converted = tow / 'mvp_2026-09-23_235353.m1'
    converted.write_text(HEADER + 'Press,Temp,Sal\n' + '\n'.join(f'{p},2,33' for p in range(1, 13)))
    profiles = casts.mvp_casts(leg)[0].profiles
    assert len(profiles) == 1
    assert profiles[0]['vars']['Temperature'] == [2] * 12
    assert profiles[0]['vars']['Salinity'] == [33] * 12


def test_unrecognized_raw_instrument_is_not_guessed(source):
    leg, tow = source
    raw_file(tow, header=HEADER.replace('30, AML_CTD_SV', '99, OTHER'))
    assert casts.mvp_casts(leg) == []


def test_recovery_only_and_incomplete_downcasts_are_not_profiles(source):
    leg, tow = source
    path = raw_file(tow)
    for events in ('EVENT: UP_BO,02:56:53\nEVENT: UP_B1,03:02:44\n', 'EVENT: DWNBO,23:53:59\n'):
        path.with_suffix('.log').write_text(events)
        assert casts.mvp_casts(leg) == []
    path.with_suffix('.log').write_text('EVENT: DWNBO,23:53:59\nEVENT: DWNB1,23:55:45\n')
    assert len(casts.mvp_casts(leg)[0].profiles) == 1


def test_cross_folder_duplicate_uses_fuller_record_and_rechecks_ownership(source):
    leg, tow = source
    partial = raw_file(tow)
    assert casts.mvp_casts(leg)[0].station == tow.name
    next_tow = tow.parent / '2026_03_003'
    next_tow.mkdir()
    complete = raw_file(next_tow, header=HEADER.replace('23:53:59.0', '23:54:00.0'))
    complete.write_text(complete.read_text() + 'M 20 1445.755 26.619 .256 3575 1361 166\n')
    result = casts.mvp_casts(leg)
    assert len(result) == 1
    assert result[0].station == next_tow.name
    assert result[0].time == '2026-09-23T23:54:00'
    assert casts.mvp_casts(leg)[0].station == next_tow.name
    partial.write_text(partial.read_text() + ''.join(f'M {p} 1445.755 26.619 .256 3575 1361 166\n' for p in range(21, 24)))
    assert casts.mvp_casts(leg)[0].station == tow.name


@pytest.mark.parametrize('different_events', [True, False])
def test_same_name_without_matching_downcast_evidence_remains_distinct(source, different_events):
    leg, tow = source
    raw_file(tow)
    next_tow = tow.parent / '2026_03_003'
    next_tow.mkdir()
    path = raw_file(next_tow, header=HEADER if different_events else HEADER.replace('23/09/2026', '24/09/2026'))
    if different_events:
        path.with_suffix('.log').write_text('EVENT: DWNBO,23:54:00\nEVENT: DWNB1,23:56:00\n')
    for _ in range(2):
        result = casts.mvp_casts(leg)
        assert [c.station for c in result] == [tow.name, next_tow.name]


def test_cross_folder_converted_profile_preferred(source):
    leg, tow = source
    raw_file(tow)
    next_tow = tow.parent / '2026_03_003'
    next_tow.mkdir()
    path = raw_file(next_tow)
    path.with_suffix('.m1').write_text(HEADER + 'Press,Temp,Sal\n' + '\n'.join(f'{p},2,33' for p in range(1, 13)))
    for _ in range(2):
        result = casts.mvp_casts(leg)
        assert len(result) == 1
        assert result[0].station == next_tow.name
        assert result[0].profiles[0]['vars']['Temperature'] == [2] * 12


def test_same_named_converted_profiles_have_separate_caches(source):
    leg, tow = source
    next_tow = tow.parent / '2026_03_003'
    next_tow.mkdir()
    for folder, temperature in ((tow, 2), (next_tow, 3)):
        (folder / 'same.m1').write_text(HEADER + 'Press,Temp,Sal\n' + '\n'.join(f'{p},{temperature},33' for p in range(1, 13)))
    for _ in range(2):
        result = casts.mvp_casts(leg)
        assert [c.station for c in result] == [tow.name, next_tow.name]
        assert [c.profiles[0]['vars']['Temperature'] for c in result] == [[2] * 12, [3] * 12]


def test_converted_legacy_cache_is_reused_only_for_its_tow(source):
    leg, tow = source
    path = tow / 'same.m1'
    path.write_text(HEADER + 'Press,Temp,Sal\n' + '\n'.join(f'{p},2,33' for p in range(1, 13)))
    parsed = casts._parse_mvp(leg, path)
    casts._store(leg.id, 'MVP_same', [path], parsed.__dict__)
    casts._cache_path(leg.id, casts._mvp_key(path)).unlink()
    with patch.object(casts, '_parse_mvp', side_effect=AssertionError('cache should be reused')):
        assert casts.mvp_casts(leg)[0].station == tow.name
    next_tow = tow.parent / '2026_03_003'
    next_tow.mkdir()
    (next_tow / path.name).write_text(path.read_text().replace(',2,33', ',3,33'))
    result = casts.mvp_casts(leg)
    assert [c.station for c in result] == [tow.name, next_tow.name]
    assert result[1].profiles[0]['vars']['Temperature'] == [3] * 12
