"""Amundsen Science's archive comes in as legs the build takes like its own."""
import json
from pathlib import Path

import pytest

from dashboard import archive, ingest
from dashboard.derive import resolve_position, resolve_variables


def write_leg(root: Path, leg="2016_LEG_01"):
    d = root / "by-leg" / leg[:4] / leg
    d.mkdir(parents=True)
    (d / "tsg.csv").write_text(
        "cruise_number,time,latitude,longitude,SST_TSG,P_sal_TSG,Fluo,SVel\n"
        "Amundsen_2016001,2016-06-05T18:00:00Z,50.1,-58.5,6.1,31.4,0.5,1450\n"
        "Amundsen_2016001,2016-06-05T18:01:00Z,50.2,-58.6,6.2,31.5,0.6,1451\n")
    (d / "avos.csv").write_text(
        "cruise_number,time,latitude,longitude,Wind_dir,Wind_speed,Air_temp,Dew_point,Pressure\n"
        "Amundsen_2016001,2016-06-05T18:00:00Z,49.0,-57.0,270,20,5.0,2.0,1012\n"
        "Amundsen_2016001,2016-06-06T00:00:00Z,50.5,-59.0,90,10,4.0,4.0,1010\n")
    (d / "ctd_1dbar.csv").write_text(
        "cruise_name,cruise_number,cast_number,station,time,latitude,longitude,PRES,TE90,PSAL,OXYM\n"
        "GreenEdge,2016001,1,G1,2016-06-05T20:37:47Z,50.3,-58.5,1,6.6,31.4,296\n"
        "GreenEdge,2016001,1,G1,2016-06-05T20:37:47Z,50.3,-58.5,2,6.5,31.5,297\n"
        "GreenEdge,2016001,2,G2,2016-06-06T03:10:00Z,51.0,-59.1,1,5.0,32.0,300\n"
        ",,NaN,,,NaN,NaN,1,3.0,30,\n")
    return d


def test_a_leg_becomes_acsd_files_the_ingest_reads_into_the_right_variables(tmp_path):
    d = write_leg(tmp_path)
    out = tmp_path / "acsd"
    assert archive.write_acsd(d, out) == 2                                   # two days
    frame, names = ingest.parse_file(out / "ACSD_20160605.csv")
    first = frame.iloc[0]
    by_var = {r.variable.name: r.key for r in resolve_variables(list(frame.columns), names) if r.key}
    assert first[by_var["SST (°C)"]] == 6.1
    assert first[by_var["Salinity (PSU)"]] == 31.4
    assert first[by_var["Air temperature (°C)"]] == 5.0
    assert first[by_var["True wind direction (°)"]] == 270
    assert 80 < first[by_var["Relative humidity (%)"]] < 83                   # from the dew point
    assert "Relative wind speed (kn)" not in by_var                          # a true wind speed is not a relative one
    lat, lon = resolve_position(list(frame.columns))[0]
    assert (first[lat], first[lon]) == (50.1, -58.5)                         # the TSG's position stands
    # a cast's minute is a position of its own
    assert frame.loc[frame.index == "2016-06-05T20:37:00Z", lat].tolist() == [50.3]


def test_casts_group_by_cast_and_skip_rows_without_one(tmp_path, monkeypatch):
    write_leg(tmp_path)
    monkeypatch.setattr(archive, "ARCHIVE_ROOT", tmp_path)
    casts = archive.casts("2016_LEG_01")
    assert [c.id for c in casts] == ["2016_LEG_01:CTD_001", "2016_LEG_01:CTD_002"]
    c = casts[0]
    assert (c.p, c.vars["Temperature"], c.units["Oxygen"], c.station) == ([1.0, 2.0], [6.6, 6.5], "µM", "G1")
    assert c.meta()["file"] == "data/casts/2016_LEG_01/CTD_001.json"


def test_discovery_marks_archive_legs_and_keeps_the_ships_own(tmp_path, monkeypatch):
    from dashboard import legs
    share = tmp_path / "Share"
    for leg, day in (("2025_LEG_01", "20250701"), ("2016_LEG_01", "20160605")):
        d = share / leg[:4] / leg
        d.mkdir(parents=True)
        (d / f"ACSD_{day}.csv").write_text("x")
    for leg, day in (("2016_LEG_01", "20160601"), ("2014_LEG_02", "20140801")):   # 2016_LEG_01 is the ship's
        d = tmp_path / "archive" / "acsd" / leg[:4] / leg
        d.mkdir(parents=True)
        (d / f"ACSD_{day}.csv").write_text("x")
    monkeypatch.setattr(legs, "DATA_ROOT", tmp_path / "Data")
    monkeypatch.setattr(legs, "SHARE_ROOT", share)
    monkeypatch.setattr(archive, "ARCHIVE_ROOT", tmp_path / "archive")
    found = {l.id: l for l in legs.discover()}
    assert set(found) == {"2014_LEG_02", "2016_LEG_01", "2025_LEG_01"}
    assert found["2014_LEG_02"].archive and not found["2016_LEG_01"].archive
    assert found["2016_LEG_01"].first_date == "20160605"                     # the ship's files, not the archive's
    assert found["2025_LEG_01"].live and found["2025_LEG_01"].meta()["archive"] is False


def test_import_replaces_a_leg_whole(tmp_path, monkeypatch):
    write_leg(tmp_path)
    monkeypatch.setattr(archive, "ARCHIVE_ROOT", tmp_path)
    stale = tmp_path / "acsd/2016/2016_LEG_01/ACSD_20160101.csv"
    stale.parent.mkdir(parents=True)
    stale.write_text("old")
    assert archive.import_all() == {"2016_LEG_01": 2}
    assert not stale.exists()
    assert sorted(p.name for p in stale.parent.iterdir()) == ["ACSD_20160605.csv", "ACSD_20160606.csv"]
