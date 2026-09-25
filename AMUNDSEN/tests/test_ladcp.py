import json
import math
import os
from pathlib import Path

import pytest

from dashboard import casts, ladcp
from dashboard.legs import Leg


PROFILE = """% Cruise_Number: 2026_03
% Cast_Number: CTD_2026_03_001.cnv
% Start_Date_Time [UTC]: 06-SEP-2026 13:22:04
% Initial_Latitude [deg]: 76.3758
% Initial_Longitude [deg]: -89.2764
% Sounding [m]: 181
% Min_Depth [m]: 7.7
% Max_Depth [m]: 23.1
% EWCT: East (true) Component of Current [m/s]
% NSCT: North (true) Component of Current [m/s]
DEPH EWCT NSCT ERRV
---- ---- ---- ----
7.7 0.3 -0.4 0.2
15.4 nan 0.1 inf
23.1 -0.1 0.2 0.3
"""


@pytest.fixture
def fixture(tmp_path):
    leg = Leg("2026_LEG_03", 2026, 3)
    data = tmp_path / "data"
    path = data / "Rosette" / leg.id / "Ladcp" / "stn001.lad"
    path.parent.mkdir(parents=True)
    path.write_text(PROFILE)
    return leg, data, path, tmp_path / "db"


def test_native_depth_true_components_and_nulls(fixture):
    leg, _, path, _ = fixture
    cast = ladcp.parse_ladcp(path, leg, {"001": {"station": "A", "label": "Shelf"}})
    assert cast.id == "2026_LEG_03:LADCP001"
    assert cast.parent_cast_id == "2026_LEG_03:CTD_001"
    assert cast.station == "A" and cast.label == "Shelf"
    assert cast.time == "2026-09-06T13:22:04Z"
    assert cast.p == [] and cast.depth == [7.7, 15.4, 23.1]
    assert cast.meta()["max_depth"] == 23.1 and cast.meta()["max_p"] is None
    assert cast.vars["Eastward current"] == [0.3, None, -0.1]
    assert cast.vars["Northward current"] == [-0.4, 0.1, 0.2]
    assert cast.vars["Current error"] == [0.2, None, 0.3]
    assert cast.vars["Current speed"] == [0.5, None, math.hypot(-0.1, 0.2)]
    assert set(cast.units.values()) == {"m/s"}
    assert cast.source["header"]["EWCT"].startswith("East (true)")
    json.dumps(cast.payload(), allow_nan=False)


@pytest.mark.parametrize("old,new", [
    ("CTD_2026_03_001", "CTD_2026_03_002"),
    ("Cruise_Number: 2026_03", "Cruise_Number: 2026_02"),
    ("CTD_2026_03_001", "CTD_2025_03_001"),
    ("Initial_Latitude [deg]: 76.3758", "Initial_Latitude [deg]: nan"),
    ("Max_Depth [m]: 23.1", "Max_Depth [m]: 24.1"),
    ("15.4 nan 0.1 inf", "7.7 nan 0.1 inf"),
    ("15.4 nan 0.1 inf", "15.4 0.1"),
    ("23.1 -0.1 0.2 0.3", "nan -0.1 0.2 0.3"),
])
def test_metadata_and_grid_validation(fixture, old, new):
    leg, _, path, _ = fixture
    path.write_text(PROFILE.replace(old, new))
    with pytest.raises(ValueError):
        ladcp.parse_ladcp(path, leg)


def test_cache_revalidates_corrected_old_files_and_logbook(fixture, monkeypatch):
    leg, data, path, db = fixture
    leg.stations = data / "logbook.csv"
    leg.stations.write_text("cast,station,label\n001,A,First\n")
    original = ladcp.ladcp_casts(leg, data, db)[0]
    parser = ladcp.parse_ladcp
    monkeypatch.setattr(ladcp, "parse_ladcp", lambda *a: pytest.fail("unchanged file must be cached"))
    assert ladcp.ladcp_casts(leg, data, db)[0].vars == original.vars
    monkeypatch.setattr(ladcp, "parse_ladcp", parser)
    stat = path.stat()
    path.write_text(PROFILE.replace("0.3 -0.4", "0.6 -0.4"))
    os.utime(path, ns=(stat.st_atime_ns, stat.st_mtime_ns + 1))
    assert ladcp.ladcp_casts(leg, data, db)[0].vars["Eastward current"][0] == 0.6
    leg.stations.write_text("cast,station,label\n001,B,Second\n")
    assert ladcp.ladcp_casts(leg, data, db)[0].station == "B"
    path.write_text("bad replacement")
    assert ladcp.ladcp_casts(leg, data, db) == []


def test_bad_file_isolated_and_build_publishes_map_bundle(fixture, tmp_path, monkeypatch):
    leg, data, path, db = fixture
    path.with_name("stn002.lad").write_text("malformed")
    monkeypatch.setattr(casts, "DATA_ROOT", data)
    monkeypatch.setattr(casts, "DB_DIR", db)
    ctd = casts.Cast(f"{leg.id}:CTD_001", leg.id, "CTD", "001", None, 76, -89,
                     p=[1, 2], vars={"Temperature": [1, 2]})
    monkeypatch.setattr(casts, "rosette_casts", lambda _: [ctd])
    monkeypatch.setattr(casts, "mvp_casts", lambda _: [])
    out = tmp_path / "www"
    index = casts.build_casts([leg], out)
    assert len(index["casts"]) == 2
    currents = json.loads((out / index["ladcp_file"]).read_text())["casts"]
    assert len(currents) == 1
    assert currents[0]["depth"] == [7.7, 15.4, 23.1]
    assert json.loads((out / currents[0]["file"]).read_text()) == currents[0]
    ctd_meta = next(c for c in index["casts"] if c["kind"] == "CTD")
    assert "max_depth" not in ctd_meta
    assert "depth" not in json.loads((out / ctd_meta["file"]).read_text())
    assert json.loads((out / "data/casts/index.json").read_text()) == index


def test_unreadable_directory_does_not_stop_build(fixture, monkeypatch):
    leg, data, _, db = fixture
    def unavailable(*args):
        raise PermissionError("share unavailable")
    monkeypatch.setattr(Path, "glob", unavailable)
    assert ladcp.ladcp_casts(leg, data, db) == []


def test_unreadable_logbook_keeps_header_metadata(fixture, monkeypatch):
    leg, data, _, db = fixture
    def unavailable(*args):
        raise PermissionError("logbook unavailable")
    monkeypatch.setattr(ladcp, "read_logbook", unavailable)
    profile = ladcp.ladcp_casts(leg, data, db)[0]
    assert profile.station == ""
    assert profile.lat == 76.3758
