"""Pure-function checks that need no shares mounted."""

import pandas as pd

from cruisereport import activities, conditions, logsheets, rosette


def test_activity_spellings_fold_into_groups():
    cases = {
        "TM-Rosette P+A": "tm_rosette", "TM-CTD": "tm_rosette", "CTD (5 bottles only)": "rosette",
        "Classic-Rosette Limited": "rosette", "Box Core - BIO": "box_core",
        "PC or GGC": "gravity_piston", "IKMT ou Beam Trawl": "ikmt_beam",
        "Monster or Hydrobios": "plankton_nets", "Transit + MVP to Basic+ CS2-1": "mvp",
        "Transit to Qaanaq": "transit", "Deploy helicpopter": "helicopter",
        "CTD-Rosette (erreur, à enlever)": "void", "Something new": "other",
    }
    for text, group in cases.items():
        assert activities.group_of(text) == group, text


def test_rosette_ice_terms():
    assert rosette.ice_english("1/10 Eau libre") == "open water (under 1/10)"
    assert rosette.ice_english("4-6/10 Banquise lâche") == "4-6/10 open drift ice"
    assert rosette.ice_english("7") is None          # a bare number: no scale stated
    assert rosette.ice_english("-99") is None


def test_sun_elevation_polar_night_and_noon():
    # Midsummer local noon at 75°N: about 90 - 75 + 23.4 degrees.
    assert abs(conditions.sun_elevation("2026-06-21T17:00:00", 75.0, -75.0) - 38.4) < 1.0
    assert conditions.sun_elevation("2026-12-21T05:00:00", 75.0, -75.0) < -30


def test_range_formatting():
    assert conditions._range([-0.4, -0.2], 0, "°") == "0°"
    assert conditions._range([1.0, 4.2], 0, "°") == "1 to 4°"
    assert conditions._range([-1.25, 0.5], 1, "°C") == "−1.2 to 0.5 °C"


def test_logsheet_roles_from_names_and_values():
    df = pd.DataFrame({"Unique Identifier": ["AMD2603-010", "AMD2603-011"],
                       "Station Name": ["CardS-3", "CardS-3"],
                       "Date & Time (UTC)": ["2026/09/06 13:21:00", "2026/09/06 14:59:00"],
                       "Latitude": [76.3, 76.3], "Longitude": [-89.2, -89.2]})
    roles = logsheets.guess_roles(df)
    assert roles["label"] == "Unique Identifier"
    assert roles["station"] == "Station Name"
    assert roles["datetime"] == "Date & Time (UTC)"
    assert roles["lat"] == "Latitude" and roles["lon"] == "Longitude"


def test_digitize_levels_and_ditto(tmp_path, monkeypatch):
    from cruisereport import digitize

    assert [digitize.level(x) for x in (3, -1, 100, 90, 75, 50, 10)] == [3, -1, 3, 2, 1, 0, -1]
    assert digitize.colour(3) != digitize.colour(-1)
    for mark in ('"', "〃", "↓", "=", "do", "Ditto"):
        assert digitize.DITTO.match(mark), mark
    assert not digitize.DITTO.match("079")

    monkeypatch.setattr(digitize, "STATE_DIR", tmp_path)
    cell = lambda t, c=3: {"t": t, "c": c}  # noqa: E731
    doc = digitize.save("page.jpg", b"jpeg", {"tables": [{"title": "log", "columns": ["ST", "CAST", "BOT"], "rows": [
        [cell("LAS-2"), cell("079"), cell("1")],
        [cell(""), cell("↓"), cell("2")],
        [cell("CONTROL"), cell(""), cell("HOT WATER")],
    ]}], "notes": []})
    cols, rows = digitize.rows_for_logsheet(doc["id"], 0, True)
    assert rows[1] == {"ST": "LAS-2", "CAST": "079", "BOT": "2"}          # continues the record
    assert rows[2]["CAST"] == ""                                          # a row of its own
    digitize.edit(doc["id"], 0, 2, 1, "080")
    assert digitize.load(doc["id"])["tables"][0]["rows"][2][1] == {"t": "080", "c": 3, "edited": True}


def test_digitize_queue_runs_fails_and_resumes(tmp_path, monkeypatch):
    from cruisereport import digitize

    monkeypatch.setattr(digitize, "STATE_DIR", tmp_path)
    monkeypatch.setattr(digitize, "_workers", [])
    monkeypatch.setattr(digitize, "_jobs", digitize.queue.Queue())
    answers = {"good.jpg": {"tables": [{"title": "t", "columns": ["A"], "rows": [[{"t": "1", "c": "likely"}]]}],
                            "notes": [], "model": "m", "usage": {}, "seconds": 1}}

    def fake(jpeg):
        name = jpeg.decode()
        if name not in answers:
            raise RuntimeError("model unavailable")
        return answers[name]
    monkeypatch.setattr(digitize, "transcribe_page", fake)

    # A page left "working" by a restart is taken up again on start.
    stale = digitize.submit("good.jpg", b"good.jpg")
    doc = digitize.load(stale["id"])
    doc["status"] = "working"
    digitize._write(stale["id"], doc)
    digitize._jobs = digitize.queue.Queue()          # the restart's empty in-memory queue

    digitize.start(workers=1)
    good = digitize.submit("good.jpg", b"good.jpg")
    bad = digitize.submit("bad.jpg", b"bad.jpg")
    digitize._jobs.join()
    assert digitize.load(stale["id"])["status"] == "done"
    done = digitize.load(good["id"])
    assert done["status"] == "done" and done["tables"][0]["rows"][0][0] == {"t": "1", "c": 1}
    failed = digitize.load(bad["id"])
    assert failed["status"] == "failed" and "model unavailable" in failed["error"]
