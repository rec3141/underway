"""The CTD logbook: a cast written on more than one row is one cast."""

from dashboard.build import read_stations
from dashboard.casts import read_logbook

HEAD = "cast,label,station,date_UTC,latitude,longitude,depth_m,bottom_m,type_cast,operator,comments\n"


def test_a_repeated_row_without_a_station_keeps_the_station(tmp_path):
    path = tmp_path / "2026_03_CTD_logbook.csv"
    path.write_text(HEAD
                    + "076,AMD2603-213,JSC-1,2026-09-24T09:01:00,76.10996,-82.80076,820,830,TM,AD,\n"
                    + "077,AMD2603-218,PHATE,2026-09-24T16:53:00,76.39776,-82.95048,295,304,Classic,EN,\n"
                    + "078,AMD2603-229,LAS-1,2026-09-25T09:20:00,75.68668,-79.03706,444,452,Classic,AD,\n"
                    + "077,AMD2603-218,,2026-09-24T16:53:00,76.39776,-82.95048,295,304,,EN,late note\n",
                    encoding="utf-8")
    rows = read_stations(path, "2026_LEG_03")
    assert [r["cast"] for r in rows] == ["076", "077", "078"]
    assert rows[1]["station"] == "PHATE" and rows[1]["type"] == "Classic" and rows[1]["comments"] == "late note"
    book = read_logbook(path)
    assert book["077"]["station"] == "PHATE" and book["077"]["type_cast"] == "Classic"
