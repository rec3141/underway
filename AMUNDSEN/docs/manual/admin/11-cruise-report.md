# The cruise report builder

The cruise report builder is a separate page at `/report/`
(`http://underway.local/report/` aboard). A science team picks its
instruments, rosette bottles and logsheets, and downloads its cruise report
as a `.docx` in Amundsen Science's template, with conditions, tables, maps
and figures drawn from the dashboard's stores. How teams use it is in
[The cruise report](../user/cruise-report.html).

The code is `cruisereport/`. It reads the shares (or the mirror) and the
dashboard's stores, and writes only its own state.

## Service

| | |
|---|---|
| Unit | `underway-report.service` (enabled by `install.sh --enable`) |
| Command | `python -m cruisereport serve --bind 127.0.0.1 --port 8044` |
| Address | Caddy routes `/report/` to it on both site blocks; `/report` and `/report.html` redirect there |
| Packages | the `report` extra: `python-docx`, `matplotlib`, `pyproj`, Pillow |
| State | `$UNDERWAY_HOME/report` (`CRUISE_STATE_DIR`): drafts, uploaded logsheets, digitized logbook pages |
| Restart | by the deploy pull when `cruisereport/*.py` changes; its page and scripts are read from disk |

To run it by hand (for example on a laptop):

```sh
cd /data/underway_server/app/AMUNDSEN
python -m cruisereport serve --bind 127.0.0.1 --port 8044
# http://127.0.0.1:8044/report.html
```

The dashboard's header links to it, and the status page counts its views as
`report`.

## Settings

Its paths follow the dashboard's, each with a `CRUISE_*` override
(`cruisereport/config.py`):

| Setting | Default | What |
|---|---|---|
| `CRUISE_DATA_ROOT` | `UNDERWAY_DATA_ROOT`, else `/mnt/ship/Data` | the Data share |
| `CRUISE_SHARE_ROOT` | `UNDERWAY_SHARE_ROOT`, else `/mnt/ship/Share` | the Science share |
| `CRUISE_UNDERWAY_DB` | `UNDERWAY_DB_DIR`, else `/data/underway_server/db` | the dashboard's stores |
| `CRUISE_STATE_DIR` | `~/.local/share/cruise-report/state`; the unit sets `$UNDERWAY_HOME/report` | its own state |
| `CRUISE_TEMPLATE` | `<share>/Guidelines, Templates & Forms/Cruise Report Template_2026.docx` | the Word template |
| `CRUISE_GEO_DIR` | the dashboard's `static/geo/` | basemap for its maps |
| `CRUISE_SHIP_TZ` | the dashboard's `LOCAL_TZ` | ship time for logsheets and figures |
| `OPENROUTER_REPORT_KEY` | the Dashboard AI key | key for digitizing logbook photos |
| `CRUISE_DIGITIZE_MODEL` | `google/gemini-3.8-flash` | the vision model |
| `CRUISE_DIGITIZE_FALLBACK` | `openai/gpt-6-luna-pro` | the model used if the first refuses |
| `CRUISE_DIGITIZE_EFFORT` | unset | the model's reasoning effort |

Ashore, put a copy of the template in the mirror at the same path
([Running ashore](ashore.html)).

Digitizing a logbook page costs about US$0.02–0.07 on OpenRouter. Without a
key, the rest of the builder works and digitizing is off.

## What it reads

| What | Where | Used for |
|---|---|---|
| Event log | `Data/EventLog/<leg>/Eventlog_<leg>.xls` | operations, positions, bridge readings |
| CTD logbook | `Data/Rosette/<leg>/Logs/*_CTD_logbook.csv` | cast number → event label |
| Rosette sheets | `Data/Rosette/<leg>/Logs/RosetteSheet_*.xlsx` | bottles, team draws, observer's weather and ice |
| Parsed casts | `<db>/casts/<leg>/*.json` | profiles, bottle values |
| Underway store | `<db>/<leg>.db` | arrival on station, then depth, met, TSG and sea state |
| Ice camera | `<db>/../ice/ice.sqlite` | ice at arrival |
| Pump stops | `<www>/data/calendar*.json` | flagging surface values |
| Ice charts | `<db>/ice-charts/*.geojson` | CIS concentration where nothing was logged |
| Basemap | the dashboard's `static/geo/*.geojson` | maps |

Conditions are taken at the ship's arrival on station: when its speed last
fell below 1 knot before the visit's first deployment. Values are the
underway record's two-minute mean from arrival; sea state is 4σ of heave
over ten minutes. Bottom depth prefers the multibeam.

## Checks

```sh
python -m unittest tests/test_cruisereport.py         # unit tests
python tools/cruisereport_e2e.py                      # drives the page in headless Chrome (Playwright, the shares)
python tools/cruisereport_bench.py                    # compares transcription models on the same photos
```
