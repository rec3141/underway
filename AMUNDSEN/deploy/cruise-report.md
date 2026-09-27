# Cruise report builder

`http://underway.local/report/` (also `/report/` on the ship's IP addresses)
builds a science team's cruise report from the ship's own records and
downloads it as a `.docx` in Amundsen Science's 2026 template
(`Share/Guidelines, Templates & Forms/Cruise Report Template_2026.docx`).
The code is `cruisereport/`; it reads the shares and the dashboard's stores
and writes only its own state (drafts, uploaded logsheets, digitized logbook
pages) under `$UNDERWAY_HOME/report`. The dashboard's header
links to it (`shell.reportBanner`), and the status page counts its views as
`report`.

## What a participant does

1. **Team.** Pick the leg, and give the team name, title, leaders and
   participants.
2. **What you did.** First bring in the team's own records: photographs of
   paper logbooks (below) and logsheets (`.xlsx`/`.csv`), whose rows are
   matched to operations by event label, cast number, station + time or date,
   time + position, or a station visited once; the match method is shown per
   row. What each column holds (station, date, time, cast…) is chosen in a
   row under its header. Dates and times are read as logbooks write them:
   day and month in either order (whichever falls in the leg), no year, day
   of year, 14:20 / 14h20 / 1420 / 2:20 PM, Excel fractions; a time with no
   date matches within the station's visit. Each table says whether its
   times are UTC or ship time (`CRUISE_SHIP_TZ`, else the dashboard's
   `LOCAL_TZ`). Any match can be removed or replaced by searching the leg's
   operations (station, label, activity, date, comment). An uploaded
   logsheet's cells and headers are editable and rows and columns can be
   added; every change re-matches the rows. The upload
   itself is kept as sent; the corrected sheet exports as TSV or XLSX
   (corrected cells filled green). A logsheet made from a digitized table is
   corrected in the transcription. Then, under "Select what's
   yours", tick logs (each sheet of a workbook is a log of its own), instruments and the team's rosette-sheet columns. The
   operations selected are those the ticked logs match plus every operation
   of the ticked instruments, and they follow the logs as they are corrected
   or re-matched; a participant's own tick or untick in the operations list
   always stands. Below the operations, **Bottles sampled** lists every
   rosette bottle on their casts. A bottle is the team's when a ticked
   rosette-sheet column drew from it or a log ticked there as a bottle source
   (apart from its tick for operations) lists it in its Bottle column; with
   neither, every bottle counts. Each bottle can be ticked or unticked by
   hand, which always stands, and its volume and a note typed in. Bottle
   tables, per-operation bottle counts and the T–S marks use that choice.

   **Digitize logbook photos.** Photograph a paper logbook page (any
   orientation) and a vision model (OpenRouter, `google/gemini-3.8-flash`;
   `openai/gpt-6-luna-pro` if it refuses) turns it into importable tables:
   one row per record, arrows and ditto marks expanded, values copied as
   written. Each cell carries the model's log10 odds that it is right
   (3 near certain … −1 a guess), shown green to red. Cells are editable;
   every correction re-checks which operation each row matches and rewrites
   any logsheet made from the table. Export TSV per table or XLSX (a sheet per
   table, the same colours as fills, Notes and Legend sheets), or use a table
   as a logsheet. The key is `OPENROUTER_REPORT_KEY` (read from the
   environment or `~/.config/underway/underway.env`); a page costs about
   US$0.02–0.07.
3. **Conditions.** A narrative of the conditions at the stations sampled:
   one summary paragraph (default), a paragraph per station visit, or none.
4. **Tables.** One row per operation, per rosette bottle, or per logsheet
   row. Columns can come from any linked table, all joined through the
   event label.
5. **Figures.** Station map (the operations over the leg's whole ship
   track, coloured by date or by any of the underway record's panels), CTD profiles (optionally on the dashboard's
   compressed, square-root depth scale), T–S diagram (coloured by
   pressure, cast time, latitude, station or any profile variable, optionally
   with the team's bottles marked ×), and the underway record: any of the Underway
   tab's panels, in its groups (Surprise, Lab, Met Station, Bridge with the
   bottom depth, Ice camera; the rosette's and cable's winch panels are left out), with grey lines at the selected operations only. Figures
   redraw when the selection changes; the underway record's time axis is UTC or
   ship time.
6. **Text.** The template's sections. The word counter includes the
   generated narrative against the template's 3000-word limit.

The page saves drafts on the server (`drafts/` in the state directory), so a
team can share one. The browser also keeps the current draft locally.

## Sources

| What | Where | Used for |
|---|---|---|
| Event log | `Data/EventLog/<leg>/Eventlog_<leg>.xls` | operations, positions, bridge met/TSG readings |
| CTD logbook | `Data/Rosette/<leg>/Logs/*_CTD_logbook.csv` | cast number → event label |
| Rosette sheets | `Data/Rosette/<leg>/Logs/RosetteSheet_*.xlsx` | bottles, team draws, observer's weather and ice |
| Parsed casts | `<underway db>/casts/<leg>/*.json` | profiles and bottle-file values |
| Underway store | `<underway db>/<leg>.db` | arrival on station (speed), then depth, met, TSG and sea state (4σ heave) there |
| Ice camera | `<underway db>/../ice/ice.sqlite` | ice concentration and types at arrival |
| TSG pump stops | `<underway www>/data/calendar*.json` | flagging surface values |
| Ice charts | `<underway db>/ice-charts/*.geojson` | CIS concentration where nothing was logged on board |
| Basemap | the dashboard's `static/geo/*.geojson` | map land and bathymetry |

Conditions are taken at the ship's arrival on station: when its speed over
ground last fell below 1 kn before the visit's first deployment (the
deployment itself when the ship was still moving then). Numeric values are
the underway record's mean over the two minutes from arrival, directions a
circular mean; the bridge's event-log reading and the rosette observer's are
used only where the record has none. Sea state is 4σ of heave over the ten
minutes around arrival, since two minutes of 10 s heave is too few samples.
Every operation of a visit shares its arrival conditions.

Each conditions value records its source. Bottom depth prefers the
multibeam; the EK60, and the event log that copies it, pick a second bottom
in places. The rosette sheets' cloud-cover and sea-state codes are kept as
typed and are not interpreted, because the sheet's own lookup does not match
the values entered.

## Running

```sh
cd AMUNDSEN
pip install '.[report]'                 # python-docx, matplotlib, pyproj, Pillow
python -m cruisereport serve --bind 127.0.0.1 --port 8044
# http://127.0.0.1:8044/report.html
```

Paths follow the dashboard's variables (`UNDERWAY_DATA_ROOT`,
`UNDERWAY_SHARE_ROOT`, `UNDERWAY_DB_DIR`), each overridable with a
`CRUISE_*` variable (`cruisereport/config.py`). Logbook digitization needs
`OPENROUTER_REPORT_KEY` in the environment or in
`~/.config/underway/underway.env`.

## On the ship

`underway-report.service` runs it from the deploy checkout on
`127.0.0.1:8044` with the dashboard's interpreter (`UNDERWAY_PYTHON`, which
needs `pip install -e 'app/AMUNDSEN[report]'`); `deploy/install.sh` installs
it with the other units and creates its state directory,
`$UNDERWAY_HOME/report`. `tools/deploy-pull.sh` restarts it when
`cruisereport/*.py` changes; its page and scripts are read from disk.
`deploy/Caddyfile` routes `/report/` to it on both site blocks, and
`/report` and `/report.html` redirect there.

## Checks

- `tests/test_cruisereport.py`: activity groups, ice terms, solar elevation,
  number formatting, logsheet roles, digitized-table carry-down and edits.
- `tools/cruisereport_e2e.py`: drives the page in headless Chrome against a
  running server (needs Playwright and the shares).
- `tools/cruisereport_bench.py`: compares transcription models on the same
  logbook photos.
