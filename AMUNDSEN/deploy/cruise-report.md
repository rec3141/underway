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
2. **What you did.** Tick the instruments. The event log's operations for
   those instruments are ticked, and any can be unticked. Tick the team's
   column names on the rosette sheets (every spelling). Optionally import the
   team's own logsheet (`.xlsx`/`.csv`). Its rows are matched to operations by
   event label, then station + time or date, then time + position. The match
   method is shown per row, and the column roles can be corrected.
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
5. **Figures.** Station map, CTD profiles, T–S diagram (optionally with the
   team's bottles marked ×), and the underway record: any of the Underway
   tab's panels, in its groups (Surprise, Lab, Met Station, Bridge, Winches,
   Ice camera), with grey lines at the selected operations only. Figures
   redraw when the selection changes.
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
| Underway store | `<underway db>/<leg>.db` | depth, met, TSG around each event |
| TSG pump stops | `<underway www>/data/calendar*.json` | flagging surface values |
| Ice charts | `<underway db>/ice-charts/*.geojson` | CIS concentration where nothing was logged on board |
| Basemap | the dashboard's `static/geo/*.geojson` | map land and bathymetry |

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
