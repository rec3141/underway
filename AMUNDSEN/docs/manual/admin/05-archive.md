# Amundsen Science's archive

The years before the ship's own record, 2002 to 2024, come from Amundsen
Science's published data: their ERDDAP server
(<https://erddap.amundsenscience.com/erddap>; catalogue at
<https://catalogue.amundsenscience.com>). The build treats each archived leg
like any other leg, marked `archive`.

## Where it lives

| Installation | Path |
|---|---|
| Workstation | `$UNDERWAY_HOME/archive` (`/data/underway_server/archive`); `UNDERWAY_ARCHIVE_DIR` overrides |
| Container | `data/archive`, from the release seed |
| grid | `/data/amundsen-archive`, where the fetch script keeps it; grid's installation links `archive` to it |

Inside:

| Path | What |
|---|---|
| `by-leg/<year>/<YYYY_LEG_NN>/nav.csv` | navigation (position, speed), per-minute means of the 1 Hz record; only where the `nav` fetch has run |
| `by-leg/…/tsg.csv` | TSG, per-minute means |
| `by-leg/…/avos.csv`, `ats.csv` | AVOS weather (2004–2022), ATS weather (2023–2024) |
| `by-leg/…/ctd_1dbar.csv` | rosette CTD in 1 dbar bins |
| `acsd/<year>/<YYYY_LEG_NN>/ACSD_YYYYMMDD.csv` | the same underway record as the ship's own daily files, written by `archive-import` |

## How the dashboard uses it

- The build takes the `acsd/` legs like any other. On the page they are
  hidden in the leg menu until someone ticks them, and grouped by year in
  the **Sources** table.
- Their casts come from `ctd_1dbar.csv`.
- A leg's track comes from one file: `nav.csv`, else the TSG, else the
  weather station (AVOS), else ATS, unless that file covers under 80% of
  the hours the best-covering file does, which is then used. The other
  files, and the casts' places, fill only gaps of more than 30 minutes in
  it. A lone fix out and back faster than 40 kn is dropped first. Mixing
  two receivers minute by minute, or keeping such a fix, inflates the
  distance run.
- The ship speed comes from `nav.csv` where the leg has one, else the TSG.
- The time-span slider's **all** reaches back to them.
- A leg the ship has itself is never replaced by the archive's.

What is not there:

- True wind speed: the dashboard's wind speed is the relative one, so the
  archive's true wind speed is left out rather than mislabelled.

The column mapping is in `dashboard/archive.py`.

## Refreshing it

The fetch runs on grid, where the network is fast; the ERDDAP server is slow
and stops answering under parallel load, so requests go one at a time.

```sh
tools/fetch-amundsen-archive.py fetch [--until 2025-01-01] [--only tsg,avos,ats,ctd,nav] [--root /data/amundsen-archive]
tools/fetch-amundsen-archive.py split      # raw/ -> by-leg/<year>/<leg>/<kind>.csv
tools/fetch-amundsen-archive.py status
```

A rerun skips what is already fetched; a month the server has nothing for
leaves an empty `.none` marker. The log is `fetch.log` under the root.

After fetching, write the ACSD files the build reads:

```sh
cd /data/underway_server/app/AMUNDSEN
/opt/miniforge3/bin/python3 -m dashboard archive-import
```

It rewrites each leg whole and prints how many legs it wrote. Run it on
every installation that has a copy (the ship and grid), then let the next
build ingest the legs. Grid rebuilds the public web copy from the same legs,
so it needs the archive too.

To copy the archive to the ship, copy `by-leg/` and `acsd/` into
`$UNDERWAY_HOME/archive`, or run `archive-import` there after copying
`by-leg/`.

## Licence

The archive is CC BY 4.0: cite Amundsen Science / ArcticNet and each
dataset's DOI. The page credits them in the footer, the **Sources** notes and
the map attribution.
