# Tables and downloads

Everything the dashboard draws, you can also take away: the underway record
as hourly or daily tables, the lists of stations, casts and bottles, and any
graph as a picture. This chapter explains the data tables of the Underway
tab, the downloads on every tab, and how to export a graph.

## The data tables

At the bottom of the [Underway tab](underway.html), under the panels, is
**Data tables**: the underway record summarised hour by hour or day by day.
Click **Data tables** to open or close it; the page remembers which.

![The Data tables, hourly means: one row per hour with its time, leg, position and a column per variable](img/12-tables-hourly.webp)

Each row is one hour (or one day). Its columns are:

- **time**: the start of the hour or day, in ship time.
- **leg**: the leg.
- **lat**, **lon**: the ship's position.
- **one column per variable**: **SST (°C)**, **Salinity (PSU)**,
  **Air temperature (°C)**, … and the surprise scores.

An empty cell means there were no measurements of that variable in that hour.

### The controls

![The controls of the Data tables: Hourly and Daily, Statistic, the filter, TSV, and the row count](img/12-tables-daily.webp)

- **Hourly** / **Daily**: one row per hour, or one per day.
- **Statistic**: what each cell holds:
    - **mean**: the average over the hour or day;
    - **min**: the lowest value;
    - **max**: the highest value;
    - **count**: how many measurements went into it.
- **filter rows**: keep the rows whose time or leg contain your text, for
  example `2026-09-10` for one day, or `Leg 2` for one leg.
- **TSV**: download the table (see below).
- **The row count**, for example **91 rows · max**. A long table shows its
  first 2000 rows on the page (**showing first 2000**); the download holds
  them all.

The table holds the legs ticked in the header's **Legs** menu. The
**Show last** span does not shorten it.

### Sorting and finding a row on the map

- Click a column's name to sort by it; click again to reverse. A **▲** or
  **▼** marks the sorting column. The table starts newest first.
- Click a row to show where the ship was at that time on the map.

### Example: the saltiest day of the season

1. Open **Data tables**.
2. Click **Daily**.
3. Set **Statistic** to **max**.
4. Click **Salinity (PSU)** in the heading, twice, so that the highest value
   is at the top.
5. The first row is the day with the saltiest water. Click it to see on the
   map where the ship was.

!!! note
    Wind directions are averaged as compass directions: 359° and 1° average
    to 0°, not to 180°. When the wind blew equally from opposite directions
    there is no average direction, and the cell shows a dash.

## Downloads

Every list on the dashboard has a **TSV** button. It downloads exactly what
the list shows at that moment: the rows left after the filter and the legs
menu, in the order you sorted them.

| Where | Button | File | What it holds |
|---|---|---|---|
| Underway › **Data tables** | **TSV** | `underway_1h_mean.tsv` (or `_1d_`, `_max`, …) | The rows of the table, with the chosen statistic. |
| **Stations** | **TSV** | `stations.tsv` | The station list. |
| **Casts** | **TSV** | `casts.tsv` | The cast list, with a **selected** column (1 for the casts you selected). |
| Casts › **Bottles fired** | **TSV** | `cast-bottles.tsv` | The bottle firings of the selected casts, at full precision. |

**TSV** means *tab-separated values*: a plain text table with a tab between
the columns. Spreadsheet programs open it directly.

To open a TSV file in a spreadsheet:

1. Click **TSV**. Your browser saves the file, usually in your Downloads
   folder.
2. Open your spreadsheet program (Excel, LibreOffice Calc, Google Sheets,
   Numbers).
3. Open the file from there. If the program asks how the columns are
   separated, choose **Tab**.

!!! note
    Times on the page are ship time, but times in the downloaded files are
    UTC (universal time). That way files from different legs and time zones
    line up.

## Exporting a graph as a picture

Every graph has a **⇩** button in its top right corner: the Underway panels,
the cast graphs (**Single**, **Multi** and **Section**) and the Schedule's
**Timeline**. It opens an export window with a copy of the graph.

![The export window for the Salinity panel: size and resolution on the left, the preview on the right](img/12-export-graph.webp)

1. Zoom and move the graph on the page until it shows what you want. The
   export keeps that view.
2. Click **⇩** on the graph. The export window opens, titled with the
   graph's name, for example **Export · Salinity (PSU)**.
3. Set the size of the picture: drag the preview's bottom-right corner, or
   type **Width (px)** and **Height (px)**.
4. For a PNG, set **PNG DPI scale** (1× to 4×): 2× gives a picture twice as
   sharp as on screen, good for a report or a slide. The line under the
   slider gives the final size in pixels.
5. Click one of:
    - **Export PNG**: a picture file, for documents and slides.
    - **Export SVG**: a drawing that stays sharp at any size, for posters or
      for editing in a drawing program.
    - **Copy to clipboard**: a PNG you can paste straight into a document,
      an email or the chat.
6. Close the window with **×**.

Exporting never changes the graph on the page. An exported **Multi** graph
carries its legend, so the casts can be told apart even if you folded the
Legend panel.

!!! note
    **Copy to clipboard** needs a secure connection. On the ship, where the
    dashboard is reached by its plain address, the window says
    **Copying requires HTTPS or localhost. Use Export PNG on this
    connection.** Use **Export PNG** instead.

The map has its own export, with a Google Earth (KMZ) option: see
[Waypoints, distances and exporting the map](waypoints-and-export.html).

## In short

- **Data tables** under the Underway panels: **Hourly** or **Daily**, with
  **mean**, **min**, **max** or **count**; click a column to sort, a row to
  find it on the map.
- **TSV** on every list downloads what you see, with times in UTC.
- **⇩** on any graph exports it as PNG or SVG, or copies it.
