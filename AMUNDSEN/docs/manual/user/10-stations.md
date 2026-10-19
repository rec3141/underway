# The Stations tab

A **station** is a place where the ship stops to work: to lower the rosette,
take a core, deploy a net. The **Stations** tab lists every station of the
legs you have chosen, with its position, its time and what was done there. It
is the quickest way to find a station on the map, or to pick the casts you
want to look at on the [Casts tab](casts.html).

Click **Stations** in the row of tabs.

## The station list

The tab is one big table, with a few controls above it.

![The Stations tab filtered to "WC-": seven rows, one of them selected, and the map showing the selected cast](img/10-stations-tab.webp)

Above the table:

- **filter stations**: type to keep only the rows that contain your text.
- **clear selection**: deselect every cast and drop the marker on the map.
  When casts are selected it shows how many, for example
  **clear selection (1)**.
- **TSV**: download the rows you see as a table (see
  [Tables and downloads](tables-and-downloads.html)).
- **The summary line**: how many stations are shown, how many have no cast,
  and how many casts are selected for the Casts tab, for example
  **7 stations (1 without a cast) · 1 selected for the Casts tab**.

The rows come from five places, named in the **source** column:

| source | What the row is |
|---|---|
| **CTD logbook** | One CTD cast, from the rosette team's logbook. A station visited three times has three rows. |
| **CTD archive** | One CTD cast of an earlier leg (2002–2024), from Amundsen Science's archive: its station name, time, position and depth. Those legs have no logbook here, so there are no event log stations or bottom depths for them. |
| **event log** | A station where work was logged but no CTD cast was made: a net, a core, an ice station. Its **activities** column lists what was done. |
| **waypoint** | A position someone saved on the map (see [Waypoints, distances and exporting the map](waypoints-and-export.html)). |
| **kmz** and a date | A station from the leg's cruise plan, when the **Plan** layer is on. These have no time. |

The columns are:

| Column | What it shows |
|---|---|
| **station** | The station's name, for example `WC-2` or `CardS-3`. |
| **lat**, **lon** | Its position in decimal degrees (negative longitude is west). |
| **time** | When the cast started, or when work at the station began, in ship time. |
| **leg** | The leg, for example **2026 Leg 3**. |
| **source** | Where the row comes from (see above). |
| **cast** | The cast number of the leg, for example `085`. |
| **label** | The sample label written on the bottles, for example `AMD2603-236`. |
| **type** | The kind of cast or station (for example the rosette used, or the station type in the plan). |
| **activities** | What was done at an event-log station, or what is planned at a planned one. |
| **bottom (m)** | The depth of the sea floor. |
| **cast depth (m)** | How deep the rosette went. |
| **comments** | Notes from the logbook, the event log or the plan. |

The table is wide. Scroll it sideways to see the right-hand columns; a scroll
bar above the table does the same, so you do not have to go to the bottom.

### Sorting

Click a column's name to sort the table by it. Click again to reverse the
order. A small **▲** or **▼** shows which column sorts the table and which
way. The table starts sorted by **time**, newest first.

### Which stations are listed

The table lists the stations of the legs ticked in the header's **Legs**
menu (see [Choosing legs and a time span](legs-and-time.html)). The
**Show last** span does not shorten it: every station of a ticked leg is
listed.

When a leg is not ticked, the first row of the table says how many of
its rows are left out, for example **1,234 rows from legs not shown · show
all legs**. Click **show all legs** to tick every leg.

Waypoints and planned stations have no leg, so they are always listed.

## Searching

The **filter stations** box searches the station name, the time, the leg,
the source, the label, the type, the activities and the comments.

- Type `WC-` to see the WC line of stations.
- Type `Box Core` to see every station where a box core was logged.
- Type `2026-09-10` to see the stations of one day.
- Type `kmz` to see only the planned stations of the cruise plan.

Click **✕** in the box, or empty it, to see everything again.

## Selecting a station on the map

A click on a row does two things: it shows the station on the map, and it
selects its cast for the Casts tab.

1. Type `WC-` in **filter stations**.
2. Click the row **WC-2**, cast **085**.
3. The row is marked **✓** and highlighted. The map moves to the station and
   opens a box with the cast's name and position, its distance from the
   ship, and the depth there.
4. The summary line now says **1 selected for the Casts tab**. Open the
   [Casts tab](casts.html) and the cast is already chosen.
5. Click the row again to deselect the cast.

A row without a cast (an **event log** station, a waypoint, a planned
station) is only shown on the map.

To select every cast in the table at once, tick the box in the first column's
heading. Combine it with the filter: type `CS1-`, tick the box, and all the
casts of the CS1 line are ready for a section on the Casts tab.

!!! tip
    It works the other way too: click a station on the map and its cast is
    selected, and its row here is ticked. See [The map](map.html).

## Positions and distances

The box that opens on the map gives the station's position and how far it is
from the ship, by sea and through the air. Distances by sea are worked out
around the land; this can take a moment ("working it out…"). The box and how
to read it are explained in
[Waypoints, distances and exporting the map](waypoints-and-export.html).

## Planned stations

When the **Plan** layer of the map is on, the stations of the cruise plan
are listed too. Their **source** is **kmz** with the date the plan was
loaded; their **leg** column shows the plan's group (for example
**MAZE & QEI Survey**), and **comments** carries the plan's description.

![The planned stations of the Leg 3 cruise plan, found by typing kmz](img/10-stations-plan.webp)

Turn the **Plan** layer off on the map to leave them out of the table.

## Saved waypoints

Waypoints are positions people have saved on the map, for example a place
to sample on the way back. They are listed with **source** **waypoint**, the
name they were given, and in **comments** who saved them and their note.

!!! note "Aboard only"
    Waypoints are saved on the ship's dashboard and shared with everyone
    aboard. A waypoint row has a **×** in its first column. Click it and
    confirm to remove the waypoint **for everyone**.

## In short

- One row per CTD cast, per station without a cast, per waypoint and per
  planned station.
- **filter stations** searches names, labels, activities and comments;
  click a column's name to sort.
- Click a row to show the station on the map and select its cast for the
  Casts tab; click again to deselect.
- The heading's tick box selects every cast in the table.
- **TSV** downloads what you see.
