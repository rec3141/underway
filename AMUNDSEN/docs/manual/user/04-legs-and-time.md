# Choosing legs and a time span

Two controls in the header decide which part of the ship's record you see:

- **Legs** chooses *which* parts of the record: this cruise, an earlier one,
  or several at once.
- **Show last** chooses *how far back* to look: the last hour, the last day,
  the last month, or everything.

They work on every tab at once: the map, the graphs, the casts, the stations,
the tables. Read this chapter before the chapters about the tabs.

![The two controls: Legs, and the Show last slider](img/04-span.webp)

## What a leg is

The *Amundsen* sails every year from early summer to autumn. That is a
*season*. A season is split into *legs*. A leg is one part of the season,
usually three to five weeks long, between two changes of the science teams.
Each leg has its own route and its own programme.

The dashboard names legs by year and number: **2026 Leg 3** is the third leg
of the 2026 season. In 2026 there were five legs:

| Leg | Dates (2026) | Where |
|---|---|---|
| **2026 Leg 0** | 1 to 10 July | the St. Lawrence |
| **2026 Leg 1** | 10 July to 5 August | from Québec City to Labrador and Baffin Island |
| **2026 Leg 2** | 5 August to 3 September | Baffin Bay and the coast of Greenland |
| **2026 Leg 3** | 3 to 30 September | the islands of the High Arctic, Jones Sound and Nansen Sound |
| **2026 Leg 4** | 30 September to 1 October | Cambridge Bay |

The dashboard also holds the earlier seasons. Those from 2002 to 2024 come
from the *archive*: the data that Amundsen Science, the organisation that
runs the ship's science, has published for everyone. The archive's legs are
marked **archive** in the menu.

!!! note
    The archive's legs carry the measurements that Amundsen Science
    published, which can be fewer than the ship records today. Some graphs
    may be empty for an archive leg.

## The Legs menu

The **Legs** button shows how many legs are shown out of how many there are.
**Legs · 5/101** means that 5 of the 101 legs are shown.

Click **Legs** to open the menu.

![The Legs menu, with the five legs of 2026 ticked](img/04-legs-menu.webp)

From top to bottom, the menu has:

**Legs to show · all · none**
:   Click **all** to show every leg, or **none** to hide them all.

**A row for each year**, newest first
:   - The small arrow, **▸**, opens the year to list its legs. When the year
      is open the arrow points down, **▾**; click it again to close the year.
    - The box ticks or unticks every leg of that year at once. When only some
      of its legs are shown, the box shows a dash instead of a tick.
    - **live** marks the year of the leg that is under way (aboard only).
    - **archive** marks a year that comes from the archive.
    - On the right, for example **5/5**, is how many of the year's legs are
      shown, out of how many it has.

**A row for each leg**, inside an open year
:   - The box shows or hides that leg.
    - The leg's name, for example **2026 Leg 3**.
    - **live** or **archive**, as for the years.
    - The leg's first and last days, as month/day: **09/03 – 09/30**.
    - The number of days of data the leg has, for example **28 d**.

**The line at the bottom**
:   How many legs are shown, and how much of the record the dashboard has
    loaded to cover them: for example "5 of 101 legs shown · the 4 mo window
    loaded".

Here is the menu with two years open. Only one leg of each year is ticked,
so the years' boxes show a dash.

![The Legs menu with 2026 Leg 3 and 2025 Leg 4 ticked, and the 2026 and 2025 years showing a dash](img/04-two-years.webp)

Each change takes effect at once: the map, the graphs and the tables redraw
while the menu stays open. Click anywhere outside the menu to close it.

The dashboard remembers which legs you show, and which years you left open.

### Opening an archive year

The archive years start closed. To look at an archive leg:

1. Click **Legs**.
2. Click the arrow **▸** beside the year, for example **2024**. The year
   opens and lists its legs, each marked **archive**.

    ![The year 2024 opened in the Legs menu, showing its six archive legs](img/04-legs-archive.webp)

3. Tick the leg you want, for example **2024 Leg 3**.
4. Set **Show last** to **all** (see below). The leg appears on the map.

!!! note
    When a year or a leg is added to the dashboard, it starts hidden, so
    that nothing new switches itself on. Tick it in the menu to show it.

!!! tip
    **all** shows all the legs of every year. That is a great deal of data:
    the page may take a while to load it, especially on a phone or over a
    slow link. Show only the legs you need.

## The Show last slider

**Show last** sets how far back from the end of the record the dashboard
looks. Its value is written beside it, in blue: **4 mo** means "the last four
months".

The slider has fifteen steps, from short to long:

| Step | Means |
|---|---|
| **1 h**, **3 h**, **6 h**, **12 h** | the last 1, 3, 6 or 12 hours |
| **24 h**, **48 h**, **72 h** | the last one, two or three days |
| **1 wk** | the last week |
| **1 mo**, **2 mo**, **4 mo** | the last one, two or four months |
| **1 yr**, **2 yr**, **4 yr** | the last one, two or four years |
| **all** | everything, back to the first leg |

To change it:

1. Drag the slider's round handle left (shorter) or right (longer). The
   value beside **Show last** changes as you drag.
2. Let go. The dashboard loads that span and redraws the map and the graphs.

On a phone, the slider is a drop-down list instead: tap it and choose a span.

### From the end of the record

The span always counts back from the *end of the record*: the newest
observation the dashboard has, from any leg. Aboard, that is a few minutes
ago. Ashore, it is the last day of the cruise.

So **24 h** means the last 24 hours of the record, not the last 24 hours of
the leg you ticked. This matters when you look at an earlier leg. See the
example [Why is the map empty?](#why-is-the-map-empty) below.

### What the span changes, and what it does not

The span changes:

- **the map**: it draws the ship's track only over the span. The camera
  icons and the event log entries on the map follow the span too.
- **the graphs on the Underway tab**: they open on the span. You can still
  zoom out of a graph, or drag it sideways, to see more of what is loaded.
- **the colours**: the colour scale of the track is stretched over the
  values in the span, so the colours always use their full range. (The
  scale leaves out the most extreme 5% of values at each end, so that one
  odd reading does not wash out the rest.)

The span does not change:

- **the station markers on the map**: they follow the legs only, so you see
  every station of the shown legs, whatever the span.
- **the tables** on the Underway, Stations and Casts tabs: they list
  everything in the shown legs.

## How the two work together

Think of the legs as choosing the pages of a logbook, and the span as how far
back you read from the last page.

- To see an earlier leg in full, tick it and set **Show last** to **all**.
- To see what happened recently, keep the current legs and choose a short
  span such as **24 h** or **72 h**.

The line at the bottom of the **Legs** menu says how much of the record the
dashboard has loaded: always enough to reach back to the first day of the
earliest leg you show.

## Examples

### Follow the ship today

!!! note "Aboard only"
    This example needs live data.

1. Click **Legs**, and click **none**.
2. Open the current year with its arrow, and tick the leg marked **live**.
3. Click outside the menu to close it.
4. Drag **Show last** to **24 h**.

The map shows the last day's track, ending at the ship. The graphs on the
**Underway** tab show the last 24 hours.

### Look at a whole earlier leg

Leg 2 of 2025 began in Lake Melville, in Labrador, and went north along the
Labrador coast to Frobisher Bay and across Davis Strait to Greenland. To see
all of it:

1. Click **Legs**, and click **none**.
2. Click the arrow beside **2025**, and tick **2025 Leg 2**.
3. Click outside the menu.
4. Drag **Show last** all the way to the right, to **all**.

The **Legs** button now reads **Legs · 1/101**, and the map shows the leg's
whole track.

![2025 Leg 2 alone, with Show last set to all](img/04-example-one-leg.webp)

### Compare two years in the same waters

In both 2025 and 2026 the ship worked among the islands of the High Arctic.
To see both tracks together:

1. Click **Legs**, and click **none**.
2. Open **2026** and tick **2026 Leg 3**.
3. Open **2025** and tick **2025 Leg 4**.
4. Set **Show last** to **all**.

Both tracks appear on the map, coloured on the same scale. Choose a colour
such as **SST (°C)** (the sea-surface temperature) to compare the two
autumns. On the **Underway** tab, the graphs show the two legs one after the
other, with a gap between them.

### Why is the map empty?

Suppose you tick only **2025 Leg 2**, and leave **Show last** at **4 mo**. The
map shows no track at all, and the graphs are empty.

![2025 Leg 2 with Show last at 4 mo: the map shows no track](img/04-empty-span.webp)

The reason: the span counts back four months from the end of the record,
which is in 2026. 2025 Leg 2 ended more than a year earlier, so none of it
falls inside the span. Move **Show last** to **2 y** or **all** and the track
appears.

The same happens if you click **none** in the **Legs** menu: no leg is shown,
so there is nothing to draw.

## Sharing a view with a link

You can send someone a link that opens the dashboard on the same legs and
span as you. Add these to the end of the dashboard's address, after a `?`,
joined with `&`:

| Add | Does |
|---|---|
| `legs=2025_LEG_02` | shows that leg only. Write legs as year, `_LEG_`, and the two-digit number. Separate several legs with commas: `legs=2025_LEG_04,2026_LEG_03`. Use `legs=all` for every leg. |
| `win=all` | sets **Show last**. Use any step from the table above, written short and without spaces: `24h`, `1w`, `4mo`, `2y`, `all`. |
| `tab=casts` | opens that tab: `underway`, `casts`, `stations`, `calendar` (the Schedule tab), `wiki`. |

For example, on the ship's Wi-Fi:

`http://underway.local/?legs=2025_LEG_02&win=all&tab=casts`

opens the Casts tab on 2025 Leg 2, over the whole leg. A link like this
replaces the legs and span that the other person's browser remembered.

## In short

- **Legs** chooses which legs to show; **Show last** chooses how far back
  from the end of the record to look.
- Both apply to every tab.
- In the **Legs** menu, the arrow opens a year, and the box beside a year
  shows or hides all its legs.
- To see an earlier leg, tick it and set **Show last** to **all**.
- If the map is empty, the span probably does not reach back to the legs you
  ticked.
