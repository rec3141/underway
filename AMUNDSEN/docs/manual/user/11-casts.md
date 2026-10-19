# The Casts tab

A **cast** is one trip of an instrument from the surface down through the
water and back. The main one is the **CTD**, an instrument that measures
Conductivity (from which salinity is worked out), Temperature and Depth on
its way down, with other sensors beside it (oxygen, fluorescence, light,
…). On the *Amundsen* it hangs in a frame of sampling bottles called the
**rosette**. A cast gives a **profile**: how each variable changes with
depth at one place and time.

The **Casts** tab lists every cast of the legs you have chosen and draws
them three ways: one cast alone (**Single**), several casts on top of each
other (**Multi**), or several casts side by side as a cross-section of the
sea (**Section**).

Click **Casts** in the row of tabs.

![The Casts tab: the cast type buttons and the cast list at the top, then Bottles fired, then the graph controls and the graphs](img/11-casts-tab.webp)

The tab has two parts:

- **The cast list**, with **Select Cast Type** and the filter above it, and
  **Bottles fired** below it.
- **The graphs**, with **Select Graph** and the buttons beside it above
  them.

## Kinds of cast

The **Select Cast Type** buttons show one kind of cast at a time, or
**All**:

| Button | What it lists |
|---|---|
| **All** | Every kind. |
| **Live** | The cast in the water right now (aboard only, see below). |
| **Rosette** | The main rosette's CTD casts. In the list they are labelled **ROS**. |
| **TM** | The trace-metal rosette: a separate, metal-free rosette for sampling trace metals such as iron. |
| **MVP** | Moving Vessel Profiler tows: a small profiler dropped and pulled up again and again from behind the moving ship. One tow holds many **dips** (profiles). |
| **LADCP** | Lowered ADCP profiles: water currents measured by an instrument on the rosette, at the same time as a CTD cast. |
| **TRS** | Saved transects: groups of casts saved as a section (see [Saving a transect](#saving-a-transect)). |

## The cast list

Each row is one cast, newest first.

| Column | What it shows |
|---|---|
| (tick box) | Ticks every cast in the list at once. |
| **type** | **ROS**, **TM**, **MVP**, **LADCP** or **TRS**. |
| **cast** | The cast number of the leg, for example `090`. A **↗** after it opens the rosette sheet: the team's log sheet of that cast, with what was sampled in each bottle. |
| **station** | The station, for example `BS-2`. |
| **label** | The sample label, for example `AMD2603-251`. |
| **time** | When the cast started, in ship time. |
| **max depth (m)** | How deep the instrument went. |
| **bottles** | How many bottles were fired (closed to take water). |
| **leg** | The leg. |

Click a column's name to sort by it; click again to reverse.

Above the list:

- **filter (station, label, date)**: type to keep only the casts whose
  number, station, label, time or leg contain your text.
- **clear selection**: deselect every cast (and empty the filter). It says
  how many are selected, for example **clear selection (2)**.
- **TSV**: download the list you see (see
  [Tables and downloads](tables-and-downloads.html)).

The list shows the casts of the legs ticked in the header's **Legs** menu
(see [Choosing legs and a time span](legs-and-time.html)).
When a leg is not ticked, the first row of the list says how many of
its rows are left out, for example **1,234 rows from legs not shown · show
all legs**. Click **show all legs** to tick every leg.

### Choosing casts

There are four ways to choose casts:

- **Click a row** in the list. It turns highlighted. Click it again to
  deselect it.
- **Tick the box in the heading** to select every cast in the list. Use the
  filter first to narrow the list.
- **Click a station on the map.** Its cast is selected, and the Casts tab
  opens if you are not on it (or on the Stations tab). Click it again to
  deselect it. See [The map](map.html).
- **On the Stations tab**, click a row (see [The Stations tab](stations.html)),
  or in the Schedule's **Event Log**, click **view data** on a logged CTD.

The line beside **Select Graph** counts what you have chosen, for example
**2 selected · 2 profiles**.

## Single: one cast

**Single** draws one cast with several variables on the same depth axis,
each with its own scale.

![Single: cast 090 at BS-2 with Temperature, Salinity and Oxygen switched on, and the bottles marked down the right edge](img/11-casts-single.webp)

- **Depth** runs down the left side, from the surface at the top.
- **Each variable has its own axis and colour.** Here Temperature (blue) has
  its axis at the top; Salinity (orange) and Oxygen (green) have theirs at
  the bottom.
- **The column of buttons on the left** lists every variable of the cast.
  Click one to switch it **On** or **Off**.
- **The Chart button** in that column divides the axes: variables above
  **Chart** get an axis at the top of the graph; those below it, at the
  bottom. Use **▲** and **▼**, or drag a button, to move a variable or the
  **Chart** divider up and down.
- **Rosette sheet ↗** opens the cast's log sheet.
- The title line gives the cast, the station, the time and the depth of the
  sea floor (**bottom 315 m**).

When several casts are selected, a row of buttons after **Select Cast:**
chooses which one Single shows. For an MVP tow, a row after **dip:** chooses
the dip.

In the example, at station BS-2 on 28 September 2026, the temperature
swings between about −1.4 and −1 °C in the top 150 m, with a warmer layer
near 30 m, while salinity rises steadily with depth and oxygen falls.

## Multi: comparing casts

**Multi** draws one graph per variable, with every selected cast on it in its
own colour. It is the way to compare casts.

### Example: two casts at the same station

At the Makkovik station, on the Labrador shelf, the rosette went down
several times in July 2026. To compare two of them:

1. In **Legs**, tick **2026 Leg 1**.
2. Type `Makkovik` in **filter (station, label, date)**.
3. Click cast **004** (15 July) and cast **008** (18 July).
4. Click **Multi**.

![Multi: casts 004 and 008 at Makkovik, three days apart, on the Temperature graph, with the Legend above](img/11-casts-multi.webp)

The **Legend** names each cast with its colour and time. Each graph shows one
variable against depth. Here the warm layer below 250 m (about 4 °C) is the
same in both casts, but in cast 008 the change from cold to warm water sits
some 15 m deeper. Three days apart, the boundary between the two water
masses has moved.

On a Multi graph:

- **Hold the mouse over a line** to read the cast, the value and the depth.
- **Drag** up and down to move along depth; **scroll** to zoom. All the
  graphs share the depth axis and follow each other.
- **Click a graph's title** to select it (coloured frame). The selected
  graph also zooms with **Shift + scroll** (values only) and
  **Ctrl + scroll** (depth only).
- **⟲** resets the zoom.
- **⇅** squeezes the depth axis (a square-root scale), so the busy top few
  hundred metres get more room. It applies to every cast graph.
- **⇩** exports the graph as a picture, with the legend (see
  [Tables and downloads](tables-and-downloads.html#exporting-a-graph-as-a-picture)).
- **—** folds the graph into the row of buttons above the Legend, for
  example **CDOM ▲**. Click that button to bring the graph back.
- **⤢** makes the graph wide.
- **Drag a graph by its handle (⋮⋮)** onto another to swap them.

The tab remembers which graphs you folded and their order.

!!! tip
    An MVP tow selected as a whole draws all its dips, each a little darker
    than the one before, so you can follow the water along the tow.

## Section: a slice through the sea

**Section** puts the selected casts side by side and fills in between them,
so you see one variable as a slice through the sea: distance along the
bottom, depth down the side, the value in colour.

### Example: across Davis Strait

In Leg 2, on 8 to 10 August 2026, the ship ran a line of stations, CS1-1 to
CS1-9, across Davis Strait from Baffin Island to Greenland.

1. In **Legs**, tick **2026 Leg 2**.
2. Type `CS1-` in the filter.
3. Click one cast at each station, from **CS1-1** to **CS1-9**. (Some
   stations have two casts; one of each is enough.)
4. Click **Section**.
5. Choose **Temperature** in **Colour by**, and click **X-axis** until it
   says **Distance**.

![A temperature section across Davis Strait: eight casts over 314 km, cold water near the surface on the west side, warm water at depth in the middle and on the east side](img/11-casts-section.webp)

What you see:

- **The coloured area** is temperature, with its scale on the right. The
  colours are filled in between the casts.
- **The numbered triangles** along the top mark the casts. The list on the
  left gives each number its cast, station and distance.
- **The grey area** at the bottom is the sea floor, from the depth sounded at
  each cast (or the deepest sample where there is no sounding).
- The title line gives the number of profiles, the length of the section and
  the unit, for example **8 profiles · 314 km · °C**.

In this section, cold water (below 0 °C, dark) fills the top 100 to 200 m on
the Baffin side, and warmer water (4 to 6 °C) lies below it and rises
towards the Greenland side.

The Section's own controls, in the second row:

- **Colour by**: the variable to draw. The list offers what the selected
  casts measured.
- **X-axis**: click to step through
    - **Time**: each cast at its own time;
    - **Distance**: the distance from cast to cast, in the order they were
      made;
    - **A–Z**: in the order of the station names;
    - **Custom**: your own order (see below).
- **Save transect**: save this section for everyone (see below).

To put the casts in your own order, drag the cards in the list on the left,
or use their **▲** and **▼**. The **X-axis** switches to **Custom**. Click
**↺ time order** under the list to go back.

!!! note
    A section needs at least two casts that measured the chosen variable.
    With fewer, the tab says so.

While **Section** is chosen, the map draws a line joining the selected casts.

## Bottles and smoothing

Two buttons beside **Select Graph** change every graph:

- **Bottles**: marks where the rosette's bottles were fired. On **Single**
  they are circles down the right edge; on **Multi** they are dots on each
  line; on **Section** they are dots at their depth. Hold the mouse over one
  to read the bottle number, time and depth.
- **Smooth**: smooths the jittery sensors (fluorescence, CDOM, transmission,
  nitrates) with a running average over 7 to 21 m, depending on how deep the
  cast goes. The dashboard smooths only what is jittery in the data, so
  temperature, salinity and oxygen are normally left as they are. Switch **Smooth** off to see the raw readings. When a section is
  smoothed, its title line says so, for example **smoothed over 11 m**.

### Bottles fired

Click **Bottles fired**, under the cast list, to open a table of every bottle
fired in the selected casts, with what was measured when it closed.

![Bottles fired: the 24 bottles of cast 090 at BS-2, with their depth and the CTD readings at that moment](img/11-casts-bottles.webp)

- One row per bottle: the leg, cast, station, position, bottle number,
  pressure (dbar), depth (m), time (ship time), and then every variable the CTD measured
  at that moment (temperature, salinity, oxygen, …).
- **filter bottles**: keep the rows that contain your text.
- Click a column's name to sort by it.
- **TSV**: download the rows you see, at full precision.

The line beside the filter says how many bottles are shown, for example
**24 of 24 bottle firings shown**. Select casts first; the table loads when
you open it.

## MVP tows

An MVP tow is one row in the list, with the number of dips in its station
column, for example **2026_03_004 98 dips**.

![An MVP tow opened to show its dips, each with its position](img/11-casts-mvp.webp)

- Click the tow's row to select the whole tow: every dip is drawn. Use this
  for a **Section** along the tow.
- Click **▸** at the start of the row to open the list of dips, each with its
  position. Click a dip to select only that dip; click more to add them.
- On the map, each tow is drawn as a line, with a dot at its start. Click
  the dot to select the tow.

## LADCP: currents

An **LADCP** cast gives the speed and direction of the water at each depth:
**Eastward current** and **Northward current** (in m/s; positive is toward
the east or north), **Current speed**, and **Current error** (the
instrument's own estimate of its uncertainty).

![Single view of the LADCP profile at BS-2: eastward and northward current and current speed against depth](img/11-casts-ladcp.webp)

Choose **LADCP**, select a profile, and use **Single**, **Multi** or
**Section** as for any cast. Each LADCP profile goes with the CTD cast of the
same number at the same station. On a section of eastward or northward
current, red and blue show the two directions.

The map's **ADCP** layer draws the currents as arrows at a depth you choose;
see [The map](map.html).

## The live cast

!!! note "Aboard only"
    **Live** works only on the ship, while the CTD's acquisition computer
    sends its data to the dashboard.

Click **Live** to follow the cast in the water as it happens. The graph
updates every two seconds.

- The status line shows the time, how long ago data last arrived, and the
  state of the feed: **LIVE** while data flow, **CONNECTED** or
  **PORT OPEN** when connected but nothing arrives, **OFFLINE** when the
  source does not answer.
- It also shows whether the package is going down (**↓ descending**), coming
  up (**↑ ascending**) or **holding**, and its deepest point so far.
- **Single** draws the chosen variables as for an archived cast; the way up
  is dotted, and a diamond marks where the package is now. **Multi** draws
  one graph per variable.
- When a cast has just ended, **in water** and **last** choose between the
  current cast and the one before.

In place of the cast list, **Live** shows the connection to the acquisition
software (Seasave). This is for the CTD technician; leave it as it is unless
you were asked to change it.

## Saving a transect

!!! note "Aboard only"
    Transects are saved on the ship's dashboard and shared with everyone
    aboard.

When a section is worth keeping, for example a line of stations you will
want to look at again:

1. Draw the section as above.
2. Click **Save transect**.
3. Type a name, for example `Davis Strait CS1`, and confirm.

The transect is saved with its casts, its order and its variable, and
appears under **TRS** in the list, with **shared by** and your chat name. A
click on it opens the section again. The **×** at the start of its row
deletes it for everyone (you are asked first).

If the ship's server does not answer, the transect is kept in this browser
only and marked **this browser**. Its **↑** button shares it later.

## Casts from the archive

The years before 2025 come from Amundsen Science's published archive
(2002 to 2024). Their rosette casts are on this tab like any other, once
you tick their year in the **Legs** menu. They can be compared with this
year's: select a 2008 cast and a 2026 cast at the same place and click
**Multi**.

!!! tip
    The archive holds over 5,000 casts. Use the **filter** to find a station
    or a date, rather than scrolling.

## In short

- Choose casts by clicking rows, ticking the heading's box, or clicking
  stations on the map.
- **Single**: one cast, many variables. **Multi**: many casts, one graph
  per variable. **Section**: many casts, one variable, as a slice.
- **Bottles** marks the bottle firings; **Bottles fired** lists them.
- **Smooth** smooths the jittery optical and chemical sensors.
- **MVP** tows unfold into dips; **LADCP** casts carry currents.
- Aboard: **Live** follows the cast in the water; **Save transect** shares a
  section.
