# The Underway tab

The **Underway** tab shows what the ship measures continuously while it sails:
the sea water pumped up from under the hull, the weather, the ship's own
motion and the winches. "Underway" means *while the ship is moving*: these
instruments run all the time, not only at a station.

Each measurement is a **variable**, such as **SST (°C)** (sea surface
temperature) or **Salinity (PSU)** (how salty the water is). Every variable
has a **panel**: a small graph of its values over time.

This chapter explains how the tab is laid out, how to open and arrange the
panels, and how to read them. The surprise score at the top of the tab has a
chapter of its own: [The surprise score](surprise.html). The tables under the
panels are in [Tables and downloads](tables-and-downloads.html).

## What you see

Click **Underway** in the row of tabs. The map stays on the left, as on every
tab. The right-hand side shows a strip of controls and, under it, a set of
cards.

![The Underway tab: the strip with X-axis and Color by at the top, then the Surprise, Lab, Met Station and Bridge cards](img/07-underway-tab.webp)

The first time you open the dashboard, every panel is folded away and you see
only the **group cards**. This keeps the page short. You open the panels you
want, and the page remembers your choice on this device.

!!! tip
    The legs and the time span chosen in the header (**Legs** and
    **Show last**) apply to this tab too. They are explained in
    [Choosing legs and a time span](legs-and-time.html).

## The group cards

The panels are sorted into groups by where their data come from. Each group
has a card.

| Card | What it holds |
|---|---|
| **Surprise** | The surprise score: **Combined** and one line per time scale (**15 min**, **1 h**, **3 h**, **12 h**, **48 h**). See [The surprise score](surprise.html). |
| **Lab** | The sea water from the ship's intake, measured by the thermosalinograph (TSG) in the lab: **SST (°C)**, **Salinity (PSU)**, **Excess heat (°C)**, **TSG flow (V)**, **Fluorescence (µg/L)**, **CDOM (mg/m³)**, **Oxygen (mL/L)**. |
| **Met Station** | The weather station on the mast: **Short-wave radiation (W/m²)** (sunlight), **Air temperature (°C)**, **Relative humidity (%)**, **Atmospheric pressure (hPa)**, **True wind direction (°)**, **Relative wind speed (kn)**. |
| **Bridge** | The ship itself: **Heading (°)**, **Ship speed (kn)**, **Sea state · 4σ heave (m)** (how much the ship rises and falls with the waves), **Roll & pitch RMS (°)** (how much it rocks). |
| **Winches** | The sea floor and the rosette's cable: **Bottom depth (m)**, **Rosette depth (m)**, **Rosette rate (m/s)**, **Cable length (m)**, **Cable rate (m/s)**. |
| **Ice camera** | Estimates of the sea ice made from the ship's camera: **Concentration**, **Ice composition** and others. These are experimental, and come from a service on the ship's network: the card is not on the public web copy. |

Here is the **Lab** card on its own.

![The Lab card: a row per variable with its latest value, and the time since the last observation at the top](img/07-underway-group.webp)

A card shows:

- **Its name** at the top left, with a handle (**⋮⋮**) to drag the card
  somewhere else on the page.
- **How old its newest value is**, at the top right (for example
  **16 d ago**, sixteen days ago). Hold the mouse over it to see the exact
  time of the latest observation.
- **A button at the top right** that opens every panel of the group at once
  (**▲**) or folds them all away again (**—**).
- **A row per variable**, with the variable's newest value on the right.
  The small arrow at the start of the row says whether its panel is folded
  (**▼**) or open (**▲**).

The **TSG** (thermosalinograph) is the instrument in the lab that measures the
temperature and salinity of the water pumped up from under the hull. Several
other sensors share the same water line.

### Opening and closing a panel

1. Find the variable in its card, for example **Salinity (PSU)** in **Lab**.
2. Click its row. The panel opens on the page, and the arrow turns to **▲**.
3. To fold it away again, click the row once more, or click **—** on the
   panel itself.

To open a whole group at once, click the **▲** at the top right of its card.

!!! tip
    **Excess heat (°C)** is the water temperature minus the temperature at
    which water of that salinity freezes. Above zero the water is warmer than
    freezing; below zero it is supercooled. Hold the mouse over its row to
    read this on the page.

!!! note "Aboard only"
    Aboard, the **Lab** card shows a **LOW FLOW** warning in its title, and
    the card takes the warning colour, when the water pump's flow is too low. The measurements of the
    water are then not reliable. See [Grey points: the pump was off](#grey-points-the-pump-was-off).

## A panel

An open panel is a graph with the variable's values over time.

![The Salinity panel over a week, coloured by salinity, with its buttons at the top right](img/07-underway-panel.webp)

Along its top you find:

- **The handle (⋮⋮)**: drag it to move the panel. Drop it on another panel or
  card and the two trade places.
- **The title**, for example **Salinity (PSU)**. Click it to colour the map
  and every graph by this variable (see [Colour by](#colour-by)).
- **The latest value**, in colour, for example **25.57 PSU**.
- **🔔** (aboard only): set an alert on this variable. See below.
- **⇩**: export the graph as a picture. See
  [Tables and downloads](tables-and-downloads.html#exporting-a-graph-as-a-picture).
- **log** (only on a few variables, such as **Fluorescence (µg/L)** and
  **CDOM (mg/m³)**): switch the vertical axis to a logarithmic scale. Use it
  when a few very high values flatten everything else.
- **⇅** (only on **Bottom depth (m)** and **Rosette depth (m)**): squeeze the
  depth axis so the shallow part gets more room (a square-root scale).
- **⟲**: reset the zoom to the time span chosen in the header.
- **—**: fold the panel back into its card.
- **⤢**: make the panel wide, across the whole pane. Click it again to make
  it narrow.

Each point on the graph is one measurement, averaged over a short time. How
short depends on the span: a few seconds when you show the last hour, an hour
when you show four months. The grey line joins the points. A gap in the line
means there are no data there; nothing is filled in.

### Reading a value

Hold the mouse over a point. A small box shows the value, the time (ship
time) and the leg.

Click a point and the map moves to where the ship was at that moment and
marks the place.

### Zooming and moving along the graph

Only one panel at a time answers the mouse: the **selected** panel, which has
a coloured frame. On the other panels, scrolling the mouse wheel scrolls the
page, so you do not zoom by accident. Click a panel once to select it.

On the selected panel:

- **Drag** to move along the time axis.
- **Scroll** to zoom in and out.
- **Shift + scroll** zooms the time axis only.
- **Ctrl + scroll** zooms the value axis only.
- **Double-click**, or **⟲**, goes back to the whole span.

All the panels share one time axis. Zoom or move one, and every open panel
follows, so the same moment is always lined up. The vertical axis of each
panel adjusts to the values you can see.

!!! tip
    On a phone or tablet, a swipe over an unselected panel scrolls the page.
    Tap the panel first if you want to move inside it.

### Arranging the panels

Your arrangement is remembered on this device.

- To move a panel or a card, drag it by its handle (**⋮⋮**) and drop it on
  another one. They trade places.
- To make a panel wide, click **⤢**.
- To give the panels the whole width of the screen, hide the map with the
  **Map** button at the start of the tab row or the **—** button on the map.
  See [A tour of the screen](tour.html).

## X-axis

The strip at the top of the tab holds two controls that change every panel.

![The strip: X-axis, Color by, and the colour bar](img/07-underway-strip.webp)

The **X-axis** button switches the horizontal axis of the graphs:

- **Time**: the ship's time, in the ship's time zone (for example EDT).
- **Distance**: the distance the ship has sailed along its track, in
  kilometres.

Click the button to switch from one to the other. **Distance** is useful when
you compare the water along a route: a long stop at a station takes no room
on a distance axis, while it takes hours on a time axis.

## Colour by

**Color by** chooses the variable that colours the points of every graph and
the ship's track on the map. The same choice is in the box above the map;
the two always agree.

1. Open the **Color by** list.
2. Choose a variable, for example **Salinity (PSU)**.
3. The points of every panel and the track on the map take the colours of
   salinity. The bar to the right of the list shows the scale, from its low
   value on the left to its high value on the right.

The scale runs from the 5th to the 95th percentile of the values in the span:
the lowest and highest few values are left out so that one odd value does not
wash out all the colours.

Clicking a panel's title does the same as choosing it in **Color by**. The
panel whose variable colours the page is the selected one. Click its title
again to unselect it.

!!! tip
    Colouring one variable by another shows how they go together. Open
    **SST (°C)** and colour by **Salinity (PSU)**: if the warm points are also
    the fresh ones, the warm water is river or melt water.

**Color by** also offers a few variables that have no panel of their own:
**Time elapsed (h)**, **Distance travelled (km)** and **TSG line warming (°C)**.

## Example: a salinity drop

On 10 September 2026, in Leg 3, the ship worked at a group of stations in the
Canadian Arctic Archipelago. To see what the surface water did:

1. In **Legs**, make sure **2026 Leg 3** is ticked.
2. Set **Show last** to **1 mo** (one month).
3. Open **Salinity (PSU)** and **Ship speed (kn)** from their cards.
4. Set **Color by** to **Salinity (PSU)**.
5. On the salinity panel, drag and zoom until the axis shows 10 September.

![Salinity and ship speed on 10 September 2026: each time the ship gets under way, the salinity falls from about 30.5 to about 23 PSU](img/07-underway-salinity-drop.webp)

While the ship is stopped on station (speed near 0), the water at the intake
is around 30 to 31 PSU. Each time the ship sails off at about 10 knots, the
salinity falls within minutes to 23 to 25 PSU: there is much fresher water
at the surface between the stations. The dark points (low salinity) are the
same moments on both graphs, and on the map the track turns dark where the
ship crossed that fresh water.

## Grey points: the pump was off

The Lab's sensors measure water pumped up from under the hull. When the pump
stops, or its flow is too low, the sensors measure the water standing still in
the pipe, not the sea. The dashboard keeps these values but draws them in
**grey**, on every Lab panel and on the map track.

Hold the mouse over a grey point: its box says **intake pump off**.

![Salinity on 2 to 4 September 2026: the grey points are the day the intake pump was off; the coloured ones before and after are the sea](img/07-underway-pump-off.webp)

The grey points do not count when the dashboard sets a panel's vertical
scale or the colour scale, so a pump stop does not squash the real
measurements. In the example above the grey salinity changes smoothly and
slowly, the way water sitting in a pipe does, while the sea's salinity on
either side is much more varied.

The **TSG flow (V)** panel shows the pump's flow itself. A flow below 0.5 V
counts as "pump off". Pump stops also appear as events in the
[Schedule tab](schedule.html).

## Alerts on a variable

!!! note "Aboard only"
    Alerts are sent only by the dashboard on the ship.

The **🔔** on a panel opens the **Underway alert** form, which sends you a
message when the variable crosses a value:

1. Click **🔔** on the panel, for example on **Salinity (PSU)**.
2. **parameter** shows the variable. You can choose another.
3. Under **when the value is**, choose **over** or **under**, and type the
   value.
4. **at most once every** sets how often you can be told, in minutes (60 by
   default).
5. Under **via**, choose **email** and type your address, or **Telegram**.
6. Click **Save alert**, or for Telegram, **Open Telegram to subscribe**.

The alert includes a six-hour map coloured by the variable and a figure of
every Lab variable. A lit bell (**🔔**) shows the variables you already
follow. The form lists your saved alerts under
**Email alerts saved for this address**, each with a **remove** button.

The **🔔** on the **Lab** card offers **AI recommendation**: instead of a
threshold, a language model looks at the last six hours of Lab data and
alerts you only when it judges the water worth sampling.

## In short

- The tab opens with every panel folded into its group card. Click a row to
  open its panel.
- Hold the mouse over a point to read it; click it to find the place on the
  map.
- Click a panel to select it, then drag to move and scroll to zoom. All
  panels follow.
- **X-axis** switches between **Time** and **Distance**.
- **Color by**, or a click on a panel's title, colours every graph and the
  map track by one variable.
- Grey points on the Lab panels are water measured while the pump was off.
