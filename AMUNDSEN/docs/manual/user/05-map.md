# The map

The map shows where the ship has been and where she is now. Her track is a
line of coloured dots, and you choose what the colour means. Around the track
the map can show the stations, the planned route, the event log, photos,
satellite pictures, ice charts, the sea floor and more. Each of these is a
*layer* that you switch on or off.

The map sits on the left of every tab, so it is always there to answer
"where was that?".

![The map card: the Colour by menu and the buttons at the top, the layer pills, and the map with the 2026 season's track](img/05-map.webp)

The map card has three parts, from top to bottom:

1. **The toolbar**: the **Colour by** menu, and five small buttons on the
   right.
2. **The layer pills**: one rounded button per layer.
3. **The map itself**, with a colour key at its top left and a scale bar at
   its bottom left.

The map is drawn as a globe, not as a flat chart. Near the pole a flat chart
stretches the land four to eight times; on the globe distances and areas
look true. When you zoom out far enough you see the Earth's curve.

!!! note
    The map's background pictures stop at 85° north. Above that latitude the
    map is blank, but the ship's track, if she goes there, is still drawn.

## Moving and zooming

| To do this | With a mouse | On a touch screen |
|---|---|---|
| Move the map | drag it | drag it with one finger |
| Zoom in or out | turn the scroll wheel | pinch with two fingers |
| Read a point | hold the mouse over it | tap it |
| Show the whole track again | click **⟲** | tap **⟲** |

The map does not turn: north stays up.

!!! note
    A double click on the map does **not** zoom in. It drops a waypoint, a
    marker that measures the distance from the ship. See [Waypoints,
    distances and exporting the map](waypoints-and-export.html).

When the map has the keyboard's attention (click on it once), the arrow keys
move it and the **+** and **−** keys zoom.

The **scale bar** at the bottom left shows a round distance, for example
**20 km**, at the centre of the map. It changes as you zoom.

## The toolbar

**Colour by**
:   The menu that chooses what the track's colour means. See [Colouring the
    track](#colouring-the-track) below.

**ⓘ** (Map details)
:   Opens a box with facts about what the map shows, and the credits for the
    map's sources. See [Map details](#map-details) below.

**⇩** (Export map)
:   Opens a preview from which you save the map as a picture or as a file
    for Google Earth. See [Waypoints, distances and exporting the
    map](waypoints-and-export.html#exporting-the-map).

**⟲** (fit to track)
:   Moves and zooms the map so that the whole track you are showing fits in
    it. It also removes the marker of a point you clicked.

**—** (Minimise map)
:   Hides the map, so the tab's pane takes the whole width. Click **Map** in
    the tab row to bring it back.

**⤢** (Maximise map)
:   Makes the map fill the page. It then shows **⤡**: click that to go back
    to half the page.

[A tour of the screen](tour.html#the-map-beside-every-tab) explains the
three map sizes.

## Colouring the track

Each dot on the track is one moment of the ship's record. Its colour shows the
value of one measurement at that moment. The **Colour by** menu chooses which
measurement.

The menu lists every measurement the dashboard has for the shown legs. They
include:

- the water under the ship, measured by the *thermosalinograph* (TSG), an
  instrument in the lab fed by a pump that draws sea water through the hull:
  **SST (°C)** (the sea-surface temperature), **Salinity (PSU)** (how salty
  the water is), **Fluorescence (µg/L)** (a sign of the algae in the water),
  **CDOM (mg/m³)** (dissolved organic matter), **Oxygen (mL/L)**;
- the weather: **Air temperature (°C)**, **Relative humidity (%)**,
  **Atmospheric pressure (hPa)**, **True wind direction (°)**, **Relative
  wind speed (kn)**, **Short-wave radiation (W/m²)** (sunlight);
- the ship: **Heading (°)**, **Ship speed (kn)**, **Sea state · 4σ heave (m)**
  (how much she rises and falls on the waves), **Roll & pitch RMS (°)**;
- the sea floor and the winches: **Bottom depth (m)**, **Rosette depth (m)**,
  **Cable length (m)**;
- the **Surprise** scores (see [The surprise score](surprise.html));
- **Time elapsed (h)** and **Distance travelled (km)**, which colour the
  track from its start to its end. Use them to see in which order the ship
  went where.

To colour the track:

1. Click the **Colour by** menu above the map.
2. Click a measurement, for example **Salinity (PSU)**.
3. The track takes the new colours at once. The colour key on the map
   changes to match.

The same choice also colours the points of the graphs on the **Underway**
tab. That tab has its own **Colour by** menu above its panels; it is the same
setting, so changing one changes the other. On the Underway tab, clicking a
panel's title also colours the map by that panel's measurement. See [The
Underway tab](underway.html).

### Reading the colour key

The colour key is the small box at the top left of the map. It shows:

- the name of the measurement, for example **Salinity (PSU)**;
- a colour bar, with the highest value at the top and the lowest at the
  bottom.

The Underway tab shows the same key, lying flat, beside its **Colour by** menu.

The key stretches over the values in the span you are showing (see [Choosing
legs and a time span](legs-and-time.html)). It leaves out the most extreme
5% at each end, so that a few odd readings do not wash out the rest. Values
beyond the ends take the end colours. When you change the legs or the span,
the key's numbers change.

Most measurements go from dark purple (low) through green to yellow (high).
Depths are the other way round: deep is dark.

### An example: fresh water in Lake Melville

In 2025, Leg 2 began in Lake Melville, a long inlet in Labrador fed by large
rivers. Its surface water is nearly fresh. Out on the Labrador Sea the water
is much saltier.

1. Choose **2025 Leg 2** alone in the **Legs** menu, and set **Show last**
   to **all** (see [Choosing legs and a time span](legs-and-time.html#look-at-a-whole-earlier-leg)).
2. In **Colour by**, choose **Salinity (PSU)**.

The track is dark purple in Lake Melville, at the bottom of the map, and
turns yellow out at sea.

![2025 Leg 2 coloured by salinity: dark (fresh) in Lake Melville, yellow (salty) at sea](img/05-colour-salinity.webp)

### Where the pump was off

The TSG measurements come from sea water pumped into the lab. When the pump
stops (for example in thick ice or during maintenance), those readings are
not sea water any more. While the track is coloured by a TSG measurement,
the stretches where the pump was off are drawn as a thin grey line, without
coloured dots.

## Reading a point

Hold the mouse over a dot of the track. A box tells you about that moment.

![Holding the mouse over the track in Lake Melville: the leg, the time, the salinity, the position and the distance along the track](img/05-hover.webp)

The box shows:

- the leg, and the date and time (ship time);
- the measurement you colour by, and its value;
- the position, in degrees of latitude and longitude;
- how far along the record the point is, in kilometres.

Every other symbol on the map has a box of its own, too. Hold the mouse over
a station, a place, an event or a photo to read it.

On a touch screen, tap the point instead.

### Clicking a point

Click a dot of the track, and a marker (a ring) appears on it with a box that
stays open. The box adds how far the point is from the ship, by sea and by
air, and the depth of the sea there. Because it stays, you can select and
copy its text. [Waypoints, distances and exporting the
map](waypoints-and-export.html#the-position-box) explains it.

To remove the marker, click it, or click anywhere on the map where there is
nothing.

## What is on the map

| Symbol | What it is |
|---|---|
| a red and white ship | the *Amundsen*, at her latest position, turned to her heading |
| coloured dots joined by a thin line | the ship's track, coloured by **Colour by** |
| white dots | stations with a CTD cast |
| green dots | stations worked without a cast (from the event log) |
| larger orange dots | casts you have selected on the Casts tab |
| dots with names, in the accent colour | waypoints people have saved |
| a dark grey line with dots | the planned route and stations of the leg |
| small coloured triangles | entries of the event log |
| beige squares with names | towns and settlements |
| a camera | one day's timelapse from the ship's cameras (aboard only) |
| a dot in a pale ring | a photograph taken on board (aboard only) |
| names in italic | seas, bays, sounds and other waters |
| upright names | islands, capes, mountains and other land |

### The ship

The ship's marker is a small red and white icon of the *Amundsen*. It points
the way she was heading, averaged over the last ten minutes. If her heading
is not known, she is drawn as a plain red dot.

Hold the mouse over her to read when that position was recorded, and her
heading.

![The ship at Cambridge Bay at the end of the 2026 season, with her box](img/05-ship.webp)

!!! note "Aboard only"
    Aboard, the dashboard also reads the bridge's live page every few
    seconds. While that page is newer than the last recorded point, the ship
    stands where it says, and her box reads **live**, with her speed in
    knots. Ashore, her box reads **latest**: her last recorded position.

### Towns and settlements

The beige squares are towns and settlements of the Canadian Arctic, Labrador,
northern Québec, Ontario and Manitoba, and Greenland. The bigger the square,
the more people live there. When you are zoomed out, only the larger places
have their names written; zoom in and the smaller ones appear. Hold the mouse
over a square to read its other names and its population.

The settlements are always on the map. They have no pill.

## The layer pills

Each pill below the toolbar switches one layer on or off. A pill that is
**on** is filled in blue (in the light themes) and its layer is on the map.
A pill that is **off** is pale. Click a pill to switch it.

![The layer pills: Plan, Track, Stations, Events, Photos, Names, History, Nature, Sat off, Bathymetry, Ice charts and ADCP](img/05-layers.webp)

The first four, joined together, are the ship's own layers. The others follow.

| Pill | Shows | Starts |
|---|---|---|
| **Plan** | the leg's planned route and stations | on |
| **Track** | the ship's track | on |
| **Stations** | stations and saved waypoints | on |
| **Events** | the event log's entries, and the MVP tows | off |
| **Photos** | camera timelapses and photographs (aboard only) | on |
| **Names** | the names of waters and lands | on |
| **History** | the wiki's history: voyages, wrecks, places, artifacts | off |
| **Nature** | the wiki's nature: sightings, soundings, readings | off |
| **Sat off** | satellite pictures: radar, then true colour | off |
| **Bathymetry** | how the sea floor is drawn | Relief |
| **Ice charts** | the Canadian Ice Service's ice charts | off |
| **ADCP** | ocean currents at a chosen depth | off |

Hold the mouse over a pill to read what it does. The dashboard remembers
which layers you have on.

On a narrow screen **Stations** becomes **Stns**, and the other pills wrap
onto as many rows as they need, each keeping its whole name.

### Plan

The **Plan** layer draws the leg's planned cruise track and its planned
stations, from the expedition's planning file. The route is a dark grey line;
an alternative route, when there is one, is fainter and thinner. The planned
stations are dots on it. Their names appear when you zoom in.

Hold the mouse over a planned station to read its name, its type, its region
and its planned depth.

![The plan of 2026 Leg 3 (dark grey) beside the ship's track among the High Arctic islands](img/05-plan.webp)

The name of the plan and its number of stations are in [Map
details](#map-details), for example "Plan 2026 Amundsen Expedition_Leg 3
MAZE, QEI & RetroSeep_31Aug2026.kmz · stations: 149".

### Adding a plan of your own

!!! note "Aboard only"
    Adding a plan works only on the ship.

You can lay a plan of your own over the map, for example a proposed survey
drawn in Google Earth. It must be a KMZ or KML file (the formats Google
Earth saves).

1. Find the file on your computer.
2. Drag it from your file window and drop it anywhere on the map.
3. A message at the bottom of the screen says "plan loaded in this browser",
   with its name and its number of stations and tracks.
4. A new pill appears after **Plan**, named after your file, in a colour of
   its own. Click it to show or hide that plan.

To remove your plan, click the small **✕** on its pill.

Your plan is kept by your browser only. Nobody else sees it, and it does not
replace the leg's own plan.

### Track

The **Track** layer is the ship's track for the legs and span you show. Switch
it off to see the other layers more clearly; the colour key goes with it.

### Track detail

The map draws more of the track as you zoom in. Zoomed out, it shows about one
point per kilometre. Zoomed in, it shows one point every 100 m, 25 m, 5 m, and
finally every point the ship recorded. It fetches only the stretch you can
see, so it stays quick.

[Map details](#map-details) says how much it is showing, for example "25,572
visible track points · 1 km detail". While it fetches, it says "Loading
visible track…".

If you show a very long stretch, the map may say "Track density limit reached
· zoom in or shorten the span for more points". It then draws only part of
the track. Zoom in, or choose a shorter **Show last**, and the rest appears.

### Stations

The **Stations** layer marks the stations of the shown legs:

- a **white dot** is a station where a CTD cast was made. (A *CTD* is the
  instrument lowered on the rosette that measures conductivity, which gives
  the salinity, temperature and depth.)
- a **green dot** is a station worked without a CTD cast, found in the
  event log: a net tow, a mooring, a sediment core and so on.
- a **larger orange dot** is a cast you have selected on the Casts tab.

Station names appear as you zoom in. The layer follows the legs you show, not
the span: you see every station of those legs.

Hold the mouse over a station to read its name, its date and time, what was
done there, and the depth of the sea floor.

Clicking a station does more than a box:

- **Click a white dot**, and its cast is selected. On the **Casts** tab it is
  added to the graphs; on the **Stations** tab its row is ticked. From any
  other tab the **Casts** tab opens. Click the dot again to take the cast
  away. See [The Casts tab](casts.html).
- **Click a green dot**, and that station is found in the list of the
  **Stations** tab, which opens unless you are on it already. See [The
  Stations tab](stations.html).

Either way the map marks the station with a ring and a box, as for any point
you click.

![After clicking the green station JSC-1C in Jones Sound: its box on the map, and the Stations tab showing it](img/05-station-click.webp)

The saved waypoints are part of this layer too: dots with their names (see
[Waypoints, distances and exporting the map](waypoints-and-export.html)).

### Events

The **Events** layer marks the entries of the ship's *event log*: the record
the bridge and the scientists keep of every operation (a cast begun, a net in
the water, a mooring recovered), with its time and position.

Each entry is a small triangle. Its colour stands for the kind of operation.
Where several entries share a position, one larger triangle stands for them
all. Hold the mouse over a triangle to read up to ten of the entries there,
newest first, with their time, station, operation and comments.

The events follow the legs and the span you show.

![The Events layer in Jones Sound, 2026 Leg 3: triangles where the event log recorded operations](img/05-events.webp)

With **Events** on, the map also draws the tows of the MVP (the *moving
vessel profiler*, an instrument towed behind the ship that dives and climbs
as she steams) as green lines. Click a tow to add it to the Casts tab. See
[The Casts tab](casts.html).

### Photos

!!! note "Aboard only"
    The pictures stay on the ship. The public web copy has no **Photos**
    pill.

The **Photos** layer shows two kinds of pictures:

- **A camera icon** marks one day of the ship's cameras. The cameras take a
  picture every few minutes; each day's pictures are joined into a short
  film, a *timelapse*. The icon stands where the ship was in the middle of
  that day.
- **A dot in a pale ring** marks a photograph taken on board and shared in
  the ship's photo folder.

To watch a day's timelapse:

1. Click a camera icon. A small player opens over the map and starts the
   film. Its title is the date; under it, the leg and the number of pictures.
2. Click **‹** for the day before, or **›** for the day after.
3. Click **open video** at the bottom to open the film on a page of its own.
4. Click **✕** to close the player.

Click a ringed photograph to open it in the Wiki tab. See [The Photos
tab](photos.html) for the gallery.

### Names

The **Names** layer writes the names of waters and lands on the map: bays,
sounds, straits, islands, capes, lakes, rivers, glaciers and mountains. The
names come from the Canadian Geographical Names Database and from GeoNames.
Waters are written in italic, land upright. Zoom in and more names appear.

### History and Nature

These two pills show the Wiki's places on the map.

- **History** marks the human past: voyages (as lines), wrecks, winterings,
  places and artifacts.
- **Nature** marks the natural record: sightings of animals, soundings,
  readings and the ship's own journal.

When one of them is on, a small menu appears after it: **Kinds** for History,
**Domains** for Nature, with how many are shown, for example **Kinds · 12/15**.
Open it to choose which kinds of things the map shows: tick or untick them,
or click **all** or **none**.

Click a mark to read its page in the Wiki. [The Wiki tab](wiki.html)
explains these layers in full.

### Sat: satellite pictures

The satellite pill shows recent pictures of the sea from the European
Sentinel satellites, under the track. It is labelled with what it shows, and
each click moves to the next choice:

1. **Sat off**: no picture.
2. **S1 radar**: a radar picture from Sentinel-1. Radar sees through cloud
   and darkness, so it shows the ice whatever the weather. Ice and rough
   water are bright; calm open water is dark.
3. **S2 optical**: a true-colour picture from Sentinel-2, like a photograph
   from space. It needs daylight and clear sky.
4. Then back to **Sat off**.

The pictures cover a large region around the ship. Near the ship, and over a
few places of interest, a sharper picture is laid on top.

While a picture is on, a stepper appears after the pill:

- **‹** shows the picture before;
- the middle shows the picture's date and time, and its number in the
  series, for example **10-16 12:39 EDT · 332/332**;
- **›** shows the picture after.

The newest picture is the last of the series. [Map details](#map-details)
names the picture on show and its date.

![A Sentinel-1 radar picture over the Sverdrup Islands and Nansen Sound, with the ship's track of 2026 Leg 3](img/05-satellite.webp)

!!! tip
    Compare the radar picture with the track to see how the ship found her
    way through the ice.

### Bathymetry: the sea floor

*Bathymetry* is the shape of the sea floor. The map always shades it, and
the hills and mountains on land too. The depths come from GEBCO, the world's chart of the ocean floor.

The **Bathymetry** pill changes how the sea floor is drawn. Each click moves
to the next of four pictures:

1. **Relief**: deep water dark blue, shading through lighter blues to warm,
   sandy colours in the shallows. This is the picture the map starts with.
2. **IBCAO**: the tints of the printed International Bathymetric Chart of
   the Arctic Ocean, one colour for each band of depth.
3. **Oslo**: from near-black in the deep to light grey-blue on the shelves.
4. **Survey**: a chart of *where the depths came from*.

Hold the mouse over the pill to read which picture is on. The pill is lit
only for **Survey**.

The **Survey** chart is worth knowing about. In these waters only about a
quarter of the sea floor has ever been measured by a ship; the rest is
estimated. The Survey chart colours each place by how its depth was found:

| Colour | How the depth was found |
|---|---|
| yellow | measured by a multibeam survey (a ship sounding a wide strip) |
| orange | measured by a singlebeam survey (a ship sounding a line) |
| green | several surveys combined |
| pink | isolated soundings |
| purple | taken from a depth model |
| mid blue | not sounded: interpolated between soundings |
| dark blue | not sounded: predicted from satellite gravity |
| greyish blue | not sounded: from a pre-made grid |
| grey | of unrecorded origin |

![The Survey chart over Lancaster Sound: yellow lines where ships have measured the floor, blue where it is estimated](img/05-bathy-survey.webp)

### Ice charts

The **Ice charts** pill lays the Canadian Ice Service's ice charts over the
map. A chart shows how much of the sea is covered by ice, in tenths:

| Colour | Ice cover |
|---|---|
| pale blue | less than 1/10 (open water) |
| green | 1 to 3 tenths |
| yellow | 4 to 6 tenths |
| orange | 7 to 8 tenths |
| red | 9 to 10 tenths (close pack) |
| grey | unknown |

When the charts are on, this key appears under the pills.

The dashboard shows the chart valid on, or just before, the last day of the
track you are showing. So when you show an earlier leg, you see the ice as it
was then.

There are two kinds of chart:

- the **regional weekly charts**, which together cover the Western Arctic,
  the Eastern Arctic and Hudson Bay;
- a **daily chart** of the ship's area, when there is one.

Each click on the pill moves to the next kind it has, then off. Hold the
mouse over the pill to read which charts are shown and their dates.

![The weekly ice charts of 14 September 2026 over the High Arctic: close pack ice (red) to the north-west, open water (pale blue) in Lancaster Sound and Baffin Bay](img/05-ice-weekly.webp)

### Reading an ice chart's egg code

On the weekly charts, click inside an area to open its *egg code*: the ice
service's oval summary of the ice in that area.

![The egg code of an area of the Western Arctic: 9/10 total, of old ice, thick first-year ice and new ice](img/05-ice-egg.webp)

The box shows:

- the region, and the date and time the chart is valid for;
- the oval: the total ice cover at the top, then, for up to three kinds of
  ice, their cover, their stage of development and their form;
- the same in words: **Total concentration**, and a table of **Ice type**,
  **Concentration**, **Stage of development** (new ice, first-year ice, old
  ice and so on) and **Form / floe size**;
- **Source SIGRID codes**: click it to see the chart's raw codes;
- links to the **Original chart data**, an **Egg-code guide** and the
  licence.

Click **Close** to close it.

If a chart cannot be loaded, a **Retry chart** button appears under the
pills. Click it to try again.

!!! note
    An ice chart is the ice service's analysis on the day it was drawn. Ice
    moves; it may have changed since.

### ADCP: ocean currents

The **ADCP** pill draws the ocean currents measured at stations by the
*lowered ADCP*, an instrument on the rosette that measures how fast the water
moves at each depth.

When it is on, a slider and a box appear after the pill. They choose the
depth, in metres. Each station with a measurement at that depth gets an
arrow:

- the arrow points the way the water flows;
- its length shows the speed: 0.1 m/s is 20 pixels on screen, whatever the
  zoom.

To look at the currents at 200 m:

1. Click **ADCP**. Arrows appear at the stations, at 50 m.
2. Type `200` in the box after the slider, and press Enter. Or drag the
   slider. The slider moves fast through the upper water and slowly through
   the deep, where there is less to see.
3. The arrows change to show the currents at 200 m. Stations not that deep
   have no arrow.

Hold the mouse over the **ADCP** pill to read how many stations have arrows,
or why there are none.

![Currents at 50 m among the islands of the High Arctic, 2026 Leg 3](img/05-adcp.webp)

## Map details

Click **ⓘ** in the toolbar to open **Map details**.

![The Map details box: the span, the legs and the distance, the track detail, the stations, the time range, the plan, and the credits](img/05-details.webp)

From top to bottom it tells you:

- the span, the number of legs shown and the distance the ship travelled in
  them, for example "4 mo span · 5 legs selected · 19334 km travelled";
- how much of the track the map is drawing (see [Track
  detail](#track-detail));
- how many CTD casts and other stations are on the map;
- the first and last times of the track shown;
- the satellite picture on show, if any;
- the plans on show, with their number of stations;
- how to move the map: "scroll to zoom · drag to pan · ⟲ fits";
- under the line, the credits for the map's sources: the sea floor (GEBCO),
  the coastlines (OpenStreetMap), the names (GeoNames), the archive data
  (Amundsen Science / ArcticNet) and the map software.

Click **×** to close it.

## When the map shows nothing

If the map shows the sea but no track:

- check that at least one leg is ticked in **Legs**;
- check that **Show last** reaches back to those legs. For an earlier leg,
  choose **all**. See [Choosing legs and a time
  span](legs-and-time.html#why-is-the-map-empty);
- check that the **Track** pill is on.

If the map is blank and says "Map unavailable", click **⟲**, then reload the
page if that does not help.

## In short

- Drag to move, scroll or pinch to zoom, **⟲** to see the whole track. A
  double click drops a waypoint; it does not zoom.
- **Colour by** chooses what the track's colour means; the key at the top left
  of the map reads it.
- Hold the mouse over anything to read it; click a point to keep its box,
  with distances from the ship.
- The pills switch layers on and off: the plan, the track, the stations, the
  events, the photos, the names, the wiki's layers, satellite pictures, the
  sea floor, ice charts and currents.
- Clicking a white station adds its cast to the Casts tab; clicking a green
  one finds it on the Stations tab.
- **ⓘ** tells you what the map is showing, and credits its sources.
