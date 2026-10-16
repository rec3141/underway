# Waypoints, distances and exporting the map

This chapter shows you how to mark a place on the map and find out how far it
is from the ship, by sea and by air, and how deep the sea is there. It then
shows you how to save the map as a picture, or as a file for Google Earth.

## Waypoints

A *waypoint* is a place you mark on the map yourself: a spot you want to
reach, a feature you spotted on a satellite picture, a place to discuss with
the bridge. The dashboard tells you how far it is from the ship and how deep
the water is there. You can give it a name and save it for everyone.

### Dropping a waypoint

You drop a waypoint on open map, away from the track, the stations and the
other symbols.

With a mouse:

1. Move the map until you can see the place you want.
2. Double-click the place. (A double click on this map does not zoom.)

On a touch screen:

1. Move the map until you can see the place you want.
2. Press the place with one finger and hold it still for about half a
   second. Then let go.

A ring appears on the place, with a box beside it.

![A waypoint dropped in Jones Sound, with its box: name, position, when it was marked, the distance by sea and by air, and the depth](img/06-waypoint.webp)

If you double-click on a symbol (a dot of the track, a station), you do not
get a waypoint: you get that point's own box instead, as if you had clicked
it.

There is one marker at a time. Dropping a new waypoint, or clicking another
point, moves the marker there.

### The position box

The box beside the marker tells you about the place. It has the same lines
whether you dropped a waypoint or clicked a point of the track or a station.
For a waypoint, it reads:

**Waypoint 1 ✎ Save**
:   The waypoint's name, a pencil to rename it, and **Save**. New waypoints
    are numbered: **Waypoint 1**, **Waypoint 2** and so on. See [Naming and
    saving a waypoint](#naming-and-saving-a-waypoint) below.

**75.9500°N, 85.2000°W**
:   The position, in degrees of latitude (north) and longitude (west), to four
    decimal places (about 10 metres).

**marked:**
:   When you dropped it, in ship time.

**by sea:**
:   The distance the ship would have to sail to get there, staying in the
    water. The map draws that route as a thin line from the ship to the
    marker.

**by air:**
:   The straight-line distance from the ship, as a bird flies, along the
    curve of the Earth.

**depth:**
:   The depth of the sea floor at the marker, in metres. On land it reads
    **elevation** instead: the height of the ground.

Distances are given in kilometres and in nautical miles (**nmi**). One
nautical mile is 1.852 km; it is the unit sailors and the bridge use.

For a station or a point of the track, the top line is the point's name (or
the start of its box) instead of the waypoint's name, and there is no
**marked:** line and no **Save**.

The box stays open while the marker is there, and follows the map when you
move it. You can select its text and copy it, for example to paste the
position into a message.

### Distance by sea and by air

The two distances answer different questions.

- **by air** is the shortest distance over the Earth's surface. It follows the
  ship: if she moves, it changes.
- **by sea** is the shortest way through the water, around the land. The
  ship's computer works it out from where the ship is when you drop the
  marker. It does not change as she moves; drop the marker again to update
  it.

Working out the route takes a few seconds. Meanwhile the box says **by sea:
working it out…**. Then the distance appears and the route is drawn.

For example, from Cambridge Bay, where the ship ended the 2026 season, to a
waypoint in Jones Sound, the distance by air is about 1000 km. By sea the
ship must go north-east through the channels to Bellot Strait, north up Prince
Regent Inlet, east along Lancaster Sound, then round the end of Devon Island:
about 1481 km.
Zoom out, and the route is drawn all the way.

![The same waypoint zoomed out: the route by sea (a thin blue line) runs from Cambridge Bay through the islands to Jones Sound](img/06-sea-route.webp)

For a place very far from the ship, the route is worked out in coarser steps
to save time, and the box says **by sea: about** and the distance. Hold the
mouse over that line to read how coarse the steps were.

If no route can be found (for example, the marker is on land far inland, or
in a lake that does not reach the sea), the box says **by sea:** and the
reason, for example **no route**.

!!! note
    The route by sea follows the shape of the coast on the map, at a scale of
    a few hundred metres. It does not know about the ice, the shallows the
    ship must avoid, or the bridge's own plans. It is a guide, not a course
    to steer.

!!! note "Aboard only"
    The distance by sea and the depth are worked out by the ship's computer.
    The public web copy cannot work them out.

### Where the depth comes from

After the depth there is a small coloured dot. It says how that depth was
found, in the colours of the **Survey** chart (see [The
map](map.html#bathymetry-the-sea-floor)). Hold the mouse over the dot to read
it in words, for example:

- "measured by a multibeam survey";
- "measured by isolated soundings";
- "not sounded: interpolated between soundings";
- "not sounded: predicted from satellite gravity".

In Arctic waters only about a quarter of the depths were ever measured by a
ship. A depth that was "not sounded" is an estimate, and the real floor can
be much shallower or deeper.

### Removing the marker

To remove the marker and its box, do one of these:

- click the marker itself;
- click the map where there is nothing;
- click **⟲** above the map.

An unsaved waypoint is then gone.

### Naming and saving a waypoint

!!! note "Aboard only"
    Saving waypoints works only on the ship.

A saved waypoint stays on the map for everyone who uses the dashboard, and is
listed on the **Stations** tab.

1. Drop a waypoint.
2. Click its name, **Waypoint 1**, or the pencil **✎** beside it. The name is
   selected.
3. Type a new name, for example `Polynya edge`. Names can be up to 80
   characters long.
4. Press Enter, or click **Save**. The box says **saving…**, then **saved**,
   and **Save** disappears.

The waypoint now shows on the map as a dot with its name, in the accent
colour, whenever the **Stations** layer is on. Hold the mouse over it to read
its name, who saved it (the name you use in the chat, if you have set one),
its position and when it was made.

To open a saved waypoint's box again, click it on the map. Its box says
**saved**, and the distances are worked out again from where the ship is now.

To rename a saved waypoint, click it, change its name in the box, and click
**Save** again.

To delete a saved waypoint, use the **Stations** tab. See [The Stations
tab](stations.html).

!!! tip
    Save only the waypoints others need: everyone on the dashboard sees them.
    For a quick measurement, drop a waypoint, read the distance and click it
    away.

## Exporting the map

You can save the map as it is on your screen, as a picture for a report or a
slide, or as a file to open in Google Earth.

1. Set the map up as you want it: the legs and span, the colour, the layers,
   the view. The export shows the map exactly as it is.
2. Click **⇩** (Export map) at the top right of the map. A box titled
   **Export · Map** opens.
3. Choose the size and the format, as described below.

![The Export · Map box: the size controls and buttons on the left, the preview on the right](img/06-export.webp)

### The export box

The preview on the right is a copy of your map, with the same view, layers and
colour key. You cannot move it; to change the view, close the box, move the
map and open the box again.

On the left, from top to bottom:

**Width (px)** and **Height (px)**
:   The size of the picture, in pixels (the dots a screen is made of), from
    160 to 4096. Type a number, or drag the bottom-right corner of the
    preview to resize it; the numbers follow.

**PNG DPI scale**
:   How sharp a PNG picture is. The slider goes from 1× to 4×, in steps of a
    half. At 2× a 670 × 614 preview makes a picture of 1340 × 1228 pixels.
    The line under the slider gives the result. Use 2× or more for print.

**Copy to clipboard**
:   Copies a PNG picture, ready to paste into a document, a slide or a chat.

**Export PNG**
:   Saves the picture as a PNG file, the best format for maps and screens.

**Export SVG**
:   Saves the picture as an SVG file. For the map, the SVG holds the same
    picture as the PNG: you cannot edit the lines in it. (Graphs exported as
    SVG do keep their lines; see [Tables and
    downloads](tables-and-downloads.html).)

**Export KMZ**
:   Saves the map's layers as a file for Google Earth. See below.

Under the buttons a line tells you how it went: "Preparing download…", then
"Downloaded." or "Copied PNG to clipboard.".

The exported picture carries the colour key, and at the bottom a white strip
with the credits for the map's sources. Keep the credits when you use the
picture.

Click **×** at the top right to close the box.

!!! note
    **Copy to clipboard** needs a secure (https) connection. The ship's
    dashboard is opened with `http://`, so aboard the box says "Copying
    requires HTTPS or localhost. Use Export PNG on this connection." Use
    **Export PNG** instead, and insert the file. On the public web copy,
    which uses `https://`, copying works.

!!! tip
    If an export fails with "Export failed. Try a smaller size or resolution,
    and check that map tiles have loaded.", wait for the map to finish
    drawing, then try a smaller size or a lower **PNG DPI scale**.

### A file for Google Earth (KMZ)

**Export KMZ** saves a file named like `amundsen-map-….kmz`. KMZ is the file
format of Google Earth. Unlike a picture, it holds the map's layers as real
points and lines, which Google Earth draws on its own globe.

The file holds every layer the map has loaded, including what lies outside
your view: the ship's track (coloured as on the map), the stations, the plan,
the events, the waypoints, the ice charts and so on. Each layer is a folder
that you can switch on and off in Google Earth. Each point keeps the text of
its box.

To open it in Google Earth:

1. Click **Export KMZ**. Your browser saves the file, usually in your
   Downloads folder.
2. Open Google Earth on your computer (Google Earth Pro), or Google Earth in
   a web browser.
3. Open the file: in Google Earth Pro, choose **File**, then **Open**, and
   pick the file; or double-click the file. In Google Earth on the web,
   import it as a KML file from your computer, in the projects panel.
4. The track and the other layers appear on Google Earth's globe.

The file needs no internet connection to open, so it works aboard too.

### Saving a graph

The graphs on the other tabs have an export button of their own, **⇩**, in
their title bar. It opens the same kind of box, without **Export KMZ**. See
[Tables and downloads](tables-and-downloads.html).

## In short

- Double-click open map (or press and hold on a touch screen) to drop a
  waypoint.
- Its box gives the position, the distance from the ship by sea and by air,
  and the depth, with a dot saying how that depth was found.
- Click the name to rename it, and **Save** to keep it for everyone (aboard
  only). Delete saved waypoints from the Stations tab.
- Click the marker, or empty map, to remove it.
- **⇩** above the map exports it as a PNG or SVG picture, or as a KMZ file
  for Google Earth.
