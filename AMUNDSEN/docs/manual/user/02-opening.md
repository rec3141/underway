# Opening the dashboard

The dashboard is a web page. To open it, you type its address into a web
browser: Firefox, Chrome, Safari or Edge, on a computer, a tablet or a phone.
Which address you use depends on where you are.

## On the ship

The ship has two networks. Your device is on one or the other:

- the **ship's Wi-Fi**, named **AMD-WiFi**, which most laptops, tablets and
  phones use;
- the **wired network** (the ship's LAN), which the computers in the labs
  and offices plugged in with a cable use.

Use the address for the network you are on:

| You are on | Type this address |
|---|---|
| the ship's Wi-Fi | `http://underway.local` |
| the ship's Wi-Fi, if that name does not work | `http://192.168.3.xxx/underway/` |
| the wired network | `http://10.0.0.xxx/` |

To open the dashboard on the ship:

1. Join the ship's Wi-Fi, **AMD-WiFi**, or plug in to the wired network. If
   you do not know the Wi-Fi's password, ask on the bridge or the ship's IT
   contact.
2. Open your web browser.
3. Click in the address bar at the top of the browser window.
4. Type the address from the table, exactly as written, starting with
   `http://`. Press Enter.
5. The dashboard opens. The first time, it can take a few seconds to load the
   map and the graphs.

!!! tip
    Once it opens, add the page to your browser's bookmarks (or favourites).
    Next time, you only need to click the bookmark.

!!! note
    Android phones and tablets cannot use the name `underway.local`. On
    Android, type the number address instead: `http://192.168.3.xxx/underway/`.

### How do I know which network I am on?

Look at your device's network settings. Its own address tells you:

- an address starting with `192.168.3.` means the ship's Wi-Fi;
- an address starting with `10.0.0.` means the wired network.

An address for one network does not work from the other one.

## On a phone or a tablet

The dashboard works on phones and tablets. Use the same addresses as on a
computer.

On a narrow screen the page rearranges itself so that everything fits. The
picture below shows the dashboard on a phone.

![The dashboard on a phone: the tabs grouped into three menus, the map below the controls](img/02-phone.webp)

What changes on a phone:

- The tabs are grouped into three menus at the top: **Map**, **Science**
  and **Extras**. Tap one to open it, then tap a tab. [A tour of the
  screen](tour.html) shows them.
- The **Show last** slider becomes a drop-down list.
- The map comes first, and the panels follow below it. Scroll down to reach
  them.
- **Language**, **Feedback**, the colour theme and the text size move to the
  very bottom of the page.

!!! tip
    On a phone, turn it sideways to give the map and the graphs more room.

## From ashore: the public web copy

When the ship can reach the internet, it sends a copy of the dashboard to the
public web. Anyone can open that copy, anywhere:

`https://cryomics.org/underway/`

The web copy shows the same map, graphs, casts, schedule, stations and wiki.
It is updated from the ship while the ship's link is up, so it can be some
minutes or hours behind the ship.

The web copy is not the whole dashboard:

- **What runs only aboard is left out.** There is no **Chat**, no **Photos**
  tab, no **Games** link, no **Cruise reports: build yours →** button, no
  **Feedback** button, no photo uploads, no alerts and no live feeds (the
  cast in the water, the bridge's live page). Measuring a distance by
  sea from the ship (see [Waypoints, distances and exporting the
  map](waypoints-and-export.html)) also needs the ship's computer.
- **It shows only what Amundsen Science has published.** For the 2026
  season, the web copy has a position every 5 minutes and a reading every 15
  minutes, not every minute. The shortest time span it offers is 12 hours.
  Its casts are those of 2024 and earlier; later stations still show on the
  map, but without their profiles.

The **Sources** tab (the **?** tab) says which copy you are looking at. On
the web copy it begins with "Web copy: published from the ship when its link
is up".

## Aboard and ashore

You will see the words *aboard* and *ashore* throughout this manual.

**Aboard** means the dashboard running on the ship during a cruise. New data
arrive every few minutes. The status line at the top of the page says
**LIVE** while they do. The schedule banner shows what is happening now.

**Ashore** means after the cruise. The ship's dashboard then runs from its
own copy of the data, with nothing new coming in. You notice three things:

- the status line says **cruise ended** and the date, instead of LIVE;
- there is no schedule banner (nothing is happening "now");
- the page opens on the legs of the last season.

Everything recorded during the cruise is still there to look at.

## When the page does not load

If the page does not open, try these steps in order:

1. **Check the address.** It must start with `http://`, not `https://`.
   Some browsers add the `s` on their own: type the whole address again,
   including `http://`.
2. **Check the network.** Make sure your device is on the ship's network,
   and use the address for that network (see the table above).
3. **Use the number, not the name.** If `http://underway.local` does not
   work, type `http://192.168.3.xxx/underway/` instead.
4. **Reload the page.** Press F5, or the circular arrow in the browser.
5. **Try another browser** on the same device.
6. **Ask for help.** If none of this works, the dashboard's computer may be
   down. Tell the person who looks after the dashboard (see
   [Sources, feedback and help](sources-and-help.html)).

### When the page opens but something is missing

Sometimes the page opens but one part cannot load. An extra line then
appears under the status line, for example:

> Track update unavailable · retrying; displayed data may be older

This means the dashboard could not fetch the newest data for that part (here,
the ship's track). It keeps trying on its own. Meanwhile it shows the last
data it had, which may be older than you think. The line goes away when the
data arrive.

If the map stays blank and shows "Map unavailable; other plots and tables
remain usable. Try resetting the map.", click the **⟲** button above the map.
If that does not help, reload the page. The map needs a browser that can
draw 3-D graphics (WebGL); very old devices may not manage it.

## You do not need to reload

The dashboard checks for new data every 30 seconds and draws them on its own.
You can leave it open all day on a lab screen and it keeps up to date.

## The page remembers your choices

The dashboard remembers your choices in your browser: the legs you show, the
time span, the colour of the track, the layers on the map, the tab you were
on, your theme and text size. When you come back, it opens as you left it.

These choices are kept by your browser on your device only. Another device,
or another browser on the same device, starts fresh. A private or incognito
window forgets them when you close it.
