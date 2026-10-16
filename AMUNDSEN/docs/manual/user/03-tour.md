# A tour of the screen

This chapter walks you round the dashboard's screen, from top to bottom. It
names every part, says what it is for, and points you to the chapter that
explains it in full.

## The parts of the screen

On a computer, the screen has four parts:

1. **The header**, at the top: the tabs, the title, the status line and the
   controls that choose what is shown.
2. **The schedule banner**, under the header, while the ship is at work
   (aboard only).
3. **The map**, on the left.
4. **The pane**, on the right: what the chosen tab shows (the panels of the
   Underway tab, the list of casts, the schedule, and so on).

Here is the header on its own.

![The header: the tab row, the language, feedback and appearance controls, the title, the status line and the leg and time-span controls](img/03-header.webp)

## The tab row

The row of buttons at the top left is the tab row. A *tab* is one view of the
dashboard. Click a tab to open it; the tab you are on is highlighted. The map
stays on the left whichever tab you choose; only the pane on the right
changes.

From left to right:

**Map**
:   Not a tab, but a switch for how much of the page the map takes. See
    [The map beside every tab](#the-map-beside-every-tab) below.

**Schedule**
:   The ship's plan for today, the event log and a calendar. See
    [The Schedule tab](schedule.html).

**Stations**
:   The list of stations the ship has worked, with their positions. See
    [The Stations tab](stations.html).

**Underway**
:   The continuous measurements, one panel per instrument group, and the
    data tables. This is the tab the dashboard opens on the first time. See
    [The Underway tab](underway.html).

**Casts**
:   The profiles measured at stations, as graphs. See [The Casts tab](casts.html).

**Photos**
:   The photo gallery. A small number on the tab counts the new photos you
    have not looked at yet. See [The Photos tab](photos.html).

**Wiki**
:   The region's history and nature. See [The Wiki tab](wiki.html).

**Chat**
:   Opens the ship's chat at the side of the page. Click it again to close
    the chat. See [Chat and games](chat-and-games.html).

**Games**
:   Opens the ship's games on a page of their own. Use your browser's back
    button to come back. See [Chat and games](chat-and-games.html).

**?**
:   The **Sources** tab: where each number comes from, which legs have which
    data, credits, and a link to this manual. See [Sources, feedback and
    help](sources-and-help.html).

!!! note "Aboard only"
    The **Photos** and **Chat** tabs appear only on the ship. The public web
    copy leaves them out.

!!! tip
    Your browser's back button works between tabs. If you go from
    **Underway** to **Casts**, the back button takes you back to
    **Underway**. You can also bookmark a tab: its address ends in
    `#tab/` and the tab's name, for example `#tab/casts`.

## Language, feedback and appearance

At the top right are four controls.

**Language**
:   Switches the page between **English** and **Français (Canada)**. French
    covers the main controls and the names of the measurements. Some panels,
    source material and wiki articles stay in English.

**Feedback**
:   Opens a small form to report a problem or suggest an improvement. See
    [Sending feedback](#sending-feedback) below.

**The theme** (it shows **Auto (system)** until you choose)
:   The look of the page: its colours and its style. See
    [Themes and text size](#themes-and-text-size) below.

**The text size** (it shows **Theme's size** until you choose)
:   Makes the writing bigger or smaller, whatever the theme.

### Themes and text size

The theme menu offers seven looks:

| Theme | What it looks like |
|---|---|
| **Auto (system)** | Follows your device: Claude light when your device is set to light, Claude dark when it is set to dark. |
| **Claude dark** | Light writing on a dark background. Easy on the eyes at night on the bridge. |
| **Claude light** | Dark writing on a light background. Best in a bright lab or for printing. |
| **Minimal dark** | Flat, plain and with bigger writing, on a dark background. Good for a screen read from across a room. |
| **Minimal light** | The same, on a light background. |
| **Navigator** | The look of a web page from 1996, for fun. |
| **Navigator dark** | The same, dark. |

The pictures in this manual use **Claude light**. Here is the same page in
**Claude dark**.

![The dashboard in the Claude dark theme](img/03-themes-dark.webp)

And here it is in **Navigator**.

![The dashboard in the Navigator theme](img/03-themes-navigator.webp)

The text-size menu has three choices:

- **Theme's size**: the size that comes with the theme (large for the
  Minimal themes, normal for the others);
- **Normal text**;
- **Large text**.

To change the theme or the text size:

1. Click the theme menu (it shows the theme in use, for example
   **Auto (system)**).
2. Click the theme you want. The whole page, the graphs and the map change
   colour at once.
3. To change the size of the writing, click the text-size menu next to it,
   and choose **Normal text** or **Large text**.

The dashboard remembers your choice in this browser.

### Sending feedback

The **Feedback** button opens a form titled **Feedback about this page**.

![The feedback form, empty](img/03-feedback.webp)

1. Click **Feedback**. The form opens over the page. Under its title, in
   grey, it names the tab and the time span you are looking at.
2. Type your message in **Your feedback**. Say what you did, what you
   expected and what happened instead.
3. If you want an answer, type your name in **Name (optional)**.
4. Click **Submit feedback**. The form says "Thank you—your feedback has
   been saved." Or click **Cancel** to close it without sending anything.

The form sends the tab and the view settings you had (the legs, the time
span, the colour of the track, where the map was, the size of your window)
along with your message, so that the person who reads it can see what
you saw.

!!! note "Aboard only"
    Feedback works only on the ship. The public web copy has no
    **Feedback** button.

## The title and the status line

Under the tab row is the title, **CCGS Amundsen — Underway**, and under it
the *status line*. The status line has three parts.

**The time now.** The date and time in ship time, for example
**16 October 2026 17:49 EDT**. On a narrow window the end of it may be cut
off with "…".

**last refresh.** The time the dashboard last brought its data up to date,
for example **last refresh 17:49**. Aboard, this happens every few minutes.

**How fresh the data are.** One of three things:

- **LIVE**, in green: new observations are arriving. The newest one is less
  than 15 minutes old. The ship's computer writes its measurements in
  batches every ten minutes, so the newest point is often a few minutes old
  even when all is well.
- **data** and an age, for example **data 2.3 h ago**: no new observations
  for that long. Something upstream may have stopped.
- **cruise ended** and a date, for example **cruise ended 2026-10-01**:
  the cruise is over and the dashboard is ashore.

### Where the data come from, and how old they are

Hold the mouse over **last refresh** (or click it) to open a list of the
dashboard's sources. Each row names a source and gives the time it last
delivered something, and how long ago that was.

![The list of sources under "last refresh", with the age of each](img/03-sources-list.webp)

The age is coloured: green when it is fresh, turning amber, then red when it
is three hours old or more. Some names are links: click one to open that
folder or page.

For example, aboard, **Data/FULL_CSV** is the folder of underway
measurements and **intranet live** is the bridge's live page. If the panels
look stuck, this list shows which source has gone quiet. Click anywhere else
to close the list.

### Messages in the status line

!!! note "Aboard only"
    If you have asked the Schedule tab to alert you on this page (see
    [The Schedule tab](schedule.html)), an alert appears in the status line
    with a bell, 🔔, for two minutes.

When you have hidden the schedule banner (see below), the status line ends
with the word **STATUS**. Click it to bring the banner back.

Under the status line, an extra line can appear when part of the page cannot
be updated, for example "Track update unavailable · retrying; displayed data
may be older". The dashboard keeps trying on its own. See [Opening the
dashboard](opening.html#when-the-page-does-not-load).

## The cruise report link

The pale button **Cruise reports: build yours →** opens the cruise report
builder, on a page of its own. There, each science team writes its part of
the cruise report from the ship's records. See [The cruise report
builder](cruise-report.html).

## Legs and time span

The last row of the header holds two controls: **Legs**, and the
**Show last** slider. Together they decide which part of the ship's record
every tab shows. They have a chapter of their own: [Choosing legs and a time
span](legs-and-time.html).

## The schedule banner

!!! note "Aboard only"
    The schedule banner appears only during a cruise, on the ship's
    dashboard. Ashore there is nothing happening "now", so it is left out.

Under the header, during a cruise, a banner shows the ship's operations
around the present moment, from the ship's schedule. It has three columns:

- **Last completed**: the operation that finished most recently, with its
  start and end times;
- **In progress**: what the ship is doing now, with the time left in its
  slot ("40 min left"), or how far it has run past it ("10 min over");
- **Coming up next**: the next operation, with its start and end times.

Each operation names its station and what is done there, for example a
rosette cast at a station. Hold the mouse over one to read the schedule's
comment about it.

On the right, two links open the same schedule elsewhere: **📅 Gcal** opens
it in Google Calendar, and **📆 ICS** lets you subscribe to it in your own
calendar program.

The banner takes room, so you can make it smaller:

1. Click the banner once. It folds into a thin strip that scrolls the three
   columns slowly from right to left, like a news ticker.
2. Click the strip. The banner disappears, and the status line ends with
   **STATUS**.
3. Click **STATUS**. The full banner comes back.

The [Schedule tab](schedule.html) explains the schedule in full.

## The map beside every tab

The map sits on the left whichever tab you open. Clicking on the map works
with the tab: on the **Casts** tab, for example, clicking a station adds its
cast to the graphs.

You choose how much of the page the map takes. There are three sizes:

- **Half map**: the map on the left, the tab's pane on the right. This is
  where the dashboard starts.
- **Full map**: the map takes the whole page, and the pane is hidden.
- **No map**: the map is hidden, and the pane takes the whole width. Useful
  for wide tables and graphs.

Here is the dashboard with **Full map**.

![The map at full size, with the pane hidden](img/03-map-full.webp)

There are two ways to change the size.

**With the Map button in the tab row.** Its symbol shows the size you are
in: ◧ for half, ■ for full, □ for none. Each click moves one step:
from half it goes to full, then back to half, then to none, then back to
half, and so on. Hold the mouse over it to see where the next click goes, for
example "Half map · click for Full map".

**With the buttons above the map**, at its top right:

- **—** hides the map (**No map**);
- **⤢** makes the map fill the page (**Full map**). In full map the button
  turns into **⤡**; click it to go back to half.

When the map is hidden, click **Map** in the tab row to bring it back.

!!! note
    Choosing a tab while the map is full brings the map back to half size,
    so that you can see the tab. When you reload the page, the map also
    starts at half size.

The map itself, its layers and its buttons are explained in [The
map](map.html).

## On a phone

On a narrow screen, such as a phone, the tab row becomes three menus:

- **◧ Map**: choose **□ No map**, **◧ Half map** or **■ Full map**;
- **☰ Science**: **Schedule**, **Stations**, **Underway** and **Casts**;
- **☰ Extras**: **Photos**, **Wiki**, **Chat**, **Games** and **Sources**.

Tap a menu to open it, then tap the tab you want.

![The Science menu open on a phone](img/03-phone-menu.webp)

**Language**, **Feedback**, the theme and the text size move to the footer,
at the very bottom of the page.

![The footer of the page on a phone: Language, Feedback, theme and text size](img/03-phone-footer.webp)

## When something goes wrong

If part of the page fails, a short message appears at the bottom of the
screen for a few seconds, starting with "Page error:". The rest of the page
keeps working. If you see one, please report it with **Feedback**, and say
what you were doing.

## In short

- The tabs choose what the right-hand pane shows; the map stays on the left.
- The status line tells you the time, when the data were last refreshed, and
  whether they are **LIVE**, how old they are, or that the **cruise ended**.
- Hold the mouse over **last refresh** to see each source and its age.
- **Map** in the tab row, and **—** and **⤢** above the map, change how much
  of the page the map takes.
- The theme and text-size menus change the look; the dashboard remembers
  them in your browser.
- Aboard, the schedule banner shows what was last done, what is in progress
  and what comes next. Click it to fold it, and again to hide it.
