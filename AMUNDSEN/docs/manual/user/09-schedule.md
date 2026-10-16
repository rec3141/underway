# The Schedule tab

The **Schedule** tab tells you what the ship is doing, what it has done and
what comes next. It brings together three sources:

- **The operations schedule**: the plan for the coming hours, kept by the
  bridge and the chief scientist on the ship's intranet (the ship's internal
  web site).
- **The event log**: what was actually done, written down as it happens (a
  CTD cast deployed, a net recovered, a crew change), with time, station and
  position.
- **The calendars**: the **Amundsen Schedule** and **Underway Updates**
  Google calendars, which you can also open in your own calendar program.

Click **Schedule** in the row of tabs. Three buttons at the top choose the
view: **Today**, **Event Log** and **Calendar**. Beside them, the
**filter events** box narrows any view to the rows that contain what you
type, for example a station name (`GF-4`) or an operation (`Box Core`).

## Today

**Today** shows the operations schedule as the intranet publishes it.

![The Today view: the schedule's title, the whiteboard note, and a table of operations under a header row for each day](img/09-schedule-today.webp)

From top to bottom:

- **Operations schedule** and the leg, for example
  **Operations schedule 2026 Leg 04**. The **🔔** beside it is explained
  under [Alerts](#alerts).
- **The whiteboard** (📋): a short note from the bridge, such as the next
  port or a welcome.
- **The table**: one row per scheduled operation, grouped under a coloured
  header row for each day (ship time). Today's header says **· today**.
- **Links**: **Ship intranet:** opens the intranet's own pages (aboard
  only); **calendars:** opens the Google calendars.
- **🔔 Get alerts for scheduled operations**: the alert form (aboard only).

The table's columns are:

| Column | What it shows |
|---|---|
| **alerts** | Two bells to follow this operation (see [Alerts](#alerts)). |
| **time** | Start and end, in ship time. |
| **status** | Where the operation stands (see below). |
| **station** | The station. Click it to show the station on the map. |
| **operation** | What is to be done: CTD Rosette, Box Core, Transit, … |
| **dur.** | The planned duration, in hours. |
| **comment** | Notes from the schedule. |

Finished operations (completed or canceled) are folded away under each
day's header. Click **▸ Show 3 finished** to see them, and
**▾ Hide 3 finished** to fold them again.

### The status words

Each row has a small coloured label:

| Label | Meaning |
|---|---|
| **now** | In progress. The whole row is tinted. |
| **next** | The next operation to start. In bold. |
| **soon** | Marked "coming soon" by the bridge. |
| **later** | Scheduled for later. |
| **done** | Completed. |
| **canceled** | Canceled. |
| **was scheduled** | Was on the schedule once, but the bridge has since taken it off. Dashed outline. Seen in the **Event Log**. |
| **logged** | Not a plan but an entry of the event log: it happened. Seen in the **Event Log**. |

!!! note "Aboard only"
    The schedule is read from the ship's intranet every few minutes. Ashore,
    after the cruise, **Today** shows the last schedule read before the
    cruise ended, so its **now** may be weeks old.

!!! note "Aboard only"
    Aboard, a bar under the header shows **Last completed**, **In progress**
    (with the time left) and **Coming up next**, on every tab. Its
    **📅 Gcal** and **📆 ICS** links open the schedule calendar. See
    [A tour of the screen](tour.html).

## Event Log

**Event Log** shows what happened, day by day, newest first, with a timeline
above.

![The Event Log: a timeline of the last week, one row per kind of operation, and the log below it](img/09-schedule-eventlog.webp)

### The timeline

The **Timeline** panel has a row for each kind of operation (**CTD
Rosette**, **TM Rosette**, **Box Core**, **IKMT**, **Tucker Net**, …), with a
dot for each entry of the event log. Rare kinds are put together under
**other**. The bottom row, **scheduled**, shows the planned operations as
bars: bright for those on the schedule now, faint for those that were taken
off. A vertical line marks **now**.

The colour of a dot tells the kind of work: the same colour for a scheduled
CTD bar and a logged CTD dot.

The timeline follows the **Legs** and **Show last** choices in the header:
set **Show last** to **1 w** to see the last week of the chosen legs.

- Hold the mouse over a dot to read its time, station and activity.
- Click a dot: the log below scrolls to that entry and highlights it, and
  the map shows where it happened.
- Drag and scroll to move and zoom along time; **⟲** goes back to the span.

### The log

Under the timeline, the log lists the days, newest first. Each day's header
row says how many events were logged and how many operations scheduled, and
which leg it belongs to, for example
**2026-09-28 · 15 events · 0 scheduled · 12 formerly · 2026 Leg 3**.

- A **logged** row is an entry of the event log, with its time, station,
  activity and event (**Deployment**, **Recovery**, …), the sample label if
  there is one (such as `AMD2603-255`), and the depth in the **depth**
  column.
- Click a logged row to show where it happened on the map.
- A logged CTD cast has a **view data** button. Click it to open that cast
  on the [Casts tab](casts.html).
- The operations that were scheduled for that day but taken off the
  schedule are folded under **▸ Show 12 formerly scheduled**.

The log shows up to the last 60 days of the chosen legs.

## Calendar

**Calendar** shows everything as a calendar: the Google calendars, the
intranet schedule and the pump stops. Two buttons at its top left choose
**3 days** or **Month**.

The bar along the top holds:

- **‹** and **›**: the previous and next three days, or month.
- **today**: back to today.
- **The key**: each source with its colour and its number of entries:
  **Amundsen Schedule**, **Underway Updates** and **intranet schedule**.
  **(cached)** after a name means the dashboard shows its last saved copy of
  that calendar.

### Month

**Month** shows a grid of the month, Monday first. Each day shows its first
six entries, then **+12 more**.

![September 2026 in the Month view: each day lists its first entries, with a coloured edge for the calendar it comes from](img/09-schedule-month.webp)

The colour on the left edge of an entry tells its source:

- **Amundsen Schedule**: the event log (entries starting `[EventLog]`) and
  the planned operations, as published to Google Calendar.
- **Underway Updates**: surprising moments in the underway record, as
  "Surprise 3.1 (3 h)" (see [The surprise score](surprise.html)), and stops
  of the TSG intake pump.
- **intranet schedule**: the operations read directly from the intranet,
  marked **scheduled** or **was scheduled**.

Click a day's number to open that day in the **3 days** view.

### 3 days

**3 days** shows three days side by side, the chosen day in the middle, with
the hours down the left in ship time. Each entry is a block as long as the
operation. Entries that overlap share the column. Today has a line at the
current time.

![The 3 days view around 10 September 2026, with the details of one operation open at the top](img/09-schedule-days.webp)

Click any entry, in either view, to see its details at the top: the
calendar it comes from, its title, its start and end with its length, and
its description. When the entry has a position, a **show on map** button
moves the map there. **✕** closes the details.

## Calendar links

You can follow the ship's calendars in your own calendar program (Google
Calendar, Outlook, Apple Calendar). The links are under the Today table
(**calendars:**) and at the foot of the Calendar view.

- **Amundsen Schedule (event log & operations)**: what is planned and what
  was done.
- **Underway Updates (surprise episodes)**: surprising moments and pump
  stops.

A link opens the calendar in Google Calendar, where you can add it to your
own. Aboard, the **📆 ICS** link in the schedule bar gives the calendar as an
ICS feed: copy the link and add it to your calendar program as a
subscription ("add calendar from URL").

!!! note
    The ship's internet link may be slow or down, and Google Calendar is
    then out of reach aboard. The Schedule tab itself keeps working: it
    reads its own copy.

## Alerts

!!! note "Aboard only"
    Alerts are sent by the dashboard on the ship. The bells and the form are
    not on the public web copy.

You can be told when an operation is about to start, starts, finishes or
moves. There are three ways to be told:

- **this browser's header bar**: a message in the dashboard's header, in
  this browser, while the page is open. The browser may also ask whether
  the dashboard may show notifications.
- **email**: a message to your address. Every alert email carries an
  unsubscribe link.
- **Telegram**: a message from the ship's Telegram bot.

### Following one operation

Each row of the schedule has two bells in its **alerts** column:

- **🔔** follows *this* operation: a heads-up 15 minutes before it starts,
  and a message at every change to it.
- **📢** follows *every* operation of this kind at any station, for example
  every CTD Rosette (every transit counts as one kind, whatever its
  destination).

1. Click a bell. A small menu opens.

    ![The menu of a bell: show in this browser's header bar, email, or Telegram](img/09-schedule-bell.webp)

2. Choose **🖥 show in this browser's header bar**, or **✉ email**, or
   **✈ Telegram**. If the dashboard does not know your email address yet,
   **✉ email me… (enter an address below)** opens the alert form so you can
   type it.
3. The bell lights up. Click it again and choose **stop showing** or
   **stop emailing** to stop.

The **🔔** beside the schedule's title follows **changes to the schedule**:
a message when a future operation is added, taken off or moved. It sends no
reminders.

### The alert form

Click **🔔 Get alerts for scheduled operations** under the Today table to
open the form.

![The alert form: via, only operations matching, warn ahead, the kinds of event, and subscribe](img/09-schedule-alerts.webp)

1. **via**: **this browser's header bar** or **email**. For email, type
   your address in the **email** box that appears.
2. **only operations matching**: leave it blank for every operation, or type
   a station or an operation, for example `CardS-3` or `CTD`.
3. **warn … ahead**: how long before the start you want the first message:
   **15 min**, **30 min**, **1 h** or **2 h**.
4. Tick the events you want: **starting soon**, **started**, **finished**,
   **time changed**.
5. Click **subscribe**. The line beside the button confirms what you
   subscribed to.

With Telegram, send `/start` to the bot named in the form. `/only CardS-3`
limits it to one station, `/lead 60` sets the warning to 60 minutes, and
`/changes` sends only changes to the schedule.

All times in alerts are ship time.

## In short

- **Today**: the operations schedule, with the status of each operation.
- **Event Log**: a timeline and a day-by-day list of what was done; **view
  data** opens a CTD cast.
- **Calendar**: **Month** or **3 days**; click an entry for its details.
- Click a station name to find it on the map.
- Aboard, the bells and the alert form tell you when operations start or
  change.
