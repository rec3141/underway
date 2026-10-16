# Sources, feedback and help

This chapter says where the dashboard's numbers come from, how to tell us
when something is wrong, and where to get help.

## The Sources tab

The last tab in the row at the top is a question mark, **?**. It opens the
**Sources** page.

1. Click **?** at the right end of the row of tabs.
2. The **Sources** page opens on the right-hand side.

![The Sources page: the links and credits at the top, then the table of sources](img/17-sources.webp)

### The links at the top

- **📖 The manual: how to use the dashboard, and how to run it** opens this
  manual in a new tab.
- **Ship intranet:** links to the ship's own web pages: the **Schedule**, the
  **Event log**, the **Rosette log**, the **Live** data page and a
  **Tutorial**. These work only aboard. On the public web copy this line
  instead reads **Web copy: published from the ship when its link is up; the
  chat, photo uploads, live feeds, alerts and feedback run only aboard**.
- **Calendars:** links to the ship's calendars: the **Amundsen Schedule
  (event log & operations)** and **Underway Updates (surprise episodes)**.
  You can add them to your own calendar app (see
  [The Schedule tab](schedule.html)).
- **Dashboard by Eric Collins · source**: who made the dashboard, and a link
  to its source code.
- **Data: Amundsen Science (ACSD)**: the ship's data come from Amundsen
  Science's underway system. **Archive: Amundsen Science / ArcticNet (CC BY
  4.0)** credits the data of past years.
- At the right, the dashboard's version and when this copy of the page was
  built, for example **underway v2.0.0 · built 2026-10-16 17:52 EDT**.

### The table of sources

The table says, for every value the dashboard shows, where it comes from, and
on which legs it exists.

- **panel**: the value as the dashboard names it, for example **SST (°C)**.
- **source column (Instrument — Variable)**: the instrument and the column of
  its data file, for example **TSG — Hull temperature (deg C)**. *derived*
  means the dashboard calculates the value from others (the surprise score,
  excess heat). **not found** means the value is expected but no data file
  has it.
- Then one column per leg: this year's legs as **26·1**, **26·2** … (year and
  leg number), and the past years as one column per year (**2002** …
  **2024**). Hold the mouse over a column's heading to see which legs it
  covers.

Each cell says whether that leg has the value:

| Mark | Meaning |
|---|---|
| **✓** | every leg in the column has it |
| **1/3** | some of the legs have it (here, one of three) |
| **–** (red) | none of them has it |
| **·** | calculated by the dashboard, not measured |

For example, the row **SST (°C)** with **–** under 2002 to 2004 and **✓**
under 2005 says that the sea surface temperature is in the archive from 2005
on.

The table is wide. On a phone, swipe it sideways; the page says so: **Swipe
the table sideways to see source columns and leg coverage.**

### The notes

Below the table, short notes explain how some values are made.

![The notes under the sources table](img/17-sources-notes.webp)

- **Surprise**: how the surprise score is calculated (see
  [The surprise score](surprise.html)).
- **LADCP currents**: where the current profiles come from and what their
  units are.
- **Zooming**: scroll on a graph to zoom; **Shift** + scroll zooms only the
  horizontal axis, **Ctrl** + scroll only the vertical one; double-click
  resets.
- **Excess heat (°C)**: how far the sea surface is above its freezing point.
- **Inputs**: how many daily data files across how many legs, and the latest.
- **Archive**: which past data are included, and how to cite them.
- **Record**: the first and last moment of data, and how many different
  columns have been seen.
- **Times**: times on the dashboard are ship time; downloaded TSV files are
  in UTC. A gap in a line is missing data; the dashboard never fills it in.
- **Basemap**, **names** and the **globe**: where the map's coastlines,
  depths and place names come from.

## Sending feedback

If something on the dashboard does not work, looks wrong, or could be better,
tell the people who look after it.

1. Go to the page you want to talk about, as you see it: the tab, the legs,
   the time span, the map.
2. Click **Feedback** at the top right of the page. On a phone, it is at the
   bottom of the page.
3. A box opens: **Feedback about this page**.

    ![The feedback box](img/17-feedback.webp)

4. Under **Your feedback**, write what you saw and what you expected. For
   example: "On the Casts tab, the Section graph for stations GF-1 to GF-3 is
   empty."
5. Under **Name (optional)**, give your name if you would like an answer.
6. Click **Submit feedback**.
7. The box says **Thank you—your feedback has been saved.** Click **Close**.

The page you were on and how it was set up are sent with your message, so the
reader can see what you saw. The small grey line under the description shows
what is included, for example **underway · 4mo** (the tab and the time
span).

If the box says **The request timed out** or shows an error, your text is
still there: click **Submit feedback** again. **Cancel** closes the box
without sending.

!!! note "Aboard only"
    Feedback is saved on the ship's server. On the public web copy, the
    **Feedback** button is not shown.

!!! tip
    Use **Feedback** for problems with the dashboard. For a mistake in a wiki
    page or a photo's description, flag the page instead with **⚑** (see
    [Flagging a page for review](wiki.html#flagging-a-page-for-review)).

## Getting help

- **This manual**: open it from the **Sources** tab (**?**), at any time.
  The [Glossary and questions](glossary.html) chapter explains the words and
  answers common questions.
- **Hold the mouse over a button.** Most buttons, pills and menus show a short
  explanation.
- **Ask in the chat** (aboard): someone in the **Mess** may know. The AI crew
  can answer questions about the schedule (`@capn`), the water (`@doc`) and
  the region's past (`@ada`). See [Chat and games](chat-and-games.html).
- **Send feedback** (aboard), as above. It reaches the person who looks after
  the dashboard.
- **For the cruise report**, follow
  [The cruise report builder](cruise-report.html) step by step.

When a page seems stuck or out of date, reload it first (the browser's reload
button, or **F5**). The dashboard keeps your settings in your browser, so a
reload loses nothing.
