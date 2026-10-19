# The cruise report builder

Every science team on the Amundsen writes a cruise report before the end of
its leg. The report follows Amundsen Science's 2026 Word template: a title, the
people, an introduction, the methods, the results, references and
recommendations.

The **cruise report builder** writes most of the dull parts for you. It reads
the ship's own records: the event log, the rosette sheets, the CTD casts and
the underway data. From them it makes your list of stations, a paragraph about
the weather and the ice at each one, tables, a station map and graphs. You add
your own words, and you download a finished Word file (`.docx`) in the
template.

This chapter takes you through it from start to finish. Plan on an hour the
first time. You can stop at any point and come back later.

!!! note "Aboard only"
    The builder runs on the ship's server and reads the ship's shared drive.
    It works only on the ship's network. The public web copy of the dashboard
    does not have it, nor its button.

## Opening the builder

1. Open the dashboard (see [Opening the dashboard](opening.html)).
2. Under the ship's name, at the top left, click the amber button
   **Cruise reports: build yours →**.
3. The builder opens in the same window. You can also type its address
   directly: `http://underway.local/report/`.

The page is one long form in six numbered steps. You fill them in from top to
bottom. Here is the top of the page.

![The builder's header: the Draft menu, Save draft, Download .docx, the six steps and the word count](img/16-header.webp)

The header holds the controls for the whole report:

- **Draft**: a menu of the reports saved on the ship's server. Choose one to
  open it (see [Saving, sharing and opening drafts](#saving-sharing-and-opening-drafts)).
- **Save draft**: saves your report on the ship's server, so you or a teammate
  can open it again from any computer aboard.
- **Download .docx**: builds the Word file and downloads it.

Below it is a bar that stays at the top of the window as you scroll:

- **1 · Team** to **6 · Text** jump to each step.
- The **word count**, for example **1,977 / 3,000 words**. The template allows
  3000 words. The count includes your own text and the generated conditions
  paragraph. Tables and captions are not counted. The count turns red when you
  go over.
- **saved …** says when the draft was last saved on the server, for example
  **saved 12 min ago**. It reads **not saved on the server** until you save.
  It turns red when the last save is more than half an hour old.

!!! tip
    Your browser keeps every change as you type, even if you never press
    **Save draft**. If you close the tab and open the builder again on the
    same computer, your report is still there. Saving on the server is what
    lets you open it from another computer, or share it with your team.

## Step 1 · Team and leg

The first step says who you are and which leg you sailed.

![Step 1: the leg, team name, project title, leaders and participants](img/16-team.webp)

1. In **Leg**, choose your leg, for example **2026 Leg 3**. The builder opens
   on the most recent leg, or on the leg of the report you last worked on.
   Changing the leg loads that leg's event log; it takes a few seconds.
2. In **Team name**, type a short name for your team, often the lead's
   surname, for example `Tremblay`. It becomes part of the file name:
   **Cruise Report_Tremblay.docx**.
3. In **Brief project title**, type your project's title. It becomes the
   title of the report.
4. Under **Project leaders**, fill in each leader's **Name**, **Email** and
   **Affiliation (institute or university)**. Click **+ Add leader** for
   another row. Click the red **×** at the end of a row to remove it.
5. Under **Cruise participants on this leg**, fill in the **Name** and
   **Affiliation** of each team member who sailed. Click **+ Add participant**
   for more rows.

In the Word file, the affiliations become numbered footnotes after the names,
as the template asks. People who share an affiliation share its number.

!!! warning
    If you change the leg after you have ticked operations, the builder asks
    **Switching legs clears the ticked operations. Continue?** Answer
    **Cancel** to keep your work.

## Step 2 · What you did

This is the most important step. You tell the builder which of the leg's
operations were yours. Everything else (the conditions paragraph, the tables,
the figures) is built from that choice.

An **operation** is one line of the ship's event log: one CTD cast, one box
core, one net tow, one helicopter flight. Each has an **event label** such as
`AMD2603-010`: the leg (`2603` is 2026, Leg 3) and a running number.

There are two ways to say what was yours, and you can use both:

- **Your own records.** Bring in the logbook or spreadsheet your team kept on
  board. The builder matches each of its rows to an operation. This is the
  best way: it picks exactly the casts you sampled.
- **Instruments.** Tick an instrument, for example **Box core**, and every
  operation of that instrument on the leg is yours. This is quick when your
  team ran an instrument on its own.

Then you check the list of operations and the list of rosette bottles, and
untick anything that was not yours.

### Bring in a paper logbook: Digitize logbook photos

If your team wrote its records by hand, you can photograph the pages. A
computer vision model reads the photo and turns every table on the page into
a spreadsheet you can correct.

1. Click **Digitize logbook photos** to open the box.
2. Click **Choose Files** and pick one or more photos of logbook pages. Any
   orientation is fine.
3. Each photo appears as a small card marked **ready**. Under each card:
    - **↻** turns the photo a quarter turn, if it is sideways.
    - **×** removes it.
    - Click the picture itself to see it large.

4. Click **Transcribe**. Each photo is sent to the ship's server; its card
   reads **sending…**, then **queued**, then **transcribing** with a running
   clock.
5. Wait. A page takes from about 30 seconds to 2 minutes, and the server
   works on two at a time. You can close the page and come back: the work
   carries on on the server.
6. When a page is finished, its card reads, for example,
   **done · 34 rows · 20 to check**. Click **results ↓** to jump to its
   tables.

Here is a transcribed page. Each cell is coloured by how sure the model is
that it read the cell right and put it in the right place.

![A transcribed logbook page: the photos along the top, the colour key, and one table with its Matched to column](img/16-digitize.webp)

The colours, from the key **odds the cell is right**:

| Colour | Meaning |
|---|---|
| green | **near certain** |
| light green | **very likely** |
| yellow | **likely** |
| orange | **even** (as likely wrong as right) |
| red | **a guess** |

The model copies what is written. It does not correct a value or invent a
missing one. A smudged number is its best reading, coloured red or orange.

To check and correct a table:

1. Compare the yellow, orange and red cells with your logbook. Click
   **🔍** on the page's card, or the small photo beside the table, to see the
   photo large; click the photo to zoom in, and press **Esc** to close it.
2. Click a cell and type the right value. Press **Enter** or click elsewhere.
   The cell turns green: you have checked it.
3. The column headers can be corrected the same way.
4. **+ row** adds an empty row at the bottom; **+ column** adds an empty
   column at the right.

Each page's header line says how many tables and cells it has, how many are
**to check (likely or less)**, which model read it, how long it took and what
it cost. Two buttons sit at the right:

- **Transcribe again** sends the same photo to the model again. It replaces
  the page's tables, and your corrections are lost.
- **Remove** takes the page out of this report.

Under each table:

- **26 of 34 rows matched** says how many rows the builder could tie to an
  operation (see [How rows are matched](#how-rows-are-matched-to-operations)).
- **Times are** **UTC** or **ship time**: what clock your logbook used. Choose
  the right one, or rows will match the wrong operations.
- **TSV** downloads this table as a text file for a spreadsheet.
- **Use as a log** turns the table into one of **Your logs** (below), ticked,
  so the operations it matches become yours.
- **carry station, cast and date down into blank and ditto (″ ↓) cells**:
  logbooks often write the station once and leave the rows below blank, or
  write ″ or an arrow. With this ticked, the blank and ditto cells take the
  value from the row above. It is ticked unless you untick it.

At the top of the page, **Download all sheets (.xlsx)** downloads every
transcribed page as one Excel file, one sheet per table, with the same
colours. **download times** chooses whether the dates and times in the
downloads are **as written**, or converted to **UTC** or **ship time**.

Any notes written outside the tables appear under **Notes outside the
tables**.

!!! note
    The photos go to an online model (through the ship's internet link), and
    each page costs a few cents. This needs the ship's satellite link to be up.

### Bring in a spreadsheet: Import your team's logsheet

If your team kept its records in Excel or a text file, import it directly.

1. Click **Import your team's logsheet** to open the box.
2. Click **Choose File** and pick your file: `.xlsx`, `.xls`, `.csv`, `.tsv`
   or `.txt`.
3. The builder reads it. Each sheet of a workbook becomes a log of its own,
   named after the sheet and the file, for example
   **EDNA · Tremblay_Logbook.xlsx**.

Here is an imported workbook.

![An imported logsheet: the file's header line, then the STATION sheet with its holds row and Matched to column](img/16-logsheet.webp)

The file's header line shows **395 of 462 rows matched**, and:

- **XLSX** downloads every sheet of the log, with the cells you corrected
  filled green.
- **download times**: **as written**, **UTC** or **ship time**, as for the
  photos.
- **Remove** takes the file out of the report.

Each sheet then shows its table. It works like a transcribed table:

- Click any cell or column name to correct it. Corrected cells turn green.
  Your uploaded file itself is kept as you sent it; the corrections live in
  the builder.
- **+ row**, **+ column** and **TSV** work as above.
- **Times are** **UTC** or **ship time (America/Toronto)**: the clock your
  sheet used.

### Tell the builder what each column holds

Under each column name is a menu: the **holds →** row. It says what the column
holds, so the builder can match the rows. The builder guesses from the
column's name and values; check its guesses. The choices are:

| Choice | Example of what the column holds |
|---|---|
| **Event label** | `AMD2603-010` |
| **Station** | `CardS-3`, `GF-1` |
| **Date & time** | `2026-09-06 13:21` in one cell |
| **Date** | `9/06/26`, `6 Sep`, `DOY 249` |
| **Time** | `14:20`, `14h20`, `1420`, `2:20 PM` |
| **Latitude**, **Longitude** | `76.3757`, `-89.2777` |
| **Depth** | the sampling depth in metres |
| **Bottle** | the rosette bottle number, `1` to `24` |
| **Cast** | the CTD cast number |
| **Sample ID** | your own sample code, for example `FT1` |
| **—** | nothing the builder needs |

Each choice belongs to one column at a time. Every change matches the rows
again at once.

The builder reads dates and times the way logbooks write them: day and month
in either order (it picks the one that falls in your leg), no year, the day of
the year, Excel's own dates, and times as `14:20`, `14h20`, `1420` or
`2:20 PM`.

### How rows are matched to operations

The first column of every table, **Matched to**, shows the operation each row
belongs to, and how the match was made, in small type. The builder tries, in
this order:

1. **label**: the row has an event label, for example `AMD2603-010`.
2. **cast**: the row has a cast number, which the leg's CTD logbook ties to an
   event label.
3. **station+time**: the same station, at the closest operation within 3
   hours. A time with no date is looked for on the days the ship was at that
   station.
4. **station+date**: the same station on the same day, when there is no
   usable time.
5. **time+position**: the closest operation within 3 hours and 5 km, from the
   row's latitude and longitude.
6. **station**: no usable date, but the ship visited that station only once.

A row that matches nothing shows a search box: **unmatched: search station,
label, activity, date…**. To match it by hand:

1. Click in the box and type part of a station name, an event label, an
   activity or a date, for example `GF-3` or `box core`.
2. Choose the operation from the list. The row now shows it, marked
   **by hand**.

To change a match:

- Click the **×** after an automatic match to remove it. The row then offers
  the search box, so you can pick another operation, or leave it unmatched.
- Click the **×** after a match you made by hand to go back to the automatic
  match.

!!! tip
    Flow-through samples from the ship's seawater intake are taken while
    under way, so they match no operation. That is fine: they still go into
    tables made from your log, and onto the map, at the ship's position at
    the time of the sample.

### Select what's yours

Now choose which logs and instruments are yours, in the **Select what's
yours** box.

![Select what's yours: the logs and instruments as tick boxes, and the Operations table](img/16-select.webp)

- **Your logs** lists every imported sheet, with the number of operations it
  matches (**40 ops**), and every transcribed table you have not yet used (with
  its number of rows). Tick a log to make every operation it matches yours.
  Ticking a transcribed table here does the same as **Use as a log**.
- **Instruments** lists the instrument groups of the leg, with the number of
  operations of each, for example **CTD-Rosette 65**, **Box core 45**,
  **Plankton nets (Monster, Hydrobios, Tucker) 32**, **Sea-ice station 4**,
  **Helicopter 10**. Tick one to make all its operations yours.

The instruments group the bridge's hand-typed activity names, so that
"TM Rosette", "TM-Rosette A2" and "TM-CTD" all count as **Trace-metal
rosette**.

### The Operations table

Below is the **Operations** table: the operations that are yours, with a count
such as **47 ticked**. Each row has a tick box, the **Station**, the event
**Label**, the **Operation** (as the bridge typed it), the **Instrument**
group, the **Start (UTC)**, the **Duration**, the bottom **Depth (m)**, and
**Logsheet rows**: how many rows of your ticked logs match it.

- Untick a row to leave that operation out. It stays in the list, greyed, so
  you can tick it again.
- The tick box in the header ticks or unticks every row listed.
- **Filter by station, label, activity, date or comment** narrows the list.
  It also finds operations that are not yours yet, so you can tick one by
  hand. For example, type `helicopter` to find the helicopter flights.
- When you have ticked or unticked rows by hand, the count says so, for
  example **47 ticked (2 by hand · reset)**. Click **reset** to forget your
  own ticks and follow your logs and instruments only.

Your own ticks and unticks always win over the logs and instruments. When you
correct a log and its matches change, the operations follow.

### Bottles sampled

The **Bottles sampled** box lists the rosette bottles on the casts you ticked,
and which of them were yours. A **rosette** is the frame of 24 water bottles
lowered with the CTD; each bottle is closed ("fired") at a chosen depth.

![Bottles sampled: the rosette-sheet team names, the logs as bottle sources, and the bottle table](img/16-bottles.webp)

A bottle is yours when:

- a team name you ticked under **Rosette-sheet columns — which are your
  team?** drew water from it, or
- a log you ticked here lists it in a column marked **Bottle**.

With neither, every bottle on your casts counts as yours.

1. Open **Rosette-sheet columns — which are your team?**. These are the team
   names typed by hand on each cast's rosette sheet. Slips such as a capital
   letter or one letter off are merged under one name; hold the mouse over a
   name to see the spellings it includes (**+3 spellings**). The number is how
   many casts that name drew water from. Tick every name that is your team.
2. Under **Your logs**, tick each log that lists your bottles. A log needs a
   column marked **Bottle** in its **holds →** row; otherwise it says
   **no Bottle column**.
3. Check the bottle table. **Yours by** says why each bottle counts as yours.
   Untick a bottle that was not yours, or tick one by hand. Your own ticks
   always stand.
4. Type a **Volume (L)** or a **Note** in a bottle's row if you like. They go
   into the report's bottle tables.

The count at the top says **149 yours**. Tick **All bottles** to list every
bottle on your casts, the ones not yours greyed, so you can tick one.

## Step 3 · Conditions at your stations

The builder writes a paragraph about the conditions where you sampled: the
dates and stations, the depth, the air temperature and wind, the surface
water, the ice and the light. It goes into your **Methodology** section,
under the heading **Conditions at the stations**.

![Step 3: the three choices and a previewed summary paragraph](img/16-conditions.webp)

1. Choose one of:
    - **One summary paragraph** (recommended: the report is limited to 3000
      words).
    - **A paragraph per station visit**: long, but complete.
    - **No narrative**.

2. Click **Preview narrative** to read it. It changes as your selection
   changes.

For example: "The team's 38 CTD-rosette casts, 4 ice stations, 2 helicopter
flights … took place between 6 September and 28 September 2026, over 41
station visits … Water depth ranged from 8 to 841 m. On arrival on station,
air temperature ranged from −5.9 to 0.3 °C …"

Where the values come from:

- The conditions are taken when the ship **arrived on station**: when its
  speed last fell below 1 knot before the first deployment. Every operation of
  a visit shares those conditions.
- Weather, sea surface temperature and salinity are the ship's underway record
  averaged over the two minutes after arrival. Where the record has nothing,
  the bridge's reading in the event log is used.
- The depth is the multibeam's where it has one.
- The ice is the bridge's reading, else what the rosette operator saw from
  deck (on the rosette sheet), else the Canadian Ice Service chart for that
  date. The ice camera's own values are separate columns for step 4.
- Something nobody recorded is said to be missing. The builder never fills a
  gap with a guess.

For a value at each operation, add a conditions table in step 4.

## Step 4 · Tables

Tables go into the Word file with numbered captions. Start from a ready-made
table and change it as you like.

1. Click one of the buttons:
    - **+ Station conditions table**: one row per operation, with the time,
      position, depth, air temperature, wind, sea surface temperature and
      salinity, and ice.
    - **+ Station list (position, depth, time)**: one row per operation, with
      its start and end, position and depth.
    - **+ Bottles sampled**: one row per bottle of yours, with its depth,
      temperature and salinity, and a column for each team name you ticked.
    - **+ Empty table**: no columns yet.

2. The new table appears at the top of the list.

Here is a table, with its preview open.

![A table: caption, rows, section, the column pills, the column tick boxes and a preview](img/16-tables.webp)

Each table has:

- **Caption**: the text under "Table 1" in Word. Type your own.
- **Rows**: what each row is.
    - **One row per operation**.
    - **One row per rosette bottle**: your bottles.
    - **All logs**: every row of your ticked logs together.
    - **Log · …**: the rows of one log, for example **Log · EDNA ·
      Tremblay_Logbook.xlsx (221 rows)**.
- **Section**: where it goes in the report: **Methodology**, **Preliminary
  Results** or **Introduction**.
- **Preview**: shows the table as it will be, with its number of rows.
- **↑** and **↓**: move the table earlier or later in the report. Tables go
  into Word in the order of this list.
- **Remove**: deletes the table.

**Columns in order** shows the table's columns as pills. Drag a pill sideways
to move the column. Click its **×** to remove it.

Below, the tick boxes list every column you can add, in groups:

- **Operation**: station, event label, operation, start and end times,
  arrival on station, duration, position, bottom depth and where it came from,
  **Bottles sampled (team)**, **Volume drawn (team) (L)**, **Logsheet rows**.
- **Conditions**: air temperature, wind, pressure, humidity, visibility, sea
  surface temperature (**SST**) and salinity (**SSS**), sea state,
  fluorescence, oxygen, ice and its source, the ice camera's values, the
  rosette sheet's cloud and sea-state codes, the sun's elevation, notes.
- For bottle rows, **Bottle**: the bottle number, target depth, trip pressure
  and depth, trip time, the values the CTD recorded when the bottle fired
  (temperature, salinity, oxygen, fluorescence …), % light, volume, note and
  the rosette sheet's comment; and
  **Drawn by team (L)**, one column per team name you ticked.
- For log rows, the **Rosette bottle** matched from the row's cast and bottle;
  **Underway · at the row's own date and time** (the ship's position and
  surface readings when the sample was taken: useful for flow-through
  samples); **what each column holds** (the station, cast, bottle … as you
  marked them); and the log's own columns.

All tables are joined through the event label, so any row can show any
operation's values. A bottle row can show its station's air temperature; a
log row can show its bottom depth.

!!! tip
    A table with more than 8 columns is put on landscape pages of its own in
    Word, so it fits.

**Example**: a table of your eDNA filters. Choose **Rows** → **Log · EDNA ·
…**. Tick **Station**, **Event label** and **Start (UTC)** under
**Operation**, **Depth (Rosette)** under **Rosette bottle**, and your own
**FILTER** column. Click **Preview**.

## Step 5 · Figures

The builder draws four kinds of figure from the ship's data.

![Step 5: the add buttons and three figures: the underway record, a T–S diagram and a station map](img/16-figures.webp)

1. Click a button to add a figure:
    - **+ Station map**
    - **+ CTD profiles**
    - **+ T–S diagram**
    - **+ Underway record**

2. The figure is drawn after a few seconds. Click it to see it large.
3. Change its options (below). It redraws by itself; **Redraw** draws it
   again by hand.
4. Edit its **Caption** if you like.
5. Choose its **Section**. Maps go into **Methodology** and graphs into
   **Preliminary Results** unless you change it.
6. **Remove** deletes it.

Figures redraw when your selection of operations changes.

### Station map

The ship's track over the leg, with your operations marked by instrument.

- **my operations**: show your ticked operations.
- One tick box per ticked log: plot that log's rows. A row goes at its own
  latitude and longitude, else at the operation it matches, else where the
  ship was at its date and time. **sample IDs** labels the points with the
  log's sample codes (the log needs a column marked **Sample ID**).
- **station names**: label the stations.
- **Colour track by**: **Date**, or any panel of the Underway tab, for
  example **Salinity (PSU)** to show the fresher water. Grey dashes mark where
  that value was not recorded.

### CTD profiles

Your casts' downcast profiles, one graph per variable, the casts coloured
from first to last.

![The CTD profiles figure and its options](img/16-fig-profiles.webp)

- Tick the variables: **Temperature**, **Salinity**, **Fluorescence**,
  **Oxygen**, **Transmission**, **PAR**, **CDOM**, **Sigma-t**.
- **max depth**: cut the graphs at this depth, in metres. Leave it empty for
  the full depth.
- **compressed depth scale**: a square-root depth axis, as on the
  dashboard's Casts tab, which opens up the upper water column.

### T–S diagram

Temperature against salinity for your casts.

- **Colour by**: **Pressure (dbar)**, **Cast time**, **Latitude (°N)**,
  **Station**, or any other variable the casts measured, such as
  **Oxygen (µM)**. The ready-made caption names the colour, for example
  "…coloured by cast time." A caption you have written yourself is left as
  it is.
- **mark the bottles sampled (×)**: marks your bottles with a small ×.

### Underway record

The ship's surface data over your sampling period, as on the
[Underway tab](underway.html), with grey lines at your operations.

- Tick the panels you want, in their groups: **Surprise**, **Lab**, **Met
  Station**, **Bridge** and **Ice camera**. A panel marked **(hourly)** is
  drawn from hourly values.
- More than 5 panels are split over several images of up to 5, each with its
  own caption in Word.
- **Times in** (beside the add buttons) sets the time axis to **UTC** or
  **ship time**.

!!! tip
    Photos and plots of your own are best added in Word after you download.
    The builder's captions are numbered with Word fields, so your own tables
    and figures can go between them and the numbers stay right (Word updates
    them when you print, or press **F9**).

## Step 6 · Your text

The last step is your own writing, one box per section of the template. Each
box shows a hint of what goes in it until you type.

![Step 6: the text boxes, one per section](img/16-text.webp)

- **Introduction & Objectives**: why the project matters, and its aims on
  board.
- **Methodology**: the field work and how you collected and analysed your
  samples. The conditions paragraph, and the tables and figures you put in
  this section, follow your text.
- **Preliminary Results**: early results, and whether you met your
  objectives.
- **References**: one APA citation per line.
- **Recommendations**: feedback on the ship's equipment, life on board or
  anything else about the cruise.
- **Published & pending work — Publications**, **— Presentations** and
  **— In progress**: work that uses data from the ship.

Leave a blank line between paragraphs: each becomes its own paragraph in
Word. Watch the word count in the bar at the top.

## Downloading the Word file

At the bottom of the page, a reminder says where to put your report.

![The closing box: where to save the file, Save draft and Download .docx](img/16-closing.webp)

1. Click **Download .docx** (here or at the top of the page). The button reads
   **Building…** for a few seconds to a minute.
2. Your browser saves **Cruise Report_Tremblay.docx** (your team name).
3. Open it in Word. Check it, add your photos and your own plots.
4. Save it, under that name, in the folder the page names, for example
   `\\10.0.0.xxx\share\2026\Leg 3\Cruise Report`, before the end of your leg.
5. Fill in the **Amundsen user survey** (the link in the box).

The Word file is the template, filled in: your title, the leaders with their
emails, the participants, the affiliations as numbered footnotes, your text in
each section, the conditions paragraph under **Methodology**, and your tables
and figures with numbered captions in the sections you chose. The template's
logo, styles and survey section stay as they are.

!!! note
    If no operations are ticked, the builder warns: **No operations are
    ticked, so the report has no generated narrative, tables or figures.**
    You still get the file with your text.

## Saving, sharing and opening drafts

A **draft** is your report as it stands, saved on the ship's server.

To save:

1. Click **Save draft** (at the top or the bottom of the page).
2. A message says **Draft saved as "Tremblay-2026_LEG_03". Anyone on the ship
   network can open it from the Draft list.** The name is your team name and
   the leg.
3. The bar at the top now reads **saved just now**.

To open a draft, on any computer aboard:

1. Open the builder.
2. Choose the draft from the **Draft** menu at the top. Each entry shows its
   name and when it was last saved.
3. The report loads, with its logs, photos, tables and figures. This takes a
   few seconds. The **Draft** menu then names the draft you are working on.

!!! warning
    A draft has one copy on the server. If two teammates edit the same draft
    on two computers, the last one to click **Save draft** wins. Take turns,
    and open the draft again before you start.

The photos you transcribed and the logsheets you imported are kept on the
server with the draft. Your browser also remembers the report you last worked
on, so a page reload never loses it.

## A worked example

Here is the whole job for an eDNA team who sampled water from the rosette and
the seawater intake on Leg 3, and kept an Excel logbook with sheets STATION,
EDNA and FLOWTHRU.

1. **Step 1**: Leg **2026 Leg 3**; team name `Tremblay`; title, leaders and
   participants.
2. **Step 2**: import `Tremblay_Logbook.xlsx`. Check each sheet's **holds →**
   row: on EDNA, OPERATION is **Event label**, STATION **Station**, CAST
   **Cast**, BOTTLE **Bottle**, DEPTH **Depth**; on FLOWTHRU, FTID is
   **Sample ID**. Set **Times are** to the clock you used.
3. Search for the few unmatched STATION rows and match them by hand.
4. Under **Select what's yours**, tick STATION and EDNA. Leave the
   instruments unticked. Untick any operation in the list that was not
   yours.
5. Under **Bottles sampled**, tick EDNA as a bottle source. Check the bottle
   list.
6. **Step 3**: **One summary paragraph**. Click **Preview narrative**.
7. **Step 4**: add **+ Station list**; add a table with **Rows** →
   **Log · FLOWTHRU** and the columns **Row time (UTC)**, **Latitude (at row
   time)**, **SST (at row time)** and **SSS (at row time)**.
8. **Step 5**: add a **Station map** coloured by **Salinity (PSU)**, with the
   FLOWTHRU log and its **sample IDs** shown; a **T–S diagram** with your
   bottles marked.
9. **Step 6**: write the introduction, methods and results.
10. Click **Save draft**, then **Download .docx**. Open it in Word, check it,
    and save it to the share.

## In short

- Open the builder from the dashboard's **Cruise reports: build yours →**
  button, aboard.
- Step 1: leg, team, people.
- Step 2: bring in your logbook (photos or spreadsheet), mark what each column
  holds, tick your logs and instruments, check the operations and the bottles.
- Steps 3 to 5: choose the conditions paragraph, add tables and figures.
- Step 6: write your text, under 3000 words.
- **Save draft** often; **Download .docx**; save it to the share before the
  leg ends.
