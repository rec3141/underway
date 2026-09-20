# UI feedback backlog

## Reported in this session

- Nature map clicks should open the sidebar without filtering or recentering the map. Fixed in this change.

## From the feedback form

Read with `python -m dashboard.feedback` on 2026-09-18; all done in that change.

- Saved transects visible to everyone: they go to the server (`api/transects`,
  `db/transects.sqlite`) and every browser lists them; one saved earlier in a
  browser shows "this browser" with an ↑ button that shares it.
- Stations table: lat and lon are the columns after station.
- Map: station hover shows the position; a click on a station pins its box
  (position, distance from the ship by air and by sea, the sea route drawn)
  so it can be copied from, until the mark is clicked again or something
  else is; a click on open map drops a waypoint with the same box.
- Bottles table: station, lat and lon columns after cast.
- "Clear selection" on Casts and Stations also empties the filter box.

## Asked for on 2026-09-20

- A waypoint is made by a double click or a held press, not by a single click,
  which was catching ordinary navigation; the map's own double-click zoom is off.
- The mark's box reads: name, position, `by air:`, `by sea:`, then `depth:` or
  `elevation:` from GEBCO.
- A waypoint's name is editable; typing shows a Save button, and saving keeps it
  for everyone on the Stations tab, whose row removes it again.
- The map exports as KMZ with everything loaded, off screen included.

## Notes recovered from page flags

Copied before clearing the flags on 2026-09-11. These notes are pending; they are not all interface issues.

- `artifact/haig-thomas-1937-029` — jstor frontspiece instead of image
- `artifact/crocker-land-1913-032` — E-took-a-shoo needs a page
- `artifact/the-sea-ice-img-309` — several from this group are in norwegian
- `artifact/glaciers-and-ice-caps-p029` — feature: do one larger preview in the window before opening in new tab
- `artifact/rock-and-the-sverdrup-basin-426` — feature: photos should expand once to large preview before opening new window
- `artifact/vessels-143` — feature, when multiple features share a spot, the disambiguation page should show their chips including the preview as in the gallery
- `artifact/franklin-search-058` — feature: provide flagging on all artifacts including explore, narratives, etc.
- `artifact/inuit-oral-history-025` — bug: dashed titles still showing up in narrative text e.g. in print within days. What followed is told in inuit-oral-history and franklin-1845:

## Submitting and reading feedback

The footer's “Feedback about this page” opens a form with optional name and a
message. It includes the URL, active tab, wiki page, selected window and legs,
colour, map view, viewport size, and data generation at the time the form opens.
Submission failures keep the text available to retry; repeated requests with
the same submission ID produce one record.

Feedback is stored outside the published web root in
`UNDERWAY_DB_DIR/feedback.sqlite`. With the server's environment configured,
read or export it using `python -m dashboard.feedback`. Submissions are stored
locally; this feature does not send email or post GitHub issues.

Validation:

```sh
python -m pytest tests/test_feedback.py tests/test_serve.py -q
FEEDBACK_UI=1 node tests/refresh-browser.cjs /usr/bin/google-chrome
NATURE_UI=1 node tests/refresh-browser.cjs /usr/bin/google-chrome
```
