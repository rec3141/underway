# UI feedback backlog

## Feedback review — 2026-09-24

Read the live `feedback.sqlite` without changing submissions: 14 records,
13 distinct messages (the ECO/CDOM report was submitted twice).
The first implementation batch is merged to `master` for deployment.
The user deferred laptop map height, skipped the bathymetry legend, TRS
follow-up and right-click tooltip toggle, and reserved the two history
requests for another session.

| Submitted | Request | Status |
| --- | --- | --- |
| Sep 11 | Feedback lists an inactive wiki page | Already fixed: wiki context is included only on the Wiki tab. |
| Sep 16 | Share TRS transects | New transects are shared automatically. Browser-local transects require their owner's ↑ share button. Further work skipped by user. |
| Sep 18 | Station coordinates after station; map coordinates | Already implemented in the table and station hover/pinned boxes. |
| Sep 18 | Bottle coordinates; clear search; waypoint position/distances | Already implemented. Waypoints use double click or held press. |
| Sep 19 | Sea before air; km (nmi); remove removal hint | Format/hint already addressed; this branch puts sea before air. |
| Sep 19 | Waypoint depth/altitude | GEBCO depth/elevation lookup runs independently so depth can appear before the sea route finishes. |
| Sep 19 | Right-click tooltip toggle | Skipped by user. |
| Sep 22 | Expand Devon Island Expedition history | Deferred to another session in the separate arctic-history repository. |
| Sep 22 | Devon/Axel Heiberg Mars analog history | Deferred to another session in the separate arctic-history repository. |
| Sep 22 | Bathymetry colour legend; taller expanded laptop map | Legend skipped by user; laptop height deferred. |
| Sep 23 | Preserve map zoom on expansion/collapse | Fixed on this branch; browser check covers full, half, hidden and restored modes, plus explicit Reset. |
| Sep 23 | MVP/Multi `gd is null` crash | Fixed on this branch: cached Multi panels are reused only while their DOM children remain present. Browser regression covers return from Single and Section, using a two-dip MVP fixture. |
| Sep 23 (twice) | Missing ECO/CDOM on Lab | Fixed on this branch: expose existing TSG EcoCdom data as CDOM (mg/m³), including Lab alert figures. Data exists in 2026 legs 02/03 and the provisional TSG feed. |

Validation: the MVP/Single/Multi/Section browser suite and desktop map-mode
check pass. Live CDOM data passed resolution, analysis, minute-window and
provisional-feed checks; 26 existing track-build and alert tests pass.

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
- The mark's box reads: name, position, `by sea:`, `by air:`, then `depth:` or
  `elevation:` from GEBCO, and no longer explains how to remove the mark.
- A waypoint's name is editable; it arrives numbered ("Waypoint 3"), sits in a
  field with a pencil beside it, and Save on the same line keeps it for everyone
  on the Stations tab, whose row removes it again. Save is offered whether or
  not the name is changed, and the mark carries the time it was made, which the
  box shows and the record keeps.
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
