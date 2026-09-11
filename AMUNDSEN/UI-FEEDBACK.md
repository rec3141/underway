# UI feedback backlog

## Reported in this session

- Nature map clicks should open the sidebar without filtering or recentering the map. Fixed in this change.

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
