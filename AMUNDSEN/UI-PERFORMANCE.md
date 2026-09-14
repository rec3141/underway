# UI refresh profile — 2026-09-10

Profiled locally in headless Chrome at a 390 × 844 mobile viewport, using the
real template, scripts and Plotly with synthetic data. No deployed files changed.
The test renders the SST chart, registers and minimizes 24 additional panels,
then requests ten refreshes with the same colour selection.

| Measurement | Before | After |
| --- | ---: | ---: |
| Synchronous refresh work, ten calls | 284 ms | 25 ms |
| Panel child-list mutations | 2,500 | 0 |
| Chart resize requests | 250 | 0 |
| Panels restored | 24 | 24 |

The timings are one local sample, not a physical-phone benchmark or a measure
of complete page loading, GPU rendering, or interaction latency. Work counts
are the regression assertions; timings are diagnostic.

`renderPanel` previously called `layoutPanels` again for each minimized panel.
Each layout moved every visible panel and resized every rendered plot, even
when nothing had changed. Detached panels were also recreated and observed
again. The fix keeps panel elements, only moves panels when their order changes,
and only requests layout-driven chart resizes when placement or width changes.
Data, chart points, and existing viewport-based lazy rendering are preserved.

Run the isolated performance regression with:

```sh
PROFILE_UI=check node tests/refresh-browser.cjs /usr/bin/google-chrome
node --test tests/data.test.cjs
```

The profile checks zero unnecessary panel moves/resizes, restoration and reuse
of minimized panels, and absence of browser errors. The browser harness now
selects a page target explicitly, avoiding Chrome extension background targets.
The full existing refresh harness separately times out on its calendar `event-1`
expectation on the unchanged baseline; the isolated profile does not exercise
that calendar scenario.
