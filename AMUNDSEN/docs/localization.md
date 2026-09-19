# Page localization

The selected Canadian French catalog contains 430 messages. Coverage includes
desktop/mobile navigation, the feedback dialog, static page controls and
accessibility labels, appearance options, the main Underway controls and chart
groups, measurement names, table headings/summaries, map details and legends,
ice-chart UI, graph/map export controls, the schedule banner, and Sources explanations.

This is not yet a site-wide translation. Most dynamic Casts/live-feed and
Schedule workflows, generated wiki/article content, photo tools and chat
dialogs still need work. Source-authored station/event descriptions, source
column names, attribution records and user messages remain unchanged.
French is an editorial draft awaiting independent language review.

The implementation is independent of the wiki's record/field translation store.
Both can share the same language choice when the wiki publisher is connected.
Do not translate generated wiki HTML into these UI catalogs.

The selected profile is `editorial-fr-ca-v3`; the v1 and v2 candidates remain
available for comparison and rollback. The game now has its own CTD catalog.

## Source, candidates and selection

- `locales/en.json` is the source catalog: stable semantic keys, English messages,
  and translator context. Prefer whole sentences and named values such as
  `{count}`, not concatenated fragments.
- `locales/variants/<locale>/<profile>.json` holds one translator version's
  candidate messages. Each records the exact source message and translated text.
  The profile identifies its translator/model/glossary. Add a new file/profile
  when changing those inputs; retain old files for comparison and rollback.
- `locales/selection.json` chooses a default profile per locale and optional
  per-key overrides. Changing selection does not change candidate files.
- `tools/build-ui-catalog.py` validates and compiles selected current candidates
  into `dashboard/static/i18n-catalog.js`. Missing or stale candidates fall back
  to English. A changed parameter set, duplicate profile, invalid plural form,
  or nonexistent selected profile fails the build. Per-key source hashes and
  selected profiles accompany the browser bundle.

Regenerate and check with:

```sh
python3 tools/build-ui-catalog.py
python3 tools/build-ui-catalog.py --check
```

The generated asset is checked in so standalone pages, existing browser tests,
and installed wheels have the same catalog. The ordinary dashboard build copies
it and includes its contents, the runtime, and responsive header in the asset
cache fingerprint. No translator or model runs in the browser.

## Browser contract

`window.UWI18n.t(key, values)` returns **plain text**. Use `textContent`, an
allowlisted attribute, or the existing HTML-escape helper when constructing HTML.
Never interpret translator-supplied text as markup. User-written feedback is
not translated or replaced when the language changes.

Static leaf nodes use `data-i18n="key"`. Title, placeholder and accessible labels
use `data-i18n-title`, `data-i18n-placeholder`, and `data-i18n-aria-label`.
Do not put a text binding on a parent that contains controls or icons: use a
separate child span. `UWI18n.apply(root)` binds newly inserted static nodes.
Dynamic components listen for `uw:localechange` and redraw their labels from
state; they must not remount the component or erase user input.

`?lang=fr-CA` takes precedence over the saved JSON string in localStorage
`uw:locale`. Choosing a language updates both, preserving the rest of the URL.
`UWI18n.locale` is the current locale; `setLocale(code)` rejects unknown locales.
The `uw:localechange` event has `detail.locale`. Plain English remains the
default when no choice is present. There are no translator controls in the
public UI; translator selection is a catalog/build decision.

Plural messages have named forms, e.g. `{ "one": "{count} bottle", "other":
"{count} bottles" }`; the runtime uses `Intl.PluralRules`. English fallback
messages use English plural rules even when the chosen language is French.
Numbers should be passed as values; explicitly decide which scientific units,
precision and numeric formatting must stay unchanged.

`UWI18n.variable(name)` translates only the display name of the 30 known
measurement fields. Dataset lookup keys, selection values, recorded units and
TSV headers remain original. Unknown instrument labels fall back unchanged.
Live switching refreshes cached labels/charts without reloading observation
windows or resetting the chosen span, variable, filters or plot zoom. Export
controls preserve entered dimensions; the preview image is a snapshot of the
plot at opening, so reopen it to capture newly translated plot annotations.

## Validation

```sh
node --test tests/i18n.test.cjs
python3 -m unittest discover -s tests -p test_ui_catalog.py
# Node 22+, Chromium, and a Python with Jinja2:
I18N_UI=1 UI_WIDTH=1400 PYTHON=python3 node tests/track-browser.cjs /path/to/chromium
```

The browser test uses an isolated HTTP fixture and mocked feedback endpoint. It
checks live language switching, preserved measurement selections/data and
table search, graph export dimensions and data, entered feedback, translated
submission state, mobile menus and desktop restoration. `I18N_SCREENSHOT=/path.png`
optionally records the French dialog. Container/table resize callbacks are
scheduled outside ResizeObserver delivery because localized text and responsive
menus change the layout; MapLibre's duplicate automatic resize is disabled for
the main map, whose existing app resize handler owns that lifecycle.

Next slices: detailed Casts/live setup and Schedule workflows, followed by
photo/chat tools and wiki-serving integration.
Game state, control bindings, variable names and numerical datasets are not
translation inputs. Published `/game/` files are generated output, not the place
to maintain translations.
