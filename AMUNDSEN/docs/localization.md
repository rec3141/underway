# Page localization

The selected Canadian French catalog contains 1,196 messages. Coverage includes
desktop/mobile navigation, the feedback dialog, static page controls and
accessibility labels, appearance options, the main Underway controls and chart
groups, measurement names, table headings/summaries, map details and legends,
ice-chart UI, graph/map export controls, the schedule banner, and Sources explanations.
Detailed Casts/live-feed setup, Schedule calendars and alert forms, photo upload
and gallery tools, chat controls, wiki navigation, and camera controls are also covered.

Generated wiki/article content is a separate translation pipeline and is not
published by this UI change. Source-authored station/event descriptions, source
column names, attribution records and user messages remain unchanged.
French is an editorial draft.

The implementation is independent of the wiki's record/field translation store.
Both can share the same language choice when the wiki publisher is connected.
Do not translate generated wiki HTML into these UI catalogs.

The selected profile is `editorial-fr-ca-v4`; v1, v2 and v3 candidates remain
available for comparison and rollback. The game maintains its own catalog.

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

`UWI18n.text(source, values)` looks up explicitly authored English messages in
the `pages.<source-hash>` namespace. This is not a general DOM translator: only
reviewed calls in source code are translated, never arbitrary wiki/user text.
Unknown sources fall back unchanged. `UWI18n.html(source, values)` escapes the
translated template before substituting trusted markup or already-escaped
values; callers must escape any untrusted interpolation themselves.

`UWI18n.preserve(root, render)` restores draft controls, file inputs, open
details, scroll position and focus after localized rendering. Plot axes use
stable `_uwIdentity` values when their display titles change, preserving zoom
without changing scientific field keys or units.

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
PAGES_I18N=1 BOTTLE_UI=1 UPLOAD_UI=1 WIKI_UI=1 CHAT_WIKI_UI=1 UI_WIDTH=1400 PYTHON=python3 node tests/refresh-browser.cjs /path/to/chromium
```

The browser test uses an isolated HTTP fixture and mocked feedback endpoint. It
checks live language switching, preserved measurement selections/data and
table search, graph export dimensions and data, entered feedback, translated
submission state, mobile menus and desktop restoration. `I18N_SCREENSHOT=/path.png`
optionally records the French dialog. Container/table resize callbacks are
scheduled outside ResizeObserver delivery because localized text and responsive
menus change the layout; MapLibre's duplicate automatic resize is disabled for
the main map, whose existing app resize handler owns that lifecycle.

The extended browser pass checks French Casts/live setup, calendars, wiki,
upload forms and chat, including locale round-trips with drafts and plot zoom.
The wiki publisher can advertise reviewed article snapshots in
`history.locales`, for example:

```json
{"fr-CA":{"base":"data/history/locales/fr-CA/0123456789abcdef/","stamp":"0123456789abcdef"}}
```

History and Nature load indexes, cards, timelines and article pages from the
selected locale's immutable tree. Without an advertised locale they retain the
existing English paths. The publisher supplies English fallback for individual
unapproved or stale fields; the browser never reads the draft translation store.
Source provenance, bibliography, media, live journal entries and contributor
drafts remain shared. Cache identity includes the English build, selected locale,
snapshot path and translation revision. Obsolete downloads cannot replace a
newer locale's data. Language switches retain the article slug, navigation, map
state and unfinished forms; draft capture occurs after downloads, just before
the DOM is replaced.

```sh
WIKI_LOCALE_UI=1 WIKI_UI=1 UI_WIDTH=1400 PYTHON=python3 node tests/refresh-browser.cjs /path/to/chromium
```

This fixture tests translated article and Nature-subject roundtrips, stable
scientific identifiers, a changed translator revision, delayed obsolete
responses, English fallback and shared provenance. It does not approve or publish
any production translation.

Game state, control bindings, variable names and numerical datasets are not
translation inputs. Published `/game/` files are generated output, not the place
to maintain translations.
