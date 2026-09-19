# Page localization pilot

The initial UI slice covers dashboard navigation (desktop and mobile), map-size
navigation, and the feedback dialog, including in-flight, success and retry
messages. It offers English and Canadian French. Other panels and wiki content
remain English in this pilot; the language control explains this coverage.
French is an editorial draft awaiting language review, not a site-wide release.

The implementation is independent of the wiki's record/field translation store.
Both can share the same language choice when the wiki publisher is connected.
Do not translate generated wiki HTML into these UI catalogs.

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

## Validation

```sh
node --test tests/i18n.test.cjs
python3 -m unittest discover -s tests -p test_ui_catalog.py
# Node 22+, Chromium, and a Python with Jinja2:
I18N_UI=1 UI_WIDTH=1400 PYTHON=python3 node tests/track-browser.cjs /path/to/chromium
```

The browser test uses an isolated HTTP fixture and mocked feedback endpoint. It
checks live language switching, preservation of entered feedback, translated
submission state, mobile menus and desktop restoration. `I18N_SCREENSHOT=/path.png`
optionally records the French dialog. Container/table resize callbacks are
scheduled outside ResizeObserver delivery because localized text and responsive
menus change the layout; MapLibre's duplicate automatic resize is disabled for
the main map, whose existing app resize handler owns that lifecycle.

Next slices: the remaining dashboard panels and their JS strings; then the
science games from the canonical `rec3141/amundsen-game` source checkout.
Game state, control bindings, variable names and numerical datasets are not
translation inputs. Published `/game/` files are generated output, not the place
to maintain translations.
