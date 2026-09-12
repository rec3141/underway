/* Amundsen underway dashboard — renders the JSON produced by the Python build.
 * Self-contained: Plotly (the charts) and MapLibre (the map, static/map.js) are
 * served from static/, and the basemap comes from static/geo/ and the local
 * tile sets.
 *
 * One record spans every leg. A window is a span back from the latest data;
 * each point carries its leg, and the leg list filters what is shown. */
(() => {
  "use strict";

  const SITE = window.__SITE__;
  let M = window.__MANIFEST__;
  const $ = (s) => document.querySelector(s);
  const { fetchJSON } = window.UWData;
  const loadErrors = new Set();
  function setLoadError(scope, failed) {
    failed ? loadErrors.add(scope) : loadErrors.delete(scope);
    const el = $("#connection");
    el.hidden = !loadErrors.size;
    el.textContent = loadErrors.size ? `${[...loadErrors].join(", ")} update unavailable · retrying; displayed data may be older` : "";
  }
  const store = {
    get(k, d) { try { return JSON.parse(localStorage.getItem("uw:" + k)) ?? d; } catch { return d; } },
    set(k, v) { try { localStorage.setItem("uw:" + k, JSON.stringify(v)); } catch { /* private mode */ } },
  };

  // the page opens on the current leg over its whole span ("leg"); later
  // choices are remembered, and a browser with settings from before these
  // defaults (prefs.v < 2) takes them once
  const newestLeg = M.legs.find((l) => l.id === M.live) || M.legs.reduce((a, b) => (!a || b.last_date > a.last_date) ? b : a, null);
  const otherLegs = M.legs.filter((l) => l.id !== newestLeg?.id).map((l) => l.id);
  if (store.get("prefs.v", 0) < 2) { store.set("prefs.v", 2); store.set("win", M.default_window); store.set("hiddenLegs", otherLegs); }
  if (store.get("prefs.v", 0) < 5) { store.set("prefs.v", 5); store.set("trackKm", null); }   // track detail follows the span (a point a km)
  const newViewer = store.get('panel',null) === null;
  const state = {
    hidden: new Set(store.get("hiddenLegs", otherLegs)),   // leg ids switched off; default: all but the current leg
    win: store.get("win", M.default_window),
    xmode: store.get("xmode", "time"),
    colour: store.get("colour", "SST (°C)"),
    log: store.get("log", {}),
    track: store.get("track", true),                    // the ship's track on the map
    trackKm: store.get("trackKm", null),                // track detail: 0 = every point, else one per so many km (null: from the span)
    stations: store.get("stations", true),
    events: store.get("events", false),                 // event-log entries on the map
    photos: store.get("photos", store.get("cameras", true)),   // the pictures on the map: a camera per daily timelapse, and the ship's own photographs (nature.js)
    communities: true,                                  // the settlements with people in them are always on the map
    plan: store.get("plan", true),                      // the leg's planned track and stations
    history: store.get("history", false),               // the History tab's artifacts and voyage tracks
    nature: store.get("nature", false),                 // the Nature tab's observations
    planData: null, planStamp: null,                    // the plan as published, and which version it is
    sat: store.get("sat", ""),                          // satellite picture under the track: "" | "s1" | "s2"
    satAt: null,                                        // an archived picture's scene time, or null for the newest
    order: store.get("order", []),
    panel: store.get("panel", {}),                    // name -> "min" | "wide" | null (a key the user has set)
    depthScale: store.get("depthScale", {}),
    raw: null,                                        // window payload as built
    data: null,                                       // same, filtered to shown legs
    geo: null,
    view: null,                                       // user's pan/zoom
  };

  const NOT_PANELS = new Set(["Time elapsed (h)", "Distance travelled (km)", "TSG line warming (°C)"]);
  const isDepth = (name) => name === 'Bottom depth (m)' || name === 'Rosette depth (m)';
  if(newViewer){for(const v of M.variables)state.panel[v.name]='min';store.set('panel',state.panel)}
  const extraPanels = new Map();
  const extraColours = new Map();
  // Surprise has one summary row per scale; extra charts start minimized.
  const SURPRISE = "Surprise (−log10 p)";
  if (!store.get('summary-tables-v1',false)) {
    for (const v of M.variables) if (v.name.startsWith('Surprise ·')) state.panel[v.name]='min';
    store.set('panel',state.panel);store.set('summary-tables-v1',true);
  }
  let VAR = Object.fromEntries(M.variables.map((v) => [v.name, v]));

  // ------------------------------------------------------------ theme
  // The stylesheet owns the colours (style.css, the token block at the top);
  // the graphs and the map read them from the root's custom properties. The
  // objects are updated in place, so a module that took THEME or C at load
  // sees the new theme at its next draw.
  const THEMES = { auto: "Auto (system)", "claude-dark": "Claude dark", "claude-light": "Claude light", "minimal-dark": "Minimal dark", "minimal-light": "Minimal light", navigator: "Navigator", "navigator-dark": "Navigator dark" };
  const SIZES = { auto: "Theme's size", normal: "Normal text", large: "Large text" };
  const themeName = () => { const t = store.get("theme", null); return THEMES[t] ? t : "auto"; };
  const sizeName = () => { const t = store.get("textsize", null); return SIZES[t] ? t : "auto"; };
  const lightOS = matchMedia("(prefers-color-scheme: light)");
  const resolveTheme = (name) => name === "auto" ? (lightOS.matches ? "claude-light" : "claude-dark") : name;   // Auto follows the system
  const C = {};                                            // the theme's colours by token name, camel-cased: C.accent2, C.markerLine, C.palette[i]
  let fontScale = 1;                                       // the root font size over the 14px the graph sizes are written for
  const fz = (n) => Math.round(n * fontScale * 10) / 10;
  const THEME = {
    paper_bgcolor: "rgba(0,0,0,0)", plot_bgcolor: "",
    font: { color: "", family: "system-ui, -apple-system, Segoe UI, Roboto, sans-serif", size: 12.5 },
    xaxis: {}, yaxis: {},
    hoverlabel: { bgcolor: "", bordercolor: "", font: { color: "", size: 12 } },
  };
  function readTheme() {
    const cs = getComputedStyle(document.documentElement);
    const v = (k) => cs.getPropertyValue("--" + k).trim();
    for (const k of ["bg", "card", "card-2", "line", "fg", "fg-2", "muted", "accent", "on-accent", "accent-2", "warn", "ok", "bad", "now", "purple", "pink", "gold",
                     "marker", "marker-line", "floor", "floor-line", "map-bg", "map-land", "map-coast", "map-ice", "sketch-coast", "plot-legend-bg"])
      C[k.replace(/-([a-z0-9])/g, (_, c) => c.toUpperCase())] = v(k);
    C.palette = v("palette").split(/\s+/);
    C.bathy = v("map-bathy").split(/\s+/);
    C.dark = cs.colorScheme !== "light";
    fontScale = (parseFloat(cs.fontSize) || 14) / 14;
    THEME.plot_bgcolor = v("plot-bg");
    Object.assign(THEME.font, { color: v("plot-fg"), size: fz(12), family: v("plot-font") || THEME.font.family });
    for (const ax of ["xaxis", "yaxis"]) Object.assign(THEME[ax], { gridcolor: v("plot-grid"), zerolinecolor: v("plot-grid"), linecolor: v("plot-line") });
    Object.assign(THEME.hoverlabel, { bgcolor: v("hover-bg"), bordercolor: v("accent") });
    Object.assign(THEME.hoverlabel.font, { color: v("hover-fg") || v("fg"), size: fz(12) });
  }
  // the theme on the page: the attribute the stylesheet keys on, the colours
  // read back, and (after a change) every graph and the map drawn again
  function applyTheme(name, redraw = false) {
    const root = document.documentElement;
    root.dataset.theme = resolveTheme(name);
    const size = sizeName(); if (size === "auto") delete root.dataset.size; else root.dataset.size = size;
    readTheme();
    root.classList.toggle("bigtype", fontScale > 1.1);            // the roomier controls, whichever theme or size asked for the large type
    const meta = document.querySelector('meta[name="theme-color"]'); if (meta) meta.content = C.bg;
    const sel = $("#theme"); if (sel && sel.value !== name) sel.value = name;
    const ssel = $("#textsize"); if (ssel && ssel.value !== size) ssel.value = size;
    if (!redraw) return;
    render();
    window.UW?.refreshActiveTab?.(true);
    document.dispatchEvent(new CustomEvent("uw:theme", { detail: { name } }));
  }
  applyTheme(themeName());
  {
    const sel = $("#theme");
    sel.innerHTML = Object.entries(THEMES).map(([k, l]) => `<option value="${k}">${l}</option>`).join("");
    sel.value = themeName();
    sel.onchange = () => { store.set("theme", sel.value); applyTheme(sel.value, true); };
    const ssel = $("#textsize");
    ssel.innerHTML = Object.entries(SIZES).map(([k, l]) => `<option value="${k}">${l}</option>`).join("");
    ssel.value = sizeName();
    ssel.onchange = () => { store.set("textsize", ssel.value); applyTheme(themeName(), true); };
    lightOS.addEventListener?.("change", () => { if (themeName() === "auto") applyTheme("auto", true); });
    // Appearance and feedback controls remain in the upper-right header.
  }
  const CFG = { displayModeBar: false, responsive: true, scrollZoom: true, doubleClick: "reset" };
  const rememberPlot = window.UWData.plotState(Plotly);
  function reactPlot(gd, data, layout, config, context = '') {
    const pane = gd.closest('.pane')?.id || '';
    const name = gd.id || gd.closest('[data-name]')?.dataset.name || '';
    const filter = pane === 'pane-underway' ? [state.win, state.xmode, [...state.hidden].sort()] : pane === 'pane-wiki' ? [location.hash] : [];
    return rememberPlot(gd, data, layout, config, JSON.stringify([pane, name, filter, context])).then(plot=>{window.UWPlotExport?.attach(plot);return plot;});
  }

  // Shift+scroll zooms the x axis alone, Ctrl+scroll the y axis alone, about
  // the cursor; a plain scroll keeps Plotly's zoom of both. Listens in the
  // capture phase so Plotly's own wheel handler never sees the modified event.
  function axisZoom(gd, opts = {}) {
    if (gd._axisZoom) return;
    gd._axisZoom = true;
    const allowX = opts.x !== false, allowY = opts.y !== false;
    gd.addEventListener("wheel", (ev) => {
      const panel = gd.closest(".panel");
      if (panel && !panel.classList.contains("on") && !panel.classList.contains("solo")) { ev.stopPropagation(); return; }   // an unselected panel: the page scrolls (a lone panel counts as selected)
      const fl = gd._fullLayout;
      if (!((ev.shiftKey && allowX) || (ev.ctrlKey && allowY)) || !fl || !fl.xaxis || !fl.yaxis) return;
      ev.preventDefault(); ev.stopPropagation();
      const rect = gd.getBoundingClientRect();
      const ax = ev.shiftKey ? fl.xaxis : fl.yaxis;
      const px = ev.shiftKey ? ev.clientX - rect.left - ax._offset : ev.clientY - rect.top - ax._offset;
      const c = ax.p2l(px);
      const k = Math.exp(ev.deltaY * 0.0015);
      const r0 = ax.r2l(ax.range[0]), r1 = ax.r2l(ax.range[1]);
      const lo = c + (r0 - c) * k, hi = c + (r1 - c) * k;
      Plotly.relayout(gd, { [`${ax._name}.range`]: [ax.l2r(lo), ax.l2r(hi)], [`${ax._name}.autorange`]: false });
    }, { passive: false, capture: true });
  }

  // Only the selected panel (the one whose variable colours the map, or a
  // camera panel showing one of its modes) pans and zooms; on the others a
  // drag or a wheel scrolls the page, and a click selects them. That keeps
  // touch gestures for scrolling and drag-and-drop.
  // the selected panel follows the colour variable; a click on its own title
  // lets it go (no panel selected until the next click)
  const panelOn = (name) => { if (state.unfocus) return false; const x = extraPanels.get(name); return x ? (x.colours || []).includes(state.colour) : name === state.colour; };
  function selectPanel(name) {
    if (panelOn(name)) { state.unfocus = true; renderPanels(); return; }
    const x = extraPanels.get(name);
    const colour = x ? (x.colours || [])[0] : name;
    if (!colour) return;
    state.unfocus = false;
    if (colour === state.colour) { renderPanels(); return; }
    state.colour = colour; store.set("colour", colour); renderControls(); render();
  }

  // ------------------------------------------------------------ helpers
  // Every time a person reads is ship time (SITE.local_tz); the instants
  // stay UTC underneath. Plotly has no zones, so a date axis gets instants
  // shifted by the offset (shipAxis) and reads as ship time.
  const _lp = new Intl.DateTimeFormat("en-CA", { timeZone: SITE.local_tz, year: "numeric", month: "2-digit", day: "2-digit", hour: "2-digit", minute: "2-digit", hourCycle: "h23" });
  const localParts = (ms) => { const o = {}; for (const p of _lp.formatToParts(new Date(ms))) o[p.type] = p.value; return o; };
  const fmtTs = (ms) => { if (ms == null || isNaN(ms)) return ""; const p = localParts(ms); return `${p.year}-${p.month}-${p.day} ${p.hour}:${p.minute}`; };
  const tzAbbr = (ms = Date.now()) => new Intl.DateTimeFormat("en-US", { timeZone: SITE.local_tz, timeZoneName: "short" }).formatToParts(new Date(ms)).find((p) => p.type === "timeZoneName")?.value || SITE.local_tz;
  const offsetMs = (ms) => { const p = localParts(ms); return Date.UTC(+p.year, +p.month - 1, +p.day, +p.hour, +p.minute) - Math.floor(ms / 60000) * 60000; };
  // Keep shifted milliseconds for arithmetic; pass explicit wall-time strings
  // to Plotly, whose numeric/Date inputs depend on the browser timezone.
  const shipAxis = (ms) => ms + offsetMs(ms);
  const plotDate = (ms) => new Date(ms).toISOString().slice(0, -1);
  const fmtLocal = (iso) => new Date(iso).toLocaleString(undefined, { timeZone: SITE.local_tz,
    month: "short", day: "numeric", hour: "2-digit", minute: "2-digit" });
  const ago = (iso) => {
    const s = Math.max(0, (Date.now() - new Date(iso)) / 1000);
    if (s < 90) return `${Math.round(s)} s ago`;
    if (s < 5400) return `${Math.round(s / 60)} min ago`;
    if (s < 48 * 3600) return `${(s / 3600).toFixed(1)} h ago`;
    return `${Math.round(s / 86400)} d ago`;
  };
  const lastFinite = (arr) => { for (let i = arr.length - 1; i >= 0; i--) if (arr[i] != null) return arr[i]; return null; };
  const fmtVal = (v, unit) => v == null ? "—" : `${Math.abs(v) >= 100 ? v.toFixed(0) : v.toFixed(2)}${unit ? " " + unit : ""}`;
  const dms = (lat, lon) => `${Math.abs(lat).toFixed(4)}°${lat >= 0 ? "N" : "S"}, ${Math.abs(lon).toFixed(4)}°${lon >= 0 ? "E" : "W"}`;
  const xvals = (d) => state.xmode === "time" ? d.t.map((t) => plotDate(shipAxis(t))) : d.dist_km;
  const xTitle = () => state.xmode === "time" ? `ship time (${tzAbbr()})` : "distance along track (km)";
  const minmax = (a) => { let lo = Infinity, hi = -Infinity; for (const x of a) if (x != null) { if (x < lo) lo = x; if (x > hi) hi = x; } return [lo, hi]; };
  const cssId = (s) => s.replace(/[^a-z0-9]+/gi, "_");
  const legById = (id) => M.legs.find((l) => l.id === id);
  const legByIndex = (i) => M.legs[i];
  const shownLegs = () => M.legs.filter((l) => !state.hidden.has(l.id));
  // The legs menu filters the data: what is loaded covers every shown leg
  // (coverWindow) and hidden legs are masked out, on every tab. The span is
  // a view on it: the panels' x axes open on the last so many hours, the map
  // draws that stretch of track at a detail to suit its length, and the
  // tables are untouched by it. The span runs back from the end of the
  // record; times without a zone are UTC.
  const tms = (s) => { if (s == null || s === "") return NaN; if (typeof s === "number") return s;
    let t = String(s).trim().replace(" ", "T").replace(/^(\d{4})\/(\d{2})\/(\d{2})/, "$1-$2-$3");
    if (!/[zZ]|[+-]\d{2}:?\d{2}$/.test(t)) t += "Z"; return Date.parse(t); };
  const spanWindowOf = (m) => m.windows.find((x) => x.label === state.win) || m.windows.find((x) => x.label === m.default_window) || m.windows[0];
  const spanWindow = () => spanWindowOf(M);
  const legRange = (l) => ({ start: Date.parse(`${l.first_date.slice(0, 4)}-${l.first_date.slice(4, 6)}-${l.first_date.slice(6, 8)}T00:00:00Z`),
                             end: Date.parse(`${l.last_date.slice(0, 4)}-${l.last_date.slice(4, 6)}-${l.last_date.slice(6, 8)}T23:59:59Z`) });
  // the first day of the earliest shown leg
  const legsStartOf = (m) => { const s = m.legs.filter((l) => !state.hidden.has(l.id)); return s.length ? Math.min(...s.map((l) => legRange(l).start)) : Date.parse(m.data_range.end); };
  const legsStart = () => legsStartOf(M);
  // the smallest window reaching back from the record's end to every shown
  // leg: the "leg" window when the newest leg is the only one (it is that
  // leg by definition), else by the hours to the earliest one's first day
  const coverWindowOf = (m) => {
    const shown = m.legs.filter((l) => !state.hidden.has(l.id));
    const legW = m.windows.find((w) => w.label === "leg");
    if (legW && shown.length === 1 && shown[0].id === (m.live || newestLeg?.id)) return legW;
    const end = Date.parse(m.data_range.end), need = legsStartOf(m);
    return m.windows.find((w) => end - w.hours * 3600e3 <= need) || m.windows[m.windows.length - 1];
  };
  // the legs: what the tables show
  function currentFilter() {
    const end = Date.parse(M.data_range.end);
    return { legs: new Set(shownLegs().map((l) => l.id)), start: -Infinity, end, label: spanWindow().label };
  }
  // the legs and the span: what the map draws and the graphs open on; the
  // span never reaches before the loaded record
  function spanFilter() {
    const f = currentFilter();
    const first = state.data?.t?.find((t) => t != null);
    return { ...f, start: Math.max(f.end - spanWindow().hours * 3600e3, first ?? -Infinity) };
  }
  function toast(text) {
    let t = $("#toast");
    if (!t) { t = document.createElement("div"); t.id = "toast"; t.className = "toast"; document.body.appendChild(t); }
    t.textContent = text; t.hidden = false; t.classList.add("show");
    clearTimeout(toast._h); toast._h = setTimeout(() => { t.classList.remove("show"); }, 5000);
  }
  // a span picked by a link or the URL (the same path as the slider)
  function setSpan(label) {
    if (label === state.win || !M.windows.some((w) => w.label === label)) return;
    state.win = label; store.set("win", state.win);
    setTrackDetail(detailFor(currentWindow()?.hours || 1)); requestFit(); renderControls(); loadWindow();
  }
  // a table's "show all legs" link
  function showAllLegs() { state.hidden.clear(); store.set("hiddenLegs", []); requestFit(); loadWindow(); }
  function inFilter(legId, time, f = currentFilter()) {
    if (legId != null && !f.legs.has(legId)) return false;
    const t = tms(time);
    return isNaN(t) || (t >= f.start && t <= f.end + 60e3);
  }

  // ------------------------------------------------------------ leg filter
  // Rows of hidden legs become null so lines break there instead of bridging
  // across a leg that is switched off.
  function applyLegFilter(raw) {
    const hiddenIdx = new Set(M.legs.filter((l) => state.hidden.has(l.id)).map((l) => l.index));
    if (!hiddenIdx.size) return raw;
    const mask = raw.leg.map((c) => c == null || !hiddenIdx.has(c));
    const nul = (arr) => arr.map((v, i) => (mask[i] ? v : null));
    const vars = Object.fromEntries(Object.entries(raw.vars).map(([k, v]) => [k, nul(v)]));
    const shown = mask.filter(Boolean).length;
    return { ...raw, lat: nul(raw.lat), lon: nul(raw.lon), dist_km: nul(raw.dist_km), vars, shown,
             limits: Object.fromEntries(Object.entries(vars).map(([k, v]) => [k, quantileLimits(v, VAR[k]?.tsg ? pumpLow(raw) : null)])) };
  }
  // the span's stretch of the record, for the map: the bins from the span's
  // start on; its colour limits colour the track and the graphs alike
  function spanSlice(d) {
    const start = spanFilter().start;
    let i0 = d.t.findIndex((t) => t != null && t >= start); if (i0 < 0) i0 = d.t.length;
    const cut = (a) => Array.isArray(a) ? a.slice(i0) : a;
    const vars = Object.fromEntries(Object.entries(d.vars).map(([k, v]) => [k, cut(v)]));
    const out = { ...d, t: cut(d.t), lat: cut(d.lat), lon: cut(d.lon), dist_km: cut(d.dist_km), leg: cut(d.leg), vars, pump_low: cut(d.pump_low), n: d.t.length - i0 };
    out.shown = out.lat.filter((x) => x != null).length;
    out.limits = Object.fromEntries(Object.entries(vars).map(([k, v]) => [k, quantileLimits(v, VAR[k]?.tsg ? pumpLow(out) : null)]));
    out.label = spanWindow().label;                                   // the map's foot names the span and its stretch
    const t0 = out.t.find((t) => t != null); if (t0 != null) out.start = new Date(t0).toISOString();
    return out;
  }
  // the cover window's bins before the span's first bin, then the span's
  // own, finer bins: one record with the span at its native resolution
  function mergeWindows(cover, span) {
    if (!span || span === cover) return cover;
    const t0 = span.t.find((t) => t != null); if (t0 == null) return cover;
    let cut = cover.t.findIndex((t) => t != null && t >= t0); if (cut < 0) cut = cover.t.length;
    const cat = (a, b, na, nb) => (a || new Array(na).fill(null)).slice(0, cut).concat(b || new Array(nb).fill(null));
    const vars = {};
    for (const k of new Set([...Object.keys(cover.vars), ...Object.keys(span.vars)])) vars[k] = cat(cover.vars[k], span.vars[k], cover.t.length, span.t.length);
    // each file's distance runs from its own start: the span's continues the cover's
    const base = lastFinite(cover.dist_km.slice(0, cut)) ?? 0, d0 = span.dist_km.find((x) => x != null) ?? 0;
    const dist = span.dist_km.map((x) => (x == null ? null : x - d0 + base));
    return { ...span, start: cover.start, n: cut + span.t.length, t: cat(cover.t, span.t), lat: cat(cover.lat, span.lat), lon: cat(cover.lon, span.lon),
             dist_km: cat(cover.dist_km, dist), leg: cat(cover.leg, span.leg), vars,
             pump_low: cover.pump_low && span.pump_low ? cat(cover.pump_low, span.pump_low) : null,
             limits: span.limits || cover.limits };
  }
  // the graphs' x range for the span: the last so many hours of ship time,
  // or the distance run over them
  function spanRange(d) {
    const f = spanFilter();
    if (!isFinite(f.start)) return undefined;
    if (state.xmode === "time") return [plotDate(shipAxis(f.start)), plotDate(shipAxis(f.end + 60e3))];
    let i0 = d.t.findIndex((t) => t != null && t >= f.start); if (i0 < 0) i0 = 0;
    const first = d.dist_km.slice(i0).find((x) => x != null), last = lastFinite(d.dist_km);
    return first != null && last != null && last > first ? [first, last] : undefined;
  }

  // Per bin, whether the intake pump was stopped: the build's flag (any
  // stopped minute in the bin, spread to the neighbours) when it is there,
  // else the bin-mean flow against the threshold.
  function pumpLow(d) {
    if (d.pump_low) return d.pump_low;
    const flow = d.vars["TSG flow (V)"];
    if (!flow) return null;
    const thr = SITE.low_flow_v ?? 0.5;
    return flow.map((f) => f != null && f < thr);
  }
  // colour limits as the build computes them; the TSG variables' from the
  // bins with the intake pump running (see pumpedRange)
  function quantileLimits(vals, low) {
    const a = vals.filter((x, i) => x != null && !(low && low[i])).sort((x, y) => x - y);
    if (a.length < 2) return null;
    const q = (p) => a[Math.min(a.length - 1, Math.floor(p * (a.length - 1)))];
    let lo = q(0.05), hi = q(0.95);
    if (lo === hi) { lo -= 0.5; hi += 0.5; }
    return [lo, hi];
  }

  function renderLegMenu() {
    const ul = $("#leglist");
    ul.innerHTML = "";
    for (const l of [...M.legs].sort((a, b) => (b.year * 100 + b.number) - (a.year * 100 + a.number))) {
      const li = document.createElement("li");
      const span = l.first_date && l.last_date
        ? `${l.first_date.slice(4, 6)}/${l.first_date.slice(6)} – ${l.last_date.slice(4, 6)}/${l.last_date.slice(6)}` : "";
      li.innerHTML = `<label><input type="checkbox" ${state.hidden.has(l.id) ? "" : "checked"}>
        <span class="name">${l.label}</span>${l.live ? '<span class="live">live</span>' : ""}
        <span class="span">${span}</span><span class="n">${l.files} d</span></label>`;
      li.querySelector("input").onchange = (e) => {
        e.target.checked ? state.hidden.delete(l.id) : state.hidden.add(l.id);
        store.set("hiddenLegs", [...state.hidden]);
        requestFit();
        loadWindow();                                                // the loaded window reaches every shown leg
      };
      ul.appendChild(li);
    }
    $("#legsummary").textContent = `Legs · ${shownLegs().length}/${M.legs.length}`;
    $("#legfoot").textContent = `${shownLegs().length} of ${M.legs.length} legs shown · the ${coverWindowOf(M).label} window loaded`;
    $("#legall").onclick = (e) => { e.preventDefault(); showAllLegs(); };
    $("#legnone").onclick = (e) => { e.preventDefault(); state.hidden = new Set(M.legs.map((l) => l.id)); store.set("hiddenLegs", [...state.hidden]); loadWindow(); };
  }

  // ------------------------------------------------------------ header
  // the subtitle: the time now (ship time), when the record last updated,
  // and LIVE while observations are still arriving. The ship's ACSD system
  // flushes its CSV every ten minutes, so the newest observation is up to
  // eleven minutes old in normal running; LIVE holds up to fifteen.
  function renderStatus() {
    for (const el of document.querySelectorAll('.dockgroup .gn[data-updated]')) {
      const text = [el.dataset.summary, ago(Number(el.dataset.updated))].filter(Boolean).join(' · ');
      if (el.textContent !== text) el.textContent = text;
    }
    const end = M.data_range.end;
    const ageMin = (Date.now() - new Date(end)) / 60000;
    const parts = Object.fromEntries(new Intl.DateTimeFormat("en-GB", { timeZone: SITE.local_tz, year: "numeric", month: "long", day: "2-digit", hour: "2-digit", minute: "2-digit", hourCycle: "h23", timeZoneName: "short" }).formatToParts(new Date()).map((p) => [p.type, p.value]));
    const zone = new Intl.DateTimeFormat("en-US", { timeZone: SITE.local_tz, timeZoneName: "short" }).formatToParts(new Date()).find((p) => p.type === "timeZoneName")?.value || "";   // EDT, where en-GB says GMT-4
    const now = `${parts.day} ${parts.month} ${parts.year} ${parts.hour}:${parts.minute} ${zone}`;   // 06 September 2026 14:45 EDT
    const live = ageMin < 15;
    // "last refresh" opens a list of the sources: each folder on the share (a
    // link) or the live page it is scraped from, and when it last had anything
    // the age coloured from green (fresh) through amber to red (three hours or more)
    const ageColour = (iso) => { const m = (Date.now() - Date.parse(iso)) / 60000; return `hsl(${Math.round(120 * (1 - Math.min(1, Math.max(0, m) / 180)))}, 70%, 60%)`; };
    const when = (iso) => iso ? `${fmtTs(Date.parse(iso)).slice(11)} <span style="color:${ageColour(iso)}">(${esc(ago(iso))})</span>` : "—";
    const esc = (x) => String(x ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
    const rows = (M.source_info || []).map((s) => {
      const t = s.key === "live" && window.UW?.intranetLatest?.fetched ? new Date(window.UW.intranetLatest.fetched * 1000).toISOString() : s.time;
      return `<tr><td>${s.url ? `<a href="${esc(s.url)}" target="_blank" rel="noopener">${esc(s.label)}</a>` : esc(s.label)}</td><td class="mono">${when(t)}</td></tr>`;
    }).join("");
    const gen = Date.parse(M.generated_utc);
    const open = $("#srcpop")?.open;
    // the sources box opens on hover, or on a click on "last refresh" or LIVE
    const schedWord = M.calendar?.now && schedMode() === "hidden" ? ` · <span class="schedlink" id="schedlink" title="show the status bar">STATUS</span>` : "";
    $("#status").innerHTML = `<b>${now}</b> · <details class="srcpop" id="srcpop"${open ? " open" : ""}><summary class="refresh">last refresh ${fmtTs(gen).slice(11)}${live ? ` · <span class="live">LIVE</span>` : ` · <span class="stale">data ${ago(end)}</span>`}</summary><table>${rows}</table></details>` + schedWord;
    const sl = $("#schedlink"); if (sl) sl.onclick = () => setSchedMode("open");
    $("#gen").textContent = `${fmtTs(Date.parse(M.generated_utc))} ${tzAbbr()}`;
  }
  setInterval(() => { if (M?.data_range) renderStatus(); }, 20000);

  function renderControls() {
    const r = $("#span"), ticks = $("#spanticks");
    const labels = M.windows.map((w) => w.label);
    r.max = labels.length - 1;
    ticks.innerHTML = labels.map((l, i) => `<option value="${i}" label="${l}"></option>`).join("");
    let idx = labels.indexOf(state.win);
    if (idx < 0) idx = Math.max(0, labels.indexOf(M.default_window));
    r.value = idx;
    $("#spanlabel").textContent = labels[idx]; r.setAttribute("aria-valuetext", labels[idx]);
    const pick = (label) => { state.win = label; store.set("win", state.win); setTrackDetail(detailFor(currentWindow()?.hours || 1)); requestFit(); loadWindow(); };
    r.oninput = () => { $("#spanlabel").textContent = labels[r.value]; r.setAttribute("aria-valuetext", labels[r.value]); };
    r.onchange = () => pick(labels[r.value]);
    // the same choice as a dropdown, which is what a phone shows instead of the slider
    const sel0 = $("#spansel");
    sel0.innerHTML = labels.map((l) => `<option value="${l}">${l === "leg" ? "this leg" : l}</option>`).join("");
    sel0.value = labels[idx];
    sel0.onchange = () => pick(sel0.value);

    // every X-axis toggle (the header's and the cast section's) shows the mode and cycles it
    for (const b of document.querySelectorAll(".xmode .xcycle")) {
      b.textContent = state.xmode === "time" ? "Time" : "Distance";
      b.onclick = () => { state.xmode = state.xmode === "time" ? "distance" : "time"; store.set("xmode", state.xmode); renderControls(); renderPanels(); window.UW?.onXMode?.(); };
    }
    // every colour picker (the map's, and the underway strip's) lists the same variables and sets the same choice
    if (!VAR[state.colour]?.resolved && !extraColours.has(state.colour)) state.colour = M.variables.find((v) => v.resolved && !v.derived)?.name || M.variables[0].name;
    for (const sel of document.querySelectorAll("select.colourpick")) {
      sel.innerHTML = "";
      for (const v of [...M.variables, ...extraColours.values()]) {
        if (!v.resolved) continue;
        const o = document.createElement("option");
        o.value = v.name; o.textContent = v.name;
        sel.appendChild(o);
      }
      sel.value = state.colour;
      sel.onchange = () => { state.colour = sel.value; store.set("colour", sel.value); renderControls(); render(); };
    }

    // the map layers: on/off toggles in the bar above the map
    for (const b of document.querySelectorAll("#maplayers button[data-layer]")) {
      const layer = b.dataset.layer;
      b.classList.toggle("on", !!state[layer]);
      b.setAttribute("aria-pressed", String(!!state[layer]));
      b.onclick = () => { state[layer] = !state[layer]; store.set(layer, state[layer]); b.classList.toggle("on", state[layer]); b.setAttribute("aria-pressed", String(state[layer])); if (layer === "photos") closeCamera(); renderMap(); };
    }
    renderSatPill();
    $("#mapattrib").innerHTML = [SITE.raster?.attribution, SITE.vector?.attribution, "Natural Earth 10 m", "GeoNames (CC BY 4.0)", "© MapLibre"].filter(Boolean).join(" · ");
    {
      const r = $("#trackstep"), out = $("#tracksteplabel"), sel = $('#trackstepsel');
      sel.innerHTML=TRACK_STEPS.map((km,i)=>`<option value="${i}">${detailLabel(km)}</option>`).join('');
      $('#trackticks').innerHTML=TRACK_STEPS.map((km,i)=>`<option value="${i}"></option>`).join('');
      if (state.trackKm == null) setTrackDetail(detailFor(currentWindow()?.hours || 1));
      let idx = TRACK_STEPS.indexOf(state.trackKm); if (idx < 0) idx = TRACK_STEPS.length - 1;
      r.value = idx; out.textContent = detailLabel(TRACK_STEPS[idx]); r.setAttribute("aria-valuetext", out.textContent);
      sel.value=idx;
      r.oninput = () => { out.textContent = detailLabel(TRACK_STEPS[r.value]); r.setAttribute("aria-valuetext", out.textContent); };
      r.onchange = () => {
        const before = windowFile(currentWindow());
        setTrackDetail(TRACK_STEPS[r.value]);
        if (windowFile(currentWindow()) !== before) loadWindow(); else renderMap();   // "all points" may mean the fine file
      };
      sel.onchange=()=>{r.value=sel.value;r.onchange();};
    }
    $('#mapdetailsopen').onclick=()=>$('#mapdetails').showModal();
    $('#mapdetailsclose').onclick=()=>$('#mapdetails').close();
    $("#mapreset").onclick = () => { requestFit(); state.focus = null; renderMap(); };
    // how much of the page the map takes: half (the left column), full (the
    // whole page, no pane) or none (the pane takes the whole width). The
    // header pill cycles through them; the map's own — and ⤢ buttons pick
    // none and full (⤢ again, back to half). Every plot resizes after.
    const MAP_MODES = ["half", "full", "none"], MAP_WORD = { half: "Half map", full: "Full map", none: "No map" };
    const MAP_ICON = { half: "◧", full: "■", none: "□" };          // the cycler in the tab row shows the state it is in
    const mapMode = () => { const m = store.get("mapmode", null); return MAP_MODES.includes(m) ? m : "half"; };
    // the classes and labels follow the stored mode; the plots resize and
    // the map refits only when the mode has actually changed (this runs on
    // every controls render, once a minute, and must not touch the view then)
    const applyMapMode = () => {
      const m = mapMode(), main = $("main");
      main.classList.toggle("mapmin", m === "none"); main.classList.toggle("mapfull", m === "full");
      $("#maptoggle").innerHTML = `<span class="ico">${MAP_ICON[m]}</span> Map`; $("#maptoggle").title = `${MAP_WORD[m]} · click for ${MAP_WORD[m === "half" ? (mapDir === "up" ? "full" : "none") : "half"].toLowerCase()}`;
      $("#mapfull").classList.toggle("on", m === "full"); $("#mapfull").textContent = m === "full" ? "⤡" : "⤢";
      if (main.dataset.mapmode === m) return;
      const first = !main.dataset.mapmode;
      main.dataset.mapmode = m;
      if (first) return;                                              // the first draw fits on its own
      setTimeout(() => {
        for (const p of document.querySelectorAll(".plot")) if (p.data) Plotly.Plots.resize(p);
        if (m !== "none" && mapView) { mapView.resize(); requestFit(); renderMap(); }
      }, 0);
    };
    const setMapMode = (m) => { store.set("mapmode", m); applyMapMode(); };
    window.UW = Object.assign(window.UW || {}, { mapMode, setMapMode });
    // the pill swings: none, half, full, half, none, ... so half is always one click away
    let mapDir = "up";
    window.UW = Object.assign(window.UW || {}, { cycleMap() {
      const m = mapMode();
      if (m === "half") setMapMode(mapDir === "up" ? "full" : "none");
      else { mapDir = m === "none" ? "up" : "down"; setMapMode("half"); }
    } });
    $("#mapnone").onclick = () => setMapMode("none");
    $("#mapfull").onclick = () => setMapMode(mapMode() === "full" ? "half" : "full");
    applyMapMode();
  }

  // the satellite pill cycles off → Sentinel-1 → Sentinel-2 → off through
  // the pictures the build has published (a sensor without one is skipped)
  const satImages = () => M?.satellite?.images || {};
  // the pictures of the shown sensor, oldest first: the archive, which ends
  // with the current picture (a region picture shares the current one's
  // corners; a backfilled box round the ship carries its own)
  function satSeries() {
    const im = satImages()[state.sat]; if (!im) return [];
    const arch = (M?.satellite?.archive || {})[state.sat] || [];
    const rows = arch.map((e) => ({ url: e.url, scene: e.scene, corners: e.corners || im.corners, label: im.label }));
    if (!rows.length || rows[rows.length - 1].scene !== (im.scene || im.fetched)) rows.push({ url: im.url, scene: im.scene || im.fetched, corners: im.corners, label: im.label });
    const near = (M?.satellite?.archive || {})[state.sat+'near'] || [];
    const detailed = near.filter(e=>e.corners).map(e=>{
      const background=rows.filter(r=>Date.parse(r.scene)<=Date.parse(e.scene)).at(-1);
      return {...(background||e),scene:e.scene,key:e.url,near:e,label:e.label||'50 m near-ship image'};
    });
    // Each moved high-resolution box is independently browsable, even when
    // its newest satellite scene is unchanged. Never borrow today's bounds.
    const current={...rows.at(-1),key:'latest',near:satImages()[state.sat+'near']||null};
    return [...rows.slice(0,-1),...detailed].sort((a,b)=>Date.parse(a.scene)-Date.parse(b.scene)).concat(current);
  }
  // the picture on the map: the one stepped back to, else the newest
  function satPicture() {
    const rows = satSeries(); if (!rows.length) return null;
    const i = state.satAt ? rows.findIndex((r) => (r.key||r.url) === state.satAt) : -1;
    return { ...(i >= 0 ? rows[i] : rows[rows.length - 1]), index: i >= 0 ? i : rows.length - 1, n: rows.length };
  }
  function renderSatPill() {
    const b = $("#satpill"), imgs = satImages(), kinds = ["s1", "s2"].filter((k) => imgs[k]);
    b.hidden = !kinds.length;
    $("#satnav").hidden = true;
    if (!kinds.length) return;
    if (state.sat && !imgs[state.sat]) state.sat = "";
    const im = imgs[state.sat];
    b.classList.toggle("on", !!im);
    b.textContent = im ? (state.sat === "s1" ? "S1 radar" : "S2 optical") : "Sat off";
    b.title = im ? `${im.label}, newest scene ${im.scene ? fmtTs(Date.parse(im.scene)) + " " + tzAbbr() : "unknown"} · click for ${state.sat === "s1" && imgs.s2 ? "Sentinel-2" : "none"}` : "recent satellite imagery around the ship: Sentinel-1 radar (sees ice through cloud), then Sentinel-2 true colour";
    b.onclick = () => { const i = kinds.indexOf(state.sat); state.sat = i < 0 ? kinds[0] : (kinds[i + 1] || ""); state.satAt = null; store.set("sat", state.sat); renderSatPill(); renderMap(); };
    // the stepper: back and forth through the archive, the newest last
    const pic = satPicture();
    if (!pic) return;
    $("#satnav").hidden = false;
    $("#satwhen").textContent = `${fmtTs(Date.parse(pic.scene)).slice(5)} ${tzAbbr()}` + (pic.n > 1 ? ` · ${pic.index + 1}/${pic.n}` : "");
    $("#satwhen").title = pic.index === pic.n - 1 ? "the newest picture" : "an earlier picture; › steps forward";
    $("#satprev").disabled = pic.index === 0;
    $("#satnext").disabled = pic.index === pic.n - 1;
    const step = (d) => { const rows = satSeries(), j = pic.index + d; if (j < 0 || j >= rows.length) return; state.satAt = j === rows.length - 1 ? null : (rows[j].key||rows[j].url); renderSatPill(); renderMap(); };
    $("#satprev").onclick = () => step(-1);
    $("#satnext").onclick = () => step(1);
  }

  // ------------------------------------------------------------ basemap
  const DEPTHS = [0, 200, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000];   // the bathymetry contours, coloured by C.bathy in order

  // The basemap lives in the MapLibre style: loaded once and re-diffed only
  // when its id changes, so a redraw never re-tiles five megabytes of
  // coastline. With a GEBCO raster the shaded relief carries bathymetry and
  // land, so those files are neither fetched nor drawn.
  async function loadGeo() {
    if (state.geoComplete) return;
    if (!SITE.geo_layers?.length) { state.geoComplete = true; return; }
    const get = async (name) => {
      try { return await fetchJSON(`static/geo/${name}`, { cache: "default" }); } catch { return null; }
    };
    // with coastline vector tiles the shore and the land come from them, not from these files
    const relief = !!SITE.raster, vt = !!SITE.vector;
    const names = { glac: "glaciated_areas.geojson", comm: "communities.geojson", ...(vt ? {} : { coast: "coastline.geojson" }),
                    ...(relief ? {} : { bathy: "bathymetry.geojson", ...(vt ? {} : { land: "land.geojson", isl: "minor_islands.geojson" }) }) };
    const got = Object.fromEntries(await Promise.all(Object.entries(names).map(async ([k, n]) => [k, await get(n)])));
    // settlements (GeoNames): kept as points for a marker trace, not a style layer
    state.communities_data = got.comm ? got.comm.features.map((f) => ({ lon: f.geometry.coordinates[0], lat: f.geometry.coordinates[1], ...f.properties })) : [];
    const geo = { glac: got.glac, coast: got.coast, land: got.land, isl: got.isl, bathy: {} };
    if (got.bathy) {                                       // one source per depth band, each its own fill
      for (const f of got.bathy.features) (geo.bathy[f.properties.depth] ||= []).push(f);
      for (const d in geo.bathy) geo.bathy[d] = { type: "FeatureCollection", features: geo.bathy[d] };
    }
    state.geoSources = geo;
    state.geoStamp = (state.geoStamp || 0) + 1;            // a new style id: the map takes the basemap in once
    state.geoComplete = Object.keys(names).filter((k) => k !== "comm").every((k) => got[k]);
    setLoadError("Basemap", !state.geoComplete);
  }

  // The GEBCO pyramid goes into the style as a proper source so MapLibre knows
  // its maxzoom and scales the deepest tiles at closer zooms. The satellite
  // pictures go in too, under the coastline, so the shore stays legible over
  // them. The style's id names everything in it: the map takes a new style
  // only when the id changes, and MapLibre applies that as a diff.
  function mapStyle(sat, near) {
    const base0 = location.origin + location.pathname.replace(/[^/]*$/, "");
    const relief = !!SITE.raster;
    const style = { version: 8, id: `underway|${state.geoStamp || 0}|${themeName()}|${sat?.url || ""}|${near?.url || ""}`,
                    sources: {},
                    sprite: base0 + (SITE.sprite || "static/geo/sprite"),   // squares, triangles, the ship (tools/make_sprite.py); versioned by the build
                    // MapLibre draws labels (and any symbol layer carrying text) only with a glyph source;
                    // Open Sans Regular PBFs are served locally so it works offline
                    glyphs: base0 + "static/geo/glyphs/{fontstack}/{range}.pbf",
                    layers: [{ id: "bg", type: "background", paint: { "background-color": C.mapBg } }] };
    if (relief) {
      // the pyramid is the globe up to one zoom and, above that, only a box (the
      // Arctic at z9): a source per run, the boxed one with bounds, so MapLibre
      // never asks for a tile that is not there and overzooms the globe elsewhere
      SITE.raster.sources.forEach((s, i) => {
        const id = i ? `gebco${i}` : "gebco";
        style.sources[id] = { type: "raster", tiles: [base0 + SITE.raster.url], tileSize: 256, minzoom: s.minzoom, maxzoom: s.maxzoom,
                              ...(s.bounds ? { bounds: s.bounds } : {}), attribution: SITE.raster.attribution };
        style.layers.push({ id, type: "raster", source: id, paint: { "raster-opacity": 1, "raster-resampling": "linear" } });
      });
    }
    const g = state.geoSources || {};
    const add = (id, data, layer) => { if (!data) return; style.sources[id] = { type: "geojson", data }; style.layers.push({ id, source: id, ...layer }); };
    // the coastline as vector tiles (OpenStreetMap, cut on grid) when the build found them: a
    // shore that stays crisp at every zoom; the Natural Earth files otherwise
    const vt = SITE.vector;
    if (vt) {
      style.sources.coast = { type: "vector", tiles: [base0 + vt.url], minzoom: vt.minzoom, maxzoom: vt.maxzoom,
                              ...(vt.bounds ? { bounds: vt.bounds } : {}), attribution: vt.attribution };
    }
    if (!relief) {
      DEPTHS.forEach((depth, i) => add(`bathy-${depth}`, g.bathy?.[depth], { type: "fill", paint: { "fill-color": C.bathy[i] || C.bathy[C.bathy.length - 1], "fill-opacity": 1 } }));
      if (vt) style.layers.push({ id: "land", type: "fill", source: "coast", "source-layer": "land", paint: { "fill-color": C.mapLand } });
      add("land", g.land, { type: "fill", paint: { "fill-color": C.mapLand } });
      add("islands", g.isl, { type: "fill", paint: { "fill-color": C.mapLand } });
    }
    add("ice", g.glac, { type: "fill", paint: { "fill-color": C.mapIce, "fill-opacity": relief ? .35 : .9 } });
    for (const [id, im, op] of [["sat", sat, .95], ["satnear", near, 1]]) {
      if (!im) continue;
      style.sources[id] = { type: "image", url: new URL(im.url, location.href).href, coordinates: im.corners };
      style.layers.push({ id, type: "raster", source: id, paint: { "raster-opacity": op } });
    }
    if (vt) style.layers.push({ id: "coast", type: "line", source: "coast", "source-layer": "coast", paint: { "line-color": C.mapCoast, "line-width": 1 } });
    add("coast", g.coast, { type: "line", paint: { "line-color": C.mapCoast, "line-width": 1 } });
    return style;
  }

  // Web-Mercator zoom that fits a lat/lon box into the map element, minus a margin.
  function fitView(lats, lons) {
    const el = $("#map");
    const W = Math.max(200, el.clientWidth), H = Math.max(200, el.clientHeight);
    let [lat0, lat1] = minmax(lats), [lon0, lon1] = minmax(lons);
    if (!isFinite(lat0) || !isFinite(lon0)) return { center: { lat: 70, lon: -90 }, zoom: 3 };
    const minSpan = 0.05;                                       // a stationary ship still gets a sensible box
    if (lat1 - lat0 < minSpan) { lat0 -= minSpan / 2; lat1 += minSpan / 2; }
    if (lon1 - lon0 < minSpan) { lon0 -= minSpan / 2; lon1 += minSpan / 2; }
    const mercY = (lat) => { const r = lat * Math.PI / 180; return Math.log(Math.tan(Math.PI / 4 + r / 2)) / (2 * Math.PI); };
    const zLon = Math.log2((W / 512) * 360 / (lon1 - lon0));
    const zLat = Math.log2((H / 512) / (mercY(lat1) - mercY(lat0)));
    const zoom = Math.min(zLon, zLat) - 0.35;
    const cLat = Math.atan(Math.sinh(Math.PI * ((mercY(lat0) + mercY(lat1))))) * 180 / Math.PI;
    return { center: { lat: cLat, lon: (lon0 + lon1) / 2 }, zoom: Math.max(1, Math.min(14, zoom)) };
  }

  // ------------------------------------------------------------ map
  // The scale bar: a 1, 2 or 5 figure of metres or kilometres, as long as it
  // is at the map's centre latitude and zoom (Web Mercator, 512 px tiles), at
  // most 120 px wide, placed over the map's bottom-left corner.
  function updateScale() {
    const el = $("#mapscale"), map = $("#map");
    if (!el || !map || !map.offsetHeight || !state.view?.center) { if (el) el.hidden = true; return; }   // no map shown: no bar
    const mpp = 40075016.686 * Math.cos(state.view.center.lat * Math.PI / 180) / (512 * Math.pow(2, state.view.zoom));
    const maxM = 120 * mpp, pow = Math.pow(10, Math.floor(Math.log10(maxM)));
    const len = [5, 2, 1].map((f) => f * pow).find((L) => L <= maxM) || pow;
    el.querySelector(".bar").style.width = `${Math.round(len / mpp)}px`;
    el.querySelector(".lbl").textContent = len >= 1000 ? `${len / 1000} km` : `${len} m`;
    el.style.left = `${map.offsetLeft + 10}px`; el.style.top = `${map.offsetTop + map.offsetHeight - 24}px`;
    el.hidden = false;
  }
  window.addEventListener("resize", () => updateScale());

  // Ask the next render to fit the track. Stray relayout events (a page
  // reflow fires one on phones) must not put a stale view back before then.
  function requestFit() { state.view = null; state.fitPending = true; }
  // Centre the map on a point (a table row, an event) and mark it.
  function focusMap(lat, lon, label, preserveZoom = false) {
    if (lat == null || lon == null) return;
    const currentZoom = mapView?.getView()?.zoom ?? state.view?.zoom ?? fitView(state.data?.lat || [lat], state.data?.lon || [lon]).zoom;
    const zoom = preserveZoom ? currentZoom : Math.max(currentZoom, 6);
    state.view = { center: { lat: +lat, lon: +lon }, zoom }; state.fitPending = false;
    state.focus = { lat: +lat, lon: +lon, label: label || "" };
    renderMap();
  }
  function chartClickAnywhere(plot,d){
    plot._chartData=d;
    if(plot._chartClickBound)return;plot._chartClickBound=true;
    let down=null;
    plot.addEventListener('pointerdown',e=>{down=[e.clientX,e.clientY]});
    plot.addEventListener('pointerup',e=>{
      if(!down||Math.hypot(e.clientX-down[0],e.clientY-down[1])>5){down=null;return}down=null;
      const axis=plot._fullLayout?.xaxis,data=plot._chartData;if(!axis||!data)return;
      const box=plot.querySelector('.svg-container')?.getBoundingClientRect()||plot.getBoundingClientRect();
      const px=e.clientX-box.left-axis._offset;if(px<0||px>axis._length)return;
      let index=-1,delta=Infinity;
      for(let i=0;i<data.t.length;i++){
        if(data.lat[i]==null||data.lon[i]==null)continue;
        const x=state.xmode==='time'?plotDate(shipAxis(data.t[i])):data.dist_km[i];if(x==null)continue;
        const distance=Math.abs(axis.d2p(x)-px);if(distance<delta){delta=distance;index=i}
      }
      if(index>=0)focusMap(data.lat[index],data.lon[index],fmtTs(data.t[index]),true);
    });
  }
  // The event log comes from data/calendar.json (the Agenda's file); fetched
  // once per build while the layer is on, then grouped by position so several
  // events at one spot share one marker and one hover. The legs before the
  // current ones sit in the calendar archive (M.calendar.archive), fetched
  // only when the filter reaches them and kept across builds by its stamp.
  const evlog = { stamp: null, events: null, loading: false, archive: { stamp: null, events: null, loading: false } };
  function archiveWanted(f) {
    const a = M.calendar?.archive; if (!a) return false;
    if ((a.legs || []).some((id) => f.legs.has(id))) return true;
    const before = tms(a.before);
    return !isNaN(before) && f.start < before;
  }
  function ensureEvents(f) {
    if (evlog.stamp !== M.generated_utc && !evlog.loading) {
      evlog.loading = true;
      fetchJSON(`${M.calendar.file}?v=${encodeURIComponent(M.generated_utc)}`)
        .then((c) => { evlog.events = c.events || []; evlog.stamp = M.generated_utc; renderMap(); })
        .catch(() => {})
        .finally(() => { evlog.loading = false; });
    }
    const a = M.calendar?.archive, ar = evlog.archive;
    if (a && archiveWanted(f) && ar.stamp !== a.stamp && !ar.loading) {
      ar.loading = true;
      fetchJSON(`${a.file}?v=${encodeURIComponent(a.stamp)}`)
        .then((c) => { ar.events = c.events || []; ar.stamp = a.stamp; renderMap(); })
        .catch(() => {})
        .finally(() => { ar.loading = false; });
    }
  }
  function eventTraces(f) {
    if (!state.events) return [];
    ensureEvents(f);
    if (!evlog.events) return [];
    const all = archiveWanted(f) && evlog.archive.events ? [...evlog.archive.events, ...evlog.events] : evlog.events;
    const ok = all.filter((e) => e.lat != null && e.lon != null && isFinite(+e.lat) && isFinite(+e.lon) && Math.abs(+e.lat) <= 90 && Math.abs(+e.lon) <= 180
      && !(+e.lat === 0 && +e.lon === 0) && inFilter(e.leg, e.time_utc, f));
    const groups = new Map();
    for (const e of ok) { const k = `${(+e.lat).toFixed(4)},${(+e.lon).toFixed(4)}`; if (!groups.has(k)) groups.set(k, []); groups.get(k).push(e); }
    const esc = (x) => String(x ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
    const acts = [...new Set(ok.map((e) => e.activity || "other"))];
    const colour = (a) => acts.indexOf(a) % PALETTE_EV.length;         // index into the sprite's tri-N icons
    const pts = [...groups.values()].map((es) => {
      es.sort((a, b) => tms(b.time_utc) - tms(a.time_utc));
      const lines = es.slice(0, 10).map((e) => `${fmtTs(tms(e.time_utc))} · <b>${esc(e.station || "")}</b> ${esc(e.activity || "")}${e.event ? " · " + esc(e.event) : ""}${e.label ? " <i>" + esc(e.label) + "</i>" : ""}${e.comment ? "<br>&nbsp;&nbsp;" + esc(e.comment) : ""}`);
      if (es.length > 10) lines.push(`… +${es.length - 10} more`);
      return { lat: +es[0].lat, lon: +es[0].lon, n: es.length, text: (es.length > 1 ? `<b>${es.length} events here</b><br>` : "") + lines.join("<br>"), colour: colour(es[0].activity || "other") };
    });
    const bySize = new Map();
    for (const p of pts) { const sz = eventBucket(p.n); if (!bySize.has(sz)) bySize.set(sz, []); bySize.get(sz).push(p); }
    return [...bySize.entries()].sort((a, b) => a[0] - b[0]).map(([sz, ps]) => ({
      type: "scattermap", mode: "markers", name: "event log", showlegend: false, hoverinfo: "text",
      lat: ps.map((p) => p.lat), lon: ps.map((p) => p.lon), text: ps.map((p) => p.text),
      marker: { symbol: ps.map((p) => `tri-${p.colour}`), size: sz, opacity: .95 },
    }));
  }
  const PALETTE_EV = ["#7ee787", "#d2a8ff", "#f2cc60", "#79c0ff", "#ffa198", "#56d364", "#e3b341", "#a5d6ff", "#ff9bce", "#ffb454"];

  // Daily camera timelapses (dashboard.cameras): a camera glyph where the
  // day's shots were taken; a click plays the day's video in a popup over
  // the map, with previous/next stepping through the shown days.
  const camsShown = (f = spanFilter()) => (M.cameras || []).map((c, i) => ({ ...c, i }))
    .filter((c) => c.lat != null && c.lon != null && state.photos && inFilter(c.leg, c.mid_utc, f))
    .sort((a, b) => a.day.localeCompare(b.day));
  function cameraTraces(f) {
    const cs = camsShown(f);
    if (!cs.length) return [];
    return [{
      type: "scattermap", mode: "markers", name: "cameras", showlegend: false, hoverinfo: "text",
      lat: cs.map((c) => c.lat), lon: cs.map((c) => c.lon), customdata: cs.map((c) => `cam:${c.i}`),
      text: cs.map((c) => `<b>Camera timelapse</b> ${c.day.slice(0, 4)}-${c.day.slice(4, 6)}-${c.day.slice(6, 8)}` +
        `<br>${c.frames} shots${c.complete ? "" : " so far"} · ${legById(c.leg)?.label || c.leg || ""}<br><i>click to play</i>`),
      marker: { symbol: "camera", size: 11, opacity: .95, allowoverlap: true },
    }];
  }
  function openCamera(i) {
    const c = (M.cameras || [])[i];
    const pop = $("#campop"), vid = $("#camvideo");
    if (!c || !pop) return;
    const cs = camsShown(); const at = cs.findIndex((x) => x.i === i);
    $("#camtitle").textContent = `${c.day.slice(0, 4)}-${c.day.slice(4, 6)}-${c.day.slice(6, 8)}`;
    $("#camsub").textContent = `${legById(c.leg)?.label || c.leg || ""} · ${c.frames} shots${c.complete ? "" : " so far"}`;
    $("#camfoot").innerHTML = `${c.start_utc ? fmtTs(Date.parse(c.start_utc)) : ""} → ${c.end_utc ? fmtTs(Date.parse(c.end_utc)).slice(11) : ""} ${tzAbbr()} · ` +
      `<a href="${c.url}" target="_blank" rel="noopener">open video</a>`;
    $("#camprev").disabled = at <= 0; $("#camnext").disabled = at < 0 || at >= cs.length - 1;
    $("#camprev").onclick = () => { if (at > 0) openCamera(cs[at - 1].i); };
    $("#camnext").onclick = () => { if (at >= 0 && at < cs.length - 1) openCamera(cs[at + 1].i); };
    $("#camclose").onclick = closeCamera;
    vid.classList.toggle("wide", c.layout !== "portrait");
    const src = new URL(c.url, location.href).href;
    if (vid.src !== src) { vid.src = src; vid.load(); }
    pop.hidden = false;
    vid.play?.().catch(() => { /* autoplay may be refused; the controls remain */ });
    state.cameraOpen = i;
  }
  function closeCamera() {
    const pop = $("#campop"), vid = $("#camvideo");
    if (!pop || pop.hidden) return;
    vid.pause?.(); vid.removeAttribute("src"); vid.load?.();
    pop.hidden = true; state.cameraOpen = null;
  }

  // Settlements: a labelled marker each; labels thin out with zoom so the
  // scientific layers stay readable (population 2000+ far out, all close in).
  // Places and events come in size buckets, a trace each; an icon's scale is
  // marker.size / 10 of its sprite image.
  const PLACE_BUCKETS = [[0, 6], [1, 8], [200, 10], [1000, 13], [5000, 16]];       // [min population, size]
  const placeBucket = (pop) => { let b = PLACE_BUCKETS[0]; for (const x of PLACE_BUCKETS) if ((pop || 0) >= x[0]) b = x; return b[1]; };
  const EVENT_BUCKETS = [[1, 8.3], [2, 10], [4, 12]];                               // [min events at the spot, size]: 8.3 → a 10 px triangle
  const eventBucket = (n) => { let b = EVENT_BUCKETS[0]; for (const x of EVENT_BUCKETS) if (n >= x[0]) b = x; return b[1]; };

  // Places (settlements): one labelled square each; labels thin out with zoom
  // so the scientific layers stay readable (population 2000+ far out, all
  // close in). Returns one trace per size bucket, all named "places".
  function placeTraces(zoom) {
    if (!state.communities || !state.communities_data?.length) return [];
    const minPop = zoom < 3.5 ? 2000 : zoom < 5 ? 400 : zoom < 6.5 ? 100 : 0;
    const esc = (x) => String(x ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
    const groups = new Map();
    for (const c of state.communities_data) { if (!(c.pop > 0)) continue; const sz = placeBucket(c.pop); if (!groups.has(sz)) groups.set(sz, []); groups.get(sz).push(c); }   // the empty sites are the wiki's Places
    return [...groups.entries()].sort((a, b) => a[0] - b[0]).map(([sz, cs]) => ({
      type: "scattermap", mode: "markers+text", name: "places", showlegend: false, hoverinfo: "text",
      lat: cs.map((c) => c.lat), lon: cs.map((c) => c.lon),
      text: cs.map((c) => (c.pop >= minPop || (c.code === "PPLA" && zoom >= 2.5)) ? c.name : ""),
      hovertext: cs.map((c) => `<b>${esc(c.name)}</b>${c.alt?.length ? " · " + esc(c.alt.join(" · ")) : ""}<br>${esc(c.region)}, ${c.cc === "GL" ? "Greenland" : "Canada"}${c.pop ? ` · pop. ${c.pop.toLocaleString()}` : ""}`),
      textposition: "top right", textfont: { size: fz(11), color: "#f2e7c9", family: "Open Sans Regular" },
      marker: { symbol: "square", size: sz, opacity: .9 },
    }));
  }
  // the ship glyph is drawn bow-right (east); marker.angle turns it (degrees
  // clockwise from north, aligned to the map)
  const shipRotate = () => Math.round(((state.shipHeading - 90) % 360 + 360) % 360);
  // Station labels: one per station name (the latest visit), thinned to one
  // per map cell so they never pile up; far out only the stations without a
  // cast and the most recent casts survive, close in every name shows.
  // the plan's station labels: from zoom 5 in, one per label cell, the first
  // named station of a cell winning
  function planLabels(st, zoom) {
    if (zoom < 5) return st.map(() => "");
    const cell = 40 / Math.pow(2, zoom), cells = new Set();
    return st.map((s) => {
      const key = `${Math.floor(s.lat / cell)}:${Math.floor(s.lon * Math.cos(s.lat * Math.PI / 180) / cell)}`;
      if (!s.name || cells.has(key)) return "";
      cells.add(key); return s.name;
    });
  }
  // Plans on the map: the leg's own (published by the build, charcoal) and
  // any this browser has loaded by dropping a KMZ (kept in its local
  // storage, one pill and one colour each). Every shown plan draws its
  // tracks (the alternate dim) and its stations, labelled by zoom.
  const PLAN_COLOURS = ["#ffb454", "#ff9bce", "#7ee787", "#c9a2ff"];
  const userPlans = () => store.get("plans.user", []);
  const planOn = (i) => !!(store.get("plans.on", {})[i] ?? true);
  // a plan's import stamp, "YYYY-MM-DD.n": the day it came and its number that day
  const planStamp = (iso, n = 1) => `${String(iso || "").slice(0, 10) || "unknown"}.${n}`;
  function plansShown() {
    const out = [];
    if (state.plan && state.planData) out.push({ ...state.planData, key: "plan", colour: "#454f5b", label: "#aab3bd", imported: planStamp(M?.plan?.stamp) });
    userPlans().forEach((pl, i) => { if (planOn(i)) out.push({ ...pl, key: `user${i}`, colour: PLAN_COLOURS[i % PLAN_COLOURS.length], label: PLAN_COLOURS[i % PLAN_COLOURS.length], imported: pl.imported || planStamp(null) }); });
    return out;
  }
  function planTraces(zoom) {
    const out = [];
    for (const pl of plansShown()) {
      // hover boxes like the stations': the text alone, no trace name beside it
      for (const t of pl.tracks) out.push({ type: "scattermap", mode: "lines", name: `${pl.key}-${t.alternate ? "alt" : "track"}`, showlegend: false,
        lat: t.coords.map((c) => c[1]), lon: t.coords.map((c) => c[0]), hovertext: t.coords.map(() => t.name), hovertemplate: "%{hovertext}<extra></extra>",
        line: { width: t.alternate ? 1.2 : 2.2, color: pl.colour }, opacity: t.alternate ? .45 : .95 });
      if (pl.stations.length) out.push({ type: "scattermap", mode: "markers+text", name: `${pl.key}-stations`, showlegend: false,
        lat: pl.stations.map((s) => s.lat), lon: pl.stations.map((s) => s.lon), text: planLabels(pl.stations, zoom), textposition: "top right", textfont: { size: fz(11), color: pl.label },
        hovertext: pl.stations.map((s) => `<b>${esc(s.name)}</b>${s.type ? " · " + esc(s.type) : ""}${s.region ? "<br>" + esc(s.region) : ""}${s.group ? "<br>" + esc(s.group) : ""}${s.depth_m != null ? `<br>depth ${Math.round(s.depth_m)} m` : ""}<br>planned station`),
        hovertemplate: "%{hovertext}<extra></extra>", marker: { size: 7, color: pl.colour, opacity: .95 } });
    }
    return out;
  }
  // the pills for this browser's own plans, after the Plan pill; ✕ forgets one
  function renderPlanPills() {
    for (const b of document.querySelectorAll("#maplayers button.userplan")) b.remove();
    const anchor = document.querySelector('#maplayers button[data-layer="plan"]'); if (!anchor) return;
    userPlans().forEach((pl, i) => {
      const b = document.createElement("button");
      b.className = `userplan ${planOn(i) ? "on" : ""}`; b.type = "button"; b.title = `${pl.name} · ${pl.stations.length} stations (this browser only) · ✕ forgets it`;
      b.innerHTML = `${esc(pl.short || `Plan ${i + 2}`)}<span class="x" title="forget this plan">✕</span>`;
      b.onclick = (e) => {
        if (e.target.classList.contains("x")) { const all = userPlans(); all.splice(i, 1); store.set("plans.user", all); store.set("plans.on", {}); renderPlanPills(); renderMap(); return; }
        const on = store.get("plans.on", {}); on[i] = !planOn(i); store.set("plans.on", on); renderPlanPills(); renderMap();
      };
      anchor.after(b);
    });
  }
  // the plan file follows the manifest: a new version (a build, or a drop
  // on the map) is fetched and drawn
  async function loadPlan() {
    const p = M?.plan;
    if (!p) { if (state.planData) { state.planData = null; state.planStamp = null; renderMap(); } return; }
    if (p.stamp === state.planStamp) return;
    try { state.planData = await fetchJSON(p.file); state.planStamp = p.stamp; renderMap(); }
    catch { /* the next manifest */ }
  }
  // a KMZ or KML dropped on the map replaces the plan for everyone
  function wirePlanDrop() {
    const sec = document.querySelector("section.map"); if (!sec) return;
    sec.addEventListener("dragover", (e) => { if ([...(e.dataTransfer?.types || [])].includes("Files")) { e.preventDefault(); sec.classList.add("dropping"); } });
    sec.addEventListener("dragleave", () => sec.classList.remove("dropping"));
    sec.addEventListener("drop", async (e) => {
      sec.classList.remove("dropping");
      const f = e.dataTransfer?.files?.[0]; if (!f) return;
      e.preventDefault();
      if (!/\.(kmz|kml)$/i.test(f.name)) { toast(`${f.name}: not a KMZ or KML`); return; }
      toast(`reading ${f.name}…`);
      try {
        const r = await fetch("api/plan", { method: "POST", headers: { "Content-Type": "application/octet-stream" }, body: f });
        const j = await r.json();
        if (!r.ok) throw new Error(j.error || r.status);
        const all = userPlans(), today = new Date().toISOString().slice(0, 10);
        const nToday = all.filter((pl) => String(pl.imported || "").startsWith(today)).length + (M?.plan?.stamp?.startsWith(today) ? 1 : 0);
        all.push({ name: j.name, short: f.name.replace(/\.(kmz|kml)$/i, "").slice(0, 18), imported: `${today}.${nToday + 1}`, tracks: j.tracks, stations: j.stations, groups: j.groups });
        store.set("plans.user", all);
        renderPlanPills(); renderMap();
        toast(`plan loaded in this browser: ${j.name} · ${j.stations.length} stations, ${j.tracks.length} tracks`);
      } catch (err) { toast(`plan not loaded: ${err.message}`); }
    });
  }
  function stationLabels(st, zoom) {
    if (zoom < 2.5) return st.map(() => "");
    const cell = 40 / Math.pow(2, zoom);                      // degrees of latitude per label cell
    const order = st.map((s, i) => i).sort((a, b) => (st[b].kind === "event") - (st[a].kind === "event") || String(st[b].time).localeCompare(String(st[a].time)));
    const names = new Set(), cells = new Set(), out = st.map(() => "");
    for (const i of order) {
      const s = st[i], name = (s.station || "").trim();
      if (!name || names.has(`${s.leg}:${name}`)) continue;
      const key = `${Math.floor(s.lat / cell)}:${Math.floor(s.lon * Math.cos(s.lat * Math.PI / 180) / cell)}`;
      if (zoom < 8 && cells.has(key)) continue;
      names.add(`${s.leg}:${name}`); cells.add(key); out[i] = name;
    }
    return out;
  }
  // Labels follow the zoom: the places' by population bucket, the stations'
  // and the plans' by half a zoom level. A zoom that ends in another bucket
  // redraws the map with the labels for it.
  const labelBuckets = (z) => [z < 3.5 ? 0 : z < 5 ? 1 : z < 6.5 ? 2 : 3, Math.round(z * 2) / 2];
  let labelsAt = [null, null];
  function onMapZoom() {
    const v = mapView?.getView(); if (!v) return;
    const [b, h] = labelBuckets(v.zoom);
    const places = state.communities && b !== labelsAt[0], stations = (state.stationList?.length || plansShown().length) && h !== labelsAt[1];
    if (!places && !stations) return;
    state.view = v; renderMap();
  }
  function mapMessage(text) { const m = $("#mapmsg"); m.hidden = !text; m.textContent = text || ""; }

  // Track detail: the window's points thinned to one per so many km along
  // the track (the gap markers, and the last fix, always stay). Every array
  // of the window is cut the same way, so hover, colours and the pump marks
  // line up with the points drawn.
  const TRACK_STEPS = [50, 20, 10, 5, 2, 1, 0.5, 0];         // left to right: coarser to every point
  // the span picks a starting detail (up to a week: a point a km; months:
  // 5 km; years: 20 km) that the slider then overrides; "all points" is a
  // choice, never the default
  // the track detail a span asks for: every point up to half a day, then coarser as the span grows
  const detailFor = (hours) => hours <= 12 ? 0 : hours <= 48 ? 0.5 : hours <= 24 * 8 ? 1 : hours <= 24 * 62 ? 5 : 20;
  const detailLabel = (km) => km ? `1 per ${km} km` : "all points";
  const currentWindow = () => M.windows.find((x) => x.label === state.win);
  // "all points" loads the window's fine variant when the build made one
  const windowFile = (w) => (state.trackKm === 0 && w?.fine_file) ? w.fine_file : w?.file;
  function setTrackDetail(km) {
    state.trackKm = km; store.set("trackKm", km);
    const r = $("#trackstep"), out = $("#tracksteplabel");
    if (r) { const i = TRACK_STEPS.indexOf(km); r.value = i < 0 ? TRACK_STEPS.length - 1 : i; out.textContent = detailLabel(km); r.setAttribute("aria-valuetext", detailLabel(km)); }
    if($('#trackstepsel'))$('#trackstepsel').value=r.value;
  }
  let thinCache = { src: null, km: null, out: null };
  function thinTrack(d, km) {
    if (!d || !km) return d;
    if (thinCache.src === d && thinCache.km === km) return thinCache.out;
    const n = d.t.length, keep = [];
    let last = -1, bucket = null;
    for (let i = n - 1; i >= 0; i--) if (d.lat[i] != null) { last = i; break; }
    for (let i = 0; i < n; i++) {
      if (d.lat[i] == null) { keep.push(i); continue; }
      const b = Math.floor((d.dist_km[i] ?? 0) / km);
      if (b !== bucket || i === last) { keep.push(i); bucket = b; }
    }
    const cut = (a) => Array.isArray(a) && a.length === n ? keep.map((i) => a[i]) : a;
    const out = {};
    for (const [k, v] of Object.entries(d)) out[k] = k === "vars" ? Object.fromEntries(Object.entries(v).map(([name, a]) => [name, cut(a)])) : cut(v);
    out.shown = keep.filter((i) => d.lat[i] != null).length;
    thinCache = { src: d, km, out };
    return out;
  }
  // the ship's position: the intranet live page when it is newer than the
  // record's last fix, else that fix (with the build's averaged heading)
  function shipNow(d, li) {
    const live = window.UW?.shipLive;
    const recT = li >= 0 ? d.t[li] : -Infinity;
    if (live && live.lat != null && live.t > recT) {
      return { lat: live.lat, lon: live.lon, heading: live.heading, t: live.t,
        text: `CCGS Amundsen · live · ${fmtTs(live.t)} ${tzAbbr()} · heading ${live.heading != null ? live.heading.toFixed(0) + "°" : "unknown"}${live.speed != null ? ` · ${live.speed.toFixed(1)} kn` : ""}` };
    }
    if (li < 0) return { lat: null };
    const heading = M.latest?.heading ?? null;
    return { lat: d.lat[li], lon: d.lon[li], heading, t: recT,
      text: `CCGS Amundsen · latest · ${fmtTs(d.t[li])} ${tzAbbr()} · heading ${heading != null ? heading.toFixed(0) + "°" : "unknown"}` };
  }
  // the ship at her latest position, and the focus mark: the map's live layers,
  // which the poller moves without touching the rest
  function liveTraces(ship) {
    const out = [];
    // the sprite's red-and-white Amundsen glyph turned to the heading the build
    // averaged over the last ten minutes (a window's last bin swings with the
    // bin width); with no heading to turn it to, a plain red dot stands in
    if (ship?.lat != null) out.push({
      type: "scattermap", mode: "markers", name: "latest", lat: [ship.lat], lon: [ship.lon], hoverinfo: "text", text: [ship.text],
      marker: ship.heading != null ? { symbol: "ship", size: 11, opacity: 1, angle: shipRotate() } : { size: 12, color: "#d52b1e", opacity: 1 },
    });
    if (state.focus) out.push({
      type: "scattermap", mode: "markers", name: "focus", hoverinfo: "text", text: [state.focus.label, state.focus.label],
      lat: [state.focus.lat, state.focus.lat], lon: [state.focus.lon, state.focus.lon],
      marker: { size: [22, 12], color: [C.accent, C.bg], opacity: [.9, 1] },
    });
    return out;
  }
  const lastFix = (d) => { for (let i = d.lat.length - 1; i >= 0; i--) if (d.lat[i] != null) return i; return -1; };
  // called by the live poller: move the marker without redrawing the map
  function moveShip() {
    const d = state.data;
    if (!mapView || !d) return;
    const ship = shipNow(d, lastFix(d));
    if (ship.lat == null) return;
    state.shipHeading = ship.heading;
    mapView.setTraces("live", liveTraces(ship));
  }
  // The colour scale beside every Color by picker: the colour map's gradient
  // with the limits at its ends, from the stops the map and the charts share.
  function renderColourBar(v, lim) {
    const stops = UW.cmap(v?.cmap || "Viridis", !!v?.reverse);   // a map read the other way (depth: deep is dark)
    const [low,high]=UWMapLegend.formatRange(lim);
    UW.mapLegend={name:state.colour,stops,showScale:!!(!v?.rgb&&lim&&isFinite(lim[0])&&isFinite(lim[1])),low,high};
    renderMapLegend();
    for (const bar of document.querySelectorAll(".cbar")) {
      const show = !v?.rgb && lim && isFinite(lim[0]) && isFinite(lim[1]);
      bar.hidden = !show;
      if (!show) continue;
      bar.querySelector(".grad").style.background = `linear-gradient(90deg, ${stops.map(([t, col]) => `${col} ${(t * 100).toFixed(1)}%`).join(", ")})`;
      bar.querySelector(".lo").textContent = low;
      bar.querySelector(".hi").textContent = high;
      bar.title = `${state.colour}: the colour scale of the track and the graph points, from the 5th to the 95th percentile of the span`;
    }
  }
  function renderMapLegend() {
    const el=$('#map');if(!mapView?.map)return;
    let canvas=el.querySelector('.map-legend');
    if(!canvas){canvas=document.createElement('canvas');canvas.className='map-legend';canvas.setAttribute('role','img');el.append(canvas);}
    UWMapLegend.render(canvas,el.clientWidth,el.clientHeight,UW.mapLegend);
  }
  // the map (static/map.js): made on the first draw, kept for the page's life
  let mapView = null, mapData = null;
  // a click on a point: the handlers of what it belongs to; a click where
  // there is no point takes the focus mark away
  function mapClick(p) {
    const d = mapData;
    if (p?.data?.name === "track" && extraColours.get(state.colour)?.onPoint) return extraColours.get(state.colour).onPoint(d, p.pointIndex ?? p.pointNumber);
    if (typeof p?.customdata === "string" && p.customdata.startsWith("cam:")) return openCamera(+p.customdata.slice(4));
    if (typeof p?.customdata === "string" && p.customdata.startsWith("hist:")) return window.UW?.onHistoryClick?.(p.customdata.slice(5), p);
    if (typeof p?.customdata === "string" && p.customdata.startsWith("nat:")) return window.UW?.onNatureClick?.(p.customdata.slice(4), p);
    if (p?.data?.name === "focus" && window.UW?.onFocusClick?.(p)) return;   // the mark took the click meant for the point under it
    if (p?.lat != null && p.data?.name !== "focus") { state.focus = { lat: +p.lat, lon: +p.lon, label: String(p.text || p.hovertext || "").replace(/<[^>]+>/g, "") }; renderMap(); }   // the mark moves to what was clicked
    if (p?.customdata) window.UW?.onStationClick?.(p.customdata);
  }
  function mapEmptyClick() { if (state.focus) { state.focus = null; renderMap(); } }
  function renderMap() {
    const d = thinTrack(state.span, state.trackKm);
    const el = $("#map");
    if (!d || !(d.shown ?? d.n)) { mapView?.clear(); mapMessage(d ? "nothing to show: no legs selected, or no track in this span" : "no data"); $("#mapfoot").textContent = ""; return; }
    mapMessage("");

    const v = VAR[state.colour] || extraColours.get(state.colour);
    const customColour = extraColours.get(state.colour)?.values(d);
    const c = customColour || d.vars[state.colour] || [];
    const lim = d.limits[state.colour] || minmax(c);
    const hover = d.t.map((ms, i) => d.lat[i] == null ? "" :
      `<b>${legByIndex(d.leg[i])?.label || ""}</b> · ${fmtTs(ms)} ${tzAbbr()}<br>${state.colour}: <b>${v?.rgb ? (c[i] === '#000000' ? 'no nearby photo' : c[i]) : fmtVal(c[i], v?.unit)}</b>` +
      `<br>${dms(d.lat[i], d.lon[i])}<br>${(d.dist_km[i] ?? 0).toFixed(1)} km along track`);

    // draw order is click order: MVP tows from the cast tab go under the
    // track, and the station markers stay on top so they get the clicks
    // draw order, bottom to top: tow tracks, the ship's track, communities,
    // event-log entries, then the stations (which keep the clicks)
    const f0 = spanFilter();
    const view = (!state.fitPending && state.view) || fitView(d.lat, d.lon);
    labelsAt = labelBuckets(view.zoom);
    const traces = [...planTraces(view.zoom), ...(window.UW?.extraMapTraces?.() || [])];
    const placeTr = placeTraces(view.zoom);
    const evTraces = eventTraces(f0);
    if (state.track) traces.push({
      type: "scattermap", mode: "lines+markers", name: "track",
      lat: d.lat, lon: d.lon, text: hover, hoverinfo: "text", connectgaps: false,
      line: { width: 1.4, color: "rgba(200,215,230,.5)" },
      marker: { size: v?.sizes?.(d) || 6, color: c, colorscale: v?.cmap || "Viridis", reversescale: !!v?.reverse, cmin: v?.rgb ? undefined : lim?.[0], cmax: v?.rgb ? undefined : lim?.[1], showscale: false, opacity: .95 },   // the scale sits by the Color by pickers (renderColourBar)
    });
    // coloured by a TSG variable, the track goes grey where the pump was off
    if (state.track && extraColours.has(state.colour) && !v?.rgb) traces.push({
      type:'scattermap',mode:'markers',name:'no nearby photo',showlegend:false,
      lat:d.lat.map((q,i)=>c[i]==null?q:null),lon:d.lon.map((q,i)=>c[i]==null?q:null),
      marker:{size:6,color:'#000000'},hovertemplate:'No matching photo<extra></extra>'
    });
    const lowMap = v?.tsg ? pumpLow(d) : null;
    if (state.track && lowMap && lowMap.some(Boolean)) traces.push({
      type: "scattermap", mode: "markers", name: "pump off", showlegend: false,
      lat: d.lat.map((q, i) => (lowMap[i] ? q : null)), lon: d.lon.map((q, i) => (lowMap[i] ? q : null)),
      text: hover.map((h, i) => (lowMap[i] ? h + "<br><i>intake pump off</i>" : "")), hoverinfo: "text",
      marker: { size: 6, color: "#7d8895", opacity: .8 },
    });
    // the intranet's live page, polled every few seconds, is fresher than any
    // file: while it is, the ship stands where it says
    const ship = shipNow(d, lastFix(d));
    state.shipHeading = ship.heading;
    traces.push(...placeTr, ...evTraces, ...cameraTraces(f0));
    const shownIds = new Set(shownLegs().map((l) => l.id));
    const f = spanFilter();
    // CTD casts (white; orange when selected) and the stations the event log
    // records without a cast (green), each a click target
    const st = state.stations ? (M.stations || []).filter((s) => inFilter(s.leg, s.time, f)) : [];
    const selected = window.UW?.selectedCastKeys?.() || new Set();
    const stKey = (s) => s.kind === "event" ? `ev:${s.leg}:${s.station}` : `${s.leg}:CTD_${String(s.cast).padStart(3, "0")}`;
    const stText = (s) => s.kind === "event"
      ? `<b>${s.station}</b>${s.type ? " · " + s.type : ""} · ${legById(s.leg)?.label || s.leg}<br>${(s.time || "").slice(0, 16)}${s.time_end && s.time_end !== s.time ? " → " + s.time_end.slice(0, 16) : ""}` +
        `<br>${(s.activities || []).length > 3 ? `${s.activities.length} events` : (s.activities || []).join(", ")}${s.bottom_m != null ? `<br>depth ${Math.round(s.bottom_m)} m` : ""}${s.comments ? "<br><i>" + s.comments + "</i>" : ""}`
      : `<b>Cast ${s.cast}</b> ${s.station}${s.label ? " · " + s.label : ""} · ${legById(s.leg)?.label || s.leg}` +
        `<br>${s.time || ""}${s.type ? "<br>" + s.type : ""}${s.bottom_m != null ? `<br>bottom ${s.bottom_m} m` : ""}` +
        `${s.comments ? "<br><i>" + s.comments + "</i>" : ""}`;
    state.stationList = st;
    if (st.length) traces.push({
      type: "scattermap", mode: "markers+text", name: "stations", showlegend: false,
      lat: st.map((s) => s.lat), lon: st.map((s) => s.lon), hoverinfo: "text",
      customdata: st.map(stKey), hovertext: st.map(stText), text: stationLabels(st, view.zoom),
      textposition: "top right", textfont: { size: fz(11), color: "#e8f4ff", family: "Open Sans Regular" },
      marker: { size: st.map((s) => selected.has(stKey(s)) ? 14 : 9),
                color: st.map((s) => selected.has(stKey(s)) ? C.accent2 : s.kind === "event" ? C.ok : "rgba(255,255,255,.9)"),
                opacity: .95 },
    });
    // the satellite picture under the track, and the same sensor at 50 m in
    // a box round the ship over it: both go into the style with the basemap
    const sat = (state.sat && satPicture()) || null;
    const near = sat?.near || null;
    mapData = d;
    if (!mapView) {
      mapView = new UW.MapView(el, { onClick: mapClick, onEmptyClick: mapEmptyClick, onZoom: onMapZoom,
        onMove: (v) => { if (!state.fitPending) state.view = v; updateScale(); } });
      window.UW.mapView = mapView;
      $('#mapexport').onclick = () => { if(mapView.map)window.UWPlotExport.openMap(mapView.map); };
    }
    try {
      mapView.draw({ style: mapStyle(sat, near), view, base: traces, live: liveTraces(ship) }).then(() => {
        state.fitPending = false;
        if (!state.view) state.view = view;
        updateScale();
      });
      mapMessage("");
    } catch (e) {
      console.warn("map draw:", e);
      mapMessage("Map unavailable; other plots and tables remain usable. Try resetting the map.");
    }
    try { renderColourBar(v, lim); } catch { /* the bar is decoration */ }

    // distance travelled: the along-track extent of each selected leg's
    // points in the span (dist_km runs on through the whole record)
    const ext = new Map();
    d.dist_km.forEach((x, i) => { if (x == null || d.lat[i] == null || d.leg[i] == null) return; const e = ext.get(d.leg[i]); if (!e) ext.set(d.leg[i], [x, x]); else { e[0] = Math.min(e[0], x); e[1] = Math.max(e[1], x); } });
    const km = [...ext.values()].reduce((a, [lo, hi]) => a + hi - lo, 0);
    const nLegs = shownLegs().length;
    $("#mapfoot").innerHTML =
      `<span><b>${d.label}</b> span · <b>${nLegs}</b> leg${nLegs === 1 ? "" : "s"} selected · <b>${km.toFixed(0)} km</b> travelled</span>` +
      (st.length ? `<span><b>${st.filter((s) => s.kind !== "event").length}</b> CTD casts${st.some((s) => s.kind === "event") ? ` · <b>${st.filter((s) => s.kind === "event").length}</b> other stations` : ""}</span>` : "") +
      `<span class="mono">${fmtTs(Date.parse(d.start))} → ${fmtTs(Date.parse(d.end))} ${tzAbbr()}</span>` +
      (state.sat && satPicture() ? `<span><b>${satPicture().label}</b> · newest scene ${fmtTs(Date.parse(satPicture().scene))} ${tzAbbr()}${near ? ` · 50 m box near the ship from ${fmtTs(Date.parse(near.scene || near.fetched)).slice(11)}` : ""} · Copernicus Sentinel data</span>` : "") +
      plansShown().map((pl) => `<span title="drop a KMZ or KML on the map to add a plan of your own"><b>Plan</b> ${esc(pl.name)} · ${pl.stations.length} stations</span>`).join("") +
      `<span class="hint"><span class="maphint" id="maphint" ${document.querySelector("main")?.classList.contains("tab-casts") ? "" : "hidden"}>click a station to add its cast · </span>scroll to zoom · drag to pan · ⟲ fits</span>`;
  }

  // ------------------------------------------------------------ panels
  function panelNames() {
    const all = [...M.variables.map((v) => v.name), ...extraPanels.keys()].filter((n) => !NOT_PANELS.has(n));
    const ordered = state.order.filter((n) => all.includes(n));
    const result=[...ordered, ...all.filter((n) => !ordered.includes(n))];
    for(const [name,spec] of extraPanels)if(spec.after&&!ordered.includes(name)){const i=result.indexOf(name);if(i>=0)result.splice(i,1);const at=result.indexOf(spec.after);result.splice(at<0?result.length:at+1,0,name);}
    return result;
  }

  // A panel draws when it is in view. Two dozen panels redraw with every
  // build, and drawing a coloured SVG marker per point is the page's main
  // cost, so the panels scrolled past (or behind another tab) keep their
  // new data and draw when they are looked at.
  const inView = new Set();
  const panelWatch = "IntersectionObserver" in window ? new IntersectionObserver((entries) => {
    let due = false;
    for (const e of entries) {
      if (e.isIntersecting) { inView.add(e.target.dataset.name); if (e.target.dataset.stale) due = true; }
      else inView.delete(e.target.dataset.name);
    }
    if (due) for (const el of $("#panels").children) if (el.dataset.stale && inView.has(el.dataset.name)) renderPanel(el.dataset.name);
  }, { rootMargin: "200px 0px" }) : null;
  // Keep detached, minimized panels too: restoring them reuses their chart
  // and observer instead of creating another observed DOM tree.
  const panelElements = new Map();
  function panelEl(name) {
    let el = panelElements.get(name);
    if (el) return el;
    const v = VAR[name] || extraPanels.get(name);
    el = document.createElement("section");
    el.className = "panel card"; el.id = "p-" + cssId(name); el.dataset.name = name; el.draggable = true;
    if (name.startsWith("Surprise")) el.classList.add("surprise");
    el.innerHTML = `<div class="head">
        <span class="handle" title="drag to reorder">⋮⋮</span>
        <h3 title="colour everything by this variable">${name}</h3>
        <div class="tools"><span class="now"></span>
          ${!isDepth(name) && v?.log_ok ? '<button class="log" title="log10 y-axis">log</button>' : ""}
          <button class="reset" title="reset zoom">⟲</button>
          ${isDepth(name) ? '<button class="dscale depthscale" title="compress the depth axis (square root)" aria-label="Toggle square-root depth spacing">⇅</button>' : ''}
          <button class="min" title="minimise to its group">—</button>
          <button class="wide" title="expand">⤢</button>
        </div></div><div class="plot"></div>`;
    el.querySelector("h3").textContent = v?.label || name;
    el.querySelector("h3").onclick = () => selectPanel(name);
    if (extraPanels.has(name)) { el.querySelector("h3").onclick = extraPanels.get(name).onTitle || null; el.querySelector("h3").title = extraPanels.get(name).description || name; }
    el.querySelector(".plot").addEventListener("click", () => { if (!el.classList.contains("on")) selectPanel(name); }, true);
    el.querySelector(".log")?.addEventListener("click", () => { state.log[name] = !state.log[name]; store.set("log", state.log); renderPanel(name); });
    el.querySelector('.depthscale')?.addEventListener('click', () => { state.depthScale[name] = !state.depthScale[name]; store.set('depthScale', state.depthScale); renderPanel(name); });
    el.querySelector(".reset").onclick = async () => { const r = state.data && spanRange(state.data), plot=el.querySelector('.plot'); await Plotly.relayout(plot, { ...(r ? { "xaxis.range": r, "xaxis.autorange": false } : { "xaxis.autorange": true }), "yaxis.autorange": true }); if(isDepth(name)){plot._uwSig=null;renderPanel(name);} };
    el.querySelector(".wide").onclick = () => setPanelState(name, state.panel[name] === "wide" ? null : "wide");
    el.querySelector(".min").onclick = () => setPanelState(name, "min");
    wireCardDrag(el, name);
    panelElements.set(name, el);
    panelWatch?.observe(el);
    return el;
  }

  // Group cards collect panels by where their data come from: an
  // extra panel names its group when it registers; a variable's follows its
  // source instrument
  const GROUPS = ["Surprise", "Lab", "Met Station", "Bridge", "Winches", "Other"];
  const GROUP_OF_INSTRUMENT = { TSG: "Lab", AVOS: "Met Station", ATS_Portside: "Met Station", ATS: "Met Station", POSMV: "Bridge", Multibeam: "Bridge" };
  function panelGroup(name) {
    const x = extraPanels.get(name); if (x) return x.group || "Other";
    const v = VAR[name]; if (!v) return "Other";
    if (name.startsWith("Surprise")) return "Surprise";
    if (/^(Bottom depth|Rosette |Cable )/.test(name)) return "Winches";
    if (/^(Air temperature|Relative humidity|Atmospheric pressure|True wind direction|Relative wind speed|Short-wave radiation)/.test(name)) return "Met Station";
    if (v.tsg) return "Lab";
    if (/^(Sea state|Roll & pitch|Heading|Ship speed)/.test(name)) return "Bridge";
    return GROUP_OF_INSTRUMENT[(v.source || "").split(" — ")[0].trim()] || "Other";
  }
  function setPanelState(name, s) {
    if (s) state.panel[name] = s; else delete state.panel[name];
    store.set("panel", state.panel);
    layoutPanels();
    renderPanel(name);
  }

  // Group cards and charts share one persistent order in the same grid.
  function wireCardDrag(el, key) {
    el.dataset.card = key;
    el.draggable = true;
    el.addEventListener("dragstart", (e) => {
      e.dataTransfer.setData("text/plain", key); el.classList.add("dragging");
    });
    el.addEventListener("dragend", () => el.classList.remove("dragging"));
    el.addEventListener("dragover", (e) => { e.preventDefault(); el.classList.add("over"); });
    el.addEventListener("dragleave", () => el.classList.remove("over"));
    el.addEventListener("drop", (e) => {
      e.preventDefault(); el.classList.remove("over");
      const names = panelNames();
      const keys = [...names, ...new Set(names.map(n => "group:" + panelGroup(n)))];
      const saved = store.get("cards.order", []).filter(k => keys.includes(k));
      const order = [...saved, ...keys.filter(k => !saved.includes(k))];
      const i = order.indexOf(e.dataTransfer.getData("text/plain")), j = order.indexOf(key);
      if (i < 0 || j < 0 || i === j) return;
      [order[i], order[j]] = [order[j], order[i]];
      store.set("cards.order", order);
      layoutPanels();
    });
  }

  function layoutPanels() {
    const grid = $("#panels");
    const names = panelNames();
    let changed = false;
    const cards = [];
    const byGroup = new Map(GROUPS.map((g) => [g, []]));
    for (const name of names) {
      const g = panelGroup(name); if (!byGroup.has(g)) byGroup.set(g, []); byGroup.get(g).push(name);
      const el = panelEl(name);
      const wide = state.panel[name] === "wide";
      if (el.classList.contains("wide") !== wide) { el.classList.toggle("wide", wide); changed = true; }
      if (state.panel[name] === "min") {
        if (el.parentElement) { el.remove(); changed = true; }
      } else {
        cards.push(el);
      }
    }
    // Table-style summaries keep all chart toggles visible, open or minimized.
    for (const [g, members] of byGroup) {
      if (!members.length) continue;
      let box = document.getElementById("g-" + cssId(g));
      if (!box) {
        box = document.createElement("section"); box.className = "panel card dockgroup"; box.id = "g-" + cssId(g);
        box.innerHTML = `<div class="head"><span class="handle" title="drag to reorder">⋮⋮</span><button type="button" class="ghead"><span class="gname"></span><span class="gn"></span><span class="gtog"></span></button></div><div class="chips" role="region" aria-label="${g} chart summaries"></div>`;
        wireCardDrag(box, "group:" + g);
      }
      cards.push(box);
      const minned = members.filter((n) => state.panel[n] === "min"), allMin = minned.length === members.length;
      box.querySelector(".gname").textContent = g;
      const summary=members.map(n=>extraPanels.get(n)?.groupSummary).find(Boolean);
      const times=members.map(n=>extraPanels.get(n)?.updated?.() ?? (()=>{const y=state.data?.vars[n];if(!y)return null;for(let i=y.length-1;i>=0;i--)if(y[i]!=null)return state.data.t[i];return null})()).filter(t=>t!=null);
      const updated=times.length?Math.max(...times):null;
      const groupAge = box.querySelector('.gn');
      groupAge.dataset.summary = summary ? summary() || '' : '';
      if (updated != null) groupAge.dataset.updated = updated; else delete groupAge.dataset.updated;
      groupAge.textContent = [groupAge.dataset.summary, updated != null ? ago(updated) : 'no data'].filter(Boolean).join(' · ');
      box.querySelector('.gn').title=updated?`Latest displayed observation: ${fmtTs(updated)} ${tzAbbr()}`:'No observations';
      box.querySelector(".gtog").textContent = allMin ? "▲" : "—";
      box.querySelector(".ghead").title = allMin ? `restore every ${g} panel` : `minimise every ${g} panel to this group`;
      box.querySelector(".ghead").onclick = () => {
        for (const n of members) { if (allMin) delete state.panel[n]; else state.panel[n] = "min"; }
        store.set("panel", state.panel);
        layoutPanels();
        if (allMin) for (const n of members) renderPanel(n);
      };
      const chips = box.querySelector(".chips");
      const voltage=state.data?.vars['TSG flow (V)']?.at(-1);
      const low=g==='Lab'&&voltage!=null&&voltage<(SITE.low_flow_v??0.5);
      box.classList.toggle('pump-alarm',low);
      box.querySelector('.gname').textContent=g+(low?' · LOW FLOW':'');
      box.title=low?`Intake pump voltage ${voltage.toFixed(2)} V: water measurements may be unreliable`:'';
      for (const name of members) {
        let chip = document.getElementById("c-" + cssId(name));
        if (!chip) { chip = document.createElement("button"); chip.className = "chip"; chip.id = "c-" + cssId(name); chip.onclick = () => setPanelState(name, state.panel[name] === 'min' ? null : 'min'); }
        const open = state.panel[name] !== 'min';
        chip.classList.toggle('on',open);chip.setAttribute('aria-pressed',String(open));
        chips.appendChild(chip);
        const y = state.data?.vars[name];
        const spec=extraPanels.get(name),preview=spec?.chip?.();
        chip.classList.toggle('ice-summary-chip',!!preview);
        chip.replaceChildren();
        const label=document.createElement('span');label.className='cname';label.textContent=spec?.label||(name===SURPRISE?'Combined':name.replace(/^Surprise · /,''));
        if(preview?.image){const img=document.createElement('img');img.src=preview.image;img.alt='Latest ROI';img.className='chip-preview';label.prepend(img)}
        const latestValue=y?lastFinite(y):null;
        const value=document.createElement('b');value.textContent=preview?.text??(/^(Rosette depth|Cable length)/.test(name)&&latestValue!=null?`${Math.round(latestValue)} m`:fmtVal(latestValue,VAR[name]?.unit));
        const arrow=document.createElement('span');arrow.className='chart-state';arrow.textContent=open?'▲':'▼';arrow.setAttribute('aria-hidden','true');chip.append(arrow,label,value);
        chip.title = `${spec?.label||name}: ${open?'minimise':'restore'}`;
      }
    }
    const saved = store.get("cards.order", []);
    const rank = new Map(saved.map((key, i) => [key, i]));
    cards.sort((a, b) => (rank.get(a.dataset.card) ?? saved.length) - (rank.get(b.dataset.card) ?? saved.length));
    cards.forEach((el, i) => {
      const next = grid.children[i];
      if (next !== el) { grid.insertBefore(el, next || null); changed = true; }
    });
    if (changed) for (const el of grid.children) { const p = el.querySelector(".plot"); if (p?.data) Plotly.Plots.resize(p); }
  }

  // The underway panels share one x-axis: a zoom, pan or reset on any of
  // them (drag, shift-scroll, the ⟲ button) is applied to the others.
  let xSyncing = false;
  function linkX(plot) {
    if (plot._xLinked) return;
    plot._xLinked = true;
    plot.on("plotly_relayout", (ev) => {
      if (xSyncing) return;
      const upd = {};
      if (ev["xaxis.autorange"]) upd["xaxis.autorange"] = true;
      else if (ev["xaxis.range"]) { upd["xaxis.range"] = ev["xaxis.range"].slice(); upd["xaxis.autorange"] = false; }
      else if (ev["xaxis.range[0]"] != null) { upd["xaxis.range"] = [ev["xaxis.range[0]"], ev["xaxis.range[1]"]]; upd["xaxis.autorange"] = false; }
      else return;
      const others = [...document.querySelectorAll("#panels .plot")].filter((p) => p !== plot && p.data && p._fullLayout?.xaxis);
      xSyncing = true;
      Promise.all(others.map((p) => Plotly.relayout(p, upd).catch(() => {}))).finally(() => { xSyncing = false; });
    });
  }
  // The y-range of a TSG variable comes from the bins with the intake pump
  // running: a stopped pump reads the stagnant line (fresh, warm, near 0 V
  // of flow) and would set the scale for everything else. The pump-off
  // points still plot, off the bottom or top of the axis. Null when nothing
  // is gated, so the axis autoranges as usual.
  function pumpedRange(name, y, d) {
    const low = VAR[name]?.tsg ? pumpLow(d) : null;
    if (!low) return null;
    const on = [], all = [];
    y.forEach((q, i) => { if (q == null) return; all.push(q); if (!low[i]) on.push(q); });
    if (on.length < 2 || on.length === all.length) return null;
    const [lo, hi] = minmax(on), pad = Math.max((hi - lo) * 0.08, 0.01);
    return [lo - pad, hi + pad];
  }

  function renderPanel(name) {
    if (state.panel[name] === "min") return;
    const d = state.data, v = VAR[name] || extraPanels.get(name), el = panelEl(name);
    const plot = el.querySelector(".plot");
    const on = panelOn(name);
    el.classList.toggle("on", on);
    el.classList.toggle("unresolved", !v.resolved);
    el.querySelector(".log")?.classList.toggle("on", !!state.log[name]);
    el.querySelector('.depthscale')?.classList.toggle('on', !!state.depthScale[name]);
    el.querySelector('.depthscale')?.setAttribute('aria-pressed', String(!!state.depthScale[name]));
    el.querySelector(".wide").classList.toggle("on", state.panel[name] === "wide");
    if (panelWatch && !inView.has(name)) { el.dataset.stale = "1"; return; }     // drawn when scrolled into view
    delete el.dataset.stale;
    if (extraPanels.has(name)) {
      extraPanels.get(name).render(el, plot);
      // their own draw queues before this, so the drag mode lands after it
      Promise.resolve().then(() => { if (plot.data) Plotly.relayout(plot, { dragmode: on ? "pan" : false }).catch(() => {}); });
      return;
    }
    let title = name;
    let y = d?.vars[name];
    el.querySelector("h3").textContent = title;
    // the div keeps its "plot" class while empty, so a later render finds it again
    const empty = (msg) => { if (plot.data) Plotly.purge(plot); plot.className = "plot empty"; plot.textContent = msg; el.querySelector(".now").textContent = ""; };
    if (!v.resolved) return empty("source column not found in any leg");
    if (!d || !y || !y.some((x) => x != null)) return empty("no data for the selected legs");
    if (plot.classList.contains("empty")) { plot.className = "plot"; plot.textContent = ""; }
    const wholeMetres=/^(Rosette depth|Cable length)/.test(name),last=lastFinite(y);
    el.querySelector(".now").textContent = wholeMetres&&last!=null?`${Math.round(last)} m`:fmtVal(last,v.unit);

    const cv = VAR[state.colour] || extraColours.get(state.colour);
    const c = extraColours.get(state.colour)?.values(d) || d.vars[state.colour] || [];
    const lim = d.limits[state.colour] || minmax(c);
    // SVG, not WebGL: a dozen scattergl panels plus the map exceed the
    // browser's WebGL context limit (Safari's is 8) and the map is what gets
    // dropped. Panels never carry more than a few thousand points.
    // a TSG variable while the pump is stopped describes the line, not the
    // sea: those points go grey, in a trace of their own over the same line
    const low = v.tsg ? pumpLow(d) : null;
    const gated = !!low && low.some((l, i) => l && y[i] != null);
    const x = xvals(d), legText = d.leg.map((i) => legByIndex(i)?.label || "");
    const trace = {
      x, y: gated ? y.map((q, i) => (low[i] ? null : q)) : y, type: "scatter", mode: v.circular ? "markers" : "lines+markers", name,
      line: { width: 1, color: "rgba(160,180,200,.45)" }, connectgaps: false,
      marker: { size: v.circular ? 4 : 3.5, color: c, colorscale: UW.cmap(cv?.cmap), reversescale: !!cv?.reverse, cmin: lim?.[0], cmax: lim?.[1], showscale: false,
                opacity: 1 },
      text: legText,
      hovertemplate: `%{y:${wholeMetres?'.0f':'.3~f'}} ${v.unit}<br>%{x}<br>%{text}<extra></extra>`,
    };
    const traces = [trace];
    if (gated) {
      // the line runs through every point; the coloured markers sit on the
      // pumped ones, the grey markers on the rest
      trace.mode = "markers";
      traces.unshift({ x, y, type: "scatter", mode: "lines", hoverinfo: "skip", connectgaps: false,
                       line: { width: 1, color: "rgba(160,180,200,.45)" } });
      traces.push({ x, y: y.map((q, i) => (low[i] ? q : null)), type: "scatter", mode: "markers", name: "pump off",
                    marker: { size: 3.5, color: "#7d8895", opacity: .55 }, text: legText,
                    hovertemplate: `%{y:.3~f} ${v.unit} · <i>intake pump off</i><br>%{x}<br>%{text}<extra></extra>` });
    }
    const depth = isDepth(name), compressed = depth && !!state.depthScale[name];
    const depthY = (n) => n == null ? null : compressed ? Math.sqrt(Math.max(0,n)) : n;
    const useLog = !depth && !!state.log[name] && y.some((q) => q > 0);
    if (depth) {
      trace.customdata = y;
      trace.y = y.map(depthY);
      trace.hovertemplate = `%{customdata:.0f} m<br>%{x}<br>%{text}<extra></extra>`;
    }
    // a zoom survives the minute refresh, and resets with the span, legs or x-mode
    const uirev = `${state.win}|${state.xmode}|${[...state.hidden].sort().join(",")}`;
    const xr = spanRange(d);                                           // the span: the axis opens on it, the data run on before it
    const layout = {
      ...THEME, margin: { l: fz(52), r: 8, t: fz(6), b: fz(34) }, showlegend: false, hovermode: "closest", hoverdistance: 14,
      dragmode: on ? "pan" : false,                                       // only the selected panel moves its axes
      uirevision: uirev,
      xaxis: { ...THEME.xaxis, title: { text: xTitle(), font: { size: fz(12) }, standoff: 4 }, tickfont: { size: fz(12) },
               ...(xr ? { range: xr, autorange: false } : {}),
               type: state.xmode === "time" ? "date" : "linear",
               hoverformat: state.xmode === "time" ? "%Y-%m-%d %H:%M:%S" : ".1f",
               ticksuffix: state.xmode === "time" ? "" : " km",
               nticks: Math.max(2,Math.floor((plot.clientWidth||300)/fz(100))), tickangle: 0, automargin:true },
      yaxis: { ...THEME.yaxis, title: { text: v.unit, font: { size: fz(12) }, standoff: 2 }, tickfont: { size: fz(12) },
               type: useLog ? "log" : "linear", ...(v.circular ? { range: [0, 360], dtick: 90 } : {}) },
    };
    if (!useLog && !v.circular) {
      const r = pumpedRange(name, y, d);
      if (r) layout.yaxis.range = r;
    }
    if (depth) {
      const maxD = Math.max(1, minmax(y)[1] * 1.03);
      layout.yaxis = { ...layout.yaxis, autorange:false, range:[depthY(maxD),0],
        title:{...layout.yaxis.title,text:compressed?'depth (m, √ spacing)':'depth (m)'} };
      if (compressed) {
        const ticks = [0,5,10,20,30,50,75,100,150,200,300,400,500,750,1000,1500,2000,3000,4000,5000,6000,8000,10000,12000].filter(n=>n<=maxD);
        layout.yaxis.tickvals = ticks.map(depthY); layout.yaxis.ticktext = ticks.map(String);
      }
    }
    if (name.startsWith("Surprise")) {
      const top = Math.max(3.5, minmax(y)[1] * 1.08);
      layout.yaxis.range = [0, top];
      layout.shapes = [{ type: "rect", xref: "paper", x0: 0, x1: 1, yref: "y", y0: 3, y1: top,
                         fillcolor: "rgba(255,180,84,.10)", line: { width: 0 } }];
    }
    // Most builds change nothing this panel shows (a long span's last bin,
    // or nothing at all when a minute's data already arrived), so a draw
    // whose data and layout match the one on screen is skipped; the click
    // handler is rebound to this build's data either way.
    const onClick = () => {
      chartClickAnywhere(plot,d);
      plot.removeAllListeners?.('plotly_click');
      plot.on('plotly_click',ev=>{const p=ev.points?.[0];if(p){const i=p.pointIndex??p.pointNumber;focusMap(d.lat[i],d.lon[i],fmtTs(d.t[i]),true);extraColours.get(state.colour)?.onPoint?.(d,i);}});
    };
    const sig = drawSignature(traces, layout, on);
    if (plot.data && plot._uwSig === sig) { onClick(); return; }
    plot._uwSig = sig;
    reactPlot(plot, traces, layout, { ...CFG, scrollZoom: on }).then(() => { axisZoom(plot); linkX(plot); onClick(); });
  }
  // what a draw depends on, small enough to compare every build: each
  // array's length, ends and a weighted sum of its values (strings hashed
  // by sample), and the layout as a whole
  function drawSignature(traces, layout, on) {
    const digest = (a) => {
      if (!a || !a.length) return "-";
      let s = 0, n = 0, h = 0;
      for (let i = 0; i < a.length; i++) {
        const v = a[i];
        if (typeof v === "number") { if (isFinite(v)) { s += v * ((i % 7) + 1); n++; } }
        else if (v != null && i % 13 === 0) { const t = String(v); for (let j = 0; j < t.length; j++) h = (h * 31 + t.charCodeAt(j)) | 0; }
      }
      return `${a.length}:${n}:${s.toPrecision(12)}:${h}:${a[0]}:${a[a.length - 1]}`;
    };
    const parts = traces.map((t) => `${t.type}/${t.mode}|${digest(t.x)}|${digest(t.y)}|${digest(t.marker?.color)}|${t.marker?.cmin}|${t.marker?.cmax}|${t.marker?.colorscale}|${t.hovertemplate}`);
    return `${on}#${parts.join("#")}#${JSON.stringify(layout)}`;
  }

  function renderPanels() {
    layoutPanels();
    for (const name of panelNames()) renderPanel(name);
  }

  // ------------------------------------------------------------ provenance
  function renderProvenance() {
    const legCols = M.legs.map((l) => `<th title="${l.label}">${l.year % 100}·${l.number}</th>`).join("");
    const rows = M.variables.map((v) =>
      `<tr><td>${v.name}</td><td class="src ${v.resolved ? "" : "bad"}">${v.derived ? "<i>derived</i>" : (v.source || "not found")}</td>` +
      M.legs.map((l) => `<td class="cov">${v.derived ? "·" : (v.coverage?.[l.id] ? "✓" : '<span class="bad">–</span>')}</td>`).join("") + "</tr>");
    $("#sources").innerHTML = `<tr><th>panel</th><th>source column (Instrument — Variable)</th>${legCols}</tr>${rows.join("")}`;
    const f = M.files;
    $("#notes").innerHTML =
      `<p><b>Surprise</b>: ${M.surprise.note || "not computed"}. Each scale is −log10 of the χ² p-value of the Mahalanobis distance from an exponentially weighted mean and covariance of the minutes before (capped at 6); the combined score is the mean over scales. Above 3 is shaded.</p>` +
      `<p><b>Zooming</b>: scroll zooms a graph, Shift+scroll its x axis only, Ctrl+scroll its y axis only; double-click resets.</p>` +
      `<p><b>Inputs</b>: ${f.total} daily files across ${M.legs.length} legs; latest <code>${f.latest}</code>.</p>` +
      `<p><b>Record</b>: ${fmtTs(Date.parse(M.data_range.start))} → ${fmtTs(Date.parse(M.data_range.end))} ${tzAbbr()}. ${M.columns_seen.length} distinct columns seen; ` +
      `the per-leg columns show where a source column exists.</p>` +
      `<p>Times and time axes are ship time (${SITE.local_tz}); TSV exports carry UTC. Gaps in lines are missing data, not interpolation. ` +
      `Basemap: ${SITE.raster ? "GEBCO 2024 shaded relief — bathymetry and land (15 arc-second grid) — and " : ""}${SITE.vector ? "OpenStreetMap coastline and land (ODbL) and Natural Earth 10 m glaciers" : "Natural Earth 10 m coastline, land and glaciers"}${SITE.raster ? "" : " and depth bands"}; places (settlements) from GeoNames (CC BY 4.0; Nunavut, NWT, Labrador, northern Québec/Ontario/Manitoba and Greenland); all served locally; Web Mercator.</p>`;
  }

  // ------------------------------------------------------------ data flow
  function applyAndRender() {
    if (!state.raw) return;
    // a remembered colour the page no longer offers (a module gone) falls back to the default
    if (!VAR[state.colour] && !extraColours.has(state.colour)) { state.colour = VAR["SST (°C)"] ? "SST (°C)" : M.variables[0]?.name; store.set("colour", state.colour); renderControls(); }
    state.data = applyLegFilter(state.raw);
    state.span = spanSlice(state.data);
    state.data.limits = state.span.limits;                              // the span's limits colour the graphs too
    renderLegMenu();
    render();
    window.UW?.onFilter?.();
  }

  let loadSeq = 0;
  let windowLoading = false;
  // the files a build's window set asks for: the cover window's, and the
  // span's own when the span is the shorter (the "all points" variant of the
  // span's where the build made one)
  function windowFiles(manifest) {
    const spanW = spanWindowOf(manifest), coverW = coverWindowOf(manifest);
    if (coverW.hours <= spanW.hours) return [windowFile(coverW)];
    return [coverW.file, windowFile(spanW)];
  }
  async function loadWindow(manifest = M) {
    state.win = spanWindowOf(manifest).label;
    renderControls();
    const files = windowFiles(manifest), key = files.join("+");
    if (manifest === M && state.rawFile === key && state.raw) { applyAndRender(); return true; }   // the same record: only the view changed
    const seq = ++loadSeq;
    windowLoading = true;
    try {
      const [cover, span] = await Promise.all(files.map((f) => fetchJSON(`${f}?v=${encodeURIComponent(manifest.generated_utc)}`)));
      if (seq !== loadSeq) return false;
      // Commit the header/leg metadata and observations together only after
      // a successful download. A failed update keeps the last good pair.
      M = manifest; VAR = Object.fromEntries(M.variables.map((v) => [v.name, v]));
      state.raw = mergeWindows(cover, span); state.rawFile = key;
      setLoadError("Underway", false);
      renderControls(); renderProvenance();
      applyAndRender(); renderAlert();
      loadPlan();
      return true;
    } catch {
      if (seq === loadSeq) setLoadError("Underway", true);
      return false;
    } finally { if (seq === loadSeq) windowLoading = false; }
  }

  function render() {
    renderStatus();
    renderMap();
    renderPanels();
  }

  let checking = false, geoLoading = false;
  async function checkForUpdate() {
    if (checking) return;
    checking = true;
    // Geography must not block observation downloads or their retry loop.
    if (!state.geoComplete && !geoLoading) {
      geoLoading = true;
      loadGeo().then(() => renderMap()).finally(() => { geoLoading = false; });
    }
    try {
      const m = await fetchJSON(`data/manifest.json?t=${Date.now()}`);
      const want = m.windows.find((x) => x.label === state.win);
      if (!windowLoading && (m.generated_utc !== M.generated_utc || !state.raw || !want || state.rawFile !== windowFiles(m).join("+"))) {
        await loadWindow(m);
      } else if (!windowLoading) setLoadError("Underway", false);
    } catch { setLoadError("Underway", true); }
    finally {
      renderStatus();
      try { await window.UW?.refreshActiveTab?.(); }
      finally { checking = false; }
    }
  }

  // the schedule bar: the operations around now (last completed, in
  // progress, coming up next) from the intranet schedule, in ship time, with
  // the calendar links
  const schedMode = () => { const m = store.get("sched.mode", null); return ["open", "ticker", "hidden"].includes(m) ? m : (store.get("sched.hidden", false) ? "ticker" : "open"); };
  const setSchedMode = (m) => { store.set("sched.mode", m); renderAlert(); renderStatus(); };
  function renderAlert() {
    const esc = (x) => String(x ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
    const c = M.calendar || {}, n = c.now;
    const bar = $("#alert");
    bar.hidden = !n;
    if (bar.hidden) return;
    const hm = (t) => t ? new Date(tms(t)).toLocaleTimeString(undefined, { timeZone: SITE.local_tz, hour: "2-digit", minute: "2-digit", hourCycle: "h23" }) : "";
    // an operation in progress shows what is left of its slot rather than its times
    const left = (r) => { const m = Math.round((tms(r.end_utc) - Date.now()) / 60000); if (isNaN(m)) return "";
      const d = (n) => n >= 60 ? `${Math.floor(n / 60)} h ${String(n % 60).padStart(2, "0")} min` : `${n} min`;
      return m >= 0 ? `${d(m)} left` : `${d(-m)} over`; };
    const op = (r, live) => `<div class="sop" title="${esc(r.comment || "")}${live ? ` (${hm(r.start_utc)}–${hm(r.end_utc)})` : ""}"><b>${esc(r.station || "")}</b> ${esc(r.operation || "")}<span class="stm">${live ? left(r) : `${hm(r.start_utc)}–${hm(r.end_utc)}`}</span></div>`;
    const col = (label, rows, cls) => `<div class="scol ${cls}"><div class="slbl">${label}</div>${rows.length ? rows.map((r) => op(r, cls === "live")).join("") : '<div class="sop muted">—</div>'}</div>`;
    // a click folds the bar to a thin strip; a click on the strip brings it back
    // three states: open, folded to the ticker, hidden (then SCHEDULE in the
    // subtitle brings it back); each click on the bar goes one step
    const mode = schedMode(), folded = mode === "ticker";
    if (mode === "hidden") { bar.hidden = true; renderStatus(); return; }
    bar.classList.toggle("folded", folded);
    bar.title = folded ? "click to hide the schedule" : "";
    $("#schedrow").hidden = folded;
    // the fold must not bubble to the bar, whose handler is installed by the re-render
    $("#schedrow").onclick = (ev) => { if (ev.target.closest("a")) return; ev.stopPropagation(); setSchedMode("ticker"); };
    bar.onclick = folded ? (ev) => { if (ev.target.closest("a")) return; setSchedMode("hidden"); } : null;
    $("#schedticker").hidden = !folded;
    const feed = (c.feeds || []).find((f) => f.key === "schedule");
    const links = (cls) => feed ? `<a class="${cls}" href="${esc(feed.url)}" target="_blank" rel="noopener" title="open the Amundsen Schedule in Google Calendar">📅 Gcal</a><a class="${cls}" href="${esc(feed.ics)}" title="subscribe to the Amundsen Schedule as an ICS feed">📆 ICS</a>` : "";
    if (folded) {
      // the folded bar is a one-line ticker: the three columns as a slow
      // marquee (two copies so the loop is seamless), the links pinned on the right
      const item = (label, rows, cls) => `<span class="tki ${cls}"><b>${label}</b> ${rows.length ? rows.map((r) => `${esc(r.station || "")} ${esc(r.operation || "")} ${cls === "live" ? `<span class="stm tkleft" data-end="${esc(r.end_utc)}"></span>` : `<span class="stm">${hm(r.start_utc)}–${hm(r.end_utc)}</span>`}`).join(" · ") : "—"}</span>`;
      const text = item("Last completed", n.completed ? [n.completed] : [], "done") + item("In progress", n.in_progress || [], "live") + item("Coming up next", n.next ? [n.next] : [], "next");
      const tk = $("#tk");
      const same = tk.dataset.text === text;
      if (!same) { tk.innerHTML = text + text; tk.dataset.text = text; }
      for (const el of tk.querySelectorAll(".tkleft")) el.textContent = left({ end_utc: el.dataset.end });   // the minutes tick without restarting the scroll
      tk.style.animationDuration = `${Math.max(20, tk.scrollWidth / 2 / 30)}s`;   // 30 px/s, so the row reads at a walking pace
      $("#tickerlinks").innerHTML = links("smallcal");
      return;
    }
    $("#schedcols").innerHTML = col("Last completed", n.completed ? [n.completed] : [], "done") + col("In progress", n.in_progress || [], "live") + col("Coming up next", n.next ? [n.next] : [], "next");
    $("#schedlinks").innerHTML = links("bigcal");
  }

  setInterval(() => { if (M?.calendar?.now) renderAlert(); }, 60e3);   // the time left counts down between refreshes

  // in-app alerts: this browser's id is its address for the "web" channel;
  // the timer queues messages for it and the strip above the schedule bar
  // shows them until cleared (and the browser notifies, when allowed)
  function webId() {
    let id = store.get("alerts.webid", "");
    if (!id) { id = (crypto.randomUUID ? crypto.randomUUID().replace(/-/g, "") : Math.random().toString(36).slice(2) + Date.now().toString(36)); store.set("alerts.webid", id); }
    return id;
  }
  const esc = (x) => String(x ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  const inapp = { msgs: [] };
  function renderInapp() {
    const el = $("#inapp");
    el.hidden = !inapp.msgs.length;
    if (el.hidden) return;
    el.innerHTML = inapp.msgs.map((m) => `<span class="msg">🔔 ${esc(m.text)} <small>${fmtTs(Date.parse(m.t)).slice(11)}</small></span>`).join("") +
      `<button type="button" class="clear" title="clear these">✕</button>`;
    el.querySelector(".clear").onclick = () => { store.set("alerts.seen", inapp.msgs[inapp.msgs.length - 1].t); inapp.msgs = []; renderInapp(); };
  }
  async function pollInapp() {
    if (!store.get("alerts.webid", "") || document.hidden) return;
    try {
      const j = await fetchJSON(`api/alerts/inbox?to=${encodeURIComponent(webId())}&since=${encodeURIComponent(store.get("alerts.seen", ""))}&t=${Date.now()}`);
      const have = new Set(inapp.msgs.map((m) => m.t + m.text));
      const fresh = (j.messages || []).filter((m) => !have.has(m.t + m.text));
      if (!fresh.length) return;
      inapp.msgs = [...inapp.msgs, ...fresh].slice(-8);
      renderInapp();
      if (window.Notification?.permission === "granted") for (const m of fresh) { try { new Notification("Amundsen schedule", { body: m.text, tag: m.t + m.text }); } catch { /* not every browser */ } }
    } catch { /* the next poll */ }
  }
  setInterval(pollInapp, 60e3);
  document.addEventListener("visibilitychange", () => { if (!document.hidden) pollInapp(); });
  setTimeout(pollInapp, 3000);

  // ------------------------------------------------------------ tabs
  // The map stays; the right-hand pane and the header controls swap.
  function showTab(name) {
    if(name==='table')name='underway'; // old bookmarks and saved tab choices
    if (name === "chat") { window.UW?.chatToggle?.(); return; }       // not a pane: the chat side bar
    if (name === "map") { window.UW?.cycleMap?.(); return; }           // nor this: the map cycler sits among the tabs
    if (name === "history" || name === "nature") name = "wiki";        // the two past tabs are one wiki; a remembered or linked name opens it
    if(name!=='wiki')store.set('lastNonWikiTab',name);
    for (const b of $("#tabs").querySelectorAll("button")) if (b.dataset.tab !== "chat" && b.dataset.tab !== "map") b.classList.toggle("on", b.dataset.tab === name);
    for (const p of document.querySelectorAll(".pane")) p.hidden = p.id !== "pane-" + name;
    if (window.UW?.mapMode?.() === "full") window.UW.setMapMode("half");   // a chosen tab wants seeing: a full map gives way to half
    const mn = document.querySelector("main"); mn.className = "tab-" + name + (mn.classList.contains("mapmin") ? " mapmin" : mn.classList.contains("mapfull") ? " mapfull" : "");   // No Map survives a tab change
    // the header row (legs, span) filters every tab; the other switches live
    // in the figure areas
    $("#controls-underway").hidden = false;
    const hint = $("#maphint"); if (hint) hint.hidden = name !== "casts";
    store.set("tab", name);
    window.UW?.onTab?.(name);
    if (name === "underway") setTimeout(() => { for (const el of $("#panels").children) { const p = el.querySelector(".plot"); if (p?.data) Plotly.Plots.resize(p); } }, 0);
  }
  for (const b of $("#tabs").querySelectorAll("button")) b.onclick = () => showTab(b.dataset.tab);
  $('#wikiclose').onclick=()=>showTab(store.get('lastNonWikiTab','underway'));

  // hooks for tabs.js
  window.UW = Object.assign(window.UW || {}, {
    state, SITE, THEME, C, fz, themeName, applyTheme, CFG, fetchJSON, setLoadError,
    fmtTs, tzAbbr, shipAxis, plotDate, offsetMs, fmtVal, dms, legById, minmax, store,
    renderMap, showTab, focusMap, requestFit, axisZoom, reactPlot, currentFilter, spanFilter, legsStart, inFilter, tms, setSpan, showAllLegs, webId, pollInapp, plansShown, toast,
    refreshExtraData() {                  // new camera data: its panel; everything only when it colours the rest
      layoutPanels();
      if (extraColours.has(state.colour)) render();
      else for (const name of extraPanels.keys()) renderPanel(name);
    },
    clearFocus() { state.focus = null; },
    moveShip,
    registerPanel(name, spec) {
      extraPanels.set(name, spec);
      if(newViewer&&!(name in state.panel)){state.panel[name]='min';store.set('panel',state.panel)}
      if(!newViewer&&spec.layoutRevision&&store.get('panel-layout:'+name,null)!==spec.layoutRevision){
        const first=spec.after,order=panelNames().filter(n=>n!==name&&n!==first);
        state.order=[first,name,...order].filter(Boolean);store.set('order',state.order);
        delete state.panel[name];store.set('panel',state.panel);
        store.set('panel-layout:'+name,spec.layoutRevision);
      }
      layoutPanels(); renderPanel(name);
    },
    linkX,
    registerColour(spec) { extraColours.set(spec.name, spec); renderControls(); if (state.colour === spec.name) render(); },
    selectColour(name) { state.colour=name; store.set('colour',name); renderControls(); render(); },
  });
  Object.defineProperty(window.UW, "M", { get: () => M, configurable: true });

  // ?win=2y&legs=2025_LEG_01,2026_LEG_03&tab=casts — a shareable view; the
  // parameters override what the browser remembered
  {
    const q = new URLSearchParams(location.search);
    if (q.get("win") && M.windows.some((w) => w.label === q.get("win"))) { state.win = q.get("win"); store.set("win", state.win); }
    if (q.get("legs")) {
      const want = q.get("legs") === "all" ? new Set(M.legs.map((l) => l.id)) : new Set(q.get("legs").split(","));
      state.hidden = new Set(M.legs.map((l) => l.id).filter((id) => !want.has(id))); store.set("hiddenLegs", [...state.hidden]);
    }
    if (q.get("tab")) store.set("tab", q.get("tab"));
  }
  // a script error is shown rather than swallowed, so it can be reported
  window.addEventListener("error", (e) => { try { toast(`Page error: ${e.message} (${(e.filename || "").split("/").pop()}:${e.lineno})`); } catch {} });
  window.addEventListener("unhandledrejection", (e) => {
    // Plotly's own promises reject harmlessly when a plot is replaced mid-draw
    if (String(e.reason?.stack || "").includes("plotly")) return;
    try { toast(`Page error: ${e.reason?.message || e.reason}`); } catch {}
  });

  (async () => {
    renderControls();
    renderProvenance();
    showTab(store.get("tab", "underway"));
    setInterval(checkForUpdate, 30 * 1000);
    window.addEventListener("online", checkForUpdate);
    document.addEventListener("visibilitychange", () => { if (!document.hidden) checkForUpdate(); });
    checkForUpdate();
    // the map box changes with the window and as the bars round it fill
    const resizeMap = () => {mapView?.resize();renderMapLegend();};
    window.addEventListener("resize", resizeMap);
    new ResizeObserver(resizeMap).observe($("#map"));
    wirePlanDrop(); renderPlanPills();
    document.addEventListener("click", (e) => { for (const m of document.querySelectorAll("details.legmenu[open]")) if (!m.contains(e.target)) m.open = false; });   // a click outside closes the legs menu and the map's kind menus
  })();
})();
