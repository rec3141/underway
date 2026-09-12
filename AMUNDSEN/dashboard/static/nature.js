/* The natural half of the Wiki: what the archipelago is and does, as the
 * record of it was written down. Subjects (a species, a rock unit, a kind of
 * ice, a variable, the aurora, the Magnetic Pole) and observations of them
 * (one subject, one date, one position, one source, the number in the unit
 * it was written in), grouped in domains (biology, geology, the sea ice...),
 * with the ship's own journal. The documents behind them (the natural topics'
 * narrative pages, images, maps, quotes, texts, people, places and events)
 * are history.js's, which owns the pane and its router and shows this half's
 * own views through UW.natureViews while the Nature label is on. The ship's
 * lines go into the journal, most of them photographs imported from the
 * share (placed by their own time and position, captioned by the model), a
 * few written by hand on the form here, and reach grid on the next push;
 * until grid ingests them they show as the ship's, ringed on the map. Doc,
 * the resident scientist, answers from this half in the chat. Loaded after
 * history.js. */
(() => {
  "use strict";
  const UW = window.UW, H = UW.histShared;
  const $ = (s) => document.querySelector(s);
  const { store, C, fz } = UW;
  const esc = H.esc;
  const cachedJSON = window.UWData.generationCache(UW.fetchJSON, () => UW.M.history?.stamp || "");
  const debounce = (f, ms) => { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => f(...a), ms); }; };

  // the domains: a colour each from the theme's palette, read when used
  const dom = (label, i, hint) => ({ label, hint, get colour() { return C.palette[i]; } });
  const DOMAINS = {
    biology: dom("Biology", 2, "the living things: mammals, birds, fish, plants, the small and the microbial"),
    geology: dom("Geology", 1, "the rock, its fossils and minerals, the landforms"),
    glaciology: dom("Glaciology", 10, "the ice on land: glaciers, ice caps, ice shelves and ice islands"),
    "sea-ice": dom("Sea ice", 6, "the ice at sea, year by year, and its vocabulary"),
    oceanography: dom("Oceanography", 0, "the water column: soundings, temperature, salinity, currents, tides"),
    hydrography: dom("Hydrography", 4, "the depths and the charts"),
    meteorology: dom("Meteorology", 5, "the weather and the climate, as the expeditions and the stations recorded them"),
    astronomy: dom("Astronomy", 11, "the sky: aurora, the polar night and the midnight sun, eclipses and comets"),
    geomagnetism: dom("Geomagnetism", 3, "the field: declination, dip, the Magnetic Pole's wander, the storms"),
    freshwater: dom("Freshwater", 8, "the rivers and the lakes, and what lives in them"),
  };
  const OTHER = dom("Other", 7, "");
  const domainOf = (d) => DOMAINS[d] || OTHER;
  // a subject's kinds, in the order of the design's table
  const SUBJECT_KINDS = { taxon: "Taxa", unit: "Rock units", fossil: "Fossils", mineral: "Minerals", landform: "Landforms", ice: "Ice", water: "Water",
    weather: "Weather", sky: "Sky", field: "Field", phenomenon: "Phenomena" };
  const METHODS = ["sighting", "hunt", "specimen", "transect", "aerial-survey", "camera", "acoustic", "edna", "catch-record", "testimony", "instrument", "sounding", "dredge", "trawl", "net", "trap", "core", "sample", "station-record", "survey", "satellite", "chart"];   // the writer's vocabulary on grid
  const NEAR_KM = 300;                                    // "near the ship" reaches this far
  const VIG_N = 6;
  
  const nat = {
    subjects: null, obs: null, topics: [], stamp: null, loading: null,
    available: false,                                      // the natural files are in this build
    journal: [],                                           // the ship's own lines, newest first, from the server
    domains: new Set(store.get("nat.domains", Object.keys(DOMAINS))),   // the domains the map layer shows
    more: { today: false, here: false },
    byName: null, bySubject: null, children: null,          // indexes built once per generation
    pages: new Map(),
  };
  const slug = () => H.slug();                             // what the pane shows: one slug for both halves

  // ---------------------------------------------------------------- data
  async function ensure() {
    if (!UW.M.history) { nat.subjects = null; return false; }
    if (nat.stamp === UW.M.history.stamp && nat.subjects) { await loadJournal(); return true; }
    if (nat.loading) return nat.loading;
    nat.loading = (async () => {
      const maybe = (k, u) => cachedJSON(k, u).catch(() => null);
      const [index, su, ob] = await Promise.all([
        maybe("nat:index", "data/history/index.json"),
        maybe("nat:subjects", "data/history/subjects.json"),
        maybe("nat:observations", "data/history/observations.json"),
      ]);
      nat.topics = (index?.topics || []).filter((t) => t.domain === "nature");
      nat.subjects = su?.subjects || []; nat.obs = ob?.observations || [];
      nat.available = !!(su || ob);
      nat.stamp = UW.M.history.stamp; nat.pages = new Map();
      for (const s of nat.subjects) { s.page ||= `subject/${slugify(s.name)}`; s.domain ||= ""; }
      for (const o of nat.obs) prep(o);
      index_();
      await Promise.all([loadJournal(), H.ensure().catch(() => null)]);
      return true;
    })().finally(() => { nat.loading = null; });
    return nat.loading;
  }
  // a row as the map and the lists read it: a decimal year, a day when it has one
  function prep(o) {
    o.date_start ||= (o.date || "").slice(0, 10);
    o._year = H.yearOf(o.date_start);
    o.id = String(o.id);
  }
  function slugify(s) {
    return String(s || "").normalize("NFKD").replace(/[̀-ͯ]/g, "").replace(/ø/gi, "o").replace(/æ/gi, "ae").toLowerCase()
      .replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "").slice(0, 80);
  }
  function index_() {
    nat.byName = new Map(); nat.bySubject = new Map(); nat.children = new Map();
    for (const s of nat.subjects) {
      for (const n of [s.name, s.english, s.french, s.inuktitut, s.kalaallisut, ...String(s.also || "").split(";")]) {
        const k = String(n || "").trim().toLowerCase(); if (k && !nat.byName.has(k)) nat.byName.set(k, s);
      }
      if (s.parent) { if (!nat.children.has(s.parent)) nat.children.set(s.parent, []); nat.children.get(s.parent).push(s); }
    }
    for (const o of nat.obs) { if (!nat.bySubject.has(o.subject)) nat.bySubject.set(o.subject, []); nat.bySubject.get(o.subject).push(o); }
    for (const xs of nat.bySubject.values()) xs.sort(byDate);
  }
  const byDate = (p, q) => ((p._year ?? 9e9) - (q._year ?? 9e9)) || String(p.date_start).localeCompare(String(q.date_start));
  const subjectOf = (name) => nat.byName?.get(String(name || "").trim().toLowerCase()) || null;
  const subjectBySlug = (slug) => nat.subjects?.find((s) => s.page === slug) || null;
  const obsById = (id) => allObs().find((o) => o.id === String(id)) || null;
  const topicOf = (slug) => nat.topics.find((t) => t.slug === slug);
  const domainOfObs = (o) => o.domain || subjectOf(o.subject)?.domain || "";
  // the journal's lines, as observations, until grid has ingested them
  async function loadJournal() {
    try {
      const r = await fetch("api/nature/journal", { cache: "no-store" });
      if (!r.ok) return;
      const j = await r.json();
      nat.journal = (j.entries || []).map((e) => { const o = { ...e, _journal: true, origin: e.origin || "ship" }; prep(o); return o; });
    } catch { /* the journal is the ship's convenience; the record stands without it */ }
  }
  function allObs() {
    const ids = new Set((nat.obs || []).map((o) => o.id));
    return [...(nat.obs || []), ...nat.journal.filter((o) => !ids.has(o.id))];
  }
  async function page(slug) {
    if (nat.pages.has(slug)) return nat.pages.get(slug);
    const p = await cachedJSON(`nat:page:${slug}`, `data/history/pages/${encodeURIComponent(slug.replace(/\//g, "__"))}.json`);
    nat.pages.set(slug, p);
    return p;
  }

  // ---------------------------------------------------------------- what is shown
  // the observations in force: within the open topic, domain, kind or subject,
  // and of the domains the map's chips allow
  function scopeOf() {
    const s = slug();
    if (s.startsWith("topic/")) return { topic: s.slice(6) };
    if (s.startsWith("domain/")) return { domain: s.split("/")[1] };
    if (s.startsWith("subjects/")) return { kind: s.slice(9) };
    if (s.startsWith("subject/")) { const su = subjectBySlug(s); return su ? { subject: su } : {}; }
    if (s.startsWith("observation/")) { const o = obsById(s.slice(12)); const su = o ? subjectOf(o.subject) : null; return su ? { subject: su } : {}; }
    return {};
  }
  // a subject and everything below it in its tree
  function family(s) {
    const out = new Set([s.name]);
    const walk = (n) => { for (const c of nat.children.get(n) || []) if (!out.has(c.name)) { out.add(c.name); walk(c.name); } };
    walk(s.name);
    return out;
  }
  function inScope(o, sc) {
    if (sc.topic && o.topic !== sc.topic) return false;
    if (sc.domain && domainOfObs(o) !== sc.domain) return false;
    if (sc.kind && subjectOf(o.subject)?.kind !== sc.kind) return false;
    if (sc.subject && !(sc._fam ||= family(sc.subject)).has(o.subject)) return false;
    return true;
  }
  function shownObs(forMap = false) {
    // Sidebar navigation must not change which map points are visible.
    const sc = forMap ? {} : scopeOf();
    return allObs().filter((o) => inScope(o, sc) && (!forMap || nat.domains.has(domainOfObs(o) || "other") || (!domainOfObs(o) && nat.domains.size)));
  }

  // ---------------------------------------------------------------- vignettes
  const parts = (iso) => { const m = /^(-?\d{1,4})-(\d{2})-(\d{2})$/.exec(iso || ""); return m ? { y: +m[1], m: +m[2], d: +m[3] } : null; };
  function onThisDay(now = new Date()) {
    const mm = now.getMonth() + 1, dd = now.getDate();
    return allObs().filter((o) => { const a = parts(o.date_start); return a && a.m === mm && a.d === dd; }).sort(byDate);
  }
  function nearShip(lat, lon) {
    if (lat == null || lon == null) return [];
    return allObs().filter((o) => o.lat != null).map((o) => ({ o, d: H.km(lat, lon, o.lat, o.lon) })).filter((x) => x.d <= NEAR_KM).sort((p, q) => p.d - q.d).slice(0, 24);
  }
  UW.natureVignettes = () => ({ today: onThisDay(), here: nearShip(UW.M.latest?.lat, UW.M.latest?.lon) });
  // one observation as a line: what, the number as written, who, where
  function obsLine(o, lead = "") {
    const su = subjectOf(o.subject);
    const what = su ? `<b class="${su.kind === "taxon" ? "sci" : ""}">${esc(displayName(su))}</b>` : `<b>${esc(o.subject)}</b>`;
    const num = numberOf(o);
    return `<a class="vig" href="#wiki/observation/${esc(o.id)}" data-slug="observation/${esc(o.id)}" data-lat="${o.lat ?? ""}" data-lon="${o.lon ?? ""}"><span class="dot" style="background:${domainOf(domainOfObs(o)).colour}"></span>${lead}<span class="txt">${what}${num ? ` <i>${esc(num)}</i>` : ""}${o.qualifier && !num ? ` <i>${esc(o.qualifier)}</i>` : ""}${o.observer ? ` <span class="muted">${esc(o.observer)}</span>` : ""}${o.place ? ` <span class="muted">· ${esc(o.place)}</span>` : ""}${o._journal ? ` <span class="status draft" title="the ship's own journal">ship</span>` : ""}${o.lat != null ? ` <span class="pin" title="on the map">⌖</span>` : ""}</span></a>`;
  }
  // the number as it was written: "about 200", "-58.5 F", "no bottom at 1,000 fathoms"
  function numberOf(o) {
    if (o.value != null && o.value !== "") return `${o.qualifier && /\bat$/.test(o.qualifier) ? o.qualifier + " " : ""}${fmtNum(o.value)}${o.unit ? " " + o.unit : ""}`;
    if (o.count) return String(o.count);
    return "";
  }
  const fmtNum = (v) => { const n = +v; return Number.isFinite(n) ? n.toLocaleString("en-CA", { maximumFractionDigits: 3 }) : String(v); };
  // a subject's name for the reader: the English name with the scientific one beside it, or the name alone
  const displayName = (s) => s.english && s.english !== s.name ? `${s.english} (${s.name})` : s.name;
  const shortName = (s) => s.english || s.name;
  function vignetteCards() {
    if (!nat.subjects || !nat.available) return "";
    const now = new Date(), pos = UW.M.latest || {};
    const today = onThisDay(now), here = nearShip(pos.lat, pos.lon);
    const dateWord = now.toLocaleDateString(undefined, { month: "long", day: "numeric" });
    const fold = (xs, key, line) => {
      const shown = nat.more[key] ? xs : xs.slice(0, VIG_N), rest = xs.length - shown.length;
      return shown.map(line).join("") + (rest > 0 ? `<button type="button" class="more" data-more="nat:${key}">see ${rest} more</button>` : nat.more[key] && xs.length > VIG_N ? `<button type="button" class="more" data-more="nat:${key}">see fewer</button>` : "");
    };
    return `<section class="vigcard"><h3>Observed on this day · ${esc(dateWord)}</h3>${today.length ? fold(today, "today", (o) => obsLine(o, `<b>${esc(H.yearLabel(o._year))}</b>`)) : `<div class="muted small">Nothing observed on ${esc(dateWord)} in the record yet.</div>`}</section>
      <section class="vigcard"><h3>Observed near the ship${pos.lat != null ? ` · ${esc(H.whereName(pos.lat, pos.lon) || "")}` : ""}</h3>${here.length ? fold(here, "here", (x) => obsLine(x.o, `<b>${Math.round(x.d)} km</b>`)) : `<div class="muted small">${pos.lat == null ? "The ship's position is not known to this build." : `Nothing observed within ${NEAR_KM} km of the ship yet.`}</div>`}</section>`;
  }

  // ---------------------------------------------------------------- the pane
  const crumb = (...rest) => H.crumb(...rest), here = (label, slug) => H.here(label, slug);
  const subjectLink = (s, cls = "") => `<a class="${cls}" href="#wiki/${esc(s.page)}" data-slug="${esc(s.page)}"><span class="dot" style="background:${domainOf(s.domain).colour}"></span>${s.kind === "taxon" ? `<i>${esc(s.name)}</i>` : esc(s.name)}${s.english && s.english !== s.name ? ` <span class="muted">${esc(s.english)}</span>` : ""}</a>`;
  const domainChip = (d, n, on = false) => { const D = domainOf(d); return `<a class="chip ${on ? "on" : ""}" href="#wiki/domain/${esc(d)}" data-slug="domain/${esc(d)}" title="${esc(D.hint)}"><span class="dot" style="background:${D.colour}"></span>${esc(D.label)}${n != null ? ` <span class="muted">${n}</span>` : ""}</a>`; };
  const pageLinkFor = (slug, label, cls = "") => `<a class="${cls}" href="#wiki/${esc(slug)}" data-slug="${esc(slug)}">${label}</a>`;
  // a subject as a line in a list: the names in the region's languages beside it
  function subjectRow(s) {
    const names = [s.english !== s.name ? s.english : "", s.inuktitut, s.kalaallisut, s.french].filter(Boolean).join(" · ");
    const n = (nat.bySubject.get(s.name) || []).length;
    return `<a class="person" href="#wiki/${esc(s.page)}" data-slug="${esc(s.page)}"><b class="${s.kind === "taxon" ? "sci" : ""}">${esc(s.name)}</b>${names ? ` <span class="muted">(${esc(names)})</span>` : ""} <span class="muted small">${esc(s.rank || s.kind || "")}${n ? ` · ${n}` : ""}</span>${s.status ? ` <span class="status" title="conservation status">${esc(s.status)}</span>` : ""}${s.note ? `<span class="role">${esc(s.note.length > 160 ? s.note.slice(0, 157) + "…" : s.note)}</span>` : ""}</a>`;
  }
  // the record: observations as a table, oldest first, with a chart above it
  // when there are numbers to draw
  function recordHTML(rows, opts = {}) {
    const tr = (o, i) => {
      const su = subjectOf(o.subject);
      const what = opts.subject ? "" : `<td>${su ? subjectLink(su) : esc(o.subject)}</td>`;
      const where = o.place ? esc(o.place) : "";
      const pin = o.lat != null ? ` <span class="pin" data-lat="${o.lat}" data-lon="${o.lon}" data-label="${esc(o.subject)}" data-layer="nature" title="on the map">⌖</span>` : "";
      const num = numberOf(o);
      const src = o.bibkey ? H.sourceRef(o.bibkey, o.pages) : o._journal ? `<span class="status draft" title="the ship's own journal">ship's journal</span>` : "";
      return `<tr data-key="${i}"${o.lat != null ? ` data-lat="${o.lat}" data-lon="${o.lon}"` : ""}><td class="mono"><a href="#wiki/observation/${esc(o.id)}" data-slug="observation/${esc(o.id)}">${esc(o.date_text || H.dateLabel(o.date_start))}</a></td>${what}` +
        `<td>${num ? `<b>${esc(num)}</b> ` : ""}${o.qualifier && !/\bat$/.test(o.qualifier) ? `<i>${esc(o.qualifier)}</i> ` : ""}${o.depth != null ? `<span class="muted">${esc(fmtNum(o.depth))} m down</span> ` : ""}${o.height != null ? `<span class="muted">${esc(fmtNum(o.height))} m up</span> ` : ""}<span class="muted">${esc(short(o.detail || "", 110))}</span></td>` +
        `<td>${where}${pin}</td><td>${esc(o.observer || "")}${o.vessel ? ` <span class="muted">${esc(o.vessel)}</span>` : ""}</td><td>${src}</td></tr>`;
    };
    const chart = rows.length >= 2 ? `<section class="panel card castplot wide solo" data-cp="natplot"><div class="head"><h3>${esc(opts.title || "Observations")}</h3><div class="tools"><span class="now">${rows.length} observations · scroll to zoom, drag to pan, click a point for its row</span><button type="button" class="reset" id="natplotreset" title="the whole record">⟲</button></div></div><div class="plot" id="natplot"></div></section>` : "";
    return chart + `<div class="hscroll" id="natrecord"><table class="sched timeline record"><thead><tr><th>Date</th>${opts.subject ? "" : "<th>Subject</th>"}<th>Observation</th><th>Where</th><th>Observer</th><th>Source</th></tr></thead><tbody>${rows.map(tr).join("")}</tbody></table></div>`;
  }
  const short = (t, n = 60) => t.length > n ? t.slice(0, n - 3) + "…" : t;
  // the chart: values against the year when the rows share a unit and carry
  // numbers; else the dates as points, a row per subject (or per domain when
  // the subjects are many)
  function drawRecord(rows, opts = {}) {
    const gd = $("#histmain #natplot"); if (!gd) return;
    const dated = rows.filter((o) => o._year != null);
    if (!dated.length) { gd.innerHTML = `<div class="empty">No dates to chart.</div>`; return; }
    const label = (o) => `${esc(short(o.subject, 60))}<br>${esc(numberOf(o) || o.qualifier || "")}${o.observer ? " · " + esc(o.observer) : ""}<br>${esc(o.date_text || H.dateLabel(o.date_start))}${o.place ? " · " + esc(o.place) : ""}`;
    const units = new Set(dated.filter((o) => o.value != null && o.value !== "" && Number.isFinite(+o.value)).map((o) => o.unit || ""));
    const numeric = dated.filter((o) => Number.isFinite(+o.value) && o.value !== "" && o.value != null);
    const traces = [];
    let layout;
    const ys = dated.map((o) => o._year), lo = Math.min(...ys), hi = Math.max(...ys), pad = Math.max(1, (hi - lo) * .03);
    if (opts.subject && units.size === 1 && numeric.length >= 3) {
      const unit = [...units][0];
      traces.push({ type: "scatter", mode: "markers+lines", name: unit, x: numeric.map((o) => o._year), y: numeric.map((o) => +o.value), customdata: numeric.map((o) => rows.indexOf(o)),
        text: numeric.map(label), hovertemplate: "%{text}<extra></extra>", line: { color: C.line, width: 1 }, marker: { size: 8, color: numeric.map((o) => o._journal ? C.accent2 : domainOf(domainOfObs(o)).colour), line: { color: C.markerLine, width: .5 } } });
      layout = { ...UW.THEME, margin: { l: fz(56), r: 12, t: fz(8), b: fz(40) }, showlegend: false, dragmode: "pan",
        xaxis: { ...UW.THEME.xaxis, ...H.yearTicks(lo - pad, hi + pad), range: [lo - pad, hi + pad], zeroline: false, title: { text: "year", font: { size: fz(12) } }, tickfont: { size: fz(12) } },
        yaxis: { ...UW.THEME.yaxis, title: { text: unit || "value", font: { size: fz(12) } }, tickfont: { size: fz(11) } } };
    } else {
      const observers = new Set(dated.map((o) => o.observer || "")).size, bySubject = new Set(dated.map((o) => o.subject)).size <= 14;
      const cat = (o) => opts.subject && observers <= 14 ? short(o.observer || "unnamed", 28) : bySubject ? short(shortName(subjectOf(o.subject) || { name: o.subject }), 28) : (domainOf(domainOfObs(o)).label);
      const cats = [...new Set(dated.map(cat))];
      traces.push({ type: "scatter", mode: "markers", name: "observations", x: dated.map((o) => o._year), y: dated.map(cat), customdata: dated.map((o) => rows.indexOf(o)),
        text: dated.map(label), hovertemplate: "%{text}<extra></extra>",
        marker: { size: dated.map((o) => o._journal ? 11 : 8), color: dated.map((o) => domainOf(domainOfObs(o)).colour), line: { color: dated.map((o) => o._journal ? C.accent2 : C.markerLine), width: dated.map((o) => o._journal ? 2 : .5) } } });
      layout = { ...UW.THEME, margin: { l: fz(150), r: 12, t: fz(8), b: fz(40) }, showlegend: false, dragmode: "pan",
        xaxis: { ...UW.THEME.xaxis, ...H.yearTicks(lo - pad, hi + pad), range: [lo - pad, hi + pad], zeroline: false, title: { text: "year", font: { size: fz(12) } }, tickfont: { size: fz(12) } },
        yaxis: { ...UW.THEME.yaxis, type: "category", categoryorder: "array", categoryarray: cats.slice().reverse(), tickfont: { size: fz(11) }, fixedrange: true } };
    }
    UW.reactPlot(gd, traces, layout, UW.CFG).then((g) => {
      UW.axisZoom(g);
      g.removeAllListeners?.("plotly_click"); g.on("plotly_click", (ev) => { const k = ev.points?.[0]?.customdata; if (k != null) showRow(k); });
      if (g._yearTicksHandler) g.removeListener("plotly_relayout", g._yearTicksHandler); g.on("plotly_relayout", g._yearTicksHandler = () => {
        const r = g._fullLayout?.xaxis?.range; if (!r) return;
        const t = H.yearTicks(r[0], r[1]);
        if (JSON.stringify(t.tickvals) !== JSON.stringify(g.layout.xaxis.tickvals)) Plotly.relayout(g, { "xaxis.tickvals": t.tickvals, "xaxis.ticktext": t.ticktext });
      });
    });
    const reset = $("#histmain #natplotreset"); if (reset) reset.onclick = () => { if (gd?.data) Plotly.relayout(gd, { "xaxis.autorange": true }); };
  }
  function showRow(key) {
    const host = $("#histmain #natrecord"), row = host?.querySelector(`tr[data-key="${key}"]`);
    if (!row) return;
    for (const x of host.querySelectorAll("tr.on")) x.classList.remove("on");
    row.classList.add("on");
    row.scrollIntoView({ block: "center", behavior: "smooth" });
    if (row.dataset.lat) focusPoint(row.dataset.lat, row.dataset.lon, row.children[1]?.textContent);
  }

  // the search: the subjects and observations found, after the documents history.js listed
  function searchExtra(el, query) {
    if (!nat.subjects) return;
    const words = query.trim().toLowerCase().split(/\s+/).filter(Boolean);
    const hit = (s) => { const t = String(s || "").toLowerCase(); return words.every((w) => t.includes(w)); };
    const subs = nat.subjects.filter((s) => hit([s.name, s.english, s.french, s.inuktitut, s.kalaallisut, s.also, s.note].join(" ")));
    const obs = allObs().filter((o) => hit([o.subject, o.detail, o.observer, o.place, o.qualifier, o.vessel].join(" "))).sort(byDate);
    el.insertAdjacentHTML("beforeend", `<h2>${subs.length} subjects · ${obs.length} observations</h2>` +
      (subs.length ? `<div class="peoplelist subjlist">${subs.slice(0, 60).map(subjectRow).join("")}</div>` : "") +
      (obs.length ? recordHTML(obs.slice(0, 200), { title: "Observations found" }) : ""));
    if (obs.length) drawRecord(obs.slice(0, 200));
  }
  // the home, after the vignettes: the journal and the domains
  function homeExtraHTML() {
    if (!nat.subjects) return "";
    const counts = new Map(); for (const o of allObs()) { const d = domainOfObs(o) || "other"; counts.set(d, (counts.get(d) || 0) + 1); }
    const doms = [...Object.keys(DOMAINS), ...[...counts.keys()].filter((d) => !DOMAINS[d])].map((d) => domainChip(d, counts.get(d) || 0)).join("");
    return (nat.available ? "" : `<p class="lead">The natural half of the record is not in this build yet: the subjects and observations arrive with the next pull once grid publishes them. The ship's journal works now.</p>`) +
      journalHTML(true) +
      `<h2>Domains <a class="chip small" href="#wiki/record" data-slug="record">All observations</a></h2><div class="domgrid">${doms}</div>`;
  }
  const wireHome = (el) => wireJournal(el);
  // Explore, after the topics: the kinds of subject
  function exploreExtraHTML() {
    if (!nat.subjects) return "";
    return `<h3>Kinds of subject</h3><div class="domgrid">${Object.entries(SUBJECT_KINDS).map(([k, label]) => { const n = nat.subjects.filter((s) => s.kind === k).length; return n ? `<a class="chip" href="#wiki/subjects/${k}" data-slug="subjects/${k}">${esc(label)} <span class="muted">${n}</span></a>` : ""; }).join("")}</div>`;
  }
  // a natural topic's page, after its documents: its subjects and observations
  function topicExtra(el, t) {
    if (!nat.subjects) return;
    const topic = topicOf(t);
    const subs = nat.subjects.filter((s) => s.topic === t).sort((a, b) => a.name.localeCompare(b.name));
    const rows = shownObs().sort(byDate);
    el.insertAdjacentHTML("beforeend",
      (subs.length ? `<h3><span class="muted">${subs.length}</span> Subjects</h3><div class="peoplelist subjlist">${subs.map(subjectRow).join("")}</div>` : "") +
      (rows.length ? `<h3><span class="muted">${rows.length}</span> Observations</h3>` + recordHTML(rows, { title: topic?.title || t }) : ""));
    drawRecord(rows);
  }
  // this half's own views: the record, the journal, an import, a domain and its parts, a kind of subject, a subject, an observation
  const handles = (s) => /^(record|journal)$|^(import|domain|subjects|subject|observation)\//.test(s);
  async function render(el, s) {
    if (!nat.subjects) { el.innerHTML = `<div class="empty">Loading the record…</div>`; return; }
    if (s === "record") {
      const rows = shownObs().sort(byDate);
      el.innerHTML = crumb(here("Observations", "record")) + `<h2><span class="muted">${rows.length}</span> Observations</h2><div class="domgrid">${Object.keys(DOMAINS).map((d) => domainChip(d, rows.filter((o) => domainOfObs(o) === d).length)).join("")}</div>` + (rows.length ? recordHTML(rows, { title: "Observations" }) : `<p class="muted">Nothing in the record yet.</p>`);
      drawRecord(rows);
      return;
    }
    if (s === "journal") { el.innerHTML = crumb(here("/Share Photos", "journal")) + journalHTML(false); wireJournal(el); return; }
    if (s.startsWith("import/")) { await renderImport(el, s.slice(7)); return; }
    if (s.startsWith("domain/")) { const [, d, part] = s.split("/"); renderDomain(el, d, part || ""); return; }
    if (s.startsWith("subjects/")) {
      const k = s.slice(9);
      const subs = nat.subjects.filter((x) => x.kind === k);
      el.innerHTML = crumb(here(esc(SUBJECT_KINDS[k] || k), s)) + `<h2><span class="muted">${subs.length}</span> ${esc(SUBJECT_KINDS[k] || k)}</h2>` + treeHTML(subs);
      return;
    }
    if (s.startsWith("observation/")) { renderObservation(el, s.slice(12)); return; }
    if (s.startsWith("subject/")) { await renderSubject(el, s); return; }
  }
  // ---------------------------------------------------------------- a domain
  // A domain's topics are those its subjects and observations belong to (the
  // artifacts carry no domain of their own), and its documents are those
  // topics': the page is a chip per part, narrative, images, events, maps
  // and glossary (the subjects), each opening its own page, over the record
  // of its observations.
  const DOMAIN_PARTS = [["pages", "Explore", "pages"], ["images", "Images", "images"], ["events", "Events", "events"], ["maps", "Maps", "maps"], ["glossary", "Glossary", "subjects"], ["observations", "Observations", "observations"]];
  function domainTopics(d) {
    const t = new Set();
    for (const x of nat.subjects) if (x.domain === d && x.topic) t.add(x.topic);
    for (const o of nat.obs) if (domainOfObs(o) === d && o.topic) t.add(o.topic);
    return t;
  }
  function domainSets(d) {
    const hd = H.data(), t = domainTopics(d);
    return { topics: t,
      pages: (hd.index?.pages || []).filter((p) => p.kind === "page" && t.has(p.topic)),
      images: (hd.artifacts || []).filter((a) => a.type === "image" && t.has(a.topic)),
      maps: (hd.artifacts || []).filter((a) => a.type === "map" && t.has(a.topic)),
      events: (hd.events || []).filter((e) => t.has(e.topic)),
      glossary: nat.subjects.filter((x) => x.domain === d),
      observations: allObs().filter((o) => domainOfObs(o) === d).sort(byDate) };
  }
  const byYear = (p, q) => ((p._year ?? H.yearOf(p.date_start) ?? 9e9) - (q._year ?? H.yearOf(q.date_start) ?? 9e9)) || String(p.title).localeCompare(String(q.title));
  function renderDomain(el, d, part) {
    const D = domainOf(d), sets = domainSets(d);
    const dlink = `<a href="#wiki/domain/${esc(d)}" data-slug="domain/${esc(d)}">${esc(D.label)}</a>`;
    const h2 = (n, label) => `<h2><span class="dot" style="background:${D.colour}"></span>${n != null ? `<span class="muted">${n}</span> ` : ""}${esc(label)}</h2>`;
    if (!part) {
      const chips = DOMAIN_PARTS.filter(([k]) => sets[k].length).map(([k, label, word]) =>
        H.collectionChip({ slug: `domain/${d}/${k}`, label, colour: D.colour, count: sets[k].length, word, items: k === "maps" ? sets.maps : sets.images })).join("");
      el.innerHTML = crumb(here(esc(D.label), `domain/${d}`)) + h2(null, D.label) + `<p class="lead">${esc(D.hint)}</p>` +
        (chips ? `<div class="colgrid">${chips}</div>` : `<p class="muted">Nothing of this domain in the wiki yet.</p>`);
      return;
    }
    const P = DOMAIN_PARTS.find(([k]) => k === part);
    if (!P) { el.innerHTML = crumb(dlink) + `<div class="empty">No such part of the domain.</div>`; return; }
    const [, label] = P, xs = sets[part];
    let body;
    if (part === "pages") {                                        // as the wiki's Explore: the domain's topics, then their pages
      const topics = (H.data().index?.topics || []).filter((t) => sets.topics.has(t.slug));
      body = `<div class="topicgrid">${topics.map((t) => H.topicCard(t, true)).join("")}</div><h3><span class="muted">${xs.length}</span> Pages</h3><div class="pagelist">${xs.map((p) => H.pageLink(p)).join("")}</div>`;
    }
    else if (part === "images" || part === "maps") body = `<div class="artgrid pictures">${xs.sort(byYear).map((a) => H.artifactCard(a, { creator: true })).join("")}</div>`;
    else if (part === "observations") body = recordHTML(xs, { title: D.label });
    else if (part === "events") body = `<div class="pagelist">${xs.sort(byYear).map((e) => H.pageLink({ slug: `event/${e.id}`, kind: "event", title: e.title, summary: [H.dateLabel(e.date_text || e.date_start || ""), e.place].filter(Boolean).join(" · ") })).join("")}</div>`;
    else {
      const kinds = [...new Set(xs.map((x) => x.kind))];
      body = kinds.map((k) => `<h3>${esc(SUBJECT_KINDS[k] || k)} <span class="muted">${xs.filter((x) => x.kind === k).length}</span></h3><div class="peoplelist subjlist">${xs.filter((x) => x.kind === k).sort((a, b) => a.name.localeCompare(b.name)).map(subjectRow).join("")}</div>`).join("");
    }
    el.innerHTML = crumb(dlink, here(esc(label), `domain/${d}/${part}`)) + h2(part === "pages" ? null : xs.length, label) + (xs.length ? body : `<p class="muted">Nothing here yet.</p>`);
    if (part === "observations") drawRecord(xs);
  }
  // the taxa and the rock units as a tree: a root is a row with no parent in the list
  function treeHTML(subs) {
    const names = new Set(subs.map((s) => s.name));
    const roots = subs.filter((s) => !s.parent || !names.has(s.parent)).sort((a, b) => a.name.localeCompare(b.name));
    const node = (s, depth) => `<div class="tnode" style="margin-left:${depth * 16}px">${subjectRow(s)}</div>` + (nat.children.get(s.name) || []).filter((c) => names.has(c.name)).sort((a, b) => a.name.localeCompare(b.name)).map((c) => node(c, depth + 1)).join("");
    return `<div class="peoplelist subjlist tree">${roots.map((s) => node(s, 0)).join("")}</div>`;
  }
  async function renderSubject(el, slug) {
    const s = subjectBySlug(slug);
    let p = null;
    try { p = await page(slug); } catch { /* a subject without a page yet: the row is enough */ }
    if (!s && !p) { el.innerHTML = crumb() + `<div class="empty">That subject is not in this build.</div>`; return; }
    const row = s || { name: p.title, kind: "", domain: "", page: slug };
    const D = domainOf(row.domain), t = row.topic ? topicOf(row.topic) : null;
    const rows = (nat.bySubject.get(row.name) || []).concat(nat.journal.filter((o) => o.subject === row.name && !nat.obs.some((x) => x.id === o.id))).sort(byDate);
    const kids = (nat.children.get(row.name) || []).sort((a, b) => a.name.localeCompare(b.name));
    const parent = row.parent ? subjectOf(row.parent) : null;
    const names = [["scientific", row.kind === "taxon" ? row.name : ""], ["name", row.kind !== "taxon" ? row.name : ""], ["English", row.english], ["French", row.french], ["Inuktitut", row.inuktitut], ["Kalaallisut", row.kalaallisut], ["also", row.also]].filter(([, v]) => v);
    const backbone = row.backbone_id ? backboneLink(row) : "";
    const meta = [row.kind ? esc(SUBJECT_KINDS[row.kind] || row.kind) : "", row.rank ? esc(row.rank) : "", row.unit ? `in ${esc(row.unit)}` : "", row.status ? `<span class="status" title="conservation status">${esc(row.status)}</span>` : ""].filter(Boolean).join(" · ");
    // the pictures of it: the evidence of its observations, and the natural topics' images tagged with its names
    const evidence = new Set(rows.map((o) => o.artifact_id).filter(Boolean));
    const hd = H.data(), lname = [row.name, row.english].filter(Boolean).map((x) => x.toLowerCase());
    const pictures = (hd.artifacts || []).filter((a) => (a.type === "image" || a.type === "map") && (evidence.has(a.id) || (a.topic && H.topicDomain(a.topic) === "nature" && (a.tags || []).some((x) => lname.includes(String(x).toLowerCase())))));
    el.innerHTML = crumb(...(t ? [`<a href="#wiki/topic/${esc(t.slug)}" data-slug="topic/${esc(t.slug)}">${esc(t.title)}</a>`] : [domainChip(row.domain || "other")]), here(`<span class="kind">subject</span>`, slug)) +
      `<h2>${row.kind === "taxon" ? `<i>${esc(row.name)}</i>` : esc(row.name)}${row.english && row.english !== row.name ? ` <span class="muted">${esc(row.english)}</span>` : ""}</h2>` +
      `<div class="artmeta"><span class="dot" style="background:${D.colour}"></span>${esc(D.label)}${meta ? " · " + meta : ""}${parent ? ` · under ${subjectLink(parent)}` : ""}${backbone ? " · " + backbone : ""}</div>` +
      `<div class="names">${names.map(([l, v]) => `<span class="lbl">${esc(l)}</span><span class="${l === "scientific" ? "sci" : ""}">${esc(v)}</span>`).join("")}</div>` +
      (row.bibkey ? H.facts([["source", H.sourceRef(row.bibkey)]]) : "") +
      `<div class="wiki">${H.markdown(p?.html || row.note || "")}</div>` +
      (pictures.length ? `<h3>Pictures <span class="muted">${pictures.length}</span></h3><div class="artgrid pictures">${pictures.slice(0, 24).map((a) => H.artifactCard(a, { creator: true })).join("")}</div>${pictures.length > 24 ? `<p class="muted small">and ${pictures.length - 24} more among the <a href="#wiki/kind/image" data-slug="kind/image">images</a></p>` : ""}` : "") +
      (kids.length ? `<h3>Below it <span class="muted">${kids.length}</span></h3><div class="peoplelist subjlist">${kids.map(subjectRow).join("")}</div>` : "") +
      `<h3>Observations <span class="muted">${rows.length}</span></h3>` + (rows.length ? recordHTML(rows, { subject: true, title: shortName(row) }) : `<p class="muted">No observations of it in the record yet.</p>`) +
      H.backlinksHTML(p?.backlinks || []);
    H.wireBackmore(el);
    H.crossLink(el.querySelector(".wiki"), slug, p?.people, row.name);
    drawRecord(rows, { subject: true });
  }
  // the backbone a subject's id points at, where the id says which
  function backboneLink(s) {
    const id = String(s.backbone_id);
    const m = /^(?:gbif:)?(\d+)$/.exec(id);
    const url = m && s.kind === "taxon" ? `https://www.gbif.org/species/${m[1]}` : /^urn:lsid:marinespecies\.org:taxname:(\d+)$/.test(id) ? `https://www.marinespecies.org/aphia.php?p=taxdetails&id=${RegExp.$1}` : "";
    return url ? `<a href="${esc(url)}" target="_blank" rel="noopener" title="the backbone this subject resolves in">${esc(id)} ↗</a>` : `<span class="muted mono">${esc(id)}</span>`;
  }
  // a slug's title for the wiki's backlinks: a subject's name, an observation's label
  const titleOf = (s) => s.startsWith("subject/") ? subjectBySlug(s)?.name || "" : s.startsWith("observation/") ? (obsById(s.slice(12))?.label || obsById(s.slice(12))?.subject || "") : "";
  function renderObservation(el, id) {
    const o = obsById(id);
    if (!o) { el.innerHTML = crumb() + `<div class="empty">That observation is not in this build.</div>`; return; }
    const s = subjectOf(o.subject), D = domainOf(domainOfObs(o)), t = o.topic ? topicOf(o.topic) : null;
    const hd = H.data();
    const when = o.date_text || [o.date_start, o.date_end].filter(Boolean).map((d) => H.dateLabel(d)).join(" to ") || (o.date || "");
    const pl = o.place ? hd.places.find((x) => x.name === o.place) : null;
    const place = o.place ? (pl ? pageLinkFor(pl.page, esc(o.place)) : esc(o.place)) : "";
    const where = o.lat != null ? ` · ${H.coordLink(o.lat, o.lon, o.subject)} · ${H.mapLink(o.lat, o.lon, o.subject, "")}` : "";
    const person = o.observer ? hd.people.find((x) => x.name === o.observer) : null;
    const vessel = o.vessel ? hd.vessels.find((x) => x.name === o.vessel) : null;
    const art = o.artifact_id ? H.artifactById(o.artifact_id) : null;
    const ev = o.event_id ? H.eventById(o.event_id) : null;
    const num = numberOf(o);
    const facts = H.facts([
      ["observed", [num, o.qualifier && !num ? o.qualifier : "", o.stage, o.sex, o.behaviour].filter(Boolean).join(" · ")],
      ["depth", o.depth != null ? `${fmtNum(o.depth)} m below the surface` : ""], ["height", o.height != null ? `${fmtNum(o.height)} m above` : ""],
      ["method", [o.method, o.instrument].filter(Boolean).join(", ")],
      ["confidence", o.confidence], ["origin", o._journal || o.origin === "ship" || o.origin === "crew" ? "the ship's own journal" : ""],
      ["source", o.bibkey ? H.sourceRef(o.bibkey, o.pages) : ""],
    ]);
    el.innerHTML = crumb(...(t ? [`<a href="#wiki/topic/${esc(t.slug)}" data-slug="topic/${esc(t.slug)}">${esc(t.title)}</a>`] : [domainChip(domainOfObs(o) || "other")]), here(`<span class="kind">observation</span>`, `observation/${o.id}`)) +
      `<h2>${s ? subjectLink(s) : esc(o.subject)}${num ? ` <span class="muted">${esc(num)}</span>` : ""}</h2>` +
      `<div class="artmeta"><span class="dot" style="background:${D.colour}"></span>${esc(D.label)}${when ? " · " + esc(when) : ""}${place ? " · " + place : ""}${where}${o.sensitive ? ` · <span class="status draft" title="a sensitive site: the position is published coarsened and the place left blank">sensitive</span>` : ""}</div>` +
      (o.observer || o.vessel ? `<div class="artpeople"><span class="lbl">By</span>${o.observer ? person ? pageLinkFor(person.page, esc(o.observer), "chip small") : `<span class="chip small">${esc(o.observer)}</span>` : ""}${o.vessel ? vessel ? pageLinkFor(vessel.page, esc(o.vessel), "chip small") : `<span class="chip small">${esc(o.vessel)}</span>` : ""}</div>` : "") +
      (o.artifact_file ? `<figure><img src="${esc(o.artifact_file.startsWith("_journal/") ? journalPic(o) : o.artifact_file)}" alt=""><figcaption>${esc(o.observer || "the ship")}</figcaption></figure>` : "") +
      facts + `<div class="wiki"><p>${esc(o.detail || "")}</p></div>` +
      (art ? `<h3>Evidence</h3><div class="artgrid">${H.artifactCard(art, { creator: true })}</div>` : "") +
      (ev ? `<div class="backlinks"><span class="lbl">Also the event</span>${pageLinkFor(`event/${ev.id}`, esc(ev.title))}</div>` : "") +
      (s ? `<div class="backlinks"><span class="lbl">Observations</span><a href="#wiki/${esc(s.page)}" data-slug="${esc(s.page)}">every observation of ${esc(shortName(s))}</a></div>` : "");
  }

  // ---------------------------------------------------------------- the journal
  // The ship's own observations. The main way in is the photo import: a
  // browser over the share to pick folders or photographs, a form for who
  // they are and how the pictures may be used, and a job on the server that
  // places each photograph, reads it with the model and writes its line;
  // the page follows the job and lists what became of each. Beside it, a
  // form writes one line by hand, in the CLI's vocabulary, with the position
  // from the ship's GPS and the time from the clock. Lines reach grid on the
  // next push; grid's writer validates them.
  const share = { path: null, list: null, jobs: [], licences: {}, watches: [], watch: null, tab: store.get("nat.jtab", "gallery"), source:store.get('nat.import.source','share') };
  // Files stay in memory while navigating the wiki. Only explicit selection
  // and Upload send bytes; journal import remains a separate consent step.
  let phoneUpload = null;
  function uploadHTML(l) {
    const u = phoneUpload;
    if (u?.batch && u.files.length && u.done === u.files.length && !u.busy) {
      return `<section class="phone-upload upload-complete" aria-labelledby="phone-saved">
        <h4 id="phone-saved">✓ ${u.done} photo${u.done === 1 ? '' : 's'} saved to the share</h4>
        <p id="phone-message" role="status">${u.journalJob ? 'Import status below.' : 'Not yet in the journal.'}</p>
        ${u.journalJob ? `<a class="chip" href="#wiki/import/${esc(u.journalJob)}" data-slug="import/${esc(u.journalJob)}">Follow journal import</a>` :
          '<p>Add your credit and licence below.</p><button type="button" class="go" id="phone-next">Next: add to journal ↓</button>'}
        <details><summary>Saved folder</summary><p class="small">/Share/${esc(u.batch.path)}</p></details>
        <button type="button" class="chip small" id="phone-clear">Upload more photos</button></section>`;
    }
    return `<section class="phone-upload"><h4>Upload photos</h4>
      <p class="muted small">New subfolder for each upload. Originals stay unchanged. Keep this page open and your phone awake.</p>
      <details><summary>File limits</summary><p>JPEG, PNG or WebP · 300 photos · 64 MiB each · 2 GiB total.</p></details>
      <p class="small">Destination: <b>${esc(u?.parent != null ? '/Share/' + u.parent : l ? '/Share/' + l.path : 'Choose a folder above')}</b> → new subfolder</p>
      <label>Folder name <input id="phone-label" maxlength="60" value="${esc(u?.label || 'Phone photos')}" ${u?.batch || u?.busy ? 'disabled' : ''}></label>
      <label>Photos <input id="phone-files" type="file" accept="image/jpeg,image/png,image/webp,.jpg,.jpeg,.png,.webp" multiple ${u?.batch || u?.busy ? 'disabled' : ''}></label>
      <div class="jtools"><button type="button" id="phone-upload" ${!u?.files.length || u.busy || u.done === u.files.length || !l || l.error ? 'disabled' : ''}>${u?.busy ? 'Uploading…' : u?.batch ? 'Retry Remaining Photos' : 'Upload Selected Photos'}</button>
      ${u && !u.busy ? '<button type="button" id="phone-clear">New selection</button>' : ''}</div>
      <progress id="phone-progress" max="100" value="${u?.percent || 0}" ${u ? '' : 'hidden'} aria-label="Photo upload progress"></progress>
      <p id="phone-message" class="small" role="status" aria-live="polite">${esc(u?.message || 'No photos selected.')}</p></section>`;
  }
  function paintUpload() {
    const u = phoneUpload, msg = $('#phone-message'), bar = $('#phone-progress');
    if (msg) msg.textContent = u.message;
    if (bar) { bar.hidden = false; bar.value = u.percent || 0; }
  }
  function sendPhoto(u, i) {
    return new Promise((resolve, reject) => {
      const xhr = new XMLHttpRequest();
      xhr.open('POST', `api/nature/upload/file?index=${i}`);
      xhr.setRequestHeader('X-Photo-Upload', '1');
      xhr.setRequestHeader('X-Upload-ID', u.batch.id);
      xhr.timeout = 300000;
      xhr.upload.onprogress = e => {
        const completed = u.files.slice(0, i).reduce((n, f) => n + f.size, 0);
        u.percent = 100 * (completed + e.loaded) / u.total;
        u.message = `Uploading ${i + 1}/${u.files.length}: ${u.files[i].name} · ${Math.floor(u.percent)}% transferred`;
        paintUpload();
      };
      xhr.onload = () => { let data; try { data = JSON.parse(xhr.responseText); } catch { data = {}; }
        if (xhr.status >= 200 && xhr.status < 300 && data.ok) resolve(data);
        else reject(new Error(data.error || `Upload failed (${xhr.status})`)); };
      xhr.onerror = xhr.ontimeout = () => reject(new Error('Connection interrupted'));
      xhr.send(u.files[i]);
    });
  }
  function wireUpload(box) {
    const picker = box.querySelector('#phone-files');
    if (picker) picker.onchange = () => {
      const files = [...picker.files], label = box.querySelector('#phone-label').value.trim() || 'Phone photos';
      const total = files.reduce((n, f) => n + f.size, 0);
      const error = files.length > 300 || total > 2 * 1024 ** 3 ? 'Choose up to 300 photos and 2 GiB per batch.' :
        files.some(f => !/\.(jpe?g|png|webp)$/i.test(f.name) || !f.size || f.size > 64 * 1024 ** 2) ? 'Use JPEG, PNG or WebP photos, each up to 64 MiB. HEIC and videos are not supported yet.' : '';
      phoneUpload = { files: error ? [] : files, label, parent: share.list?.path, total, done: 0, percent: 0, busy: false,
        message: error || `${files.length} photo${files.length === 1 ? '' : 's'} selected · ${(total / 1024 ** 2).toFixed(1)} MiB. Ready to upload.` };
      H.rerender();
    };
    box.querySelector('#phone-clear')?.addEventListener('click', () => { phoneUpload = null; H.rerender(); });
    box.querySelector('#phone-next')?.addEventListener('click', async () => {
      if (share.list?.path !== phoneUpload.batch.path) { await loadShare(phoneUpload.batch.path); H.rerender(); }
      const form = $('#natimportform');
      form?.scrollIntoView({block:'start', behavior:'smooth'});
      form?.querySelector('[name=name]')?.focus({preventScroll:true});
    });
    const uploadButton = box.querySelector('#phone-upload');
    if (uploadButton) uploadButton.onclick = async () => {
      const u = phoneUpload; if (!u || u.busy || !u.files.length) return;
      u.label = box.querySelector('#phone-label').value.trim() || 'Phone photos';
      if (!u.batch) u.parent = share.list.path;
      u.busy = true; u.message = 'Preparing a new upload folder…'; H.rerender();
      try {
        if (!u.batch) {
          const r = await fetch('api/nature/upload', { method: 'POST', headers: {'Content-Type': 'application/json', 'X-Photo-Upload': '1'},
            body: JSON.stringify({parent: u.parent, label: u.label, files: u.files.map(f => ({name: f.name, size: f.size}))}) });
          const j = await r.json(); if (!r.ok) throw new Error(j.error || r.status); u.batch = j;
        }
        for (; u.done < u.files.length; u.done++) await sendPhoto(u, u.done);
        u.percent = 100;
        u.message = `Saved ${u.done} photo${u.done === 1 ? '' : 's'} to /Share/${u.batch.path}.`;
        await loadShare(u.batch.path);
      } catch (e) {
        u.message = `${u.done}/${u.files.length} saved. ${e.message || e}. Keep this page open and retry; saved photos will not be uploaded again.`;
      } finally {
        u.busy = false; H.rerender();
        if (u.done === u.files.length) requestAnimationFrame(() => $('#phone-next')?.scrollIntoView({block:'nearest', behavior:'smooth'}));
      }
    };
  }
  window.addEventListener('beforeunload', e => { if (phoneUpload?.busy) { e.preventDefault(); e.returnValue = ''; } });
  const jentry = (o) => `<div class="jentry">${obsLine(o, `<b>${esc((o.date || o.date_start || "").slice(0, 16).replace("T", " ").replace(/(\d\d:\d\d)$/, "$1 UTC"))}</b>`)}</div>`;
  const journalPic = (o) => "journal/" + o.artifact_file.replace(/^_journal\/img\//, "");
  // the page: Gallery (the journal's pictures, newest first, each a card to its page) | Submit (the import, and one line by hand)
  function journalHTML(brief) {
    if (brief) {
      const lines = nat.journal.slice(0, 5);
      const list = lines.length ? lines.map(jentry).join("") + (nat.journal.length > lines.length ? `<a class="chip small" href="#wiki/journal" data-slug="journal">all ${nat.journal.length} entries</a>` : "") : `<p class="muted small">Nothing in the ship's journal yet.</p>`;
      return `<section class="journal card"><h3>/Share Photos <a class="chip small" href="#wiki/journal" data-slug="journal">open</a></h3>${list}` +
        `<div class="jtools"><button type="button" class="chip" id="natimport">Add photos</button></div></section>`;
    }
    const tab = share.tab === "submit" ? "submit" : "gallery";
    const tabs = `<div class="group seg jtabs" id="jtabs"><button type="button" data-t="gallery" class="${tab === "gallery" ? "on" : ""}">Gallery</button><button type="button" data-t="submit" class="${tab === "submit" ? "on" : ""}">Submit</button></div>`;
    const body = tab === "submit" ? importHTML() : galleryHTML();
    return `<section class="journal card"><h3>/Share Photos</h3>${tabs}${body}</section>`;
  }
  function galleryHTML() {
    const pics = nat.journal.filter((o) => o.artifact_file), rest = nat.journal.filter((o) => !o.artifact_file);
    const cap = (o) => (o.detail || "").split(/(?<=\.)\s+/)[0] || o.subject || "";
    const cards = pics.map((o) => `<a class="gcard" href="#wiki/observation/${esc(o.id)}" data-slug="observation/${esc(o.id)}" title="${esc(o.subject || "")}"><img src="${esc(journalPic(o))}" alt="" loading="lazy"><div class="cap">${esc(cap(o))}</div><div class="who">${esc(o.observer || "")}${o.date ? " · " + esc(o.date.slice(0, 10)) : ""}</div></a>`).join("");
    return (pics.length ? `<div class="gallery">${cards}</div>` : `<p class="muted small">No photos yet. Choose Submit to add some.</p>`) +
      (rest.length ? `<h4>Observations without a picture</h4>` + rest.map(jentry).join("") : "");
  }
  // the import panel: the browser over the share (the folder open is the one imported), the form with the
  // permission to keep importing from it, the folders being watched, the imports so far
  const CLOCKS = [["exif", "Camera / ship"], ["ship", "Ship (Eastern)"], ["utc", "UTC"], ["+01:00", "UTC+01"], ["+02:00", "UTC+02"], ["-05:00", "UTC−05"], ["-06:00", "UTC−06"], ["-07:00", "UTC−07"]];
  const LICENCE_LABELS = {'attribution':'With credit', 'cc-by-4.0':'CC BY 4.0', 'cc-by-sa-4.0':'CC BY-SA 4.0', 'cc-by-nc-4.0':'CC BY-NC 4.0', 'cc0':'CC0', 'open-access':'Open access', 'rights-reserved':'Rights reserved'};
  const folderCount = (l) => l ? l.files.length + l.folders.reduce((n, d) => n + (d.images || 0), 0) : 0;
  function importHTML() {
    const name = store.get("chat.name", ""), f = store.get("nat.import.form", {}) || {};
    const l = share.list, at = (nm) => (l.path ? l.path + "/" : "") + nm;
    const crumbs = l ? ["Share", ...l.path.split("/").filter(Boolean)].map((seg, i, a) => i === a.length - 1 ? `<b>${esc(seg)}</b>` : `<a href="#" data-share="${esc(a.slice(1, i + 1).join("/"))}">${esc(seg)}</a>`).join(" › ") : "…";
    const folders = l ? l.folders.map((d) => `<a class="shfolder" href="#" data-share="${esc(at(d.name))}" title="open this folder">📁 ${esc(d.name)}${d.images ? ` <span class="muted">${d.images}</span>` : ""}</a>`).join("") : "";
    const files = l ? l.files.map((x) => `<figure class="shfile"><img src="api/nature/share/thumb?path=${encodeURIComponent(at(x.name))}" alt="" loading="lazy"><figcaption title="${esc(x.name)}">${esc(x.name)}</figcaption></figure>`).join("") : "";
    const lic = Object.entries(share.licences).map(([k, v]) => `<option value="${esc(k)}" title="${esc(v)}" ${(f.licence || "attribution") === k ? "selected" : ""}>${esc(LICENCE_LABELS[k] || v)}</option>`).join("");
    const clocks = CLOCKS.map(([k, v]) => `<option value="${k}" ${(f.clock || "exif") === k ? "selected" : ""}>${esc(v)}</option>`).join("");
    const n = folderCount(l), here = l && l.path ? l.path.split("/").pop() : "";
    const watched = l && share.watches.find((w) => w.path === l.path);
    const w = share.watch;
    return `<section class="import card">
      <h3>Add photos</h3>
      <div class="import-source"><span>Import from:</span><div class="group seg" role="group" aria-label="Import from"><button type="button" data-import-source="share" class="${share.source==='share'?'on':''}" aria-pressed="${share.source==='share'}">/Share folder</button><button type="button" data-import-source="device" class="${share.source==='device'?'on':''}" aria-pressed="${share.source==='device'}">This device</button></div></div>
      <p class="muted small">${share.source==='device'?'Choose a /Share destination below.':'Choose a /Share folder to import.'}</p>
      <div class="sharebar"><span class="crumbs">${crumbs}</span>${l ? `<span class="muted small">${l.files.length} photograph${l.files.length === 1 ? "" : "s"} · ${l.folders.length} folder${l.folders.length === 1 ? "" : "s"}${l.error ? ` · <span class="warn">${esc(l.error)}</span>` : ""}</span>` : ""}${watched ? `<span class="chip small on" title="new photographs here are imported every ten minutes">watched · ${esc(watched.form?.name || "")}</span>` : ""}</div>
      <div class="sharelist" id="sharelist">${l ? (folders + files || `<p class="muted small">Nothing here.</p>`) : `<p class="muted small">Reading the share…</p>`}</div>
      ${share.source==='device' ? uploadHTML(l) : ''}
      <div id="import-status">${w ? importStatus(w) : ''}</div>
      <form id="natimportform" autocomplete="off" ${share.source==='device' && (!phoneUpload?.files.length || phoneUpload.done!==phoneUpload.files.length)?'hidden':''}>
        <h4 class="wide">Add to the journal</h4>
        <label>Name<input name="name" value="${esc(f.name || name)}" required maxlength="80"></label>
        <label>Organisation<input name="org" value="${esc(f.org || "")}" maxlength="120"></label>
        <label>Email <span class="muted">Private to the ship</span><input name="email" type="email" value="${esc(f.email || "")}" maxlength="120"></label>
        <label>Licence<select name="licence">${lic}</select></label>
        <label>Time zone<select name="clock">${clocks}</select></label>
        <details class="wide"><summary>Licence & time zone</summary><p id="import-licence-help">${esc(share.licences[f.licence || 'attribution'] || '')}</p><p>Time zone applies when the photo has none. Camera / ship uses the camera's zone, then ship time.</p></details>
        <label class="row wide"><input type="checkbox" name="watch" ${watched ? "checked" : ""}><span>Keep importing from this /Share folder <small>Using this credit and licence.</small></span></label>
        <div class="wide"><button type="submit" class="go" id="natimportgo" ${n && here && !share.starting && !importActive(w) ? "" : "disabled"}>${here ? `Add to journal · ${n} photo${n === 1 ? "" : "s"}` : "Choose a folder"}</button> <span class="muted small" id="natimportmsg"></span></div>
      </form>
      ${share.watches.length ? `<div class="watches"><span class="lbl">Watched folders</span>${share.watches.map((x) => `<span class="watch"><a href="#" data-share="${esc(x.path)}">📁 ${esc(x.path.split("/").slice(-2).join("/"))}</a> · ${esc(x.form?.name || "")} · ${x.imported || 0} added${x.unchanged_failed ? ` · ${x.unchanged_failed} unchanged failures skipped` : ''} <button type="button" class="chip small" data-unwatch="${esc(x.path)}">Stop</button></span>`).join("")}</div>` : ""}
      ${share.jobs.length ? `<details class="import-history"><summary>Imports Status</summary><div class="imports">${share.jobs.slice(0, 8).map((j) => `<a class="chip small" href="#wiki/import/${esc(j.id)}" data-slug="import/${esc(j.id)}">${esc(j.form?.name || "")} · ${j.imported}/${j.total}${j.failed ? ` · ${j.failed} failed` : j.status !== "done" ? " · " + esc(j.status) : ""}${j.from_watch ? " · auto" : ""}</a>`).join(" ")}</div></details>` : ""}
    </section>`;
  }
  const importActive = j => j && ['queued','running'].includes(j.status);
  function importStatus(j) {
    const active = importActive(j), items = j.items || [];
    const added = j.imported ?? items.filter(it=>it.id).length;
    const skipped = j.skipped ?? items.filter(it=>it.status==='skipped').length;
    const failed = j.failed ?? items.filter(it=>it.status==='failed').length;
    const title = active ? 'Importing…' : j.status==='failed'||(failed && !added) ? 'Import failed' : 'Import finished';
    const reasons = [...new Set([j.error, ...(j.reasons || items.map(it=>it.error))].filter(Boolean))];
    return `<section class="import-progress" aria-label="Your import"><h4>${title}</h4>
      <progress max="100" ${!active || j.progress != null ? `value="${active ? Math.max(0,Math.min(100,j.progress)) : 100}"` : ''} aria-label="Import progress"></progress>
      <p role="status">${active ? esc(j.stage || 'Queued') : `${added}/${j.total} added · ${skipped} skipped · ${failed} failed`}</p>
      ${!active && j.failed_images ? '<p class="warn">Failed images are skipped until their contents change.</p>' : ''}
      ${j.unchanged_failed ? `<p>${j.unchanged_failed} unchanged failed images skipped.</p>` : ''}
      ${reasons.length ? `<details><summary>${active ? 'Warnings' : 'Why?'}</summary>${reasons.slice(0,3).map(r=>`<p>${esc(r)}</p>`).join('')}</details>` : ''}
      <a href="#wiki/import/${esc(j.id)}" data-slug="import/${esc(j.id)}">${active ? 'Details' : 'View results'}</a></section>`;
  }
  async function loadShare(path) {
    try {
      const r = await fetch(`api/nature/share${path == null ? "" : "?path=" + encodeURIComponent(path)}`, { cache: "no-store" });
      const j = await r.json(); if (!r.ok) throw new Error(j.error || r.status);
      share.list = j; share.path = j.path;
    } catch (e) { share.list = { path: path || "", parent: null, folders: [], files: [], error: e.message || String(e) }; }
  }
  async function loadImports() {
    try { const r = await fetch("api/nature/import", { cache: "no-store" }); const j = await r.json(); share.jobs = j.jobs || []; share.watches = j.watches || []; share.licences = j.licences || share.licences; } catch { /* the panel stands without the list */ }
  }
  function wireImport(el) {
    const box = el.querySelector(".import"); if (!box) return;
    wireUpload(box);
    for (const b of box.querySelectorAll('[data-import-source]')) b.onclick=()=>{share.source=b.dataset.importSource;store.set('nat.import.source',share.source);H.rerender();};
    if (!share.watch && store.get('nat.import.current','')) watchJob(store.get('nat.import.current',''));
    if (!share.list) Promise.all([loadShare(share.path), loadImports()]).then(() => { if (slug() === "journal") H.rerender(); });
    for (const a of box.querySelectorAll("a[data-share]")) a.onclick = (ev) => { ev.preventDefault(); if (phoneUpload?.busy) return; share.list = null; share.path = a.dataset.share; if (phoneUpload && !phoneUpload.batch) phoneUpload.parent = a.dataset.share; H.rerender(); };
    for (const b of box.querySelectorAll("button[data-unwatch]")) b.onclick = async () => {
      b.disabled = true;
      try { await fetch("api/nature/watch", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify({ path: b.dataset.unwatch, stop: true }) }); } catch { /* the list below says */ }
      await loadImports(); H.rerender();
    };
    const go = box.querySelector("#natimportgo"), form = box.querySelector("#natimportform"), msg = box.querySelector("#natimportmsg");
    form.oninput = () => { const f = form.elements; store.set('nat.import.form', {name:f.name.value, org:f.org.value, email:f.email.value, licence:f.licence.value, clock:f.clock.value}); box.querySelector('#import-licence-help').textContent=share.licences[f.licence.value] || ''; };
    form.onsubmit = async (ev) => {
      ev.preventDefault();
      if (share.starting || importActive(share.watch)) return;
      const f = form.elements, l = share.list; if (!l || !l.path) return;
      const body = { folder: l.path, name: f.name.value.trim(), org: f.org.value.trim(), email: f.email.value.trim(), licence: f.licence.value, clock: f.clock.value, watch: f.watch.checked, token: store.get("chat.token", "") };
      store.set("nat.import.form", { name: body.name, org: body.org, email: body.email, licence: body.licence, clock: body.clock });
      if (body.name) store.set("chat.name", body.name);
      share.starting = true; go.disabled = true; msg.textContent = "Starting…";
      try {
        const r = await fetch("api/nature/import", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body) });
        const j = await r.json(); if (!r.ok) throw new Error(j.error || r.status);
        share.watch = j.job;
        store.set('nat.import.current', j.job.id);
        if (phoneUpload?.batch?.path === body.folder) phoneUpload.journalJob = j.job.id;
        UW.toast?.(`Importing ${j.job.total} photograph${j.job.total === 1 ? "" : "s"}${j.job.known ? ` (${j.job.known} already in the journal)` : ""}${body.watch ? "; the folder is watched" : ""}`);
        watchJob(j.job.id);
        await loadImports(); share.starting = false; H.rerender();
      } catch (e) { msg.textContent = `Not started: ${e.message || e}`; go.disabled = false; }
      finally { share.starting = false; }
    };
  }
  // the job followed from the page: the message under the form while it runs; the journal and the map when it is done
  const importPolls = new Set();
  async function watchJob(id) {
    if (importPolls.has(id)) return;
    importPolls.add(id);
    try { for (;;) {
      let j; try { const r = await fetch(`api/nature/import?job=${encodeURIComponent(id)}`, { cache: "no-store" });
        if (r.status===404) { store.set('nat.import.current',''); return; }
        j = await r.json(); if (!r.ok) throw new Error();
      } catch { await new Promise(r=>setTimeout(r,5000)); continue; }
      share.watch = j;
      const status = $('#import-status'); if (status) status.innerHTML = importStatus(j);
      if (importActive(j) && $('#natimportgo')) $('#natimportgo').disabled = true;
      if (j.status === "done" || j.status === "failed") {
        await Promise.all([loadJournal(), loadImports()]);
        if (slug() === "journal" || slug() === `import/${id}` || !slug()) H.rerender();
        if (UW.state.nature) UW.renderMap();
        UW.toast?.(`${j.imported ?? j.done}/${j.total} added${j.skipped ? ` · ${j.skipped} skipped` : ''}${j.failed ? ` · ${j.failed} failed` : ''}`);
        return;
      }
      await new Promise(r=>setTimeout(r,2500));
    } } finally { importPolls.delete(id); }
  }
  // an import's page: what became of each photograph, live while it runs
  async function renderImport(el, id) {
    let j = null;
    try { const r = await fetch(`api/nature/import?job=${encodeURIComponent(id)}`, { cache: "no-store" }); if (r.ok) j = await r.json(); } catch { /* shown as missing */ }
    if (!j) { el.innerHTML = crumb(here("Import", slug())) + `<p class="muted">No such import.</p>`; return; }
    const live = j.status === "queued" || j.status === "running";
    const item = (it) => {
      const pic = it.artifact_file ? `journal/${esc(it.artifact_file.replace(/^_journal\/img\//, ""))}` : `api/nature/share/thumb?path=${encodeURIComponent(it.file)}`;
      const s = it.subject_page ? subjectOf(it.subject_page) : null;
      const subj = s ? `<a href="#wiki/${esc(s.page)}" data-slug="${esc(s.page)}"><span class="dot" style="background:${domainOf(s.domain).colour}"></span>${esc(displayName(s))}</a>` : esc(it.subject || "");
      const where = it.lat != null ? `${(+it.lat).toFixed(3)}, ${(+it.lon).toFixed(3)}${it.position ? ` <span class="muted">(${esc(it.position)})</span>` : ""}` : "";
      return `<article class="impitem ${esc(it.status)}">${it.id ? `<a href="#wiki/observation/${esc(it.id)}" data-slug="observation/${esc(it.id)}">` : "<span>"}<img src="${pic}" alt="" loading="lazy">${it.id ? "</a>" : "</span>"}<div class="body"><b>${esc(it.caption || it.file.split("/").pop())}</b>
        <div class="meta">${[subj, it.kind ? esc(it.kind) : "", it.date ? esc(it.date.replace("T", " ").replace("Z", " UTC")) : "", where].filter(Boolean).join(" · ")}</div>
        ${it.tags?.length ? `<div class="tags">${it.tags.map((t) => `<span>${esc(t)}</span>`).join("")}</div>` : ""}
        <div class="st">${esc(it.status)}${it.error ? `: ${esc(it.error)}` : ""}${it.id ? ` · ${esc(it.id)}` : ""} · <span class="muted">${esc(it.file)}</span></div></div></article>`;
    };
    el.innerHTML = crumb(hlinkJournal(), here("Import", slug())) +
      `<h2>${esc(j.form?.name || "")}'s photographs${j.form?.org ? ` <span class="muted">${esc(j.form.org)}</span>` : ""}</h2>` +
      (j.folder ? `<p class="muted small">📁 ${esc(j.folder)}${j.from_watch ? " · imported by the watch on this folder" : ""}${j.known ? ` · ${j.known} already in the journal, passed over` : ""}</p>` : "") +
      importStatus(j) +
      `<div class="implist">${j.items.map(item).join("")}</div>`;
    if (live) setTimeout(() => { if (slug() === `import/${id}`) H.rerender(); }, 3000);
  }
  const hlinkJournal = () => `<a href="#wiki/journal" data-slug="journal">/Share Photos</a>`;
  function wireJournal(el) {
    const imp = el.querySelector("#natimport"); if (imp) imp.onclick = () => { share.tab = "submit"; store.set("nat.jtab", "submit"); H.open("journal"); };
    for (const b of el.querySelectorAll("#jtabs button")) b.onclick = () => { share.tab = b.dataset.t; store.set("nat.jtab", share.tab); H.rerender(); };
    wireImport(el);
  }

  // ---------------------------------------------------------------- the menus
  // under the map's Nature pill, while that layer is on, the domains as a
  // filter menu (history.js's); and the domains for the wiki's Browse menu
  function mapMenu() {
    const bar = $("#mapnatlayers");
    if (!bar) return;
    H.menu(bar, "Domains", Object.entries(DOMAINS).map(([d, D]) => ({ key: d, label: D.label, colour: D.colour, hint: D.hint })), nat.domains, "nat.domains", () => UW.state.nature);
    bar.hidden = !UW.state.nature || !UW.M.history;
  }
  const domainOptions = (cur) => Object.entries(DOMAINS).map(([d, D]) => `<option value="domain/${d}" ${cur === `domain/${d}` || cur.startsWith(`domain/${d}/`) ? "selected" : ""}>${esc(D.label)}</option>`).join("");
  // the works the natural record cites, for the wiki's bibliography
  const cited = () => [...nat.obs, ...nat.subjects].map((x) => x.bibkey).filter(Boolean);

  // ---------------------------------------------------------------- the map
  // a position on the map, the nature layer switched on for it
  const focusPoint = (lat, lon, label) => H.focusPoint(lat, lon, label, "", "nature");
  UW.onNatureClick = (id, pt) => {
    if (pt && pt.lat != null) UW.state.focus = { lat: +pt.lat, lon: +pt.lon, label: String(pt.text || "").replace(/<br>.*$/s, "").replace(/<[^>]+>/g, "") };
    H.open(`observation/${id}`, { fromMap: true });
  };

  // ---------------------------------------------------------------- the map layer
  // the observations; the natural topics' artifacts and places come from
  // history.js, which draws each past layer's own domain
  const prevExtra = UW.extraMapTraces;
  // the observations on the Nature layer; the ship's own photographs on the Photos layer, with the cameras
  UW.extraMapTraces = () => {
    const out = prevExtra ? prevExtra() : [];
    if (!(UW.state.nature || UW.state.photos) || !UW.M.history) return out;
    if (!nat.subjects) { ensure().then(() => UW.renderMap()); return out; }
    const hover = (o) => { const s = subjectOf(o.subject); return `${esc(short(s ? shortName(s) : o.subject, 50))}${numberOf(o) ? " · " + esc(numberOf(o)) : ""}<br>${esc(o.date_text || H.dateLabel(o.date_start) || (o.date || ""))}${o.observer ? " · " + esc(short(o.observer, 40)) : ""}${o._journal ? "<br>the ship's journal" : ""}`; };
    const pts = UW.state.nature ? shownObs(true).filter((o) => o.lat != null && !o._journal) : [];
    if (pts.length) out.push({ type: "scattermap", mode: "markers", name: "nature", showlegend: false, hoverinfo: "text",
      lat: pts.map((o) => o.lat), lon: pts.map((o) => o.lon), text: pts.map(hover), customdata: pts.map((o) => `nat:${o.id}`),
      marker: { size: 9, color: pts.map((o) => domainOf(domainOfObs(o)).colour), opacity: .92 } });
    // the ship's own photographs, ringed, whatever page is open and whatever the domains menu allows
    const mine = UW.state.photos ? allObs().filter((o) => o._journal && o.lat != null) : [];
    if (mine.length) {
      out.push({ type: "scattermap", mode: "markers", name: "photos-ring", showlegend: false, hoverinfo: "skip",
        lat: mine.map((o) => o.lat), lon: mine.map((o) => o.lon), marker: { size: 18, color: C.accent2, opacity: .35 } });
      out.push({ type: "scattermap", mode: "markers", name: "photos", showlegend: false, hoverinfo: "text",
        lat: mine.map((o) => o.lat), lon: mine.map((o) => o.lon), text: mine.map(hover), customdata: mine.map((o) => `nat:${o.id}`),
        marker: { size: 12, color: mine.map((o) => domainOf(domainOfObs(o)).colour), opacity: .92 } });
    }
    return out;
  };

  // ---------------------------------------------------------------- wiring
  // what history.js asks of this half: its data, its views, its parts of the shared pages
  UW.natureViews = { ensure, handles, render, vignetteCards, homeExtraHTML, wireHome, exploreExtraHTML, searchExtra, topicExtra,
    toggleMore: (k) => { nat.more[k] = !nat.more[k]; }, mapMenu, domainOptions, titleOf, cited, DOMAINS, available: () => nat.available };
  // the pane may be showing already, drawn before this file loaded: its natural parts arrive with the data
  if (!$("#pane-wiki").hidden || UW.state.nature) ensure().then(() => { if (!$("#pane-wiki").hidden) H.refresh(); else { mapMenu(); if (UW.state.nature) UW.renderMap(); } }).catch(() => {});
})();
