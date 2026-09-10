/* The Nature tab: the natural half of the history layer. What the archipelago
 * is and does, as the record of it was written down: subjects (a species, a
 * rock unit, a kind of ice, a variable, the aurora, the Magnetic Pole) and
 * observations of them (one subject, one date, one position, one source, the
 * number in the unit it was written in), with the documents behind them: the
 * natural topics' narrative pages, images, maps, quotes, texts, people,
 * places and events, which history.js renders into this pane the way it
 * renders the human half into its own (UW.histShared.render, in this tab's
 * namespace). The topics of the index carry a domain, "history" or "nature",
 * and this tab takes the natural ones. The ship's own sightings go into a
 * journal through the form here and reach grid on the next push; until grid
 * ingests them they show as the ship's, ringed on the map. Doc, the resident
 * scientist, answers from this half in the chat. Loaded after history.js. */
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
  const BACK_SHOWN = 40;

  const nat = {
    subjects: null, obs: null, topics: [], stamp: null, loading: null,
    available: false,                                      // the natural files are in this build
    journal: [],                                           // the ship's own lines, newest first, from the server
    slug: store.get("nat.slug", ""),                       // "" home, explore, record, journal, domain/<d>, subjects/<kind>, kind/<document kind>, topic/<t>, subject/<s>, observation/<id>, or a page
    domains: new Set(store.get("nat.domains", Object.keys(DOMAINS))),   // the domains the map layer shows
    search: "",
    more: { today: false, here: false },
    byName: null, bySubject: null, children: null,          // indexes built once per generation
    pages: new Map(),
  };
  UW.natureSlug = () => nat.slug;                          // history.js reads it for the views it renders here

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
      const r = await fetch("/api/nature/journal", { cache: "no-store" });
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
    const s = nat.slug;
    if (s.startsWith("topic/")) return { topic: s.slice(6) };
    if (s.startsWith("domain/")) return { domain: s.slice(7) };
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
    const sc = scopeOf();
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
    return `<a class="vig" href="#nature/observation/${esc(o.id)}" data-nslug="observation/${esc(o.id)}" data-lat="${o.lat ?? ""}" data-lon="${o.lon ?? ""}"><span class="dot" style="background:${domainOf(domainOfObs(o)).colour}"></span>${lead}<span class="txt">${what}${num ? ` <i>${esc(num)}</i>` : ""}${o.qualifier && !num ? ` <i>${esc(o.qualifier)}</i>` : ""}${o.observer ? ` <span class="muted">${esc(o.observer)}</span>` : ""}${o.place ? ` <span class="muted">· ${esc(o.place)}</span>` : ""}${o._journal ? ` <span class="status draft" title="the ship's own line, not yet ingested on grid">ship</span>` : ""}${o.lat != null ? ` <span class="pin" title="on the map">⌖</span>` : ""}</span></a>`;
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
  function vignetteHTML() {
    const now = new Date(), pos = UW.M.latest || {};
    const today = onThisDay(now), here = nearShip(pos.lat, pos.lon);
    const dateWord = now.toLocaleDateString(undefined, { month: "long", day: "numeric" });
    const fold = (xs, key, line) => {
      const shown = nat.more[key] ? xs : xs.slice(0, VIG_N), rest = xs.length - shown.length;
      return shown.map(line).join("") + (rest > 0 ? `<button type="button" class="more" data-more="${key}">see ${rest} more</button>` : nat.more[key] && xs.length > VIG_N ? `<button type="button" class="more" data-more="${key}">see fewer</button>` : "");
    };
    return `<div class="vignettes">
      <section class="vigcard"><h3>On this day · ${esc(dateWord)}</h3>${today.length ? fold(today, "today", (o) => obsLine(o, `<b>${esc(H.yearLabel(o._year))}</b>`)) : `<div class="muted small">Nothing observed on ${esc(dateWord)} in the record yet.</div>`}</section>
      <section class="vigcard"><h3>Near the ship${pos.lat != null ? ` · ${esc(H.whereName(pos.lat, pos.lon) || "")}` : ""}</h3>${here.length ? fold(here, "here", (x) => obsLine(x.o, `<b>${Math.round(x.d)} km</b>`)) : `<div class="muted small">${pos.lat == null ? "The ship's position is not known to this build." : `Nothing observed within ${NEAR_KM} km of the ship yet.`}</div>`}</section>
    </div>`;
  }

  // ---------------------------------------------------------------- the pane
  const crumb = (...rest) => `<div class="crumb"><a href="#nature/" data-nslug="">Nature</a>${rest.map((r) => ` › ${r}`).join("")}</div>`;
  const here = (label, slug) => `<a class="here" href="#nature/${esc(slug)}" data-nslug="${esc(slug)}" title="this page's address">${label}</a>`;
  const hlink = (slug, label) => `<a href="#history/${esc(slug)}" data-slug="${esc(slug)}">${label}</a>`;
  const subjectLink = (s, cls = "") => `<a class="${cls}" href="#nature/${esc(s.page)}" data-nslug="${esc(s.page)}"><span class="dot" style="background:${domainOf(s.domain).colour}"></span>${s.kind === "taxon" ? `<i>${esc(s.name)}</i>` : esc(s.name)}${s.english && s.english !== s.name ? ` <span class="muted">${esc(s.english)}</span>` : ""}</a>`;
  const domainChip = (d, n, on = false) => { const D = domainOf(d); return `<a class="chip ${on ? "on" : ""}" href="#nature/domain/${esc(d)}" data-nslug="domain/${esc(d)}" title="${esc(D.hint)}"><span class="dot" style="background:${D.colour}"></span>${esc(D.label)}${n != null ? ` <span class="muted">${n}</span>` : ""}</a>`; };
  // the documents of the natural topics, by kind, as chips with their counts
  function documentChips() {
    const hd = H.data(), isNature = (x) => x.topic && H.topicDomain(x.topic) === "nature";
    const n = { people: hd.people.filter(isNature).length, animal: hd.animals.filter(isNature).length, vessel: hd.vessels.filter(isNature).length,
      place: hd.places.filter(isNature).length, event: hd.events.filter(isNature).length };
    for (const a of hd.artifacts) if (isNature(a)) n[a.type] = (n[a.type] || 0) + 1;
    return `<div class="domgrid">${Object.entries(H.KINDS).filter(([k]) => n[k]).map(([k, K]) => `<a class="chip" href="#nature/kind/${k}" data-nslug="kind/${k}"><span class="dot" style="background:${K.colour}"></span>${esc(K.label)} <span class="muted">${n[k]}</span></a>`).join("")}</div>`;
  }
  // a link to a page, on whichever tab owns it
  const pageLinkFor = (slug, label, cls = "") => H.pageDomain(slug) === "nature" ? `<a class="${cls}" href="#nature/${esc(slug)}" data-nslug="${esc(slug)}">${label}</a>` : `<a class="${cls}" href="#history/${esc(slug)}" data-slug="${esc(slug)}">${label}</a>`;
  // a subject as a line in a list: the names in the region's languages beside it
  function subjectRow(s) {
    const names = [s.english !== s.name ? s.english : "", s.inuktitut, s.kalaallisut, s.french].filter(Boolean).join(" · ");
    const n = (nat.bySubject.get(s.name) || []).length;
    return `<a class="person" href="#nature/${esc(s.page)}" data-nslug="${esc(s.page)}"><b class="${s.kind === "taxon" ? "sci" : ""}">${esc(s.name)}</b>${names ? ` <span class="muted">(${esc(names)})</span>` : ""} <span class="muted small">${esc(s.rank || s.kind || "")}${n ? ` · ${n}` : ""}</span>${s.status ? ` <span class="status" title="conservation status">${esc(s.status)}</span>` : ""}${s.note ? `<span class="role">${esc(s.note.length > 160 ? s.note.slice(0, 157) + "…" : s.note)}</span>` : ""}</a>`;
  }
  // the record: observations as a table, oldest first, with a chart above it
  // when there are numbers to draw
  function recordHTML(rows, opts = {}) {
    const tr = (o, i) => {
      const su = subjectOf(o.subject);
      const what = opts.subject ? "" : `<td>${su ? subjectLink(su) : esc(o.subject)}</td>`;
      const where = o.place ? esc(o.place) : "";
      const pin = o.lat != null ? ` <span class="pin" data-lat="${o.lat}" data-lon="${o.lon}" data-label="${esc(o.subject)}" title="on the map">⌖</span>` : "";
      const num = numberOf(o);
      const src = o.bibkey ? H.sourceRef(o.bibkey, o.pages) : o._journal ? `<span class="status draft" title="the ship's own line, not yet ingested on grid">ship's journal</span>` : "";
      return `<tr data-key="${i}"${o.lat != null ? ` data-lat="${o.lat}" data-lon="${o.lon}"` : ""}><td class="mono"><a href="#nature/observation/${esc(o.id)}" data-nslug="observation/${esc(o.id)}">${esc(o.date_text || H.dateLabel(o.date_start))}</a></td>${what}` +
        `<td>${num ? `<b>${esc(num)}</b> ` : ""}${o.qualifier && !/\bat$/.test(o.qualifier) ? `<i>${esc(o.qualifier)}</i> ` : ""}${o.depth != null ? `<span class="muted">${esc(fmtNum(o.depth))} m down</span> ` : ""}${o.height != null ? `<span class="muted">${esc(fmtNum(o.height))} m up</span> ` : ""}<span class="muted">${esc(short(o.detail || "", 110))}</span></td>` +
        `<td>${where}${pin}</td><td>${esc(o.observer || "")}${o.vessel ? ` <span class="muted">${esc(o.vessel)}</span>` : ""}</td><td>${src}</td></tr>`;
    };
    const chart = rows.length >= 2 ? `<section class="panel card castplot wide solo" data-cp="natplot"><div class="head"><h3>${esc(opts.title || "The record")}</h3><div class="tools"><span class="now">${rows.length} observations · scroll to zoom, drag to pan, click a point for its row</span><button type="button" class="reset" id="natplotreset" title="the whole record">⟲</button></div></div><div class="plot" id="natplot"></div></section>` : "";
    return chart + `<div class="hscroll" id="natrecord"><table class="sched timeline record"><thead><tr><th>Date</th>${opts.subject ? "" : "<th>Subject</th>"}<th>Observation</th><th>Where</th><th>Observer</th><th>Source</th></tr></thead><tbody>${rows.map(tr).join("")}</tbody></table></div>`;
  }
  const short = (t, n = 60) => t.length > n ? t.slice(0, n - 3) + "…" : t;
  // the chart: values against the year when the rows share a unit and carry
  // numbers; else the dates as points, a row per subject (or per domain when
  // the subjects are many)
  function drawRecord(rows, opts = {}) {
    const gd = $("#pane-nature #natplot"); if (!gd) return;
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
    Plotly.react(gd, traces, layout, UW.CFG).then((g) => {
      UW.axisZoom(g);
      g.removeAllListeners?.("plotly_click"); g.on("plotly_click", (ev) => { const k = ev.points?.[0]?.customdata; if (k != null) showRow(k); });
      g.removeAllListeners?.("plotly_relayout"); g.on("plotly_relayout", () => {
        const r = g._fullLayout?.xaxis?.range; if (!r) return;
        const t = H.yearTicks(r[0], r[1]);
        if (JSON.stringify(t.tickvals) !== JSON.stringify(g.layout.xaxis.tickvals)) Plotly.relayout(g, { "xaxis.tickvals": t.tickvals, "xaxis.ticktext": t.ticktext });
      });
    });
    const reset = $("#pane-nature #natplotreset"); if (reset) reset.onclick = () => { if (gd?.data) Plotly.relayout(gd, { "xaxis.autorange": true }); };
  }
  function showRow(key) {
    const host = $("#pane-nature #natrecord"), row = host?.querySelector(`tr[data-key="${key}"]`);
    if (!row) return;
    for (const x of host.querySelectorAll("tr.on")) x.classList.remove("on");
    row.classList.add("on");
    row.scrollIntoView({ block: "center", behavior: "smooth" });
    if (row.dataset.lat) focusPoint(row.dataset.lat, row.dataset.lon, row.children[1]?.textContent);
  }

  async function renderMain() {
    const el = $("#natmain");
    el.scrollTop = 0;
    for (const id of ["#natplot", "#histplot"]) { const plot = el.querySelector(id); if (plot?.data) Plotly.purge(plot); }
    if (!UW.M.history) { el.innerHTML = `<div class="empty">No history has been published yet.</div>`; return; }
    if (!nat.subjects) { el.innerHTML = `<div class="empty">Loading the record…</div>`; return; }
    const q = nat.search.trim().toLowerCase();
    if (q) {                                                        // search: the documents (history.js), then the subjects and observations
      const words = q.split(/\s+/).filter(Boolean);
      const hit = (s) => { const t = String(s || "").toLowerCase(); return words.every((w) => t.includes(w)); };
      const subs = nat.subjects.filter((s) => hit([s.name, s.english, s.french, s.inuktitut, s.kalaallisut, s.also, s.note].join(" ")));
      const obs = allObs().filter((o) => hit([o.subject, o.detail, o.observer, o.place, o.qualifier, o.vessel].join(" "))).sort(byDate);
      await H.render.search(el, nat.search);
      el.insertAdjacentHTML("beforeend", `<h2>${subs.length} subjects · ${obs.length} observations</h2>` +
        (subs.length ? `<div class="peoplelist subjlist">${subs.slice(0, 60).map(subjectRow).join("")}</div>` : "") +
        (obs.length ? recordHTML(obs.slice(0, 200), { title: "Observations found" }) : ""));
      if (obs.length) drawRecord(obs.slice(0, 200));
      return;
    }
    if (!nat.slug) {                                              // the home: today, here, the journal, the domains, the topics
      const counts = new Map(); for (const o of allObs()) { const d = domainOfObs(o) || "other"; counts.set(d, (counts.get(d) || 0) + 1); }
      const doms = [...Object.keys(DOMAINS), ...[...counts.keys()].filter((d) => !DOMAINS[d])].map((d) => domainChip(d, counts.get(d) || 0)).join("");
      el.innerHTML = (nat.available ? vignetteHTML() : `<p class="lead">The natural half of the record is not in this build yet: the subjects and observations arrive with the next pull once grid publishes them. The ship's journal works now.</p>`) +
        journalHTML(true) +
        `<h2>Domains <a class="chip small" href="#nature/record" data-nslug="record">The whole record</a></h2><div class="domgrid">${doms}</div>` +
        `<h2>Documents</h2>` + documentChips() +
        (nat.topics.length ? `<h2>Narratives <a class="chip small" href="#nature/explore" data-nslug="explore">Explore</a></h2><div class="topicgrid chips">${nat.topics.filter((t) => t.pages > 0 || t.artifacts > 0).map((t) => H.topicCard(t)).join("")}</div>` : "");
      for (const b of el.querySelectorAll("button.more")) b.onclick = () => { nat.more[b.dataset.more] = !nat.more[b.dataset.more]; renderMain(); };
      wireJournal(el);
      return;
    }
    if (nat.slug === "explore") {
      const hd = H.data(), nArts = hd.artifacts.filter((a) => a.topic && H.topicDomain(a.topic) === "nature").length;
      el.innerHTML = crumb(here("Explore", "explore")) + `<h2>Explore</h2><p class="lead">${nat.topics.length} topics, ${nat.subjects.length} subjects, ${allObs().length} observations and ${nArts} documents: what the archipelago is and does, as the people of the History tab wrote it down on the way, and as the stations, the surveys and the ship record it now. Every observation cites its source; every number stands in the unit it was written in.</p>` +
        (nat.topics.length ? `<div class="topicgrid">${nat.topics.map((t) => H.topicCard(t, true)).join("")}</div>` : `<p class="muted">No natural topics in this build yet.</p>`) +
        `<h3>Documents</h3>` + documentChips() +
        `<h3>Kinds of subject</h3><div class="domgrid">${Object.entries(SUBJECT_KINDS).map(([k, label]) => { const n = nat.subjects.filter((s) => s.kind === k).length; return n ? `<a class="chip" href="#nature/subjects/${k}" data-nslug="subjects/${k}">${esc(label)} <span class="muted">${n}</span></a>` : ""; }).join("")}</div>`;
      return;
    }
    if (nat.slug === "record") {
      const rows = shownObs().sort(byDate);
      el.innerHTML = crumb(here("The record", "record")) + `<h2><span class="muted">${rows.length}</span> Observations</h2><div class="domgrid">${Object.keys(DOMAINS).map((d) => domainChip(d, rows.filter((o) => domainOfObs(o) === d).length)).join("")}</div>` + (rows.length ? recordHTML(rows, { title: "The record" }) : `<p class="muted">Nothing in the record yet.</p>`);
      drawRecord(rows);
      return;
    }
    if (nat.slug === "journal") { el.innerHTML = crumb(here("The ship's journal", "journal")) + journalHTML(false); wireJournal(el); return; }
    if (nat.slug.startsWith("domain/")) {
      const d = nat.slug.slice(7), D = domainOf(d);
      const subs = nat.subjects.filter((s) => s.domain === d).sort((a, b) => a.name.localeCompare(b.name));
      const rows = shownObs().sort(byDate);
      const kinds = [...new Set(subs.map((s) => s.kind))];
      el.innerHTML = crumb(here(esc(D.label), nat.slug)) + `<h2><span class="dot" style="background:${D.colour}"></span>${esc(D.label)}</h2><p class="lead">${esc(D.hint)}</p>` +
        kinds.map((k) => `<h3>${esc(SUBJECT_KINDS[k] || k)} <span class="muted">${subs.filter((s) => s.kind === k).length}</span></h3><div class="peoplelist subjlist">${subs.filter((s) => s.kind === k).map(subjectRow).join("")}</div>`).join("") +
        (rows.length ? `<h3><span class="muted">${rows.length}</span> Observations</h3>` + recordHTML(rows, { title: D.label }) : "");
      drawRecord(rows);
      return;
    }
    if (nat.slug.startsWith("subjects/")) {
      const k = nat.slug.slice(9);
      const subs = nat.subjects.filter((s) => s.kind === k);
      el.innerHTML = crumb(here(esc(SUBJECT_KINDS[k] || k), nat.slug)) + `<h2><span class="muted">${subs.length}</span> ${esc(SUBJECT_KINDS[k] || k)}</h2>` + treeHTML(subs);
      return;
    }
    if (nat.slug.startsWith("kind/")) { await H.render.kind(el, nat.slug.slice(5)); return; }      // a document kind: images, maps, people, places, events…
    if (nat.slug.startsWith("topic/")) {
      const t = topicOf(nat.slug.slice(6));
      if (!t) { el.innerHTML = crumb() + `<div class="empty">no such topic</div>`; return; }
      await H.render.topic(el, t.slug);                                                            // the narrative pages and the documents
      const subs = nat.subjects.filter((s) => s.topic === t.slug).sort((a, b) => a.name.localeCompare(b.name));
      const rows = shownObs().sort(byDate);
      el.insertAdjacentHTML("beforeend",
        (subs.length ? `<h3><span class="muted">${subs.length}</span> Subjects</h3><div class="peoplelist subjlist">${subs.map(subjectRow).join("")}</div>` : "") +
        (rows.length ? `<h3><span class="muted">${rows.length}</span> Observations</h3>` + recordHTML(rows, { title: t.title }) : ""));
      drawRecord(rows);
      return;
    }
    if (nat.slug.startsWith("observation/")) { renderObservation(el, nat.slug.slice(12)); return; }
    if (nat.slug.startsWith("subject/")) { await renderSubject(el, nat.slug); return; }
    await H.render.page(el, nat.slug);                                                            // an artifact, a person, a place, an event, a source, a narrative
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
    const pictures = hd.artifacts.filter((a) => (a.type === "image" || a.type === "map") && (evidence.has(a.id) || (a.topic && H.topicDomain(a.topic) === "nature" && (a.tags || []).some((x) => lname.includes(String(x).toLowerCase())))));
    el.innerHTML = crumb(...(t ? [`<a href="#nature/topic/${esc(t.slug)}" data-nslug="topic/${esc(t.slug)}">${esc(t.title)}</a>`] : [domainChip(row.domain || "other")]), here(`<span class="kind">subject</span>`, slug)) +
      `<h2>${row.kind === "taxon" ? `<i>${esc(row.name)}</i>` : esc(row.name)}${row.english && row.english !== row.name ? ` <span class="muted">${esc(row.english)}</span>` : ""}</h2>` +
      `<div class="artmeta"><span class="dot" style="background:${D.colour}"></span>${esc(D.label)}${meta ? " · " + meta : ""}${parent ? ` · under ${subjectLink(parent)}` : ""}${backbone ? " · " + backbone : ""}</div>` +
      `<div class="names">${names.map(([l, v]) => `<span class="lbl">${esc(l)}</span><span class="${l === "scientific" ? "sci" : ""}">${esc(v)}</span>`).join("")}</div>` +
      (row.bibkey ? H.facts([["source", H.sourceRef(row.bibkey)]]) : "") +
      `<div class="wiki">${H.markdown(p?.html || row.note || "")}</div>` +
      (pictures.length ? `<h3>Pictures <span class="muted">${pictures.length}</span></h3><div class="artgrid pictures">${pictures.slice(0, 24).map((a) => H.artifactCard(a, { creator: true })).join("")}</div>${pictures.length > 24 ? `<p class="muted small">and ${pictures.length - 24} more among the <a href="#nature/kind/image" data-nslug="kind/image">images</a></p>` : ""}` : "") +
      (kids.length ? `<h3>Below it <span class="muted">${kids.length}</span></h3><div class="peoplelist subjlist">${kids.map(subjectRow).join("")}</div>` : "") +
      `<h3>The record <span class="muted">${rows.length}</span></h3>` + (rows.length ? recordHTML(rows, { subject: true, title: shortName(row) }) : `<p class="muted">No observations of it in the record yet.</p>`) +
      mentionedHTML(p?.backlinks || []);
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
  function mentionedHTML(back) {
    if (!back.length) return "";
    const hidx = H.data().index;
    const title = (slug) => hidx?.pages.find((x) => x.slug === slug)?.title || subjectBySlug(slug)?.name || slug;
    const link = (slug) => H.pageDomain(slug) === "nature" ? `<a href="#nature/${esc(slug)}" data-nslug="${esc(slug)}">${esc(title(slug))}</a>` : hlink(slug, esc(title(slug)));
    return `<div class="backlinks"><span class="lbl">Mentioned in</span>${back.slice(0, BACK_SHOWN).map(link).join("")}${back.length > BACK_SHOWN ? `<span class="muted">and ${back.length - BACK_SHOWN} more</span>` : ""}</div>`;
  }
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
      ["confidence", o.confidence], ["origin", o._journal ? "the ship's journal, not yet ingested on grid" : o.origin === "ship" || o.origin === "crew" ? "the ship's own journal" : ""],
      ["source", o.bibkey ? H.sourceRef(o.bibkey, o.pages) : ""],
    ]);
    el.innerHTML = crumb(...(t ? [`<a href="#nature/topic/${esc(t.slug)}" data-nslug="topic/${esc(t.slug)}">${esc(t.title)}</a>`] : [domainChip(domainOfObs(o) || "other")]), here(`<span class="kind">observation</span>`, `observation/${o.id}`)) +
      `<h2>${s ? subjectLink(s) : esc(o.subject)}${num ? ` <span class="muted">${esc(num)}</span>` : ""}</h2>` +
      `<div class="artmeta"><span class="dot" style="background:${D.colour}"></span>${esc(D.label)}${when ? " · " + esc(when) : ""}${place ? " · " + place : ""}${where}${o.sensitive ? ` · <span class="status draft" title="a sensitive site: the position is published coarsened and the place left blank">sensitive</span>` : ""}</div>` +
      (o.observer || o.vessel ? `<div class="artpeople"><span class="lbl">By</span>${o.observer ? person ? pageLinkFor(person.page, esc(o.observer), "chip small") : `<span class="chip small">${esc(o.observer)}</span>` : ""}${o.vessel ? vessel ? pageLinkFor(vessel.page, esc(o.vessel), "chip small") : `<span class="chip small">${esc(o.vessel)}</span>` : ""}</div>` : "") +
      (o.artifact_file ? `<figure><img src="${esc(o.artifact_file.startsWith("_journal/") ? "journal/" + o.artifact_file.slice(9) : o.artifact_file)}" alt=""><figcaption>${esc(o.observer || "the ship")}</figcaption></figure>` : "") +
      facts + `<div class="wiki"><p>${esc(o.detail || "")}</p></div>` +
      (art ? `<h3>Evidence</h3><div class="artgrid">${H.artifactCard(art, { creator: true })}</div>` : "") +
      (ev ? `<div class="backlinks"><span class="lbl">Also the event</span>${pageLinkFor(`event/${ev.id}`, esc(ev.title))}</div>` : "") +
      (s ? `<div class="backlinks"><span class="lbl">The record</span><a href="#nature/${esc(s.page)}" data-nslug="${esc(s.page)}">every observation of ${esc(shortName(s))}</a></div>` : "");
    if (o.lat != null) focusPoint(o.lat, o.lon, o.subject);
  }

  // ---------------------------------------------------------------- the journal
  // The ship's own observations: a form that writes one line of the journal
  // in the CLI's vocabulary, with the position from the ship's GPS and the
  // time from the clock, the subject picked from the published list by any
  // of its names, a photograph attached, a sensitive sighting marked. The
  // line reaches grid on the next push; grid's writer validates it, so the
  // form asks only for what a line must carry.
  function journalHTML(brief) {
    const lines = brief ? nat.journal.slice(0, 5) : nat.journal;
    const status = (o) => nat.obs.some((x) => x.id === o.id) ? `<span class="st">published</span>` : `<span class="st">awaiting grid</span>`;
    const entry = (o) => `<div class="jentry">${obsLine(o, `<b>${esc((o.date || o.date_start || "").slice(0, 16).replace("T", " ").replace(/(\d\d:\d\d)$/, "$1 UTC"))}</b>`)}${status(o)}</div>`;
    const list = lines.length ? lines.map(entry).join("") + (brief && nat.journal.length > lines.length ? `<a class="chip small" href="#nature/journal" data-nslug="journal">all ${nat.journal.length} entries</a>` : "") : `<p class="muted small">Nothing in the ship's journal yet.</p>`;
    const form = brief ? `<button type="button" class="chip" id="natnew">+ New observation</button>` : formHTML();
    return `<section class="journal card"><h3>The ship's journal${brief ? ` <a class="chip small" href="#nature/journal" data-nslug="journal">open</a>` : ""}</h3>${list}${form}</section>`;
  }
  function formHTML() {
    const pos = UW.M.latest || {}, name = store.get("chat.name", "");
    const now = new Date().toISOString().slice(0, 16) + "Z";
    const opts = nat.subjects.map((s) => `<option value="${esc(s.name)}">${esc([s.english !== s.name ? s.english : "", s.inuktitut, s.french].filter(Boolean).join(" · "))}${s.kind ? ` (${esc(s.kind)}${s.domain ? ", " + esc(s.domain) : ""})` : ""}</option>`).join("");
    return `<form id="natform" autocomplete="off">
      <label class="wide">Subject <span class="muted">any of its names: muskox, umingmak, Ovibos moschatus</span><input name="subject" list="natsubjects" required placeholder="what was seen, measured, sounded, collected"><datalist id="natsubjects">${opts}</datalist><span class="hint muted small" id="natsubhint"></span></label>
      <label>When (UTC)<input name="date" value="${esc(now)}" required pattern="\\d{4}-\\d{2}-\\d{2}(T\\d{2}:\\d{2}(:\\d{2})?Z?)?" title="2026-09-11T14:22Z"><button type="button" class="chip small" data-fill="now">now</button></label>
      <label>Latitude<input name="lat" type="number" step="0.0001" min="-90" max="90" value="${pos.lat != null ? (+pos.lat).toFixed(4) : ""}" required></label>
      <label>Longitude<input name="lon" type="number" step="0.0001" min="-180" max="180" value="${pos.lon != null ? (+pos.lon).toFixed(4) : ""}" required><button type="button" class="chip small" data-fill="ship" title="the ship's position now">ship</button></label>
      <label>Count <span class="muted">for living things</span><input name="count" placeholder="about 200 · 2 · a herd · present"></label>
      <label>Value<input name="value" type="number" step="any" placeholder="the number as read"></label>
      <label>Unit <span class="muted">as written</span><input name="unit" placeholder="C · m · fathoms · hPa · deg"></label>
      <label>Qualifier<input name="qualifier" placeholder="first of the season · estimated"></label>
      <label>Method<select name="method">${METHODS.map((m) => `<option ${m === "sighting" ? "selected" : ""}>${m}</option>`).join("")}</select></label>
      <label>Observer<input name="observer" value="${esc(name)}" placeholder="who saw it, or the watch" required></label>
      <label>Vessel<input name="vessel" value="CCGS Amundsen"></label>
      <label class="wide">Detail <span class="muted">the sentence that says what was seen, naming the observer</span><textarea name="detail" rows="2" required maxlength="2000"></textarea></label>
      <label class="row"><input type="checkbox" name="sensitive"> sensitive site <span class="muted">a den, a nest, a haul-out, a calving ground: published coarsened</span></label>
      <label>Photograph<input name="image" type="file" accept="image/jpeg,image/png,image/webp"></label>
      <label>Its licence<select name="licence"><option value="attribution">attribution, the photographer credited</option><option value="cc-by-4.0">CC BY 4.0</option></select></label>
      <div class="wide"><button type="submit" class="go">Add to the journal</button> <span class="muted small" id="natformmsg"></span></div>
    </form>`;
  }
  function wireJournal(el) {
    const add = el.querySelector("#natnew"); if (add) add.onclick = () => open("journal");
    const form = el.querySelector("#natform"); if (!form) return;
    const msg = form.querySelector("#natformmsg"), hint = form.querySelector("#natsubhint");
    const sub = form.elements.subject;
    sub.oninput = () => { const s = subjectOf(sub.value); hint.textContent = s ? `${displayName(s)} · ${s.kind}${s.domain ? ", " + s.domain : ""}` : sub.value.trim() && nat.subjects.length ? "not in the published list: grid will ask before it is entered" : ""; };
    for (const b of form.querySelectorAll("button[data-fill]")) b.onclick = () => {
      if (b.dataset.fill === "now") form.elements.date.value = new Date().toISOString().slice(0, 16) + "Z";
      else { const p = UW.M.latest || {}; if (p.lat != null) { form.elements.lat.value = (+p.lat).toFixed(4); form.elements.lon.value = (+p.lon).toFixed(4); } }
    };
    form.onsubmit = async (ev) => {
      ev.preventDefault();
      const f = form.elements, s = subjectOf(f.subject.value);
      const entry = { kind: "observation", subject: s ? s.name : f.subject.value.trim(), date: f.date.value.trim(), lat: +f.lat.value, lon: +f.lon.value,
        count: f.count.value.trim(), value: f.value.value === "" ? null : +f.value.value, unit: f.unit.value.trim(), qualifier: f.qualifier.value.trim(),
        method: f.method.value, observer: f.observer.value.trim(), vessel: f.vessel.value.trim(), detail: f.detail.value.trim(), sensitive: f.sensitive.checked ? 1 : 0,
        origin: "ship", token: store.get("chat.token", ""), name: store.get("chat.name", "") };
      const file = f.image.files?.[0];
      if (file) {
        if (file.size > 10 * 1024 * 1024) { msg.textContent = "the photograph is over 10 MB"; return; }
        entry.image = await new Promise((res, rej) => { const r = new FileReader(); r.onload = () => res(r.result); r.onerror = rej; r.readAsDataURL(file); });
        entry.licence = f.licence.value;
      }
      form.querySelector("button.go").disabled = true; msg.textContent = "writing…";
      try {
        const r = await fetch("/api/nature/journal", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(entry) });
        const j = await r.json();
        if (!r.ok) throw new Error(j.error || r.status);
        msg.textContent = `written as ${j.entry.id}`;
        if (s) store.set("chat.name", entry.observer || store.get("chat.name", ""));
        await loadJournal();
        renderMain(); if (UW.state.nature) UW.renderMap();
        UW.toast?.(`Journalled: ${entry.subject}`);
      } catch (e) { msg.textContent = `not written: ${e.message || e}`; form.querySelector("button.go").disabled = false; }
    };
  }

  // ---------------------------------------------------------------- the chips
  // two rows in the pane's tools: the document kinds (as on the History tab,
  // each a page of the natural topics' documents) and the domains
  function chipsHTML(onOf, cls = "") {
    return Object.entries(DOMAINS).map(([d, D]) => `<button type="button" data-d="${d}" class="${cls} ${onOf(d) ? "on" : ""}" title="${esc(D.hint)}"><span class="dot" style="background:${D.colour}"></span>${esc(D.label)}</button>`).join("");
  }
  function renderChips() {
    const docs = $("#natdocs");
    if (docs) {
      docs.innerHTML = H.kindChips((k) => nat.slug === `kind/${k}`);
      for (const b of docs.querySelectorAll("button[data-t]")) b.onclick = () => open(nat.slug === `kind/${b.dataset.t}` ? "" : `kind/${b.dataset.t}`);
    }
    const pane = $("#natkinds");
    if (pane) {
      pane.innerHTML = chipsHTML((d) => nat.slug === `domain/${d}`, "chip");
      for (const b of pane.querySelectorAll("button[data-d]")) b.onclick = () => open(nat.slug === `domain/${b.dataset.d}` ? "" : `domain/${b.dataset.d}`);
    }
    const sel = $("#natkindsel");
    if (sel) {
      const opt = (slug, label) => `<option value="${slug}" ${nat.slug === slug ? "selected" : ""}>${esc(label)}</option>`;
      sel.innerHTML = `<option value="" ${!nat.slug || !/^(domain\/|kind\/|subjects\/|explore$|record$|journal$)/.test(nat.slug) ? "selected" : ""}>Browse…</option>` +
        H.kindOptions(nat.slug) +
        `<optgroup label="Domains">${Object.entries(DOMAINS).map(([d, D]) => opt(`domain/${d}`, D.label)).join("")}</optgroup>` +
        `<optgroup label="Pages">${opt("explore", "Explore")}${opt("record", "The record")}${opt("journal", "The ship's journal")}</optgroup>`;
      sel.onchange = () => open(sel.value);
    }
    const bar = $("#mapnatlayers");
    if (bar) {
      bar.innerHTML = chipsHTML((d) => nat.domains.has(d));
      for (const b of bar.querySelectorAll("button[data-d]")) b.onclick = () => {
        if (nat.domains.has(b.dataset.d)) nat.domains.delete(b.dataset.d); else nat.domains.add(b.dataset.d);
        store.set("nat.domains", [...nat.domains]); renderChips(); if (UW.state.nature) UW.renderMap();
      };
      bar.hidden = !UW.state.nature || !UW.M.history;
    }
  }
  function renderTools() {
    $("#nathome").classList.toggle("on", !nat.slug && !nat.search);
    $("#natexplore").classList.toggle("on", nat.slug === "explore");
    $("#natrecordchip").classList.toggle("on", nat.slug === "record");
    $("#natjournalchip").classList.toggle("on", nat.slug === "journal");
    $("#natback").disabled = !nat.slug && nav.n === 0;
    const mt = $("#maptoggle"); if (mt) $("#natmap").textContent = mt.textContent;
  }
  async function render() {
    renderChips(); renderTools();
    await renderMain();
    if (UW.state.nature) UW.renderMap();
  }

  // ---------------------------------------------------------------- navigation
  const nav = { n: 0 };
  function open(slug, opts = {}) {
    nat.slug = slug; store.set("nat.slug", slug);
    nat.more = { today: false, here: false };
    if (nat.search) { nat.search = ""; const q = $("#natsearch"); if (q) q.value = ""; }
    if (!opts.pop) { nav.n++; try { history.pushState({ nat: slug, n: nav.n }, "", `#nature/${slug}`); } catch {} }
    if (!opts.quiet && $("#pane-nature").hidden) UW.showTab("nature");
    render();
  }
  window.addEventListener("popstate", (e) => {
    const s = e.state?.nat ?? (location.hash.startsWith("#nature/") ? decodeURIComponent(location.hash.slice(8)) : null);
    if (s == null) { if (!$("#pane-nature").hidden && !location.hash.startsWith("#history/")) { nav.n = 0; nat.slug = ""; store.set("nat.slug", ""); render(); } return; }
    nav.n = e.state?.n ?? 0;
    open(s, { pop: true });
  });
  function goBack() {
    if (nav.n > 0 && history.state?.nat != null) history.back();
    else open("");
  }
  function focusPoint(lat, lon, label) {
    if (lat == null) return;
    if (!UW.state.nature) { UW.state.nature = true; store.set("nature", true); document.querySelector('#maplayers button[data-layer="nature"]')?.classList.add("on"); renderChips(); }
    UW.focusMap(+lat, +lon, label || "");
    if (UW.mapMode?.() === "none") UW.setMapMode("half");
  }
  // a link in this pane opens here when its page is the natural half's, on the History tab otherwise
  const openPage = (slug) => { if (!slug || H.pageDomain(slug) === "nature") open(slug); else UW.historyOpen?.(slug); };
  document.addEventListener("click", (e) => {
    if (e.target.closest(".flag[data-flag], a[href^='#kw-']")) return;                   // history.js handles the flags and the keyword nav
    const pin = e.target.closest("#pane-nature .pin[data-lat]");
    if (pin) { e.preventDefault(); e.stopPropagation(); focusPoint(pin.dataset.lat, pin.dataset.lon, pin.dataset.label); return; }
    const a = e.target.closest("#pane-nature a[data-nslug], #pane-nature a[data-slug], #pane-nature a[data-topic]");
    if (!a) return;
    e.preventDefault();
    if (a.classList.contains("vig") && a.dataset.lat) focusPoint(a.dataset.lat, a.dataset.lon, a.querySelector(".txt")?.textContent || "");
    if (a.dataset.nslug != null) open(a.dataset.nslug);
    else if (a.dataset.topic != null) openPage(`topic/${a.dataset.topic}`);
    else openPage(a.dataset.slug || "");
  });
  UW.onNatureClick = (id, pt) => {
    if (pt && pt.lat != null) UW.state.focus = { lat: +pt.lat, lon: +pt.lon, label: String(pt.text || "").replace(/<br>.*$/s, "").replace(/<[^>]+>/g, "") };
    open(`observation/${id}`);
  };

  // ---------------------------------------------------------------- the map layer
  // the observations; the natural topics' artifacts and places come from
  // history.js, which draws each past layer's own domain
  const prevExtra = UW.extraMapTraces;
  UW.extraMapTraces = () => {
    const out = prevExtra ? prevExtra() : [];
    if (!UW.state.nature || !UW.M.history) return out;
    if (!nat.subjects) { ensure().then(() => UW.renderMap()); return out; }
    const pts = shownObs(true).filter((o) => o.lat != null);
    if (!pts.length) return out;
    const hover = (o) => { const s = subjectOf(o.subject); return `${esc(short(s ? shortName(s) : o.subject, 50))}${numberOf(o) ? " · " + esc(numberOf(o)) : ""}<br>${esc(o.date_text || H.dateLabel(o.date_start) || (o.date || ""))}${o.observer ? " · " + esc(short(o.observer, 40)) : ""}${o._journal ? "<br>the ship's journal" : ""}`; };
    out.push({ type: "scattermap", mode: "markers", name: "nature", showlegend: false, hoverinfo: "text",
      lat: pts.map((o) => o.lat), lon: pts.map((o) => o.lon), text: pts.map(hover), customdata: pts.map((o) => `nat:${o.id}`),
      marker: { size: pts.map((o) => o._journal ? 12 : 9), color: pts.map((o) => domainOf(domainOfObs(o)).colour), opacity: .92 } });
    // the ship's own lines ring their marks until grid has them
    const mine = pts.filter((o) => o._journal);
    if (mine.length) out.push({ type: "scattermap", mode: "markers", name: "nature-journal", showlegend: false, hoverinfo: "skip",
      lat: mine.map((o) => o.lat), lon: mine.map((o) => o.lon), marker: { size: 18, color: C.accent2, opacity: .35 } });
    return out;
  };

  // ---------------------------------------------------------------- Doc
  // the page being read, for Doc's context in the chat: a subject or an observation
  // the page being read, for Doc's context in the chat: a subject, an observation or a document
  UW.natureContext = () => (nat.slug && !["explore", "record", "journal"].includes(nat.slug) && !/^(topic|kind|subjects|domain|at)\//.test(nat.slug)) ? nat.slug : "";
  UW.natureOpen = (slug) => open(slug);

  // ---------------------------------------------------------------- wiring
  function wire() {
    $("#natsearch").oninput = debounce((e) => { nat.search = e.target.value; renderMain(); }, 150);
    $("#natback").onclick = goBack;
    $("#nathome").onclick = () => open("");
    $("#natexplore").onclick = () => open(nat.slug === "explore" ? "" : "explore");
    $("#natrecordchip").onclick = () => open(nat.slug === "record" ? "" : "record");
    $("#natjournalchip").onclick = () => open(nat.slug === "journal" ? "" : "journal");
    $("#natask").onclick = () => UW.chatRoom?.("doc");
    const mt = $("#maptoggle");
    if (mt) { $("#natmap").onclick = () => mt.click(); new MutationObserver(() => { $("#natmap").textContent = mt.textContent; }).observe(mt, { childList: true, characterData: true, subtree: true }); }
    const pill = document.querySelector('#maplayers button[data-layer="nature"]');
    if (pill) { pill.hidden = !UW.M.history; pill.addEventListener("click", () => setTimeout(renderChips, 0)); }
    if (location.hash.startsWith("#nature/")) { nat.slug = decodeURIComponent(location.hash.slice(8)); store.set("nat.slug", nat.slug); try { history.replaceState({ nat: nat.slug, n: 0 }, "", location.hash); } catch {} }
  }
  const prevTab = UW.onTab;
  UW.onTab = (name) => {
    prevTab?.(name);
    if (name !== "nature") return;
    ensure().then((ok) => { if (ok) render(); else renderMain(); }).catch(() => { UW.setLoadError("Nature", true); });
  };
  const prevRefresh = UW.refreshExtraData;
  UW.refreshExtraData = () => { prevRefresh?.(); const pill = document.querySelector('#maplayers button[data-layer="nature"]'); if (pill) pill.hidden = !UW.M.history;
    if (!$("#pane-nature").hidden || UW.state.nature) ensure().then(() => { if (!$("#pane-nature").hidden) render(); else { renderChips(); UW.renderMap(); } }).catch(() => {}); };
  wire();
  document.addEventListener("uw:theme", () => { if (!$("#pane-nature").hidden && nat.subjects) render(); });
  if (location.hash.startsWith("#nature/") && $("#pane-nature").hidden) UW.showTab("nature");
  else if (document.querySelector("#tabs button.on")?.dataset.tab === "nature") UW.onTab("nature");
  else if (UW.state.nature && UW.M.history) ensure().then(() => { renderChips(); UW.renderMap(); }).catch(() => {});
})();
