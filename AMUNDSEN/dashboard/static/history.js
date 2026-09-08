/* The History tab: the region's past on the map, in a wiki, on a timeline,
 * with a historian to ask. Everything it shows was published by the build from
 * the arctic-history database into data/history/: an index of pages and
 * topics, the artifacts with their positions and tracks, the atomic dates,
 * and one JSON file per wiki page. Loaded after tabs.js; talks to app.js
 * through window.UW like the other panes. */
(() => {
  "use strict";
  const UW = window.UW;
  const $ = (s) => document.querySelector(s);
  const { THEME, CFG, store } = UW;
  const esc = (s) => String(s ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  const cachedJSON = window.UWData.generationCache(UW.fetchJSON, () => UW.M.history?.stamp || "");
  const debounce = (f, ms) => { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => f(...a), ms); }; };

  // artifact kinds: a symbol and a colour each, for the map and the chips
  const TYPES = {
    track: { label: "Tracks", colour: "#ffb454", symbol: "circle" },
    event: { label: "Events", colour: "#ff7b72", symbol: "circle" },
    place: { label: "Places", colour: "#7ee787", symbol: "circle" },
    image: { label: "Images", colour: "#5cc8ff", symbol: "circle" },
    map:   { label: "Maps", colour: "#79c0ff", symbol: "circle" },
    quote: { label: "Quotes", colour: "#d2a8ff", symbol: "circle" },
    text:  { label: "Texts", colour: "#a5d6ff", symbol: "circle" },
    object: { label: "Objects", colour: "#f2cc60", symbol: "circle" },
  };
  const TOPIC_COLOURS = ["#ffb454", "#5cc8ff", "#7ee787", "#ff7b72", "#d2a8ff", "#f2cc60", "#79c0ff", "#ffa198", "#56d364", "#e3b341", "#a5d6ff", "#ff9bce"];

  const hist = {
    index: null, artifacts: null, timeline: null, stamp: null, loading: null,
    slug: store.get("hist.slug", ""),                     // the page being read; "" is the home
    topic: store.get("hist.topic", ""),
    types: new Set(store.get("hist.types", Object.keys(TYPES))),
    from: store.get("hist.from", ""), to: store.get("hist.to", ""),
    search: "",
    timelineOn: store.get("hist.timeline", false),
    sideOn: store.get("hist.side", false),                 // the topics column, shown only when asked for
    pages: new Map(),                                     // slug -> page JSON, this generation
    trail: [],                                            // slugs visited, for back links
  };

  // ---------------------------------------------------------------- data
  async function ensure() {
    if (!UW.M.history) { hist.index = null; return false; }
    if (hist.stamp === UW.M.history.stamp && hist.index) return true;
    if (hist.loading) return hist.loading;
    hist.loading = (async () => {
      // places and people are newer exports; a build without them still works
      const maybe = (k, u) => cachedJSON(k, u).catch(() => null);
      const [index, arts, tl, pl, pe] = await Promise.all([
        cachedJSON("index", "data/history/index.json"),
        cachedJSON("artifacts", "data/history/artifacts.json"),
        cachedJSON("timeline", "data/history/timeline.json"),
        maybe("places", "data/history/places.json"),
        maybe("people", "data/history/people.json"),
      ]);
      hist.index = index; hist.artifacts = arts.artifacts || []; hist.timeline = tl.timeline || [];
      hist.places = pl?.places || []; hist.people = pe?.people || [];
      hist.stamp = UW.M.history.stamp; hist.pages = new Map();
      for (const a of hist.artifacts) a._year = yearOf(a.date_start);
      return true;
    })().finally(() => { hist.loading = null; });
    return hist.loading;
  }
  async function page(slug) {
    if (hist.pages.has(slug)) return hist.pages.get(slug);
    const p = await cachedJSON(`page:${slug}`, `data/history/pages/${encodeURIComponent(slug.replace(/\//g, "__"))}.json`);
    hist.pages.set(slug, p);
    return p;
  }
  // a partial ISO date as a decimal year: "1845-09" -> 1845.67; "-2500" -> -2500
  function yearOf(s) {
    if (!s) return null;
    const m = /^(-?\d{1,4})(?:-(\d{1,2}))?(?:-(\d{1,2}))?$/.exec(s);
    if (!m) return null;
    return +m[1] + ((+m[2] || 1) - 1) / 12 + ((+m[3] || 1) - 1) / 365;
  }
  const topicOf = (slug) => hist.index?.topics.find((t) => t.slug === slug);
  const topicColour = (slug) => TOPIC_COLOURS[Math.max(0, hist.index?.topics.findIndex((t) => t.slug === slug) || 0) % TOPIC_COLOURS.length];
  const artifactById = (id) => hist.artifacts?.find((a) => a.id === id);

  // ---------------------------------------------------------------- vignettes
  // "On this day": every atomic date whose month and day are today's, in any
  // year, plus the voyages that were under way on this date (a span that
  // covers today's date in some year) and the track waypoints dated today.
  // "In this place": whatever lies within reach of the ship's position,
  // widening the radius until there is something to say.
  const R_EARTH = 6371;
  function km(lat1, lon1, lat2, lon2) {
    const r = Math.PI / 180, dLat = (lat2 - lat1) * r, dLon = (lon2 - lon1) * r;
    const a = Math.sin(dLat / 2) ** 2 + Math.cos(lat1 * r) * Math.cos(lat2 * r) * Math.sin(dLon / 2) ** 2;
    return 2 * R_EARTH * Math.asin(Math.sqrt(a));
  }
  const parts = (iso) => { const m = /^(-?\d{1,4})-(\d{2})-(\d{2})$/.exec(iso || ""); return m ? { y: +m[1], m: +m[2], d: +m[3] } : null; };
  const ymd = (y, m, d) => y * 10000 + m * 100 + d;
  // where a timeline row's page is: an artifact's own page, a person's or
  // place's by name, an event's topic (events have no page of their own)
  function pageFor(kind, id) {
    if (kind === "artifact") return artifactById(id)?.page || "";
    if (kind === "event") return "";
    return hist.index?.pages.find((p) => p.kind === kind && p.title === id)?.slug || "";
  }
  function onThisDay(now = new Date()) {
    if (!hist.timeline) return [];
    const mm = now.getMonth() + 1, dd = now.getDate(), out = [];
    for (const r of hist.timeline) {
      const a = parts(r.date);
      if (!a || r.precision !== "day") continue;
      if (!r.date_end) {
        if (a.m === mm && a.d === dd) out.push({ year: a.y, kind: r.role === "start" ? "began" : r.role === "end" ? "ended" : "", label: r.label, place: r.place, topic: r.topic, lat: r.lat, lon: r.lon, slug: pageFor(r.entity_kind, r.entity_id), ref: r });
        continue;
      }
      const b = parts(r.date_end); if (!b) continue;
      if (a.m === mm && a.d === dd) out.push({ year: a.y, kind: "began", label: r.label, place: r.place, topic: r.topic, lat: r.lat, lon: r.lon, slug: pageFor(r.entity_kind, r.entity_id), ref: r });
      if (b.m === mm && b.d === dd) out.push({ year: b.y, kind: "ended", label: r.label, place: r.place, topic: r.topic, lat: r.lat, lon: r.lon, slug: pageFor(r.entity_kind, r.entity_id), ref: r });
      // under way on this date: the span covers today's month and day in some year
      for (let y = a.y; y <= b.y && y - a.y < 40; y++) {
        const t = ymd(y, mm, dd);
        if (t > ymd(a.y, a.m, a.d) && t < ymd(b.y, b.m, b.d)) {
          const day = Math.round((Date.UTC(y, mm - 1, dd) - Date.UTC(a.y, a.m - 1, a.d)) / 864e5);
          out.push({ year: y, kind: `day ${day + 1} of ${Math.round((Date.UTC(b.y, b.m - 1, b.d) - Date.UTC(a.y, a.m - 1, a.d)) / 864e5) + 1}`, label: r.label, place: r.place, topic: r.topic, lat: r.lat, lon: r.lon, slug: pageFor(r.entity_kind, r.entity_id), ref: r });
        }
      }
    }
    // the ships: a track waypoint dated today says where a voyage was
    for (const a of hist.artifacts || []) {
      if (a.type !== "track" || !a.waypoints) continue;
      for (const w of a.waypoints) {
        const d = parts(w.date);
        if (d && d.m === mm && d.d === dd) out.push({ year: d.y, kind: "was here", label: a.title, place: w.note || "", topic: a.topic, lat: w.lat, lon: w.lon, slug: a.page, ref: a });
      }
    }
    // the people: born or died on this date, when the record gives the day
    for (const p of hist.people || []) {
      for (const [field, word] of [["born", "born"], ["died", "died"]]) {
        const d = parts(p[field]);
        if (d && d.m === mm && d.d === dd) out.push({ year: d.y, kind: word, label: p.name, place: p.role || "", topic: p.topic, lat: null, lon: null, slug: p.page, ref: p });
      }
    }
    const seen = new Set();
    return out.filter((x) => { const k = `${x.year}|${x.label}|${x.kind}`; if (seen.has(k)) return false; seen.add(k); return true; })
      .sort((p, q) => p.year - q.year);
  }
  function inThisPlace(lat, lon) {
    if (lat == null || lon == null || !hist.artifacts) return { radius: 0, items: [] };
    const cand = [];
    for (const a of hist.artifacts) {
      if (a.type === "track") {
        // the closest approach of the voyage: the nearest waypoint if it has
        // dates, else the nearest vertex of the line
        let best = null;
        for (const w of (a.waypoints?.length ? a.waypoints : (a.geometry?.coordinates || []).map((c) => ({ lat: c[1], lon: c[0] })))) {
          if (w.lat == null) continue;
          const d = km(lat, lon, w.lat, w.lon);
          if (!best || d < best.d) best = { d, w };
        }
        if (best) cand.push({ d: best.d, a, when: best.w.date || a.date_text, note: best.w.note || "", lat: best.w.lat, lon: best.w.lon });
      } else if (a.lat != null) {
        cand.push({ d: km(lat, lon, a.lat, a.lon), a, when: a.date_text, note: "", lat: a.lat, lon: a.lon });
      }
    }
    for (const e of hist.timeline || []) {
      if (e.lat == null || e.entity_kind !== "event") continue;
      cand.push({ d: km(lat, lon, e.lat, e.lon), e, when: e.date + (e.date_end ? " → " + e.date_end : ""), note: e.place || "", lat: e.lat, lon: e.lon });
    }
    for (const p of hist.places || []) {
      if (p.lat == null) continue;
      const names = [p.inuktitut, p.historic].filter((n) => n && n !== p.name).join(", ");
      cand.push({ d: km(lat, lon, p.lat, p.lon), p, when: p.kind || "place", note: names, lat: p.lat, lon: p.lon });
    }
    cand.sort((p, q) => p.d - q.d);
    let radius = 50;
    while (radius < 400 && cand.filter((c) => c.d <= radius).length < 6) radius *= 2;
    const seen = new Set();
    const items = cand.filter((c) => c.d <= radius).filter((c) => { const k = c.a ? c.a.id : c.p ? `pl:${c.p.name}` : `ev:${c.e.entity_id}`; if (seen.has(k)) return false; seen.add(k); return true; }).slice(0, 14)
      .map((c) => c.a ? { d: c.d, label: c.a.title, when: c.when, note: c.note, topic: c.a.topic, type: c.a.type, slug: c.a.page, lat: c.lat, lon: c.lon }
                : c.p ? { d: c.d, label: c.p.name, when: c.when, note: c.note, topic: c.p.topic, type: "place", slug: c.p.page, lat: c.lat, lon: c.lon }
                      : { d: c.d, label: c.e.label, when: c.when, note: c.note, topic: c.e.topic, type: "event", slug: "", lat: c.lat, lon: c.lon });
    return { radius, items };
  }
  UW.historyVignettes = () => ({ today: onThisDay(), here: inThisPlace(UW.M.latest?.lat, UW.M.latest?.lon) });

  function vignetteHTML() {
    const now = new Date();
    const today = onThisDay(now);
    const pos = UW.M.latest || {};
    const here = inThisPlace(pos.lat, pos.lon);
    const dateWord = now.toLocaleDateString(undefined, { month: "long", day: "numeric" });
    const item = (x, extra) => `<a class="vig" href="#history/${esc(x.slug)}" data-slug="${esc(x.slug)}" data-lat="${x.lat ?? ""}" data-lon="${x.lon ?? ""}" data-topic="${esc(x.topic)}"><span class="dot" style="background:${topicColour(x.topic)}"></span>${extra}<span class="txt">${esc(x.label)}${x.kind ? ` <i>${esc(x.kind)}</i>` : x.type === "place" && x.when ? ` <i>${esc(x.when)}</i>` : ""}${x.note ? ` <span class="muted">${esc(x.note)}</span>` : ""}${x.place && !x.note ? ` <span class="muted">${esc(x.place)}</span>` : ""}</span></a>`;
    return `<div class="vignettes">
      <section class="vigcard"><h3>On this day · ${esc(dateWord)}</h3>${today.length ? today.slice(0, 14).map((x) => item(x, `<b>${x.year}</b>`)).join("") : `<div class="muted small">Nothing dated to the day on ${esc(dateWord)} yet. The crew's atomic dates fill this in as the topics are written.</div>`}</section>
      <section class="vigcard"><h3>In this place${here.radius ? ` · within ${here.radius} km` : ""}</h3>${here.items.length ? here.items.map((x) => item(x, `<b>${Math.round(x.d)} km</b>`)).join("") : `<div class="muted small">${pos.lat == null ? "The ship's position is not known to this build." : "Nothing in the history within 400 km of the ship yet."}</div>`}</section>
    </div>`;
  }

  // the artifacts the current filters allow: topic, kinds, years
  function shownArtifacts() {
    if (!hist.artifacts) return [];
    const from = hist.from === "" ? -Infinity : +hist.from, to = hist.to === "" ? Infinity : +hist.to;
    return hist.artifacts.filter((a) => (!hist.topic || a.topic === hist.topic) && hist.types.has(a.type)
      && (a._year == null || (a._year >= from && a._year <= to + 1)));
  }

  // ---------------------------------------------------------------- markdown
  // The pages come as Markdown with the wikilinks already turned into plain
  // links whose target is a page slug. A small renderer is enough: headings,
  // paragraphs, emphasis, lists, quotes, tables, links and images.
  function inline(s) {
    s = esc(s);
    s = s.replace(/!\[([^\]]*)\]\(([^)\s]+)\)/g, (m, alt, src) => `<img src="${src}" alt="${alt}" loading="lazy">`);
    s = s.replace(/\[([^\]]+)\]\(([^)\s]+)\)/g, (m, label, href) =>
      /^https?:\/\//.test(href) ? `<a href="${href}" target="_blank" rel="noopener">${label}</a>` : `<a href="#history/${href}" data-slug="${href}">${label}</a>`);
    s = s.replace(/&lt;span class=&quot;wanted&quot; title=&quot;no page yet&quot;&gt;(.*?)&lt;\/span&gt;/g, '<span class="wanted" title="no page yet">$1</span>');
    s = s.replace(/`([^`]+)`/g, "<code>$1</code>").replace(/\*\*([^*]+)\*\*/g, "<b>$1</b>").replace(/\*([^*]+)\*/g, "<i>$1</i>");
    return s;
  }
  function markdown(md) {
    const lines = String(md || "").replace(/\r/g, "").split("\n");
    const out = []; let para = [], list = null, quote = [], table = null;
    const flush = () => {
      if (para.length) { out.push(`<p>${inline(para.join(" "))}</p>`); para = []; }
      if (list) { out.push(`<${list.tag}>${list.items.map((i) => `<li>${inline(i)}</li>`).join("")}</${list.tag}>`); list = null; }
      if (quote.length) { out.push(`<blockquote>${inline(quote.join(" "))}</blockquote>`); quote = []; }
      if (table) { out.push(`<div class="hscroll"><table>${table.map((r, i) => `<tr>${r.map((c) => `<${i ? "td" : "th"}>${inline(c)}</${i ? "td" : "th"}>`).join("")}</tr>`).join("")}</table></div>`); table = null; }
    };
    for (const raw of lines) {
      const line = raw.trimEnd();
      let m;
      if (!line.trim()) { flush(); continue; }
      if ((m = /^(#{1,4})\s+(.*)$/.exec(line))) { flush(); const h = Math.min(4, m[1].length + 1); out.push(`<h${h}>${inline(m[2])}</h${h}>`); continue; }
      if (/^(-{3,}|\*{3,})$/.test(line)) { flush(); out.push("<hr>"); continue; }
      if ((m = /^\s*[-*]\s+(.*)$/.exec(line))) { if (para.length || quote.length || table) flush(); if (!list || list.tag !== "ul") { flush(); list = { tag: "ul", items: [] }; } list.items.push(m[1]); continue; }
      if ((m = /^\s*\d+[.)]\s+(.*)$/.exec(line))) { if (para.length || quote.length || table) flush(); if (!list || list.tag !== "ol") { flush(); list = { tag: "ol", items: [] }; } list.items.push(m[1]); continue; }
      if ((m = /^>\s?(.*)$/.exec(line))) { if (para.length || list || table) flush(); quote.push(m[1]); continue; }
      if (/^\|.*\|$/.test(line)) {
        if (para.length || list || quote.length) flush();
        const cells = line.slice(1, -1).split("|").map((c) => c.trim());
        if (cells.every((c) => /^:?-+:?$/.test(c))) continue;          // the header rule
        (table ||= []).push(cells); continue;
      }
      if (list || quote.length || table) flush();
      para.push(line.trim());
    }
    flush();
    return out.join("\n");
  }

  // ---------------------------------------------------------------- the pane
  const fmtDate = (a) => a.date_text || a.date_start || "";
  function artifactCard(a, opts = {}) {
    const t = TYPES[a.type] || {};
    const img = a.url && (a.type === "image" || a.type === "map") ? `<img class="thumb" src="${esc(a.url)}" alt="" loading="lazy">` : "";
    return `<a class="artcard ${esc(a.type)}" href="#history/${esc(a.page)}" data-slug="${esc(a.page)}" title="${esc(a.title)}">${img}<span class="dot" style="background:${t.colour || "#8b9bb0"}"></span>` +
      `<span class="kind">${esc(a.type)}</span><b>${esc(a.title)}</b><span class="when">${esc(fmtDate(a))}</span>` +
      (opts.creator && a.creator ? `<span class="who">${esc(a.creator)}</span>` : "") + `</a>`;
  }
  function pageLink(p, cls = "") {
    return `<a class="pglink ${cls}" href="#history/${esc(p.slug)}" data-slug="${esc(p.slug)}"><span class="kind">${esc(p.kind)}</span>${esc(p.title)}${p.summary ? `<span class="sum">${esc(p.summary)}</span>` : ""}</a>`;
  }

  function renderSide() {
    const el = $("#histside"); if (!hist.index) { el.innerHTML = ""; return; }
    const q = hist.search.trim().toLowerCase();
    if (q) {
      const words = q.split(/\s+/).filter(Boolean);
      const hit = (s) => { const t = String(s || "").toLowerCase(); return words.every((w) => t.includes(w)); };
      const pages = hist.index.pages.filter((p) => (!hist.topic || p.topic === hist.topic || !p.topic) && (hit(p.title) || hit(p.summary)))
        .sort((a, b) => (hit(b.title) - hit(a.title)) || (a.kind === "page" ? -1 : 1));
      const arts = shownArtifacts().filter((a) => hit(a.title) || hit(a.description) || hit((a.people || []).join(" ")) || hit((a.tags || []).join(" ")));
      el.innerHTML = `<div class="sidehead">${pages.length} pages · ${arts.length} artifacts</div>` +
        pages.slice(0, 60).map((p) => pageLink(p)).join("") +
        (arts.length ? `<div class="sidehead">Artifacts</div>` + arts.slice(0, 60).map((a) => artifactCard(a)).join("") : "");
      return;
    }
    const topics = hist.index.topics.filter((t) => !hist.topic || t.slug === hist.topic);
    el.innerHTML = `<div class="sidehead">Topics</div>` + topics.map((t) =>
      `<a class="topic ${t.slug === hist.topic ? "on" : ""}" href="#history/topic/${esc(t.slug)}" data-topic="${esc(t.slug)}"><span class="dot" style="background:${topicColour(t.slug)}"></span><b>${esc(t.title)}</b>` +
      `<span class="counts">${t.pages} pages · ${t.artifacts} artifacts${t.status && t.status !== "open" ? " · " + esc(t.status) : ""}</span></a>`).join("");
    if (hist.topic) {
      const pages = hist.index.pages.filter((p) => p.topic === hist.topic && p.kind === "page");
      const arts = shownArtifacts().filter((a) => a.topic === hist.topic);
      el.innerHTML += `<div class="sidehead">Narrative</div>` + (pages.map((p) => pageLink(p, p.slug === hist.slug ? "on" : "")).join("") || `<div class="muted small">no pages written yet</div>`) +
        `<div class="sidehead">Artifacts (${arts.length})</div>` + arts.map((a) => artifactCard(a)).join("");
    }
  }

  async function renderMain() {
    const el = $("#histmain");
    if (!UW.M.history) { el.innerHTML = `<div class="empty">No history has been published yet. The research crew's database is pulled with <code>tools/history-sync.sh pull</code> and the next build renders it here.</div>`; return; }
    if (!hist.index) { el.innerHTML = `<div class="empty">Loading the history…</div>`; return; }
    if (!hist.slug) {                                              // the home: every topic, and the recent additions
      const t = hist.index.topics;
      const n = (k) => t.reduce((s, x) => s + (x[k] || 0), 0);
      el.innerHTML = vignetteHTML() + `<h2>The history of these waters</h2>
        <p class="lead">${t.length} topics, ${n("pages")} narrative pages and ${hist.artifacts.length} artifacts, from the Tuniit to the ships of the last century: voyages as tracks, winterings and besetments as spans on the timeline, people and places as pages that link to one another, every item credited and sourced. Pick a topic on the left, search, or ask the historian below.</p>
        <div class="topicgrid">${t.map((x) => `<a class="topiccard" href="#history/topic/${esc(x.slug)}" data-topic="${esc(x.slug)}" style="border-left-color:${topicColour(x.slug)}"><b>${esc(x.title)}</b><span>${esc(x.summary)}</span><span class="counts">${x.pages} pages · ${x.artifacts} artifacts</span></a>`).join("")}</div>`;
      return;
    }
    if (hist.slug.startsWith("topic/")) {
      const t = topicOf(hist.slug.slice(6));
      if (!t) { el.innerHTML = `<div class="empty">no such topic</div>`; return; }
      const pages = hist.index.pages.filter((p) => p.topic === t.slug && p.kind === "page");
      const arts = hist.artifacts.filter((a) => a.topic === t.slug);
      const byType = {}; for (const a of arts) (byType[a.type] ||= []).push(a);
      el.innerHTML = `<h2>${esc(t.title)}</h2><p class="lead">${esc(t.summary)}</p>` +
        (pages.length ? `<div class="pagelist">${pages.map((p) => pageLink(p)).join("")}</div>` : `<p class="muted">No narrative pages yet; the artifacts below are what the crew has entered so far.</p>`) +
        Object.entries(byType).map(([k, xs]) => `<h3>${esc(TYPES[k]?.label || k)} <span class="muted">${xs.length}</span></h3><div class="artgrid">${xs.map((a) => artifactCard(a, { creator: true })).join("")}</div>`).join("");
      return;
    }
    let p;
    try { p = await page(hist.slug); }
    catch { el.innerHTML = `<div class="empty">That page is not in this build.</div>`; return; }
    const t = topicOf(p.topic);
    const a = p.kind === "artifact" ? artifactById(p.ref) : null;
    const back = (p.backlinks || []).map((s) => hist.index.pages.find((x) => x.slug === s)).filter(Boolean);
    let head = `<div class="crumb"><a href="#history/" data-slug="">History</a>${t ? ` › <a href="#history/topic/${esc(t.slug)}" data-topic="${esc(t.slug)}">${esc(t.title)}</a>` : ""} › <span class="kind">${esc(p.kind)}</span></div><h2>${esc(p.title)}</h2>`;
    if (p.summary && p.kind === "page") head += `<p class="lead">${esc(p.summary)}</p>`;
    let media = "";
    if (a) {
      if (a.url && (a.type === "image" || a.type === "map")) media = `<figure><a href="${esc(a.url)}" target="_blank" rel="noopener"><img src="${esc(a.url)}" alt="${esc(a.title)}"></a><figcaption>${esc(a.credit)}${a.licence ? " · " + esc(a.licence) : ""}</figcaption></figure>`;
      else if (a.url && a.type === "text") media = `<p><a class="chip" href="${esc(a.url)}" target="_blank" rel="noopener">open the full text</a></p>`;
      const where = a.lat != null ? `<button type="button" class="chip" id="histfocus">show on the map</button>` : "";
      const track = a.type === "track" && a.waypoints?.length ? `<div class="hscroll"><table class="waypoints"><tr><th>date</th><th>position</th><th>note</th></tr>${a.waypoints.map((w) => `<tr><td>${esc(w.date || "")}</td><td class="mono">${w.lat != null ? (+w.lat).toFixed(2) + ", " + (+w.lon).toFixed(2) : ""}</td><td>${esc(w.note || "")}</td></tr>`).join("")}</table></div>` : "";
      head += `<div class="artmeta"><span class="dot" style="background:${TYPES[a.type]?.colour || "#8b9bb0"}"></span>${esc(a.type)} · ${esc(fmtDate(a))}${a.creator ? " · " + esc(a.creator) : ""} ${where}</div>`;
      media += track;
    }
    el.innerHTML = head + media + `<div class="wiki">${markdown(p.html)}</div>` +
      (back.length ? `<div class="backlinks"><span class="lbl">Mentioned in</span>${back.map((b) => `<a href="#history/${esc(b.slug)}" data-slug="${esc(b.slug)}">${esc(b.title)}</a>`).join("")}</div>` : "");
    $("#histfocus")?.addEventListener("click", () => focusArtifact(a));
    el.scrollTop = 0;
  }

  function renderMeta() {
    const m = $("#histmeta"); if (!hist.index) { m.textContent = ""; return; }
    const shown = shownArtifacts();
    m.textContent = `${shown.length} of ${hist.artifacts.length} artifacts shown${hist.index.wanted && Object.keys(hist.index.wanted).length ? ` · ${Object.keys(hist.index.wanted).length} pages still wanted` : ""}`;
    $("#histmap").classList.toggle("on", !!UW.state.history);
    $("#histtl").classList.toggle("on", hist.timelineOn);
    $("#histtimeline").hidden = !hist.timelineOn;
  }
  function renderTypes() {
    const el = $("#histtypes");
    el.innerHTML = `<button type="button" id="histside-btn" class="${hist.sideOn ? "on" : ""}" title="the topics column: the topics, their pages and artifacts">Topics</button>` +
      Object.entries(TYPES).map(([k, t]) => `<button type="button" data-t="${k}" class="${hist.types.has(k) ? "on" : ""}" title="${t.label}"><span class="dot" style="background:${t.colour}"></span>${t.label}</button>`).join("");
    for (const b of el.querySelectorAll("button[data-t]")) b.onclick = () => {
      if (hist.types.has(b.dataset.t)) hist.types.delete(b.dataset.t); else hist.types.add(b.dataset.t);
      store.set("hist.types", [...hist.types]); render();
    };
    $("#histside-btn").onclick = () => { hist.sideOn = !hist.sideOn; store.set("hist.side", hist.sideOn); renderSideToggle(); renderTypes(); };
  }
  function renderSideToggle() {
    document.querySelector(".hist-layout")?.classList.toggle("noside", !hist.sideOn);
    $("#histside").hidden = !hist.sideOn;
  }
  function renderTopicSelect() {
    const sel = $("#histtopic"); if (!hist.index) return;
    const cur = sel.value;
    sel.innerHTML = `<option value="">all topics</option>` + hist.index.topics.map((t) => `<option value="${esc(t.slug)}">${esc(t.title)}</option>`).join("");
    sel.value = hist.topic || cur || "";
  }

  async function render() {
    renderTopicSelect(); renderTypes(); renderMeta(); renderSideToggle(); renderSide();
    await renderMain();
    if (hist.timelineOn) renderTimeline();
    if (UW.state.history) UW.renderMap();
  }

  // ---------------------------------------------------------------- navigation
  function open(slug, opts = {}) {
    if (slug !== hist.slug) hist.trail.push(hist.slug);
    hist.slug = slug; store.set("hist.slug", slug);
    if (slug.startsWith("topic/")) { hist.topic = slug.slice(6); store.set("hist.topic", hist.topic); }
    if (!opts.quiet && $("#pane-history").hidden) UW.showTab("history");
    render();
  }
  function focusArtifact(a) {
    if (!a || a.lat == null) return;
    if (!UW.state.history) { UW.state.history = true; store.set("history", true); document.querySelector('#maplayers button[data-layer="history"]')?.classList.add("on"); }
    if (a.type === "track" && a.geometry?.coordinates?.length) {
      const c = a.geometry.coordinates; const mid = c[Math.floor(c.length / 2)];
      UW.focusMap(mid[1], mid[0], a.title);
    } else UW.focusMap(a.lat, a.lon, a.title);
    if (UW.mapMode?.() === "none") UW.setMapMode("half");
  }
  // clicks in the pane: page links, topic links, artifact cards
  document.addEventListener("click", (e) => {
    const a = e.target.closest("#pane-history a[data-slug], #pane-history a[data-topic]");
    if (!a) return;
    e.preventDefault();
    if (a.classList.contains("vig")) {
      if (a.dataset.lat) { const art = a.dataset.slug ? artifactById(a.dataset.slug.replace(/^artifact\//, "")) : null; if (art) focusArtifact(art); else { if (!UW.state.history) { UW.state.history = true; store.set("history", true); document.querySelector('#maplayers button[data-layer="history"]')?.classList.add("on"); } UW.focusMap(+a.dataset.lat, +a.dataset.lon, a.querySelector(".txt")?.textContent || ""); } }
      if (a.dataset.slug) open(a.dataset.slug); else if (a.dataset.topic) { hist.topic = a.dataset.topic; store.set("hist.topic", hist.topic); open(`topic/${a.dataset.topic}`); }
      return;
    }
    if (a.dataset.topic != null) { hist.topic = a.dataset.topic; store.set("hist.topic", hist.topic); open(`topic/${a.dataset.topic}`); }
    else open(a.dataset.slug || "");
  });
  // a pin or a track on the map opens its page
  UW.onHistoryClick = (id) => {
    const a = artifactById(id.split("|")[0]);
    if (a) open(a.page);
  };

  // ---------------------------------------------------------------- the map layer
  const prevExtra = UW.extraMapTraces;
  UW.extraMapTraces = () => {
    const out = prevExtra ? prevExtra() : [];
    if (!UW.state.history || !hist.artifacts) { if (UW.state.history && !hist.artifacts) ensure().then(() => UW.renderMap()); return out; }
    const shown = shownArtifacts();
    // the tooltip is the title and the year; everything else goes to the info box
    const year = (a) => { const y = yearOf(a.date_start); return y == null ? "" : ` · ${Math.floor(y)}`; };
    const hover = (a) => `${esc(a.title.length > 60 ? a.title.slice(0, 57) + "…" : a.title)}${year(a)}`;
    // tracks: one line each, coloured by topic, with the dated waypoints as small markers
    for (const a of shown.filter((x) => x.type === "track" && x.geometry?.coordinates?.length > 1)) {
      const c = a.geometry.coordinates, col = topicColour(a.topic);
      out.push({ type: "scattermap", mode: "lines", name: `hist-${a.id}`, showlegend: false, hoverinfo: "text",
        lat: c.map((p) => p[1]), lon: c.map((p) => p[0]), text: c.map(() => hover(a)), customdata: c.map(() => `hist:${a.id}`),
        line: { width: 2.4, color: col }, opacity: .85 });
      if (a.waypoints?.length) out.push({ type: "scattermap", mode: "markers", name: `hist-${a.id}-wp`, showlegend: false, hoverinfo: "text",
        lat: a.waypoints.map((w) => w.lat), lon: a.waypoints.map((w) => w.lon),
        text: a.waypoints.map((w) => `${esc(w.date || "")}${w.note ? " · " + esc(w.note.length > 50 ? w.note.slice(0, 47) + "…" : w.note) : ""}`),
        customdata: a.waypoints.map((w) => `hist:${a.id}|${w.date || ""}`), marker: { size: 6, color: col, opacity: .9 } });
    }
    // everything else: a pin coloured by kind
    const pins = shown.filter((x) => x.type !== "track" && x.lat != null);
    if (pins.length) out.push({ type: "scattermap", mode: "markers", name: "history", showlegend: false, hoverinfo: "text",
      lat: pins.map((a) => a.lat), lon: pins.map((a) => a.lon), text: pins.map(hover), customdata: pins.map((a) => `hist:${a.id}`),
      marker: { size: pins.map((a) => a.type === "event" ? 11 : 9), color: pins.map((a) => TYPES[a.type]?.colour || "#8b9bb0"), opacity: .92 } });
    return out;
  };

  // ---------------------------------------------------------------- the info box
  // A tooltip that follows the cursor cannot hold a credit line. Hovering a
  // history pin or track fills a box fixed in the map's corner instead, which
  // stays until the next hover or its close; clicking opens the page.
  function infoBox() {
    let box = $("#mapinfo");
    if (!box) {
      box = document.createElement("div"); box.id = "mapinfo"; box.className = "mapinfo"; box.hidden = true;
      document.querySelector("section.map")?.appendChild(box);
      box.addEventListener("click", (e) => {
        if (e.target.closest(".x")) { box.hidden = true; return; }
        const a = e.target.closest("a[data-slug]"); if (a) { e.preventDefault(); open(a.dataset.slug); }
      });
    }
    return box;
  }
  const INFO_BOX = false;                                     // the box is off for now: too much for the map
  function showInfo(a, wp) {
    if (!INFO_BOX) return;
    const box = infoBox(); if (!a) return;
    const t = topicOf(a.topic);
    const img = a.url && (a.type === "image" || a.type === "map") ? `<img src="${esc(a.url)}" alt="" loading="lazy">` : "";
    box.innerHTML = `<button type="button" class="x" title="close">✕</button>${img}<div class="body"><span class="dot" style="background:${TYPES[a.type]?.colour || "#8b9bb0"}"></span><span class="kind">${esc(a.type)}</span>` +
      `<b><a href="#history/${esc(a.page)}" data-slug="${esc(a.page)}">${esc(a.title)}</a></b>` +
      `<div class="when">${esc(fmtDate(a))}${a.creator ? " · " + esc(a.creator) : ""}${t ? ` · <i>${esc(t.title)}</i>` : ""}</div>` +
      (wp ? `<div class="wp">${esc(wp.date || "")}${wp.note ? " · " + esc(wp.note) : ""}</div>` : "") +
      (a.description ? `<div class="desc">${esc(a.description.length > 220 ? a.description.slice(0, 217) + "…" : a.description)}</div>` : "") +
      (a.credit ? `<div class="credit">${esc(a.credit)}${a.licence ? " · " + esc(a.licence) : ""}</div>` : "") + `</div>`;
    box.hidden = false;
  }
  function wireHover() {
    if (!INFO_BOX) return;
    const el = $("#map"); if (!el || el._histHover) return;
    if (!el.on) { setTimeout(wireHover, 1500); return; }              // the map is not drawn yet
    el._histHover = true;
    el.on("plotly_hover", (ev) => {
      const p = ev.points?.[0]; const cd = typeof p?.customdata === "string" ? p.customdata : "";
      if (!cd.startsWith("hist:")) return;
      const [id, date] = cd.slice(5).split("|");
      const a = artifactById(id); if (!a) return;
      const wp = date && a.waypoints ? a.waypoints.find((w) => w.date === date) : null;
      showInfo(a, wp);
    });
  }
  const prevOnTabForHover = UW.onTab;
  UW.onTab = (name) => { prevOnTabForHover?.(name); if (name === "history") wireHover(); else { const b = $("#mapinfo"); if (b) b.hidden = true; } };

  // ---------------------------------------------------------------- the timeline
  // Every atomic date the crew entered: spans as bars, moments as dots, one
  // row per topic, on a numeric year axis so the Tuniit and the Karluk share it.
  function renderTimeline() {
    const el = $("#histtimeline"); if (!hist.timeline) return;
    const rows = hist.timeline.filter((d) => (!hist.topic || d.topic === hist.topic) && d.topic);
    const from = hist.from === "" ? -Infinity : +hist.from, to = hist.to === "" ? Infinity : +hist.to;
    const topics = [...new Set(rows.map((d) => d.topic))].sort((a, b) => (hist.index.topics.findIndex((t) => t.slug === a)) - (hist.index.topics.findIndex((t) => t.slug === b)));
    const label = (slug) => topicOf(slug)?.title.replace(/,.*$/, "").slice(0, 34) || slug;
    const traces = [];
    for (const tp of topics) {
      const col = topicColour(tp);
      const spans = rows.filter((d) => d.topic === tp && d.date_end && yearOf(d.date) != null && yearOf(d.date_end) != null && yearOf(d.date) <= to && yearOf(d.date_end) >= from);
      const pts = rows.filter((d) => d.topic === tp && !d.date_end && yearOf(d.date) != null && yearOf(d.date) >= from && yearOf(d.date) <= to);
      if (spans.length) traces.push({ type: "scatter", mode: "lines", name: label(tp), showlegend: false, hoverinfo: "text", connectgaps: false,
        x: spans.flatMap((d) => [yearOf(d.date), Math.max(yearOf(d.date_end), yearOf(d.date) + 0.05), null]), y: spans.flatMap(() => [label(tp), label(tp), null]),
        text: spans.flatMap((d) => [tl(d), tl(d), ""]), customdata: spans.flatMap((d) => [ref(d), ref(d), null]),
        line: { width: 7, color: col }, opacity: .8 });
      if (pts.length) traces.push({ type: "scatter", mode: "markers", name: label(tp), showlegend: false, hoverinfo: "text",
        x: pts.map((d) => yearOf(d.date)), y: pts.map(() => label(tp)), text: pts.map(tl), customdata: pts.map(ref),
        marker: { size: 7, color: col, line: { width: 1, color: "#0f1419" } } });
    }
    const layout = { ...THEME, height: Math.max(160, 28 * topics.length + 60), margin: { l: 210, r: 16, t: 8, b: 32 },
      xaxis: { ...THEME.xaxis, title: "", tickformat: "d", zeroline: false, gridcolor: "#1c2632" },
      yaxis: { ...THEME.yaxis, type: "category", categoryorder: "array", categoryarray: topics.map(label).reverse(), gridcolor: "#1c2632", tickfont: { size: 11 } },
      hovermode: "closest", showlegend: false };
    Plotly.react(el, traces, layout, CFG).then(() => {
      el.removeAllListeners?.("plotly_click");
      el.on("plotly_click", (ev) => { const r = ev.points?.[0]?.customdata; if (!r) return;
        if (r.kind === "artifact") { const a = artifactById(r.id); if (a) open(a.page); }
        else if (r.lat != null) { UW.focusMap(r.lat, r.lon, r.label); }
      });
    });
    function tl(d) { const q = d.qualifier ? d.qualifier + " " : ""; return `<b>${esc(d.label || d.entity_id)}</b><br>${q}${esc(d.date)}${d.date_end ? " → " + esc(d.date_end) : ""} (${esc(d.precision)})${d.place ? "<br>" + esc(d.place) : ""}`; }
    function ref(d) { return { kind: d.entity_kind, id: d.entity_id, lat: d.lat, lon: d.lon, label: d.label }; }
  }

  // ---------------------------------------------------------------- the historian
  // Questions go to the historian's room in the chat; the page being read is
  // sent along as context, and an answer's pages open here.
  UW.historyContext = () => (hist.slug && !hist.slug.startsWith("topic/")) ? hist.slug : "";
  UW.historyOpen = (slug) => open(slug);

  // ---------------------------------------------------------------- wiring
  function wire() {
    $("#histsearch").oninput = debounce((e) => { hist.search = e.target.value; if (hist.search && !hist.sideOn) { hist.sideOn = true; store.set("hist.side", true); renderSideToggle(); renderTypes(); } renderSide(); }, 150);
    $("#histtopic").onchange = (e) => { hist.topic = e.target.value; store.set("hist.topic", hist.topic); if (hist.topic) open(`topic/${hist.topic}`); else { hist.slug = ""; store.set("hist.slug", ""); render(); } };
    const years = debounce(() => { hist.from = $("#histfrom").value; hist.to = $("#histto").value; store.set("hist.from", hist.from); store.set("hist.to", hist.to); render(); }, 300);
    $("#histfrom").value = hist.from; $("#histto").value = hist.to;
    $("#histfrom").oninput = years; $("#histto").oninput = years;
    $("#histmap").hidden = true;                            // the history layer is always on here
    $("#histtl").onclick = () => { hist.timelineOn = !hist.timelineOn; store.set("hist.timeline", hist.timelineOn); renderMeta(); if (hist.timelineOn) renderTimeline(); };
    $("#histask").onclick = () => UW.chatRoom?.("ada");
    // the map's own History pill appears once there is history to show
    const pill = document.querySelector('#maplayers button[data-layer="history"]');
    if (pill) pill.hidden = !UW.M.history;
  }
  // On the History tab the map is the history's: the ship's own layers
  // (track, stations, cameras, events, satellite) step aside and come back
  // when the tab is left; plan, places and history stay. The header's leg
  // and span controls hide with them, by a class on the body.
  const SHIP_LAYERS = ["stations", "cameras", "events", "track"];
  let stashed = null;
  function historyMap(on) {
    const pill = (layer, state) => { const b = document.querySelector(`#maplayers button[data-layer="${layer}"]`); if (b) { b.classList.toggle("on", !!state); b.setAttribute("aria-pressed", String(!!state)); } };
    if (on && !stashed) {
      stashed = { sat: UW.state.sat, satAt: UW.state.satAt, history: UW.state.history };
      for (const l of SHIP_LAYERS) { stashed[l] = UW.state[l]; UW.state[l] = false; pill(l, false); }
      UW.state.sat = ""; UW.state.satAt = null;
      UW.state.history = true; pill("history", true);
    } else if (!on && stashed) {
      for (const l of SHIP_LAYERS) { UW.state[l] = stashed[l]; pill(l, stashed[l]); }
      UW.state.sat = stashed.sat; UW.state.satAt = stashed.satAt;
      UW.state.history = stashed.history; pill("history", stashed.history);
      stashed = null;
    }
    document.body.classList.toggle("tab-history", on);
  }
  const prevTab = UW.onTab;
  UW.onTab = (name) => {
    prevTab?.(name);
    const on = name === "history";
    const was = !!stashed;
    historyMap(on);
    if (on !== was) UW.renderMap();
    if (!on) return;
    ensure().then((ok) => { if (ok) render(); else renderMain(); }).catch(() => { UW.setLoadError("History", true); });
  };
  const prevRefresh = UW.refreshExtraData;
  UW.refreshExtraData = () => { prevRefresh?.(); const pill = document.querySelector('#maplayers button[data-layer="history"]'); if (pill) pill.hidden = !UW.M.history;
    if (!$("#pane-history").hidden || UW.state.history) ensure().then(() => { if (!$("#pane-history").hidden) render(); else UW.renderMap(); }).catch(() => {}); };
  wire();
  if (document.querySelector("#tabs button.on")?.dataset.tab === "history") UW.onTab("history");
  else if (UW.state.history && UW.M.history) ensure().then(() => UW.renderMap()).catch(() => {});
})();
