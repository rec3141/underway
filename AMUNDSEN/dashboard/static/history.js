/* The History tab: the region's past on the map, in a wiki, on a timeline,
 * with Ada in the Library to ask. Everything it shows was published by the
 * build from the arctic-history database into data/history/: an index of
 * pages and topics, the artifacts with their positions and tracks, the atomic
 * dates, the people and places, the bibliography, and one JSON file per wiki
 * page. Loaded after tabs.js; talks to app.js through window.UW like the
 * other panes. */
(() => {
  "use strict";
  const UW = window.UW;
  const $ = (s) => document.querySelector(s);
  const { store } = UW;
  const esc = (s) => String(s ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  const cachedJSON = window.UWData.generationCache(UW.fetchJSON, () => UW.M.history?.stamp || "");
  const debounce = (f, ms) => { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => f(...a), ms); }; };

  // artifact kinds: a colour each, for the map and the chips
  const TYPES = {
    track: { label: "Tracks", colour: "#ffb454" },
    event: { label: "Events", colour: "#ff7b72" },
    place: { label: "Places", colour: "#7ee787" },
    image: { label: "Images", colour: "#5cc8ff" },
    map:   { label: "Maps", colour: "#79c0ff" },
    quote: { label: "Quotes", colour: "#d2a8ff" },
    text:  { label: "Texts", colour: "#a5d6ff" },
    object: { label: "Objects", colour: "#f2cc60" },
  };
  // the pane's chips: one page per kind, People among them
  const KINDS = { track: TYPES.track, event: TYPES.event, place: TYPES.place, people: { label: "People", colour: "#ffa198" }, image: TYPES.image, map: TYPES.map, quote: TYPES.quote, text: TYPES.text, object: TYPES.object };
  const TOPIC_COLOURS = ["#ffb454", "#5cc8ff", "#7ee787", "#ff7b72", "#d2a8ff", "#f2cc60", "#79c0ff", "#ffa198", "#56d364", "#e3b341", "#a5d6ff", "#ff9bce"];
  const TIMELINE_FROM = 1500;                              // the Events page opens here; earlier rows on request
  const VIG_N = 5;                                         // vignette lines shown before "see more"

  const hist = {
    index: null, artifacts: null, timeline: null, places: [], people: [], bib: null, stamp: null, loading: null,
    slug: store.get("hist.slug", ""),                     // what is shown: "" home, explore, bib, kind/<k>, topic/<t>, or a page
    types: new Set(store.get("hist.types", Object.keys(TYPES))),   // the kinds the map layer shows
    search: "",
    earlier: false,                                       // the Events page before 1500
    more: { today: false, here: false },                  // the vignettes unfolded
    pages: new Map(),                                     // slug -> page JSON, this generation
  };

  // ---------------------------------------------------------------- data
  async function ensure() {
    if (!UW.M.history) { hist.index = null; return false; }
    if (hist.stamp === UW.M.history.stamp && hist.index) return true;
    if (hist.loading) return hist.loading;
    hist.loading = (async () => {
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
      hist.stamp = UW.M.history.stamp; hist.pages = new Map(); hist.bib = null;
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
  // the bibliography, parsed from the BibTeX the build publishes
  async function bibliography() {
    if (hist.bib) return hist.bib;
    const r = await fetch(`data/history/references.bib?v=${encodeURIComponent(UW.M.history?.stamp || "")}`, { cache: "no-store" });
    const text = r.ok ? await r.text() : "";
    const out = [];
    for (const m of text.matchAll(/@(\w+)\s*\{\s*([^,\s]+)\s*,([\s\S]*?)\n\}/g)) {
      const e = { type: m[1].toLowerCase(), key: m[2] };
      for (const f of m[3].matchAll(/(\w+)\s*=\s*\{((?:[^{}]|\{[^{}]*\})*)\}/g)) e[f[1].toLowerCase()] = f[2].replace(/[{}]/g, "").replace(/\s+/g, " ").trim();
      out.push(e);
    }
    hist.bib = out;
    return out;
  }
  // an entry in MLA (9th) form, as far as the fields allow
  function mla(e) {
    const authors = (e.author || "").split(/\s+and\s+/).map((a) => a.trim()).filter(Boolean);
    const name = (a, flip) => { const [last, first] = a.includes(",") ? a.split(",").map((x) => x.trim()) : [a.split(" ").slice(-1)[0], a.split(" ").slice(0, -1).join(" ")]; return flip ? `${last}, ${first}`.replace(/, $/, "") : `${first} ${last}`.trim(); };
    let by = authors.length === 0 ? "" : authors.length === 1 ? name(authors[0], true) : authors.length === 2 ? `${name(authors[0], true)}, and ${name(authors[1])}` : `${name(authors[0], true)}, et al.`;
    const title = e.title || e.key;
    const container = e.journal || e.booktitle || e.container || "";
    const parts = [];
    if (by) parts.push(by + ".");
    if (container) { parts.push(`"${title}."`); parts.push(`<i>${esc(container)}</i>` + (e.volume ? `, vol. ${esc(e.volume)}` : "") + (e.number ? `, no. ${esc(e.number)}` : "") + (e.year ? `, ${esc(e.year)}` : "") + (e.pages ? `, pp. ${esc(e.pages)}` : "") + "."); }
    else { parts.push(`<i>${esc(title)}</i>.`); if (e.publisher) parts.push(esc(e.publisher) + (e.year ? `, ${esc(e.year)}.` : ".")); else if (e.year) parts.push(esc(e.year) + "."); }
    if (e.url) parts.push(`<a href="${esc(e.url)}" target="_blank" rel="noopener">${esc(e.url)}</a>.`);
    if (e.urldate) parts.push(`Accessed ${esc(e.urldate)}.`);
    return (by ? esc(parts[0]) + " " : "") + parts.slice(by ? 1 : 0).map((p, i) => (i === 0 && container) ? esc(p) : p).join(" ");
  }

  // a partial ISO date as a decimal year: "1845-09" -> 1845.67; "-2500" -> -2500
  function yearOf(s) {
    if (!s) return null;
    const m = /^(-?\d{1,4})(?:-(\d{1,2}))?(?:-(\d{1,2}))?$/.exec(s);
    if (!m) return null;
    return +m[1] + ((+m[2] || 1) - 1) / 12 + ((+m[3] || 1) - 1) / 365;
  }
  // a year for people: 2500 BCE, 985 AD, 1845
  function yearLabel(y) {
    if (y == null) return "";
    const n = Math.floor(y);
    return n <= 0 ? `${-n} BCE` : n < 1000 ? `${n} AD` : String(n);
  }
  // a date as written, with early years spelled out: "-2500" -> "2500 BCE", "0985-06" -> "985 AD, June"
  function dateLabel(iso) {
    if (!iso) return "";
    const m = /^(-?\d{1,4})(?:-(\d{1,2}))?(?:-(\d{1,2}))?$/.exec(String(iso).split("/")[0].trim());
    if (!m) return iso;
    const y = +m[1];
    if (y >= 1000) return iso;
    const tail = m[2] ? ", " + new Date(2000, +m[2] - 1, 1).toLocaleDateString(undefined, { month: "long" }) + (m[3] ? ` ${+m[3]}` : "") : "";
    return yearLabel(y) + tail;
  }
  const topicOf = (slug) => hist.index?.topics.find((t) => t.slug === slug);
  const topicColour = (slug) => TOPIC_COLOURS[Math.max(0, hist.index?.topics.findIndex((t) => t.slug === slug) || 0) % TOPIC_COLOURS.length];
  const artifactById = (id) => hist.artifacts?.find((a) => a.id === id);
  const topicImage = (slug) => hist.artifacts?.find((a) => a.topic === slug && a.url && (a.type === "image" || a.type === "map"));
  const placeByPage = (slug) => hist.places.find((p) => p.page === slug);
  // the topic in force: the one whose page is open, else none
  const curTopic = () => hist.slug.startsWith("topic/") ? hist.slug.slice(6) : "";

  // ---------------------------------------------------------------- vignettes
  const R_EARTH = 6371;
  function km(lat1, lon1, lat2, lon2) {
    const r = Math.PI / 180, dLat = (lat2 - lat1) * r, dLon = (lon2 - lon1) * r;
    const a = Math.sin(dLat / 2) ** 2 + Math.cos(lat1 * r) * Math.cos(lat2 * r) * Math.sin(dLon / 2) ** 2;
    return 2 * R_EARTH * Math.asin(Math.sqrt(a));
  }
  const parts = (iso) => { const m = /^(-?\d{1,4})-(\d{2})-(\d{2})$/.exec(iso || ""); return m ? { y: +m[1], m: +m[2], d: +m[3] } : null; };
  const ymd = (y, m, d) => y * 10000 + m * 100 + d;
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
        if (a.m === mm && a.d === dd) out.push({ year: a.y, kind: r.role === "start" ? "began" : r.role === "end" ? "ended" : "", label: r.label, place: r.place, topic: r.topic, lat: r.lat, lon: r.lon, slug: pageFor(r.entity_kind, r.entity_id) });
        continue;
      }
      const b = parts(r.date_end); if (!b) continue;
      if (a.m === mm && a.d === dd) out.push({ year: a.y, kind: "began", label: r.label, place: r.place, topic: r.topic, lat: r.lat, lon: r.lon, slug: pageFor(r.entity_kind, r.entity_id) });
      if (b.m === mm && b.d === dd) out.push({ year: b.y, kind: "ended", label: r.label, place: r.place, topic: r.topic, lat: r.lat, lon: r.lon, slug: pageFor(r.entity_kind, r.entity_id) });
      for (let y = a.y; y <= b.y && y - a.y < 40; y++) {
        const t = ymd(y, mm, dd);
        if (t > ymd(a.y, a.m, a.d) && t < ymd(b.y, b.m, b.d)) {
          const day = Math.round((Date.UTC(y, mm - 1, dd) - Date.UTC(a.y, a.m - 1, a.d)) / 864e5);
          out.push({ year: y, kind: `day ${day + 1} of ${Math.round((Date.UTC(b.y, b.m - 1, b.d) - Date.UTC(a.y, a.m - 1, a.d)) / 864e5) + 1}`, label: r.label, place: r.place, topic: r.topic, lat: r.lat, lon: r.lon, slug: pageFor(r.entity_kind, r.entity_id) });
        }
      }
    }
    for (const a of hist.artifacts || []) {
      if (a.type !== "track" || !a.waypoints) continue;
      for (const w of a.waypoints) {
        const d = parts(w.date);
        if (d && d.m === mm && d.d === dd) out.push({ year: d.y, kind: "was here", label: a.title, place: w.note || "", topic: a.topic, lat: w.lat, lon: w.lon, slug: a.page });
      }
    }
    for (const p of hist.people || []) {
      for (const [field, word] of [["born", "born"], ["died", "died"]]) {
        const d = parts(p[field]);
        if (d && d.m === mm && d.d === dd) out.push({ year: d.y, kind: word, label: p.name, place: p.role || "", topic: p.topic, lat: null, lon: null, slug: p.page });
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
    const seen = new Set();
    const items = cand.filter((c) => c.d <= 600).filter((c) => { const k = c.a ? c.a.id : c.p ? `pl:${c.p.name}` : `ev:${c.e.entity_id}`; if (seen.has(k)) return false; seen.add(k); return true; }).slice(0, 20)
      .map((c) => c.a ? { d: c.d, label: c.a.title, when: c.when, note: c.note, topic: c.a.topic, type: c.a.type, slug: c.a.page, lat: c.lat, lon: c.lon }
                : c.p ? { d: c.d, label: c.p.name, when: c.when, note: c.note, topic: c.p.topic, type: "place", slug: c.p.page, lat: c.lat, lon: c.lon }
                      : { d: c.d, label: c.e.label, when: c.when, note: c.note, topic: c.e.topic, type: "event", slug: "", lat: c.lat, lon: c.lon });
    return { name: whereName(lat, lon), items };
  }
  // the name of where the ship is: the settlement it lies off, else the bay,
  // strait, polynya or region the history knows there, else "near" a settlement
  const WATER_KINDS = new Set(["bay", "strait", "polynya", "harbour", "region", "island", "lake", "river"]);
  function whereName(lat, lon) {
    if (lat == null || lon == null) return "";
    const towns = [...(UW.state.communities_data || []).map((c) => ({ name: c.name, d: km(lat, lon, c.lat, c.lon) })),
      ...hist.places.filter((p) => p.lat != null && (p.kind === "community" || p.kind === "port")).map((p) => ({ name: p.name, d: km(lat, lon, p.lat, p.lon) }))];
    const waters = hist.places.filter((p) => p.lat != null && WATER_KINDS.has(p.kind)).map((p) => ({ name: p.name, d: km(lat, lon, p.lat, p.lon) }));
    const nearest = (xs, max) => xs.filter((x) => x.d <= max).sort((a, b) => a.d - b.d)[0];
    const town = nearest(towns, 30); if (town) return town.name;
    const water = nearest(waters, 200), t2 = nearest(towns, 120);
    if (water && (!t2 || water.d < t2.d)) return water.name;
    if (t2) return `near ${t2.name}`;
    return water ? water.name : "";
  }
  // the day's picks: the same five all day, a different five tomorrow; the
  // things that began, ended, were born, died or were here come before the
  // "day 212 of 400" lines of the long winterings
  function pickForDay(items, n, now) {
    let x = now.getFullYear() * 10000 + (now.getMonth() + 1) * 100 + now.getDate();
    const rnd = () => ((x = (x * 1103515245 + 12345) & 0x7fffffff) / 0x7fffffff);
    const shuffle = (xs) => { const a = [...xs]; for (let i = a.length - 1; i > 0; i--) { const j = Math.floor(rnd() * (i + 1)); [a[i], a[j]] = [a[j], a[i]]; } return a; };
    const firsts = shuffle(items.filter((x) => !/^day \d/.test(x.kind))), rest = shuffle(items.filter((x) => /^day \d/.test(x.kind)));
    return [...firsts, ...rest].slice(0, n).sort((p, q) => p.year - q.year);
  }
  UW.historyVignettes = () => ({ today: onThisDay(), here: inThisPlace(UW.M.latest?.lat, UW.M.latest?.lon) });

  function vignetteHTML() {
    const now = new Date();
    const today = onThisDay(now);
    const pos = UW.M.latest || {};
    const here = inThisPlace(pos.lat, pos.lon);
    const dateWord = now.toLocaleDateString(undefined, { month: "long", day: "numeric" });
    const item = (x, extra) => `<a class="vig" href="#history/${esc(x.slug)}" data-slug="${esc(x.slug)}" data-lat="${x.lat ?? ""}" data-lon="${x.lon ?? ""}" data-topic="${esc(x.topic)}"><span class="dot" style="background:${topicColour(x.topic)}"></span>${extra}<span class="txt">${esc(x.label)}${x.kind ? ` <i>${esc(x.kind)}</i>` : x.type === "place" && x.when ? ` <i>${esc(x.when)}</i>` : ""}${x.note ? ` <span class="muted">${esc(x.note)}</span>` : ""}${x.place && !x.note ? ` <span class="muted">${esc(x.place)}</span>` : ""}${x.lat != null ? ` <span class="pin" title="on the map">⌖</span>` : ""}</span></a>`;
    const fold = (xs, key, line) => {
      const shown = hist.more[key] ? xs : xs.slice(0, VIG_N), rest = xs.length - shown.length;
      return shown.map(line).join("") + (rest > 0 ? `<button type="button" class="more" data-more="${key}">see ${rest} more</button>` : hist.more[key] && xs.length > VIG_N ? `<button type="button" class="more" data-more="${key}">see fewer</button>` : "");
    };
    const todayShown = hist.more.today ? today : pickForDay(today, VIG_N, now);
    const foldPicked = (all, shown, key, line) => shown.map(line).join("") +
      (all.length > shown.length ? `<button type="button" class="more" data-more="${key}">see ${all.length - shown.length} more</button>` : hist.more[key] && all.length > VIG_N ? `<button type="button" class="more" data-more="${key}">see fewer</button>` : "");
    return `<div class="vignettes">
      <section class="vigcard"><h3>On this day · ${esc(dateWord)}</h3>${today.length ? foldPicked(today, todayShown, "today", (x) => item(x, `<b>${esc(yearLabel(x.year))}</b>`)) : `<div class="muted small">Nothing dated to the day on ${esc(dateWord)} yet.</div>`}</section>
      <section class="vigcard"><h3>In this place${here.name ? ` · ${esc(here.name)}` : ""}</h3>${here.items.length ? fold(here.items, "here", (x) => item(x, `<b>${Math.round(x.d)} km</b>`)) : `<div class="muted small">${pos.lat == null ? "The ship's position is not known to this build." : "Nothing in the history near the ship yet."}</div>`}</section>
    </div>`;
  }

  // the artifacts on the map: the kinds the map's chips allow, within the open topic
  function shownArtifacts() {
    if (!hist.artifacts) return [];
    const t = curTopic();
    return hist.artifacts.filter((a) => (!t || a.topic === t) && hist.types.has(a.type));
  }
  const byYear = (p, q) => ((p._year ?? 9e9) - (q._year ?? 9e9)) || p.title.localeCompare(q.title);

  // ---------------------------------------------------------------- markdown
  function inline(s) {
    s = esc(s);
    s = s.replace(/!\[([^\]]*)\]\(([^)\s]+)\)/g, (m, alt, src) => `<img src="${src}" alt="${alt}" loading="lazy">`);
    s = s.replace(/\[([^\]]+)\]\(([^)\s]+)\)/g, (m, label, href) =>
      /^https?:\/\//.test(href) ? `<a href="${href}" target="_blank" rel="noopener">${label}</a>` : `<a href="#history/${href}" data-slug="${href}">${label}</a>`);
    s = s.replace(/&lt;span class=&quot;wanted&quot; title=&quot;no page yet&quot;&gt;(.*?)&lt;\/span&gt;/g, '<span class="wanted">$1</span>');
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
        if (cells.every((c) => /^:?-+:?$/.test(c))) continue;
        (table ||= []).push(cells); continue;
      }
      if (list || quote.length || table) flush();
      para.push(line.trim());
    }
    flush();
    return out.join("\n");
  }

  // ---------------------------------------------------------------- the pane
  const fmtDate = (a) => dateLabel(a.date_text || a.date_start || "");
  const crumb = (...rest) => `<div class="crumb"><a href="#history/" data-slug="">History</a>${rest.map((r) => ` › ${r}`).join("")}</div>`;
  function artifactCard(a, opts = {}) {
    const t = TYPES[a.type] || {};
    const img = a.url && (a.type === "image" || a.type === "map") ? `<img class="thumb" src="${esc(a.url)}" alt="" loading="lazy">` : "";
    return `<a class="artcard ${esc(a.type)}" href="#history/${esc(a.page)}" data-slug="${esc(a.page)}" title="${esc(a.title)}">${img}<span class="dot" style="background:${t.colour || "#8b9bb0"}"></span>` +
      `<span class="kind">${esc(a.type)}</span><b>${esc(a.title)}</b><span class="when">${esc(fmtDate(a))}${a.lat != null ? ' <span class="pin" title="on the map">⌖</span>' : ""}</span>` +
      (opts.creator && a.creator ? `<span class="who">${esc(a.creator)}</span>` : "") + `</a>`;
  }
  function pageLink(p, cls = "") {
    return `<a class="pglink ${cls}" href="#history/${esc(p.slug)}" data-slug="${esc(p.slug)}"><span class="kind">${esc(p.kind)}</span>${esc(p.title)}${p.summary ? `<span class="sum">${esc(p.summary)}</span>` : ""}</a>`;
  }
  // a topic as a card with its picture: the home's narrative chips, and Explore
  function topicCard(x, full = false) {
    const im = topicImage(x.slug);
    return `<a class="topiccard ${im ? "" : "noimg"}" href="#history/topic/${esc(x.slug)}" data-topic="${esc(x.slug)}" style="border-left-color:${topicColour(x.slug)}">` +
      (im ? `<img src="${esc(im.url)}" alt="" loading="lazy">` : "") +
      `<span class="body"><b>${esc(x.title)}</b>${full ? `<span>${esc(x.summary)}</span>` : ""}<span class="counts">${x.pages} pages · ${x.artifacts} artifacts</span></span></a>`;
  }

  async function renderMain() {
    const el = $("#histmain");
    const plot = $("#histplot"); if (plot?.data) Plotly.purge(plot);
    if (!UW.M.history) { el.innerHTML = `<div class="empty">No history has been published yet.</div>`; return; }
    if (!hist.index) { el.innerHTML = `<div class="empty">Loading the history…</div>`; return; }
    const q = hist.search.trim().toLowerCase();
    if (q) {                                                        // search: pages and artifacts, in the main area
      const words = q.split(/\s+/).filter(Boolean), t = curTopic();
      const hit = (s) => { const t = String(s || "").toLowerCase(); return words.every((w) => t.includes(w)); };
      const pages = hist.index.pages.filter((p) => (!t || p.topic === t || !p.topic) && (hit(p.title) || hit(p.summary)))
        .sort((a, b) => (hit(b.title) - hit(a.title)) || (a.kind === "page" ? -1 : 1));
      const arts = hist.artifacts.filter((a) => (!t || a.topic === t) && (hit(a.title) || hit(a.description) || hit((a.people || []).join(" ")) || hit((a.tags || []).join(" "))));
      el.innerHTML = crumb(`search <i>${esc(hist.search.trim())}</i>`) + `<h2>${pages.length} pages · ${arts.length} artifacts</h2>` +
        `<div class="pagelist">${pages.slice(0, 80).map((p) => pageLink(p)).join("")}</div>` +
        (arts.length ? `<div class="artgrid">${arts.slice(0, 80).map((a) => artifactCard(a, { creator: true })).join("")}</div>` : "");
      return;
    }
    if (!hist.slug) {                                              // the home: today, here, and the narratives
      const t = hist.index.topics;
      el.innerHTML = vignetteHTML() + `<h2>Narratives <a class="chip small" href="#history/explore" data-slug="explore">Explore</a></h2>` +
        `<div class="topicgrid chips">${t.filter((x) => x.pages > 0).map((x) => topicCard(x)).join("")}</div>`;
      for (const b of el.querySelectorAll("button.more")) b.onclick = () => { hist.more[b.dataset.more] = !hist.more[b.dataset.more]; renderMain(); };
      return;
    }
    if (hist.slug === "explore") {
      const t = hist.index.topics;
      const n = (k) => t.reduce((s, x) => s + (x[k] || 0), 0);
      el.innerHTML = crumb("Explore") + `<h2>Explore</h2><p class="lead">${t.length} topics, ${n("pages")} narrative pages and ${hist.artifacts.length} artifacts, from the Tuniit to the ships of the last century: voyages as tracks, winterings and besetments as spans on the timeline, people and places as pages that link to one another, every item credited and sourced.</p>` +
        `<div class="topicgrid">${t.map((x) => topicCard(x, true)).join("")}</div>`;
      return;
    }
    if (hist.slug.startsWith("kind/")) { renderKind(el, hist.slug.slice(5)); return; }
    if (hist.slug === "bib") { await renderBib(el); return; }
    if (hist.slug.startsWith("topic/")) {
      const t = topicOf(hist.slug.slice(6));
      if (!t) { el.innerHTML = `<div class="empty">no such topic</div>`; return; }
      const pages = hist.index.pages.filter((p) => p.topic === t.slug && p.kind === "page");
      const arts = hist.artifacts.filter((a) => a.topic === t.slug).sort(byYear);
      const byType = {}; for (const a of arts) (byType[a.type] ||= []).push(a);
      const im = topicImage(t.slug);
      el.innerHTML = crumb(`<a href="#history/explore" data-slug="explore">Explore</a>`, esc(t.title)) + `<h2>${esc(t.title)}</h2>` +
        (im ? `<figure class="topicfig"><img src="${esc(im.url)}" alt=""><figcaption>${esc(im.title)} · ${esc(im.credit)}</figcaption></figure>` : "") +
        `<p class="lead">${esc(t.summary)}</p>` +
        (pages.length ? `<div class="pagelist">${pages.map((p) => pageLink(p)).join("")}</div>` : `<p class="muted">No narrative pages yet; the artifacts below are what the crew has entered so far.</p>`) +
        Object.entries(byType).map(([k, xs]) => `<h3>${esc(TYPES[k]?.label || k)} <span class="muted">${xs.length}</span></h3><div class="artgrid">${xs.map((a) => artifactCard(a, { creator: true })).join("")}</div>`).join("");
      return;
    }
    let p;
    try { p = await page(hist.slug); }
    catch { el.innerHTML = crumb() + `<div class="empty">That page is not in this build.</div>`; return; }
    const t = topicOf(p.topic);
    const a = p.kind === "artifact" ? artifactById(p.ref) : null;
    const pl = p.kind === "place" ? placeByPage(p.slug) : null;
    const back = (p.backlinks || []).map((s) => hist.index.pages.find((x) => x.slug === s)).filter(Boolean);
    let head = crumb(...(t ? [`<a href="#history/topic/${esc(t.slug)}" data-topic="${esc(t.slug)}">${esc(t.title)}</a>`] : []), `<span class="kind">${esc(p.kind)}</span>`) + `<h2>${esc(p.title)}</h2>`;
    if (p.summary && p.kind === "page") head += `<p class="lead">${esc(p.summary)}</p>`;
    let media = "";
    if (a) {
      if (a.url && (a.type === "image" || a.type === "map")) media = `<figure><a href="${esc(a.url)}" target="_blank" rel="noopener"><img src="${esc(a.url)}" alt="${esc(a.title)}"></a><figcaption>${esc(a.credit)}${a.licence ? " · " + esc(a.licence) : ""}</figcaption></figure>`;
      else if (a.url && a.type === "text") media = `<p><a class="chip" href="${esc(a.url)}" target="_blank" rel="noopener">open the full text</a></p>`;
      const where = a.lat != null ? `<button type="button" class="chip" id="histfocus">⌖ show on the map</button>` : "";
      const track = a.type === "track" && a.waypoints?.length ? `<div class="hscroll"><table class="waypoints"><tr><th>date</th><th>position</th><th>note</th></tr>${a.waypoints.map((w) => `<tr><td>${esc(dateLabel(w.date || ""))}</td><td class="mono">${w.lat != null ? (+w.lat).toFixed(2) + ", " + (+w.lon).toFixed(2) : ""}</td><td>${esc(w.note || "")}</td></tr>`).join("")}</table></div>` : "";
      head += `<div class="artmeta"><span class="dot" style="background:${TYPES[a.type]?.colour || "#8b9bb0"}"></span>${esc(a.type)} · ${esc(fmtDate(a))}${a.creator ? " · " + esc(a.creator) : ""} ${where}</div>`;
      media += track;
    }
    if (pl && pl.lat != null) {
      head += `<div class="artmeta"><span class="dot" style="background:${TYPES.place.colour}"></span>${esc(pl.kind || "place")} · <span class="mono">${(+pl.lat).toFixed(3)}, ${(+pl.lon).toFixed(3)}</span> <button type="button" class="chip" id="histfocus">⌖ show on the map</button></div>`;
    }
    if (p.kind === "source") {
      const bib = await bibliography();
      const e = bib.find((x) => x.key === p.slug.split("/").pop());
      if (e) head += `<p class="mlaline"><span class="lbl">MLA</span> ${mla(e)}</p>`;
    }
    el.innerHTML = head + media + `<div class="wiki">${markdown(p.html)}</div>` +
      (back.length ? `<div class="backlinks"><span class="lbl">Mentioned in</span>${back.map((b) => `<a href="#history/${esc(b.slug)}" data-slug="${esc(b.slug)}">${esc(b.title)}</a>`).join("")}</div>` : "");
    $("#histfocus")?.addEventListener("click", () => a ? focusArtifact(a) : focusPoint(pl.lat, pl.lon, pl.name));
    window.scrollTo?.(0, 0);
  }

  // ---------------------------------------------------------------- the kind pages
  // One page per chip: every track, place, person, image, map, quote, text or
  // object in the record; the Events page is the timeline.
  const letterList = (xs, name, line) => {
    let letter = "";
    return xs.map((x) => { const L = (name(x)[0] || "?").toUpperCase(); const head = L !== letter ? `<h3>${esc(L)}</h3>` : ""; letter = L; return head + line(x); }).join("");
  };
  function renderKind(el, kind) {
    const K = KINDS[kind];
    if (!K) { el.innerHTML = crumb() + `<div class="empty">no such kind</div>`; return; }
    if (kind === "event") { el.innerHTML = crumb("Events") + eventsHTML(); wireEvents(el); return; }
    if (kind === "people") {
      const people = [...hist.people].sort((a, b) => a.name.localeCompare(b.name));
      const life = (p) => [p.born, p.died].some(Boolean) ? ` <span class="muted mono">${esc(dateLabel(p.born || "?"))}–${esc(dateLabel(p.died || ""))}</span>` : "";
      el.innerHTML = crumb("People") + `<h2>People <span class="muted">${people.length}</span></h2><div class="peoplelist">` +
        letterList(people, (p) => p.name, (p) => `<a class="person ${p.indigenous ? "inuit" : ""}" href="#history/${esc(p.page)}" data-slug="${esc(p.page)}"><b>${esc(p.name)}</b>${p.also ? ` <span class="muted">(${esc(p.also)})</span>` : ""}${life(p)}${p.role ? `<span class="role">${esc(p.role)}</span>` : ""}</a>`) + `</div>`;
      return;
    }
    if (kind === "place") {
      const places = [...hist.places].sort((a, b) => a.name.localeCompare(b.name));
      const names = (p) => [p.inuktitut, p.historic].filter((n) => n && n !== p.name).join(", ");
      el.innerHTML = crumb("Places") + `<h2>Places <span class="muted">${places.length}</span></h2><div class="peoplelist">` +
        letterList(places, (p) => p.name, (p) => `<a class="person" href="#history/${esc(p.page)}" data-slug="${esc(p.page)}"><b>${esc(p.name)}</b>${names(p) ? ` <span class="muted">(${esc(names(p))})</span>` : ""} <span class="muted small">${esc(p.kind || "")}</span>` +
          (p.lat != null ? ` <span class="pin" data-lat="${p.lat}" data-lon="${p.lon}" data-label="${esc(p.name)}" title="on the map">⌖</span>` : "") + (p.note ? `<span class="role">${esc(p.note.length > 160 ? p.note.slice(0, 157) + "…" : p.note)}</span>` : "") + `</a>`) + `</div>`;
      return;
    }
    const arts = hist.artifacts.filter((a) => a.type === kind).sort(byYear);
    el.innerHTML = crumb(esc(K.label)) + `<h2><span class="dot" style="background:${K.colour}"></span>${esc(K.label)} <span class="muted">${arts.length}</span></h2>` +
      (arts.length ? `<div class="artgrid ${kind === "image" || kind === "map" ? "pictures" : ""}">${arts.map((a) => artifactCard(a, { creator: true })).join("")}</div>` : `<p class="muted">Nothing of this kind in the record yet.</p>`);
  }

  // ---------------------------------------------------------------- the Events page
  // Every atomic date the crew entered: a chart like the Event Log's timeline
  // (a row per topic, spans as bars, dates as points; click one for its row)
  // over the table. It opens at 1500; the centuries before are a click away.
  function timelineRows() {
    const t = curTopic();
    const rows = (hist.timeline || []).filter((d) => d.topic && (!t || d.topic === t));
    return rows.filter((d) => { const y = yearOf(d.date); return y != null && (hist.earlier || y >= TIMELINE_FROM); })
      .sort((p, q) => yearOf(p.date) - yearOf(q.date));
  }
  const shortTopic = (slug) => topicOf(slug)?.title.replace(/,.*$/, "") || slug;
  function eventsHTML() {
    const rows = timelineRows(), t = curTopic();
    const before = (hist.timeline || []).filter((d) => d.topic && (!t || d.topic === t) && yearOf(d.date) != null && yearOf(d.date) < TIMELINE_FROM).length;
    const tr = (d, i) => {
      const q = d.qualifier ? `<i>${esc(d.qualifier)}</i> ` : "";
      const when = q + esc(dateLabel(d.date)) + (d.date_end ? ` → ${esc(dateLabel(d.date_end))}` : "") + (d.precision && d.precision !== "day" ? ` <span class="muted">(${esc(d.precision)})</span>` : "");
      const slug = pageFor(d.entity_kind, d.entity_id);
      const what = slug ? `<a href="#history/${esc(slug)}" data-slug="${esc(slug)}">${esc(d.label || d.entity_id)}</a>` : esc(d.label || d.entity_id);
      const pin = d.lat != null ? ` <span class="pin" data-lat="${d.lat}" data-lon="${d.lon}" data-label="${esc(d.label || "")}" title="on the map">⌖</span>` : "";
      return `<tr data-key="${i}"${d.lat != null ? ` data-lat="${d.lat}" data-lon="${d.lon}"` : ""}><td class="mono">${when}</td><td>${what}${pin}</td><td>${esc(d.place || "")}</td><td><a href="#history/topic/${esc(d.topic)}" data-topic="${esc(d.topic)}"><span class="dot" style="background:${topicColour(d.topic)}"></span>${esc(shortTopic(d.topic))}</a></td></tr>`;
    };
    const spans = rows.filter((d) => d.date_end).length;
    return `<section class="panel card castplot wide" data-cp="histplot"><div class="head"><h3>Timeline</h3><div class="tools"><span class="now">${rows.length} dates · ${spans} spans · scroll to zoom, drag to pan · click a point for its row</span><button type="button" class="reset" id="histplotreset" title="reset zoom">⟲</button></div></div><div class="plot" id="histplot"></div></section>` +
      `<h2>Events <span class="muted">${rows.length} dates${hist.earlier ? "" : ` from ${TIMELINE_FROM}`}</span>` +
      (before && !hist.earlier ? ` <button type="button" class="chip small" id="histearlier">show the ${before} before ${TIMELINE_FROM}</button>` : hist.earlier ? ` <button type="button" class="chip small" id="histearlier">from ${TIMELINE_FROM} only</button>` : "") + `</h2>` +
      `<div class="hscroll" id="histevents"><table class="sched timeline"><thead><tr><th>Date</th><th>What</th><th>Where</th><th>Topic</th></tr></thead><tbody>${rows.map(tr).join("")}</tbody></table></div>`;
  }
  // the year axis: ticks at round years, read as BCE / AD / plain
  function yearTicks(lo, hi) {
    const span = Math.max(1, hi - lo), raw = span / 8;
    const step = [1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 5000].find((x) => x >= raw) || 5000;
    const vals = []; for (let y = Math.ceil(lo / step) * step; y <= hi; y += step) vals.push(y);
    return { tickvals: vals, ticktext: vals.map((y) => yearLabel(y)) };
  }
  function drawEvents() {
    const gd = $("#histplot"); if (!gd) return;
    const rows = timelineRows();
    if (!rows.length) { gd.innerHTML = `<div class="empty">No dates to chart.</div>`; return; }
    const cats = [...new Set(rows.map((d) => d.topic))].map(shortTopic);
    const ys = rows.map((d) => yearOf(d.date)), ye = rows.map((d) => d.date_end ? yearOf(d.date_end) : null);
    const lo = Math.min(...ys), hi = Math.max(...ys, ...ye.filter((y) => y != null));
    const label = (d) => `${esc((d.label || d.entity_id).length > 70 ? (d.label || d.entity_id).slice(0, 67) + "…" : d.label || d.entity_id)}<br>${esc(d.qualifier ? d.qualifier + " " : "")}${esc(dateLabel(d.date))}${d.date_end ? " → " + esc(dateLabel(d.date_end)) : ""}${d.place ? " · " + esc(d.place) : ""}`;
    const pts = rows.map((d, i) => [d, i]).filter(([d]) => !d.date_end), bars = rows.map((d, i) => [d, i]).filter(([d]) => d.date_end);
    const rgba = (hex, a) => `rgba(${parseInt(hex.slice(1, 3), 16)},${parseInt(hex.slice(3, 5), 16)},${parseInt(hex.slice(5, 7), 16)},${a})`;
    const traces = [];
    if (bars.length) traces.push({ type: "bar", orientation: "h", name: "spans", base: bars.map(([d]) => yearOf(d.date)), x: bars.map(([d]) => Math.max(0.3, yearOf(d.date_end) - yearOf(d.date))), y: bars.map(([d]) => shortTopic(d.topic)),
      customdata: bars.map(([, i]) => i), text: bars.map(([d]) => label(d)), hovertemplate: "%{text}<extra></extra>", textposition: "none",
      marker: { color: bars.map(([d]) => rgba(topicColour(d.topic), .45)), line: { color: bars.map(([d]) => topicColour(d.topic)), width: 1 } }, width: .5 });
    if (pts.length) traces.push({ type: "scatter", mode: "markers", name: "dates", x: pts.map(([d]) => yearOf(d.date)), y: pts.map(([d]) => shortTopic(d.topic)),
      customdata: pts.map(([, i]) => i), text: pts.map(([d]) => label(d)), hovertemplate: "%{text}<extra></extra>",
      marker: { size: 8, color: pts.map(([d]) => topicColour(d.topic)), line: { color: "#0b1620", width: .5 } } });
    const pad = Math.max(2, (hi - lo) * .02);
    const layout = { ...UW.THEME, margin: { l: 150, r: 12, t: 8, b: 40 }, showlegend: false, dragmode: "pan", barmode: "overlay",
      xaxis: { ...UW.THEME.xaxis, ...yearTicks(lo - pad, hi + pad), range: [lo - pad, hi + pad], zeroline: false, title: { text: "year", font: { size: 12 } }, tickfont: { size: 12 } },
      yaxis: { ...UW.THEME.yaxis, type: "category", categoryorder: "array", categoryarray: cats.slice().reverse(), tickfont: { size: 11 }, fixedrange: true } };
    Plotly.react(gd, traces, layout, UW.CFG).then((g) => { UW.axisZoom(g); g.removeAllListeners?.("plotly_click"); g.on("plotly_click", (ev) => { const k = ev.points?.[0]?.customdata; if (k != null) showEventRow(k); }); });
  }
  function showEventRow(key) {
    const host = $("#histevents"), row = host?.querySelector(`tr[data-key="${key}"]`);
    if (!row) return;
    for (const x of host.querySelectorAll("tr.on")) x.classList.remove("on");
    row.classList.add("on");
    row.scrollIntoView({ block: "center", behavior: "smooth" });
    if (row.dataset.lat) focusPoint(row.dataset.lat, row.dataset.lon, row.children[1]?.textContent);
  }
  function wireEvents(el) {
    const b = $("#histearlier"); if (b) b.onclick = () => { hist.earlier = !hist.earlier; renderMain(); };
    $("#histplotreset").onclick = () => { const gd = $("#histplot"); if (gd?.data) Plotly.relayout(gd, { "xaxis.autorange": true }); };
    drawEvents();
  }

  // ---------------------------------------------------------------- the bibliography
  // Every work cited, in MLA form, with the whole list to take away as
  // BibTeX (the build's own file), RIS, CSL-JSON or plain text.
  const RIS_TYPE = { article: "JOUR", book: "BOOK", incollection: "CHAP", inbook: "CHAP", inproceedings: "CONF", phdthesis: "THES", mastersthesis: "THES", techreport: "RPRT", online: "ELEC", misc: "GEN", unpublished: "UNPB" };
  const CSL_TYPE = { article: "article-journal", book: "book", incollection: "chapter", inbook: "chapter", inproceedings: "paper-conference", phdthesis: "thesis", mastersthesis: "thesis", techreport: "report", online: "webpage", misc: "document", unpublished: "manuscript" };
  const authorsOf = (e) => (e.author || "").split(/\s+and\s+/).map((a) => a.trim()).filter(Boolean)
    .map((a) => a.includes(",") ? { family: a.split(",")[0].trim(), given: a.split(",").slice(1).join(",").trim() } : { family: a.split(" ").slice(-1)[0], given: a.split(" ").slice(0, -1).join(" ") });
  function ris(bib) {
    return bib.map((e) => {
      const L = [["TY", RIS_TYPE[e.type] || "GEN"], ["ID", e.key], ...authorsOf(e).map((a) => ["AU", a.given ? `${a.family}, ${a.given}` : a.family]), ["TI", e.title], ["T2", e.journal || e.booktitle], ["VL", e.volume], ["IS", e.number],
        ["SP", (e.pages || "").split(/-+/)[0]], ["EP", (e.pages || "").split(/-+/)[1]], ["PY", e.year], ["PB", e.publisher], ["CY", e.address], ["UR", e.url], ["N1", e.note]];
      return L.filter(([, v]) => v).map(([k, v]) => `${k}  - ${v}`).join("\r\n") + "\r\nER  - \r\n";
    }).join("\r\n");
  }
  function csl(bib) {
    return JSON.stringify(bib.map((e) => {
      const o = { id: e.key, type: CSL_TYPE[e.type] || "document", title: e.title };
      const au = authorsOf(e); if (au.length) o.author = au;
      if (e.journal || e.booktitle) o["container-title"] = e.journal || e.booktitle;
      for (const [k, v] of [["volume", e.volume], ["issue", e.number], ["page", e.pages], ["publisher", e.publisher], ["publisher-place", e.address], ["URL", e.url], ["note", e.note]]) if (v) o[k] = v;
      if (e.year && /^\d{1,4}$/.test(e.year)) o.issued = { "date-parts": [[+e.year]] };
      return o;
    }), null, 1);
  }
  function download(name, text, mime) {
    const url = URL.createObjectURL(new Blob([text], { type: mime }));
    const a = Object.assign(document.createElement("a"), { href: url, download: name }); document.body.appendChild(a); a.click(); a.remove();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
  }
  async function renderBib(el) {
    const bib = await bibliography();
    const sorted = [...bib].sort((a, b) => (a.author || a.title || "").localeCompare(b.author || b.title || ""));
    el.innerHTML = crumb("Bibliography") + `<h2>Bibliography <span class="muted">${sorted.length} works</span></h2>` +
      `<div class="bibexport"><span class="lbl">Export</span><a class="chip small" href="data/history/references.bib" download title="the build's own BibTeX file">BibTeX</a><button type="button" class="chip small" data-export="ris" title="RIS, for EndNote, Zotero and Mendeley">RIS</button><button type="button" class="chip small" data-export="csl" title="CSL-JSON, for citation processors and Zotero">CSL-JSON</button><button type="button" class="chip small" data-export="txt" title="the MLA list as plain text">Plain text</button></div>` +
      `<ol class="mla">${sorted.map((e) => `<li id="bib-${esc(e.key)}">${mla(e)} <a class="muted small" href="#history/source/${esc(e.key)}" data-slug="source/${esc(e.key)}">page</a></li>`).join("")}</ol>`;
    for (const b of el.querySelectorAll("button[data-export]")) b.onclick = () => {
      const stamp = (UW.M.history?.stamp || "").slice(0, 10) || "latest";
      if (b.dataset.export === "ris") download(`arctic-history-${stamp}.ris`, ris(sorted), "application/x-research-info-systems");
      else if (b.dataset.export === "csl") download(`arctic-history-${stamp}.json`, csl(sorted), "application/vnd.citationstyles.csl+json");
      else download(`arctic-history-${stamp}.txt`, sorted.map((e) => { const d = document.createElement("div"); d.innerHTML = mla(e); return d.textContent; }).join("\n\n") + "\n", "text/plain");
    };
  }

  // ---------------------------------------------------------------- the chips
  // The kinds, twice: in the pane's tools each chip opens that kind's page;
  // under the map's layer pills, while the history layer is on, each is a
  // filter of what the map shows.
  function chipsHTML(kinds, onOf, cls = "") {
    return Object.entries(kinds).map(([k, t]) => `<button type="button" data-t="${k}" class="${cls} ${onOf(k) ? "on" : ""}" title="${t.label}"><span class="dot" style="background:${t.colour}"></span>${t.label}</button>`).join("");
  }
  function renderChips() {
    const pane = $("#histkinds");
    if (pane) {
      pane.innerHTML = chipsHTML(KINDS, (k) => hist.slug === `kind/${k}`, "chip");
      for (const b of pane.querySelectorAll("button[data-t]")) b.onclick = () => open(hist.slug === `kind/${b.dataset.t}` ? "" : `kind/${b.dataset.t}`);
    }
    const bar = $("#maphistlayers");
    if (bar) {
      bar.innerHTML = chipsHTML(TYPES, (k) => hist.types.has(k));
      for (const b of bar.querySelectorAll("button[data-t]")) b.onclick = () => {
        if (hist.types.has(b.dataset.t)) hist.types.delete(b.dataset.t); else hist.types.add(b.dataset.t);
        store.set("hist.types", [...hist.types]); renderChips(); if (UW.state.history) UW.renderMap();
      };
      bar.hidden = !UW.state.history || !UW.M.history;
    }
  }
  function renderTools() {
    $("#histexplore").classList.toggle("on", hist.slug === "explore");
    $("#histbibchip").classList.toggle("on", hist.slug === "bib");
    $("#histback").disabled = !hist.slug && nav.n === 0;
    const mt = $("#maptoggle"); if (mt) $("#histmap").textContent = mt.textContent;
  }
  async function render() {
    renderChips(); renderTools();
    await renderMain();
    if (UW.state.history) UW.renderMap();
  }

  // ---------------------------------------------------------------- navigation
  // Every view is a browser history entry (#history/<slug>), so the browser's
  // back button walks back through the pages and the pane's Back button does
  // the same; past the first view it goes home rather than off the site.
  const nav = { n: 0 };                                   // how many views this visit has pushed
  const ALIAS = { timeline: "kind/event", people: "kind/people" };
  function open(slug, opts = {}) {
    slug = ALIAS[slug] || slug;
    hist.slug = slug; store.set("hist.slug", slug);
    hist.more = { today: false, here: false };
    if (!opts.pop) { nav.n++; try { history.pushState({ hist: slug, n: nav.n }, "", `#history/${slug}`); } catch {} }
    if (!opts.quiet && $("#pane-history").hidden) UW.showTab("history");
    render();
  }
  window.addEventListener("popstate", (e) => {
    const s = e.state?.hist ?? (location.hash.startsWith("#history/") ? decodeURIComponent(location.hash.slice(9)) : null);
    if (s == null) { nav.n = 0; if (!$("#pane-history").hidden) { hist.slug = ""; store.set("hist.slug", ""); render(); } return; }
    nav.n = e.state?.n ?? 0;
    open(s, { pop: true });
  });
  function goBack() {
    if (nav.n > 0 && history.state?.hist != null) history.back();
    else open("");
  }
  function focusPoint(lat, lon, label) {
    if (lat == null) return;
    if (!UW.state.history) { UW.state.history = true; store.set("history", true); document.querySelector('#maplayers button[data-layer="history"]')?.classList.add("on"); }
    UW.focusMap(+lat, +lon, label || "");
    if (UW.mapMode?.() === "none") UW.setMapMode("half");
  }
  function focusArtifact(a) {
    if (!a || a.lat == null) return;
    if (a.type === "track" && a.geometry?.coordinates?.length) {
      const c = a.geometry.coordinates; const mid = c[Math.floor(c.length / 2)];
      focusPoint(mid[1], mid[0], a.title);
    } else focusPoint(a.lat, a.lon, a.title);
  }
  document.addEventListener("click", (e) => {
    const pin = e.target.closest("#pane-history .pin[data-lat]");
    if (pin) { e.preventDefault(); e.stopPropagation(); focusPoint(pin.dataset.lat, pin.dataset.lon, pin.dataset.label); return; }
    const a = e.target.closest("#pane-history a[data-slug], #pane-history a[data-topic]");
    if (!a) return;
    e.preventDefault();
    if (a.classList.contains("vig")) {
      if (a.dataset.lat) { const art = a.dataset.slug ? artifactById(a.dataset.slug.replace(/^artifact\//, "")) : null; if (art) focusArtifact(art); else focusPoint(a.dataset.lat, a.dataset.lon, a.querySelector(".txt")?.textContent || ""); }
      if (a.dataset.slug) open(a.dataset.slug); else if (a.dataset.topic) open(`topic/${a.dataset.topic}`);
      return;
    }
    if (a.dataset.topic != null) open(`topic/${a.dataset.topic}`);
    else open(a.dataset.slug || "");
  });
  UW.onHistoryClick = (id) => { const a = artifactById(id.split("|")[0]); if (a) open(a.page); };

  // ---------------------------------------------------------------- the map layer
  const prevExtra = UW.extraMapTraces;
  UW.extraMapTraces = () => {
    const out = prevExtra ? prevExtra() : [];
    if (!UW.state.history || !hist.artifacts) { if (UW.state.history && !hist.artifacts) ensure().then(() => UW.renderMap()); return out; }
    const shown = shownArtifacts();
    const year = (a) => { const y = yearOf(a.date_start); return y == null ? "" : ` · ${yearLabel(y)}`; };
    const hover = (a) => `${esc(a.title.length > 60 ? a.title.slice(0, 57) + "…" : a.title)}${year(a)}`;
    for (const a of shown.filter((x) => x.type === "track" && x.geometry?.coordinates?.length > 1)) {
      const c = a.geometry.coordinates, col = topicColour(a.topic);
      out.push({ type: "scattermap", mode: "lines", name: `hist-${a.id}`, showlegend: false, hoverinfo: "text",
        lat: c.map((p) => p[1]), lon: c.map((p) => p[0]), text: c.map(() => hover(a)), customdata: c.map(() => `hist:${a.id}`),
        line: { width: 2.4, color: col }, opacity: .85 });
      if (a.waypoints?.length) out.push({ type: "scattermap", mode: "markers", name: `hist-${a.id}-wp`, showlegend: false, hoverinfo: "text",
        lat: a.waypoints.map((w) => w.lat), lon: a.waypoints.map((w) => w.lon),
        text: a.waypoints.map((w) => `${esc(dateLabel(w.date || ""))}${w.note ? " · " + esc(w.note.length > 50 ? w.note.slice(0, 47) + "…" : w.note) : ""}`),
        customdata: a.waypoints.map((w) => `hist:${a.id}|${w.date || ""}`), marker: { size: 6, color: col, opacity: .9 } });
    }
    const pins = shown.filter((x) => x.type !== "track" && x.lat != null);
    if (pins.length) out.push({ type: "scattermap", mode: "markers", name: "history", showlegend: false, hoverinfo: "text",
      lat: pins.map((a) => a.lat), lon: pins.map((a) => a.lon), text: pins.map(hover), customdata: pins.map((a) => `hist:${a.id}`),
      marker: { size: pins.map((a) => a.type === "event" ? 11 : 9), color: pins.map((a) => TYPES[a.type]?.colour || "#8b9bb0"), opacity: .92 } });
    return out;
  };

  // ---------------------------------------------------------------- Ada
  UW.historyContext = () => (hist.slug && !["explore", "bib"].includes(hist.slug) && !/^(topic|kind)\//.test(hist.slug)) ? hist.slug : "";
  UW.historyOpen = (slug) => open(slug);

  // ---------------------------------------------------------------- wiring
  function wire() {
    $("#histsearch").oninput = debounce((e) => { hist.search = e.target.value; renderMain(); }, 150);
    $("#histback").onclick = goBack;
    $("#histexplore").onclick = () => open(hist.slug === "explore" ? "" : "explore");
    $("#histbibchip").onclick = () => open(hist.slug === "bib" ? "" : "bib");
    $("#histask").onclick = () => UW.chatRoom?.("ada");
    // the Map chip is the header's map pill, where the pane's tools are
    const mt = $("#maptoggle");
    if (mt) { $("#histmap").onclick = () => mt.click(); new MutationObserver(() => { $("#histmap").textContent = mt.textContent; }).observe(mt, { childList: true, characterData: true, subtree: true }); }
    const pill = document.querySelector('#maplayers button[data-layer="history"]');
    if (pill) { pill.hidden = !UW.M.history; pill.addEventListener("click", () => setTimeout(renderChips, 0)); }
    hist.slug = ALIAS[hist.slug] || hist.slug;
    // a link into the history (#history/<slug>) opens that view
    if (location.hash.startsWith("#history/")) { hist.slug = decodeURIComponent(location.hash.slice(9)); store.set("hist.slug", hist.slug); try { history.replaceState({ hist: hist.slug, n: 0 }, "", location.hash); } catch {} }
  }
  // On the History tab the map is the history's: the ship's own layers step
  // aside and come back when the tab is left; plan, places and history stay.
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
    renderChips();
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
    if (!$("#pane-history").hidden || UW.state.history) ensure().then(() => { if (!$("#pane-history").hidden) render(); else { renderChips(); UW.renderMap(); } }).catch(() => {}); };
  wire();
  if (location.hash.startsWith("#history/") && $("#pane-history").hidden) UW.showTab("history");
  else if (document.querySelector("#tabs button.on")?.dataset.tab === "history") UW.onTab("history");
  else if (UW.state.history && UW.M.history) ensure().then(() => { renderChips(); UW.renderMap(); }).catch(() => {});
})();
