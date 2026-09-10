/* The Wiki tab: the region's past and its nature on the map, in a wiki, on
 * a timeline, with the crew in the chat to ask. Everything it shows was
 * published by the build from the arctic-history database into
 * data/history/: an index of pages and topics, the artifacts with their
 * positions and tracks, the atomic dates, the people and places, the
 * bibliography, and one JSON file per wiki page. A topic carries a domain,
 * "history" or "nature", and the pane's two labels choose which domains it
 * lists, one or both. This file owns the pane, its router and the human
 * half's views; nature.js (loaded next) adds the natural half's own views
 * (the subjects, the observations, the domains, the ship's journal) through
 * UW.natureViews, and each domain has a map layer of its own. Loaded after
 * tabs.js; talks to app.js through window.UW like the other panes. */
(() => {
  "use strict";
  const UW = window.UW;
  const $ = (s) => document.querySelector(s);
  const { store, C, fz } = UW;
  const esc = (s) => String(s ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  const cachedJSON = window.UWData.generationCache(UW.fetchJSON, () => UW.M.history?.stamp || "");
  const debounce = (f, ms) => { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => f(...a), ms); }; };

  // artifact kinds: a colour each, for the map and the chips — an index into
  // the theme's palette, read when used so a theme change recolours them
  const kind = (label, i) => ({ label, get colour() { return C.palette[i]; } });
  const TYPES = {
    track: kind("Tracks", 1),
    event: kind("Events", 3),
    place: kind("Places", 2),
    image: kind("Images", 0),
    map:   kind("Maps", 6),
    quote: kind("Quotes", 4),
    text:  kind("Texts", 10),
    object: kind("Objects", 5),
  };
  // the pane's chips: one page per kind, People among them
  const KINDS = { people: kind("People", 7), object: TYPES.object, quote: TYPES.quote, text: TYPES.text, place: TYPES.place, track: TYPES.track, map: TYPES.map, event: TYPES.event, image: TYPES.image,
    animal: kind("Animals", 9), vessel: kind("Vessels", 8) };
  // the kinds in their hierarchy and in the Browse menu's order, wherever
  // the chips appear: the animals and what people made and said sit under
  // People, the vessels and what lies on the ground under Places
  const GROUPS = [
    { head: "image", under: [] },
    { head: "event", under: [] },
    { head: "people", under: ["animal", "object", "quote", "text"] },
    { head: "place", under: ["vessel", "track", "map"] },
  ];
  // the artifact kinds whose page is a grid of collections, one a topic
  const COLLECTED = new Set(["image", "map", "quote", "text", "object", "track"]);
  const DOMAINS_ALL = ["history", "nature"];                  // the pane's two labels
  const KIND_LABEL = { page: "explore" };                  // a narrative page is an Explore page on the site
  const kindLabel = (k) => KIND_LABEL[k] || k;
  // a page's standing with the crew: marked while it is a draft, unmarked once checked
  const statusTag = (s) => s === "draft" ? `<span class="status draft" title="the crew are still at work on this page">draft</span>` : "";
  const TOPIC_ORDER = [1, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];   // topics take the palette amber first
  const TIMELINE_FROM = 1400;                              // the Events chart opens zoomed to here; the table and the chart hold everything
  const VIG_N = 5;                                         // vignette lines shown before "see more"

  const hist = {
    index: null, artifacts: null, timeline: null, places: [], people: [], events: [], animals: [], vessels: [], bib: null, stamp: null, loading: null,
    flags: new Map(),                                     // artifact id → the flag anyone has raised for review (shared through the server)
    names: null,                                          // every person and place name that has a page, longest first, for the cross-links
    slug: store.get("wiki.slug", ""),                     // what is shown: "" home, explore, bib, provenance, kind/<k>[/<topic>], topic/<t>, one of the natural half's own views, or a page
    domains: new Set(store.get("wiki.domains", DOMAINS_ALL).filter((d) => DOMAINS_ALL.includes(d))),   // the labels on: the domains the pane lists
    types: new Set(store.get("hist.types", Object.keys(TYPES))),   // the kinds the map layers show
    search: "",
    faces: null,                                          // the face crops the backend publishes, or null
    coast: null,                                          // the coastline (Natural Earth) as lines, for the route sketches
    more: { today: false, here: false },                  // the vignettes unfolded
    pages: new Map(),                                     // slug -> page JSON, this generation
  };
  // A topic carries a domain, "history" or "nature". The pane lists the
  // documents (the pages, artifacts, people, places, events and dates of the
  // topics) of the domains its labels have on; each domain's map layer draws
  // its own. `ns` names what is being rendered, the pane or one layer; every
  // link is #wiki/<slug>.
  if (!hist.domains.size) hist.domains = new Set(DOMAINS_ALL);
  const NS = {
    wiki: { key: "wiki", domains: () => hist.domains },
    history: { key: "history", domains: () => new Set(["history"]) },   // `key` names a layer's traces
    nature: { key: "nature", domains: () => new Set(["nature"]) },
  };
  const LAYERS = ["history", "nature"];                       // the map layers, one a domain
  let ns = NS.wiki;
  const topicDomain = (slug) => hist.index?.topics.find((t) => t.slug === slug)?.domain || "history";
  // a row belongs to the domain its topic has; a row without a topic is history's
  const inDomain = (x) => ns.domains().has(x.topic ? topicDomain(x.topic) : "history");
  const domainOn = (d) => hist.domains.has(d);
  const domainsOn = () => DOMAINS_ALL.filter(domainOn);
  const arts = () => (hist.artifacts || []).filter(inDomain);
  const topics = () => (hist.index?.topics || []).filter((t) => ns.domains().has(t.domain || "history"));
  const pagesOf = () => (hist.index?.pages || []).filter(inDomain);
  // which pane a page belongs to, by its slug: the natural half's own kinds,
  // else the domain of the topic that owns it
  function pageDomain(slug) {
    if (/^(subject|observation)\//.test(slug)) return "nature";
    if (slug.startsWith("artifact/")) { const a = artifactById(slug.slice(9)); return a ? topicDomain(a.topic) : "history"; }
    if (slug.startsWith("event/")) { const e = eventById(slug.slice(6)); return e ? topicDomain(e.topic) : "history"; }
    if (slug.startsWith("topic/")) return topicDomain(slug.slice(6));
    const p = hist.index?.pages.find((x) => x.slug === slug);
    return p ? (p.topic ? topicDomain(p.topic) : "history") : "history";
  }

  // ---------------------------------------------------------------- data
  async function ensure() {
    if (!UW.M.history) { hist.index = null; return false; }
    if (hist.stamp === UW.M.history.stamp && hist.index) return true;
    if (hist.loading) return hist.loading;
    hist.loading = (async () => {
      const maybe = (k, u) => cachedJSON(k, u).catch(() => null);
      const [index, arts, tl, pl, pe, fa, ev, an, ve, pv] = await Promise.all([
        cachedJSON("index", "data/history/index.json"),
        cachedJSON("artifacts", "data/history/artifacts.json"),
        cachedJSON("timeline", "data/history/timeline.json"),
        maybe("places", "data/history/places.json"),
        maybe("people", "data/history/people.json"),
        maybe("faces", "data/history/faces.json"),
        maybe("events", "data/history/events.json"),
        maybe("animals", "data/history/animals.json"),
        maybe("vessels", "data/history/vessels.json"),
        maybe("provenance", "data/history/provenance.json"),
      ]);
      hist.animals = an?.animals || []; hist.vessels = ve?.vessels || []; hist.provenance = pv || null;
      hist.index = index;
      hist.artifacts = arts.artifacts || []; hist.timeline = tl.timeline || [];
      hist.places = pl?.places || []; hist.people = pe?.people || []; hist.faces = fa?.faces || null; hist.events = ev?.events || [];
      hist.stamp = UW.M.history.stamp; hist.pages = new Map(); hist.bib = null; hist.names = null;
      for (const a of hist.artifacts) a._year = yearOf(a.date_start);
      await loadFlags();
      return true;
    })().finally(() => { hist.loading = null; });
    return hist.loading;
  }
  // both halves: the natural half's files come through nature.js, once it has loaded
  const ensureAll = () => Promise.all([ensure(), UW.natureViews?.ensure?.() ?? null]).then(([ok]) => ok);
  // the review flags: raised on an artifact's card by anyone, with a note,
  // and kept on the server so every browser shows the same ones; the alerts
  // timer reports them to the keeper. Whoever raised a flag withdraws it
  // while theirs is the only one; once several people have, only an admin.
  const me = () => ({ token: store.get("chat.token", ""), name: store.get("chat.name", "") });
  function takeFlags(r) {
    hist.flags = new Map((r?.flags || []).map((f) => [f.id, f])); hist.admin = !!r?.admin;
    for (const el of document.querySelectorAll("#pane-wiki .flag[data-flag]")) {
      const f = hist.flags.get(el.dataset.flag);
      el.classList.toggle("on", !!f); el.title = flagTitle(f); el.textContent = flagText(f);
    }
  }
  async function loadFlags() {
    const { token, name } = me();
    try { const r = await fetch(`/api/history/flags?token=${encodeURIComponent(token)}&name=${encodeURIComponent(name)}`, { cache: "no-store" }); if (r.ok) takeFlags(await r.json()); }
    catch { /* the flags are a convenience; the page stands without them */ }
  }
  const flagText = (f) => "⚑" + (f && f.raisers.length > 1 ? f.raisers.length : "");
  function flagTitle(f) {
    if (!f) return "Flag for review";
    const by = f.raisers.map((r) => (r.who || "someone") + (r.note ? ": " + r.note : "")).join("; ");
    const can = hist.admin || (f.mine && f.raisers.length === 1);
    return `Flagged for review by ${by} · ${can ? "click to withdraw" : f.mine ? "only an admin can withdraw it now" : "click to add your own flag"}`;
  }
  const flagMark = (a) => { const f = hist.flags.get(a.id); return `<span class="flag ${f ? "on" : ""}" role="button" tabindex="0" data-flag="${esc(a.id)}" title="${esc(flagTitle(f))}">${flagText(f)}</span>`; };
  async function toggleFlag(id) {
    const a = artifactById(id); if (!a) return;
    const f = hist.flags.get(id), { token, name } = me();
    let on, note = "";
    if (!f || !(hist.admin || f.mine)) {                      // not flagged, or flagged by others: add this device's flag
      on = true;
      note = window.prompt(`${f ? "Also flag" : "Flag"} "${a.title}" for review.\nWhat should be looked at? (optional)`, "");
      if (note === null) return;
    } else if (hist.admin && f.raisers.length > 1) {
      on = false;
      if (!window.confirm(`Withdraw the flags ${f.raisers.length} people have raised on "${a.title}"?`)) return;
    } else if (f.mine && f.raisers.length > 1) { UW.toast?.(flagTitle(f)); return; }
    else on = false;
    try {
      const r = await fetch("/api/history/flag", { method: "POST", headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ id, on, token, name, title: a.title, page: a.page, note }) });
      const j = await r.json();
      if (!r.ok) throw new Error(j.error || r.status);
      takeFlags(j);
    } catch (e) { UW.toast?.(`The flag could not be saved: ${e.message || e}`); }
  }
  // the coastline under the route sketches: fetched once, as arrays of [lon, lat]
  let coastLoading = null;
  function ensureCoast() {
    if (hist.coast) return Promise.resolve(hist.coast);
    if (coastLoading) return coastLoading;
    coastLoading = UW.fetchJSON("static/geo/coastline.geojson").then((g) => {
      const lines = [];
      for (const f of g.features || []) { const gm = f.geometry; if (!gm) continue; if (gm.type === "LineString") lines.push(gm.coordinates); else if (gm.type === "MultiLineString") lines.push(...gm.coordinates); }
      hist.coast = lines; return lines;
    }).catch(() => { hist.coast = []; return []; }).finally(() => { coastLoading = null; });
    return coastLoading;
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
  const topicColour = (slug) => C.palette[TOPIC_ORDER[Math.max(0, hist.index?.topics.findIndex((t) => t.slug === slug) || 0) % TOPIC_ORDER.length]];
  const artifactById = (id) => hist.artifacts?.find((a) => a.id === id);
  const artifactByUrl = (url) => url ? hist.artifacts?.find((a) => a.url === url || a.thumb === url || a.original_url === url) : null;
  const eventById = (id) => hist.events?.find((e) => String(e.id) === String(id));
  const eventSlug = (id) => eventById(id) ? `event/${id}` : "";
  // a fixed order that looks like none: the same for everyone, every visit
  const mixKey = (s) => { let h = 2166136261; for (const ch of String(s)) { h ^= ch.charCodeAt(0); h = Math.imul(h, 16777619) >>> 0; } return h; };
  const byMix = (p, q) => mixKey(p.id) - mixKey(q.id) || p.id.localeCompare(q.id);
  const topicImage = (slug) => hist.artifacts?.find((a) => a.topic === slug && a.url && (a.type === "image" || a.type === "map"));
  const placeByPage = (slug) => hist.places.find((p) => p.page === slug);
  // the topic in force: the one whose page is open, else none
  const curTopic = () => { const s = hist.slug; return s.startsWith("topic/") ? s.slice(6) : ""; };

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
    if (kind === "event") return eventSlug(id);
    if (kind === "observation") return `observation/${id}`;          // the natural half's atom: the Nature tab shows it
    return hist.index?.pages.find((p) => p.kind === kind && p.title === id)?.slug || "";
  }
  function onThisDay(now = new Date()) {
    if (!hist.timeline) return [];
    const mm = now.getMonth() + 1, dd = now.getDate(), out = [];
    for (const r of hist.timeline) {
      const a = parts(r.date);
      if (!a || r.precision !== "day" || !inDomain(r)) continue;
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
          const len = Math.round((Date.UTC(b.y, b.m - 1, b.d) - Date.UTC(a.y, a.m - 1, a.d)) / 864e5) + 1;
          if (day < 7 || len - day <= 7) out.push({ year: y, kind: `day ${day + 1} of ${len}`, label: r.label, place: r.place, topic: r.topic, lat: r.lat, lon: r.lon, slug: pageFor(r.entity_kind, r.entity_id) });
        }
      }
    }
    for (const a of arts()) {
      if (a.type !== "track" || !a.waypoints) continue;
      for (const w of a.waypoints) {
        const d = parts(w.date);
        if (d && d.m === mm && d.d === dd) out.push({ year: d.y, kind: "was here", label: a.title, place: w.note || "", topic: a.topic, lat: w.lat, lon: w.lon, slug: a.page });
      }
    }
    for (const p of [...(hist.people || []), ...(hist.animals || [])].filter(inDomain)) {
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
    for (const a of arts()) {
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
      if (e.lat == null || e.entity_kind !== "event" || !inDomain(e)) continue;
      cand.push({ d: km(lat, lon, e.lat, e.lon), e, when: e.date + (e.date_end ? " → " + e.date_end : ""), note: e.place || "", lat: e.lat, lon: e.lon });
    }
    for (const p of (hist.places || []).filter(inDomain)) {
      if (p.lat == null) continue;
      const names = [p.inuktitut, p.historic].filter((n) => n && n !== p.name).join(", ");
      cand.push({ d: km(lat, lon, p.lat, p.lon), p, when: p.kind || "place", note: names, lat: p.lat, lon: p.lon });
    }
    cand.sort((p, q) => p.d - q.d);
    const seen = new Set();
    const items = cand.filter((c) => c.d <= 600).filter((c) => { const k = c.a ? c.a.id : c.p ? `pl:${c.p.name}` : `ev:${c.e.entity_id}`; if (seen.has(k)) return false; seen.add(k); return true; }).slice(0, 20)
      .map((c) => c.a ? { d: c.d, label: c.a.title, when: c.when, note: c.note, topic: c.a.topic, type: c.a.type, slug: c.a.page, lat: c.lat, lon: c.lon }
                : c.p ? { d: c.d, label: c.p.name, when: c.when, note: c.note, topic: c.p.topic, type: "place", slug: c.p.page, lat: c.lat, lon: c.lon }
                      : { d: c.d, label: c.e.label, when: c.when, note: c.note, topic: c.e.topic, type: "event", slug: eventSlug(c.e.entity_id), lat: c.lat, lon: c.lon });
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
    const item = (x, extra) => `<a class="vig" href="#wiki/${esc(x.slug)}" data-slug="${esc(x.slug)}" data-lat="${x.lat ?? ""}" data-lon="${x.lon ?? ""}" data-topic="${esc(x.topic)}"><span class="dot" style="background:${topicColour(x.topic)}"></span>${extra}<span class="txt">${esc(x.label)}${x.kind ? ` <i>${esc(x.kind)}</i>` : x.type === "place" && x.when ? ` <i>${esc(x.when)}</i>` : ""}${x.note ? ` <span class="muted">${esc(x.note)}</span>` : ""}${x.place && !x.note ? ` <span class="muted">${esc(x.place)}</span>` : ""}${x.lat != null ? ` <span class="pin" title="on the map">⌖</span>` : ""}</span></a>`;
    const fold = (xs, key, line) => {
      const shown = hist.more[key] ? xs : xs.slice(0, VIG_N), rest = xs.length - shown.length;
      return shown.map(line).join("") + (rest > 0 ? `<button type="button" class="more" data-more="${key}">see ${rest} more</button>` : hist.more[key] && xs.length > VIG_N ? `<button type="button" class="more" data-more="${key}">see fewer</button>` : "");
    };
    const todayShown = hist.more.today ? today : pickForDay(today, VIG_N, now);
    const foldPicked = (all, shown, key, line) => shown.map(line).join("") +
      (all.length > shown.length ? `<button type="button" class="more" data-more="${key}">see ${all.length - shown.length} more</button>` : hist.more[key] && all.length > VIG_N ? `<button type="button" class="more" data-more="${key}">see fewer</button>` : "");
    const own = `<section class="vigcard"><h3>On this day · ${esc(dateWord)}</h3>${today.length ? foldPicked(today, todayShown, "today", (x) => item(x, `<b>${esc(yearLabel(x.year))}</b>`)) : `<div class="muted small">Nothing dated to the day on ${esc(dateWord)} yet.</div>`}</section>
      <section class="vigcard"><h3>In this place${here.name ? ` · ${esc(here.name)}` : ""}</h3>${here.items.length ? fold(here.items, "here", (x) => item(x, `<b>${Math.round(x.d)} km</b>`)) : `<div class="muted small">${pos.lat == null ? "The ship's position is not known to this build." : "Nothing in the wiki near the ship yet."}</div>`}</section>`;
    // the observations' two cards beside the documents', while Nature is on
    const obs = domainOn("nature") ? (UW.natureViews?.vignetteCards?.() || "") : "";
    return `<div class="vignettes">${own}${obs}</div>`;
  }

  // the artifacts on the map: the pane's own, of the kinds the map's chips allow, within the open topic
  function shownArtifacts() {
    if (!hist.artifacts) return [];
    const t = curTopic();
    return arts().filter((a) => (!t || a.topic === t) && hist.types.has(a.type));
  }
  // the map layers that are on, each in its own namespace
  function eachLayer(fn) {
    const prev = ns;
    try { for (const key of LAYERS) if (UW.state[key]) { ns = NS[key]; fn(key); } }
    finally { ns = prev; }
  }
  const byYear = (p, q) => ((p._year ?? 9e9) - (q._year ?? 9e9)) || p.title.localeCompare(q.title);

  // ---------------------------------------------------------------- markdown
  function inline(s) {
    s = esc(s);
    s = s.replace(/!\[([^\]]*)\]\(([^)\s]+)\)/g, (m, alt, src) => {
      const img = `<img src="${src}" alt="${alt}" loading="lazy">`, a = artifactByUrl(src);
      return a ? `<a class="imglink" href="#wiki/${esc(a.page)}" data-slug="${esc(a.page)}" title="${esc(a.title)}">${img}</a>` : img;
    });
    s = s.replace(/\[([^\]]+)\]\(([^)\s]+)\)/g, (m, label, href) =>
      /^https?:\/\//.test(href) ? `<a href="${href}" target="_blank" rel="noopener">${label}</a>` : `<a href="#wiki/${href}" data-slug="${href}">${label}</a>`);
    s = s.replace(/&lt;span class=&quot;wanted&quot; title=&quot;no page yet&quot;&gt;(.*?)&lt;\/span&gt;/g, '<span class="wanted">$1</span>');
    s = s.replace(/`([^`]+)`/g, "<code>$1</code>").replace(/\*\*([^*]+)\*\*/g, "<b>$1</b>").replace(/\*([^*]+)\*/g, "<i>$1</i>");
    s = s.replace(/(-?\d{1,2}\.\d{2,6}),\s*(-?\d{1,3}\.\d{2,6})(?![\d.])/g, (m, la, lo) => coordLink(la, lo));
    return s;
  }
  // a pair of coordinates as a link that puts the map there
  // (spans, not anchors: they sit inside cards and rows that are links themselves)
  const coordLink = (lat, lon, label = "") => `<span class="pin coord mono" role="link" tabindex="0" data-lat="${lat}" data-lon="${lon}" data-label="${esc(label)}" title="on the map">${(+lat).toFixed(3)}, ${(+lon).toFixed(3)}</span>`;
  const mapLink = (lat, lon, label = "", type = "") => `<span class="pin maplink" role="link" tabindex="0" data-lat="${lat}" data-lon="${lon}" data-label="${esc(label)}" data-type="${esc(type)}" title="on the map">map ↗</span>`;
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
      // a wrapped list item continues on the next line, as in Markdown
      if (list && !para.length) { list.items[list.items.length - 1] += " " + line.trim(); continue; }
      if (quote.length || table) flush();
      para.push(line.trim());
    }
    flush();
    return out.join("\n");
  }

  // ---------------------------------------------------------------- the pane
  const fmtDate = (a) => dateLabel(a.date_text || a.date_start || "");
  const crumb = (...rest) => `<div class="crumb"><a href="#wiki/" data-slug="">Wiki</a>${rest.map((r) => ` › ${r}`).join("")}</div>`;
  // the last step of a crumb: the page itself, as its own link (the address to pass on)
  const here = (label, slug) => `<a class="here" href="#wiki/${esc(slug)}" data-slug="${esc(slug)}" title="this page's address">${label}</a>`;
  // a route as a small drawing: the line in a box, north up, longitudes
  // shrunk by the cosine of the latitude so the shape is roughly right
  // the coastline is drawn beneath it when it has loaded (ensureCoast)
  function trackSketch(a, W = 120, H = 72, cls = "sketch") {
    const c = a.geometry?.coordinates; if (!c || c.length < 2) return "";
    const lat0 = c.reduce((s, p) => s + p[1], 0) / c.length, k = Math.cos(lat0 * Math.PI / 180);
    const xs = c.map((p) => p[0] * k), ys = c.map((p) => p[1]);
    let x0 = Math.min(...xs), x1 = Math.max(...xs), y0 = Math.min(...ys), y1 = Math.max(...ys);
    // some room around the route, and a box at least a degree or so across
    const mx = Math.max((x1 - x0) * .18, .6), my = Math.max((y1 - y0) * .18, .4);
    x0 -= mx; x1 += mx; y0 -= my; y1 += my;
    const sc = Math.min(W / (x1 - x0), H / (y1 - y0));
    const ox = (W - (x1 - x0) * sc) / 2, oy = (H - (y1 - y0) * sc) / 2;
    const X = (x) => (ox + (x - x0) * sc).toFixed(1), Y = (y) => (oy + (y1 - y) * sc).toFixed(1);
    let coast = "";
    if (hist.coast) {
      const lo0 = x0 / k, lo1 = x1 / k, runs = [];
      for (const line of hist.coast) {
        let run = [];
        for (const p of line) {
          if (p[0] >= lo0 && p[0] <= lo1 && p[1] >= y0 && p[1] <= y1) run.push(`${X(p[0] * k)},${Y(p[1])}`);
          else if (run.length) { if (run.length > 1) runs.push(run.join(" ")); run = []; }
        }
        if (run.length > 1) runs.push(run.join(" "));
      }
      coast = runs.map((r) => `<polyline points="${r}" fill="none" stroke="${C.sketchCoast}" stroke-width="1"/>`).join("");
    }
    const pts = c.map((p, i) => `${X(xs[i])},${Y(ys[i])}`).join(" ");
    const col = topicColour(a.topic);
    return `<svg class="${cls}" viewBox="0 0 ${W} ${H}" preserveAspectRatio="xMidYMid meet" aria-hidden="true">${coast}<polyline points="${pts}" fill="none" stroke="${col}" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/><circle cx="${X(xs[0])}" cy="${Y(ys[0])}" r="2.6" fill="${col}"/></svg>`;
  }
  const trackMid = (a) => { const c = a.geometry?.coordinates; if (!c?.length) return [a.lat, a.lon]; const m = c[Math.floor(c.length / 2)]; return [m[1], m[0]]; };
  // the words themselves: the longest quoted passage in the description if it
  // has one (the crew often set the quote in quotation marks after a note on
  // its source), else the description, trimmed to the card
  function quoteOf(d) {
    const m = [...d.matchAll(/["\u201c]([^"\u201d]{40,})["\u201d]|(?:^|[\s(])'([^']{40,})'/g)].map((x) => x[1] || x[2]).sort((x, y) => y.length - x.length)[0];
    const t = (m || d).trim();
    return t.length > 240 ? t.slice(0, 237) + "…" : t;
  }
  // an artifact's rights line: the licence code's phrase (the publish
  // supplies it; a build from before the codes has the wording itself). The
  // rights note on the holder and the term is the crew's, not the reader's.
  const rights = (a) => a.licence_label || a.licence || "";
  function artifactCard(a, opts = {}) {
    const t = TYPES[a.type] || {};
    let media = "";
    const picture = a.url && /\.(jpe?g|png|gif|tiff?|webp|bmp)$/i.test(a.url);
    if (a.type === "track") media = trackSketch(a);
    else if (picture && (a.type === "image" || a.type === "map")) media = `<img class="thumb" src="${esc(a.thumb || a.url)}" alt="" loading="lazy">`;
    else if (a.type === "image" || a.type === "map") media = `<span class="thumb none">${a.url ? esc(a.url.split(".").pop().toUpperCase()) + " · no picture yet" : "interactive resource · no picture yet"}</span>`;
    const quote = a.type === "quote" && a.description ? `<q>${esc(quoteOf(a.description))}</q>` : "";
    const [plat, plon] = a.type === "track" ? trackMid(a) : [a.lat, a.lon];
    return `<a class="artcard ${esc(a.type)}" href="#wiki/${esc(a.page)}" data-slug="${esc(a.page)}" title="${esc(a.title)}">${flagMark(a)}${media}<span class="dot" style="background:${t.colour || C.muted}"></span>` +
      `<span class="kind">${esc(a.type)}</span><b>${esc(a.title)}</b>${quote}<span class="when">${esc(fmtDate(a))}${plat != null ? " " + mapLink(plat, plon, a.title, a.type) : ""}</span>` +
      (opts.creator && a.creator ? `<span class="who">${esc(a.creator)}</span>` : "") + `</a>`;
  }
  function pageLink(p, cls = "") {
    return `<a class="pglink ${cls}" href="#wiki/${esc(p.slug)}" data-slug="${esc(p.slug)}"><span class="kind">${esc(kindLabel(p.kind))}</span>${esc(p.title)}${statusTag(p.status)}${p.summary ? `<span class="sum">${esc(p.summary)}</span>` : ""}</a>`;
  }
  // a topic as a card with its picture: the home's narrative chips, and Explore
  function topicCard(x, full = false) {
    const im = topicImage(x.slug);
    return `<a class="topiccard ${im ? "" : "noimg"}" href="#wiki/topic/${esc(x.slug)}" data-topic="${esc(x.slug)}" style="border-left-color:${topicColour(x.slug)}">` +
      (im ? `<img src="${esc(im.url)}" alt="" loading="lazy">` : "") +
      `<span class="body"><b>${esc(x.title)}</b>${full ? `<span>${esc(x.summary)}</span>` : ""}<span class="counts">${x.pages} pages · ${x.artifacts} artifacts</span></span></a>`;
  }

  // where each artifact is first mentioned across a topic's narrative pages,
  // read in their order: page slug -> a rank
  async function mentionOrder(pages) {
    const order = new Map();
    const docs = await Promise.all(pages.map((p) => page(p.slug).catch(() => null)));
    let n = 0;
    for (const d of docs) for (const m of String(d?.html || "").matchAll(/\]\((artifact\/[^)\s]+)\)/g)) if (!order.has(m[1])) order.set(m[1], n++);
    return order;
  }
  // search: the pane's pages, artifacts and events, a section a kind in the
  // Browse menu's order, the title hits first within each
  const SEARCH_MAX = 60;
  function renderSearch(el, query) {
    const q = query.trim().toLowerCase();
    const words = q.split(/\s+/).filter(Boolean), t = curTopic();
    const hit = (s) => { const t = String(s || "").toLowerCase(); return words.every((w) => t.includes(w)); };
    const groups = new Map();                                   // kind -> [{ p (a page row) | a (an artifact), title, first }]
    const add = (kind, item) => { if (!groups.has(kind)) groups.set(kind, []); groups.get(kind).push(item); };
    for (const p of pagesOf()) if (p.kind !== "artifact" && (!t || p.topic === t || !p.topic) && (hit(p.title) || hit(p.summary))) add(p.kind === "person" ? "people" : p.kind, { p, title: p.title, first: hit(p.title) });
    for (const a of arts()) if ((!t || a.topic === t) && (hit(a.title) || hit(a.description) || hit((a.people || []).join(" ")) || hit((a.tags || []).join(" ")))) add(a.type, { a, title: a.title, first: hit(a.title) });
    const seen = new Set((groups.get("event") || []).map((x) => x.p?.slug).filter(Boolean));   // an event found by its detail or its place, not already by its page
    for (const e of hist.events.filter(inDomain)) if (!seen.has(`event/${e.id}`) && (!t || e.topic === t) && (hit(e.title) || hit(e.detail) || hit(e.place) || hit((e.people || []).join(" "))))
      add("event", { p: { slug: `event/${e.id}`, kind: "event", title: e.title, summary: [dateLabel(e.date_text || e.date_start || ""), e.place].filter(Boolean).join(" · ") }, title: e.title, first: hit(e.title) });
    const rank = (k) => { const i = BACK_ORDER.indexOf(k); return i < 0 ? BACK_ORDER.length : i; };
    const total = [...groups.values()].reduce((n, xs) => n + xs.length, 0);
    const section = ([k, xs]) => {
      xs.sort((x, y) => (y.first - x.first) || x.title.localeCompare(y.title));
      const K = KINDS[k], colour = K ? K.colour : k === "page" ? C.accent : C.muted;
      const head = `<h3><span class="dot" style="background:${colour}"></span><span class="muted">${xs.length}</span> ${esc(K ? K.label : BACK_LABEL[k] || k)}</h3>`;
      const shown = xs.slice(0, SEARCH_MAX), more = xs.length > SEARCH_MAX ? `<p class="muted small">and ${xs.length - SEARCH_MAX} more: narrow the search</p>` : "";
      const pages = shown.filter((x) => x.p), cards = shown.filter((x) => x.a);   // the events: their pages, and the artifacts of that kind
      return head + (pages.length ? `<div class="pagelist">${pages.map((x) => pageLink(x.p)).join("")}</div>` : "") +
        (cards.length ? `<div class="artgrid ${k === "image" || k === "map" ? "pictures" : ""} ${k === "quote" ? "quotes" : ""}">${cards.map((x) => artifactCard(x.a, { creator: true })).join("")}</div>` : "") + more;
    };
    el.innerHTML = crumb(`search <i>${esc(query.trim())}</i>`) + `<h2>${total} found</h2>` +
      [...groups.entries()].sort((x, y) => rank(x[0]) - rank(y[0]) || x[0].localeCompare(y[0])).map(section).join("");
    if (domainOn("nature")) UW.natureViews?.searchExtra?.(el, query);
  }
  const LEAD = {
    history: "the human past, from the Tuniit to the ships of the last century: voyages as tracks, winterings and besetments as spans on the timeline, people and places as pages that link to one another",
    nature: "what the archipelago is and does: the rock, the ice, the water, the sky, the weather, the field and the living things, as the record has them, with the ship's own journal",
  };
  async function renderMain() {
    ns = NS.wiki;
    const el = $("#histmain");
    el.scrollTop = 0;                                              // a new view opens at its top
    for (const id of ["#histplot", "#natplot"]) { const plot = el.querySelector(id); if (plot?.data) Plotly.purge(plot); }
    if (!UW.M.history) { el.innerHTML = `<div class="empty">No wiki has been published yet.</div>`; return; }
    if (!hist.index) { el.innerHTML = `<div class="empty">Loading the wiki…</div>`; return; }
    loadFlags();                                                   // what other browsers have flagged since; the cards update when it lands
    const natv = domainOn("nature") ? UW.natureViews : null;      // the natural half's own parts, while Nature is on
    if (hist.search.trim()) { renderSearch(el, hist.search); return; }
    if (!hist.slug) {                                              // the home: today, here, the journal and the domains, the narratives
      const t = topics();
      el.innerHTML = vignetteHTML() + (natv?.homeExtraHTML?.() || "") +
        `<h2>Narratives <a class="chip small" href="#wiki/explore" data-slug="explore">Explore</a></h2>` +
        `<div class="topicgrid chips">${t.filter((x) => x.pages > 0 || x.artifacts > 0).map((x) => topicCard(x)).join("")}</div>`;
      for (const b of el.querySelectorAll("button.more")) b.onclick = () => { const k = b.dataset.more; if (k.startsWith("nat:")) natv?.toggleMore(k.slice(4)); else hist.more[k] = !hist.more[k]; renderMain(); };
      natv?.wireHome?.(el);
      return;
    }
    if (hist.slug === "explore") {
      const t = topics();
      const n = (k) => t.reduce((s, x) => s + (x[k] || 0), 0);
      el.innerHTML = crumb(here("Explore", "explore")) + `<h2>Explore</h2><p class="lead">${t.length} topics, ${n("pages")} narrative pages and ${arts().length} artifacts: ${domainsOn().map((d) => LEAD[d]).join("; and ")}. Every item is credited and sourced.</p>` +
        `<div class="topicgrid">${t.map((x) => topicCard(x, true)).join("")}</div>` + (natv?.exploreExtraHTML?.() || "");
      return;
    }
    if (/^kind\/track(\/|$)/.test(hist.slug) || hist.slug.startsWith("topic/") || hist.slug.startsWith("artifact/")) { await ensureCoast(); ns = NS.wiki; }
    if (hist.slug.startsWith("kind/")) { const [, k, ...rest] = hist.slug.split("/"); renderKind(el, k, rest.join("/")); return; }
    if (hist.slug === "bib") { await renderBib(el); return; }
    if (hist.slug === "provenance") { renderProvenance(el); return; }
    if (hist.slug.startsWith("at/")) { renderSite(el); return; }
    if (UW.natureViews?.handles(hist.slug)) { await UW.natureViews.render(el, hist.slug); ns = NS.wiki; return; }   // the record, the journal, a domain, a subject, an observation
    if (hist.slug.startsWith("topic/")) { await renderTopic(el, hist.slug.slice(6)); return; }
    await renderPage(el, hist.slug);
  }
  // a topic: its narrative pages and its artifacts of every kind
  async function renderTopic(el, slug) {
    const n = ns, t = topicOf(slug);
    if (!t) { el.innerHTML = `<div class="empty">no such topic</div>`; return; }
    const pages = hist.index.pages.filter((p) => p.topic === t.slug && p.kind === "page");
    // the artifacts of every kind together: first those the narratives
    // mention, in the order they are mentioned, then the rest in a fixed
    // order that looks like none
    const order = await mentionOrder(pages);
    ns = n;
    const found = hist.artifacts.filter((a) => a.topic === t.slug).sort((p, q) => ((order.get(p.page) ?? 1e9) - (order.get(q.page) ?? 1e9)) || byMix(p, q));
    const counts = GROUPS.flatMap((g) => [g.head, ...g.under]).filter((k) => TYPES[k]).map((k) => [k, found.filter((a) => a.type === k).length]).filter(([, n]) => n)
      .map(([k, n]) => `${n} ${TYPES[k].label.toLowerCase()}`).join(" · ");
    const im = topicImage(t.slug);
    el.innerHTML = crumb(`<a href="#wiki/explore" data-slug="explore">Explore</a>`, here(esc(t.title), `topic/${t.slug}`)) + `<h2>${esc(t.title)}${statusTag(t.status)}</h2>` +
      (im ? `<figure class="topicfig"><a href="#wiki/${esc(im.page)}" data-slug="${esc(im.page)}" title="the picture's own page"><img src="${esc(im.url)}" alt=""></a><figcaption><a href="#wiki/${esc(im.page)}" data-slug="${esc(im.page)}">${esc(im.title)}</a> · ${esc(im.credit)}</figcaption></figure>` : "") +
      `<p class="lead">${esc(t.summary)}</p>` +
      (pages.length ? `<div class="pagelist">${pages.map((p) => pageLink(p)).join("")}</div>` : `<p class="muted">No narrative pages yet; the artifacts below are what the crew has entered so far.</p>`) +
      (found.length ? `<h3><span class="muted">${found.length}</span> Artifacts <span class="muted small">${esc(counts)}</span></h3><div class="artgrid">${found.map((a) => artifactCard(a, { creator: true })).join("")}</div>` : "");
    if (topicDomain(t.slug) === "nature") UW.natureViews?.topicExtra?.(el, t.slug);
  }
  // a page: a narrative, or a generated one (artifact, person, place, event, source, animal, vessel)
  async function renderPage(el, slug) {
    const n = ns;
    // an event's page comes from the publisher; a build without them gets the pane's own
    if (slug.startsWith("event/") && !hist.index.pages.some((x) => x.slug === slug)) { renderEvent(el, slug.slice(6)); return; }
    let p;
    try { p = await page(slug); }
    catch { el.innerHTML = crumb() + `<div class="empty">That page is not in this build.</div>`; return; }
    ns = n;
    const t = topicOf(p.topic);
    const a = p.kind === "artifact" ? artifactById(p.ref) : null;
    const pl = p.kind === "place" ? placeByPage(p.slug) : null;
    const ev = p.kind === "event" ? eventById(p.ref) : null;
    const back = (p.backlinks || []).map((s) => hist.index.pages.find((x) => x.slug === s)).filter(Boolean);
    let head = crumb(...(t ? [`<a href="#wiki/topic/${esc(t.slug)}" data-topic="${esc(t.slug)}">${esc(t.title)}</a>`] : []), here(`<span class="kind">${esc(kindLabel(p.kind))}</span>`, p.slug)) + `<h2>${esc(p.title)}${p.kind === "page" ? statusTag(p.status) : ""}</h2>`;
    if (p.summary && p.kind === "page") head += `<p class="lead">${esc(p.summary)}</p>`;
    let media = "", tail = "";                                   // a route's waypoints table follows the description
    if (a) {
      const picture = a.url && /\.(jpe?g|png|gif|tiff?|webp|bmp)$/i.test(a.url);
      if (picture && (a.type === "image" || a.type === "map")) media = `<figure><a href="${esc(a.url)}" target="_blank" rel="noopener"><img src="${esc(a.url)}" alt="${esc(a.title)}"></a><figcaption>${esc(a.credit)}${rights(a) ? " · " + esc(rights(a)) : ""}</figcaption></figure>`;
      // the copies: the one on the ship, the original it was rendered from, and the one on the web
      const ext = (u) => esc(u.split(".").pop().toUpperCase());
      const links = [
        a.url ? `<a class="chip" href="${esc(a.url)}" target="_blank" rel="noopener" title="the ship's own copy">on the ship · ${a.type === "text" && !picture ? "full text" : ext(a.url)}</a>` : "",
        a.original_url ? `<a class="chip" href="${esc(a.original_url)}" target="_blank" rel="noopener" title="the file the picture was rendered from">the original · ${ext(a.original_url)}</a>` : "",
        a.source_url ? `<a class="chip" href="${esc(a.source_url)}" target="_blank" rel="noopener" title="at the holding institution, on the web">on the web ↗</a>` : "",
      ].filter(Boolean);
      if (links.length) media += `<p class="artlinks">${links.join(" ")}</p>`;
      const [plat, plon] = a.type === "track" ? trackMid(a) : [a.lat, a.lon];
      const where = plat != null ? ` · ${a.type === "track" ? "" : coordLink(plat, plon, a.title) + " · "}${mapLink(plat, plon, a.title, a.type)}` : "";
      const track = a.type === "track" && a.waypoints?.length ? `<div class="hscroll"><table class="waypoints"><tr><th>date</th><th>position</th><th>note</th></tr>${a.waypoints.map((w) => `<tr><td>${esc(dateLabel(w.date || ""))}</td><td>${w.lat != null ? coordLink(w.lat, w.lon, w.note || a.title) : ""}</td><td>${esc(w.note || "")}</td></tr>`).join("")}</table></div>` : "";
      head += `<div class="artmeta"><span class="dot" style="background:${TYPES[a.type]?.colour || C.muted}"></span>${esc(a.type)} · ${esc(fmtDate(a))}${a.creator ? " · " + esc(a.creator) : ""}${where}${flagMark(a)}</div>`;
      head += peopleStrip(a.people);
      if (a.type === "track") media = `<figure class="routefig">${trackSketch(a, 480, 300, "sketch large")}</figure>` + media;
      tail = track;
    }
    // a generated page's body is the row's prose alone; the row's other
    // fields are laid out here, as facts under the title
    if (a) head += facts([["credit", !(a.url && /\.(jpe?g|png|gif|tiff?|webp|bmp)$/i.test(a.url) && (a.type === "image" || a.type === "map")) ? a.credit : ""], ["rights", a.url ? "" : rights(a)], ["source", sourceRef(a.bibkey, a.pages)]]);
    if (pl) {
      if (pl.lat != null) head += `<div class="artmeta"><span class="dot" style="background:${TYPES.place.colour}"></span>${esc(pl.kind || "place")} · ${coordLink(pl.lat, pl.lon, pl.name)} · ${mapLink(pl.lat, pl.lon, pl.name, "place")}</div>`;
      head += facts([["also", [pl.inuktitut, pl.historic].filter(Boolean).join(", ")], ["source", sourceRef(pl.bibkey)]]);
    }
    if (ev) {
      const when = ev.date_text || [ev.date_start, ev.date_end].filter(Boolean).map((d) => dateLabel(d)).join(" to ");
      const at = ev.place ? (hist.places.find((x) => x.name === ev.place) ? `<a href="#wiki/${esc(hist.places.find((x) => x.name === ev.place).page)}" data-slug="${esc(hist.places.find((x) => x.name === ev.place).page)}">${esc(ev.place)}</a>` : esc(ev.place)) : "";
      head += `<div class="artmeta"><span class="dot" style="background:${TYPES.event.colour}"></span>event${when ? " · " + esc(when) : ""}${at ? " · " + at : ""}${ev.lat != null ? " · " + coordLink(ev.lat, ev.lon, ev.title) + " · " + mapLink(ev.lat, ev.lon, ev.title, "event") : ""}</div>`;
      head += peopleStrip(ev.people) + facts([["source", sourceRef(ev.bibkey)]]);
    }
    if (p.kind === "person") {
      const r = hist.people.find((x) => x.page === p.slug);
      if (r) head += facts([["", [r.role, r.affiliation, lifespan(r)].filter(Boolean).join(" · ")], ["also written", r.also], ["source", sourceRef(r.bibkey)]]);
    }
    if (p.kind === "animal") {
      const r = hist.animals.find((x) => x.page === p.slug);
      if (r) head += facts([["", [r.kind, r.role, r.affiliation, lifespan(r)].filter(Boolean).join(" · ")], ["also called", r.also], ["source", sourceRef(r.bibkey)]]);
    }
    if (p.kind === "vessel") {
      const r = hist.vessels.find((x) => x.page === p.slug);
      if (r) head += facts([["", [[r.kind_label || r.kind, r.kind_note].filter(Boolean).join(", "), r.affiliation, r.tonnage].filter(Boolean).join(" · ")], ["", r.role], ["built", r.built], ["fate", r.lost], ["also", r.also], ["source", sourceRef(r.bibkey)]]);
    }
    if (p.kind === "source") {
      const bib = await bibliography();
      ns = n;
      const e = bib.find((x) => x.key === p.slug.split("/").pop());
      // the bibliography's note joins the archive, the call number, the
      // licence and the source's own note; that note is the page body, so
      // the held line keeps only what comes before it
      const plain = String(p.html || "").replace(/\[([^\]]+)\]\([^)]*\)/g, "$1").replace(/\s+/g, " ").trim();
      let held = (e?.note || "").replace(/\s+/g, " ").trim();
      if (plain && held.endsWith(plain)) held = held.slice(0, -plain.length).replace(/;\s*$/, "").trim();
      if (e) head += `<p class="mlaline"><span class="lbl">MLA</span> ${mla(e)}</p>` + facts([["", e.keywords === "primary" ? "primary source" : e.keywords === "secondary" ? "secondary source" : ""], ["held", held]]);
    }
    el.innerHTML = head + media + `<div class="wiki">${markdown(p.html)}${tail}</div>` + backlinksHTML(p.backlinks || []);
    wireBackmore(el);
    crossLink(el.querySelector(".wiki"), p.slug, a?.people || ev?.people || p.people, p.kind === "page" ? p.title : "");
    if (p.kind === "page") enrich(el.querySelector(".wiki"));
    el.scrollTop = 0; window.scrollTo?.(0, 0);
  }
  // a narrative's artifacts where the text reaches them: the cards of those
  // a block links, not shown higher up, set beside it with the text flowing
  // round them, on the right and the left by turns
  function enrich(root) {
    if (!root) return;
    const shown = new Set();
    let side = 0;
    for (const block of [...root.querySelectorAll("p, ul, ol, blockquote, .hscroll")]) {
      if (block.closest(".inrefs")) continue;
      const arts = [...block.querySelectorAll('a[data-slug^="artifact/"]')].map((x) => artifactById(x.dataset.slug.slice(9))).filter((x) => x && !shown.has(x.id));
      if (!arts.length) continue;
      for (const x of arts) shown.add(x.id);
      block.insertAdjacentHTML("beforebegin", `<div class="inrefs ${side++ % 2 ? "left" : "right"}">${arts.map((a) => artifactCard(a, { creator: true })).join("")}</div>`);
    }
  }
  // ---------------------------------------------------------------- an event's page
  // A build from a publisher without event pages: the pane makes one from
  // the record — when and where, the people, the source, the timeline.
  function renderEvent(el, id) {
    const e = eventById(id);
    if (!e) { el.innerHTML = crumb() + `<div class="empty">That event is not in this build.</div>`; return; }
    const t = topicOf(e.topic);
    const when = esc(dateLabel(e.date_text || e.date_start || "")) + (e.date_end && e.date_end !== e.date_start ? ` → ${esc(dateLabel(e.date_end))}` : "");
    const where = e.lat != null ? ` · ${coordLink(e.lat, e.lon, e.title)} · ${mapLink(e.lat, e.lon, e.title, "event")}` : "";
    const src = e.bibkey ? hist.index.pages.find((x) => x.slug === `source/${e.bibkey}`) : null;
    el.innerHTML = crumb(...(t ? [`<a href="#wiki/topic/${esc(t.slug)}" data-topic="${esc(t.slug)}">${esc(t.title)}</a>`] : []), here(`<span class="kind">event</span>`, `event/${id}`)) + `<h2>${esc(e.title)}</h2>` +
      `<div class="artmeta"><span class="dot" style="background:${TYPES.event.colour}"></span>event · ${when}${e.place ? " · " + esc(e.place) : ""}${where}</div>` +
      peopleStrip(e.people) +
      `<div class="wiki">${markdown(e.detail || "")}</div>` +
      (e.tags?.length ? `<div class="backlinks"><span class="lbl">Tags</span>${e.tags.map((x) => `<span class="muted">${esc(x)}</span>`).join("")}</div>` : "") +
      `<div class="backlinks">${src ? `<span class="lbl">Source</span><a href="#wiki/${esc(src.slug)}" data-slug="${esc(src.slug)}">${esc(src.title)}</a>` : ""}<span class="lbl">On the timeline</span><a href="#wiki/kind/event" data-slug="kind/event">Events</a></div>`;
    crossLink(el.querySelector(".wiki"), `event/${id}`, e.people, "");
    el.scrollTop = 0; window.scrollTo?.(0, 0);
  }
  // "Mentioned in": the pages that link here, in sections by kind in the
  // order of the Browse menu, each under its dot. A well-cited source is
  // mentioned by a couple of hundred, so the tail waits behind a fold.
  const BACK_SHOWN = 40;
  const BACK_ORDER = ["page", "image", "event", "people", "place", "map", "quote", "text", "object", "track", "animal", "vessel", "subject", "observation", "source", "topic"];
  const BACK_LABEL = { page: "Pages", subject: "Subjects", observation: "Observations", source: "Sources", topic: "Topics" };
  function backKind(slug) {
    if (slug.startsWith("artifact/")) return artifactById(slug.slice(9))?.type || "artifact";
    const p = hist.index?.pages.find((x) => x.slug === slug);
    const k = p ? p.kind : slug.includes("/") ? slug.split("/")[0] : "page";
    return k === "person" ? "people" : k;
  }
  function backTitle(slug) {
    return hist.index?.pages.find((x) => x.slug === slug)?.title || (slug.startsWith("artifact/") ? artifactById(slug.slice(9))?.title : "") || UW.natureViews?.titleOf?.(slug) || slug;
  }
  function backlinksHTML(slugs) {
    if (!slugs?.length) return "";
    const groups = new Map();
    for (const slug of slugs) { const k = backKind(slug); if (!groups.has(k)) groups.set(k, []); groups.get(k).push({ slug, title: backTitle(slug) }); }
    const rank = (k) => { const i = BACK_ORDER.indexOf(k); return i < 0 ? BACK_ORDER.length : i; };
    const link = (b) => `<a href="#wiki/${esc(b.slug)}" data-slug="${esc(b.slug)}">${esc(b.title)}</a>`;
    const shown = [], later = [];
    let n = 0;
    for (const [k, xs] of [...groups.entries()].sort((x, y) => rank(x[0]) - rank(y[0]) || x[0].localeCompare(y[0]))) {
      xs.sort((p, q) => p.title.localeCompare(q.title));
      const K = KINDS[k], colour = K ? K.colour : k === "page" ? C.accent : C.muted;
      const head = `<span class="ghead"><span class="dot" style="background:${colour}"></span>${esc(K ? K.label : BACK_LABEL[k] || k)}<span class="muted">${xs.length}</span></span>`;
      const first = xs.slice(0, Math.max(0, BACK_SHOWN - n)), rest = xs.slice(first.length);
      if (first.length) shown.push(`<div class="bgroup">${head}${first.map(link).join("")}${rest.length ? `<span class="backmore" hidden>${rest.map(link).join("")}</span>` : ""}</div>`);
      else later.push(`<div class="bgroup">${head}${xs.map(link).join("")}</div>`);
      n += xs.length;
    }
    const more = slugs.length - BACK_SHOWN;
    return `<div class="backlinks grouped"><span class="lbl">Mentioned in</span>${shown.join("")}${later.length ? `<div class="backmore" hidden>${later.join("")}</div>` : ""}${more > 0 ? `<button type="button" class="chip small more" data-more="back">and ${more} more</button>` : ""}</div>`;
  }
  function wireBackmore(el) {
    const more = el.querySelector('button[data-more="back"]'); if (more) more.onclick = () => { for (const x of el.querySelectorAll(".backmore")) x.hidden = false; more.remove(); };
  }
  // the people named on an artifact or an event, each a link to their page
  // the facts under a generated page's title: [label, value] pairs, the
  // empty ones dropped; a value may carry markup the caller has escaped
  const lifespan = (r) => (r.born || r.died) ? `${r.born || "?"}–${r.died || ""}` : "";
  function sourceRef(bibkey, pages = "") {
    if (!bibkey) return "";
    const slug = `source/${bibkey}`, pg = hist.index?.pages.find((x) => x.slug === slug);
    return `<a href="#wiki/${esc(slug)}" data-slug="${esc(slug)}">${esc(pg ? pg.title : bibkey)}</a>${pages ? ", " + esc(pages) : ""}`;
  }
  function facts(pairs) {
    const rows = pairs.filter(([, v]) => v).map(([l, v]) => `<div class="fact">${l ? `<span class="lbl">${esc(l)}</span>` : ""}<span>${l === "source" ? v : esc(v)}</span></div>`);
    return rows.length ? `<div class="artfacts">${rows.join("")}</div>` : "";
  }
  function peopleStrip(names) {
    if (!names?.length) return "";
    const one = (n) => { const p = hist.people.find((x) => x.name === n); return p ? `<a class="chip small" href="#wiki/${esc(p.page)}" data-slug="${esc(p.page)}">${esc(n)}</a>` : `<span class="chip small wanted" title="no page yet">${esc(n)}</span>`; };
    return `<div class="artpeople"><span class="lbl">People</span>${names.map(one).join("")}</div>`;
  }

  // ---------------------------------------------------------------- cross-links
  // A wiki is its links. The crew's wikilinks come rendered; the pane adds
  // the rest: the first mention on a page of any person or place that has
  // a page becomes a link to it, and a surname alone links when the page's
  // own list of people makes it plain who is meant. The names found, with
  // the crew's links, make the strip of people and places under the text.
  function nameIndex() {
    if (hist.names) return hist.names;
    const byName = new Map();                             // a name -> its page; a name two things share goes to the later one
    const put = (name, slug, kind) => { name = String(name || "").trim(); if (name.length >= 4 && !/^the /i.test(name)) byName.set(name, { name, slug, kind }); };
    for (const p of hist.people) { put(p.name, p.page, "person"); for (const a of (p.also || "").split(";")) put(a, p.page, "person"); }
    for (const p of hist.places) { put(p.name, p.page, "place"); for (const a of [p.historic, p.inuktitut]) for (const n of (a || "").split(";")) put(n, p.page, "place"); }
    const rx = new RegExp(`(?<![\\p{L}\\p{N}])(?:${[...byName.keys()].sort((x, y) => y.length - x.length).map(rxEscape).join("|")})(?![\\p{L}\\p{N}])`, "gu");
    return (hist.names = { byName, rx });
  }
  const rxEscape = (s) => s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  // the first mention of each name in the text nodes under root becomes a
  // link; `linked` holds the pages already linked, `taken` the names already spoken for
  function linkMentions(root, rx, byName, linked, taken) {
    const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT, { acceptNode: (n) => n.parentElement.closest("a, h2, h3, h4, code, .wanted, figcaption") ? NodeFilter.FILTER_REJECT : NodeFilter.FILTER_ACCEPT });
    const texts = []; for (let n = walker.nextNode(); n; n = walker.nextNode()) texts.push(n);
    for (const node of texts) {
      const s = node.nodeValue; let last = 0, m, frag = null;
      rx.lastIndex = 0;
      while ((m = rx.exec(s))) {
        const x = byName.get(m[0]); if (!x || linked.has(x.slug) || taken.has(m[0])) continue;
        linked.add(x.slug); taken.add(m[0]);
        frag ||= document.createDocumentFragment();
        frag.appendChild(document.createTextNode(s.slice(last, m.index)));
        const a = document.createElement("a"); a.href = `#wiki/${x.slug}`; a.dataset.slug = x.slug; a.className = "auto"; a.textContent = m[0]; frag.appendChild(a);
        last = m.index + m[0].length;
      }
      if (frag) { frag.appendChild(document.createTextNode(s.slice(last))); node.parentNode.replaceChild(frag, node); }
    }
  }
  // the strips under a page: the people and places (linked or found), and
  // the animals and vessels the crew linked (their names are too plain to find)
  const STRIPS = [["person", "People"], ["place", "Places"], ["animal", "Animals"], ["vessel", "Vessels"]];
  // a slug written bare in the prose ("see davis-baffin-hudson", "frobisher-004")
  // becomes a link that reads as the page's title; an artifact's card then
  // follows the paragraph like any other link's
  const SLUG_RX = /(?<![\w/#-])([a-z][a-z0-9]*(?:-[a-z0-9]+)+|(?:artifact|person|place|event|source|topic|vessel|animal|subject|observation)\/[a-z0-9][\w-]*)(?![\w/-])/g;
  function linkSlugs(root, self) {
    if (!root || !hist.index) return;
    const pageOf = (tok) => {
      if (tok === self) return null;
      const p = hist.index.pages.find((x) => x.slug === tok); if (p) return p;
      const a = artifactById(tok); return a ? { slug: a.page, title: a.title } : null;
    };
    const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT, { acceptNode: (n) => n.parentElement.closest("a, code, .wanted") ? NodeFilter.FILTER_REJECT : NodeFilter.FILTER_ACCEPT });
    const texts = []; for (let n = walker.nextNode(); n; n = walker.nextNode()) texts.push(n);
    for (const node of texts) {
      const s = node.nodeValue; let last = 0, m, frag = null;
      SLUG_RX.lastIndex = 0;
      while ((m = SLUG_RX.exec(s))) {
        const p = pageOf(m[1]); if (!p) continue;
        frag ||= document.createDocumentFragment();
        frag.appendChild(document.createTextNode(s.slice(last, m.index)));
        const a = document.createElement("a"); a.href = `#wiki/${p.slug}`; a.dataset.slug = p.slug; a.className = "auto"; a.textContent = p.title; frag.appendChild(a);
        last = m.index + m[0].length;
      }
      if (frag) { frag.appendChild(document.createTextNode(s.slice(last))); node.parentNode.replaceChild(frag, node); }
    }
  }
  function crossLink(root, self, people, title) {
    if (!root) return;
    linkSlugs(root, self);
    const { byName, rx } = nameIndex();
    const linked = new Set([self]), taken = new Set();
    for (const a of root.querySelectorAll("a[data-slug]")) linked.add(a.dataset.slug);
    // the surnames of the page's own people first, when only one of them
    // bears the name: on their page "Isachsen" is the man, not the station
    const surnames = new Map();
    for (const n of people || []) {
      const p = hist.people.find((x) => x.name === n); if (!p) continue;
      const last = n.split(/\s+/).pop();
      if (last.length >= 4 && people.filter((m) => m.split(/\s+/).pop() === last).length === 1) surnames.set(last, { name: last, slug: p.page, kind: "person" });
    }
    if (surnames.size) {
      linkMentions(root, new RegExp(`(?<![\\p{L}\\p{N}])(?:${[...surnames.keys()].map(rxEscape).join("|")})(?![\\p{L}\\p{N}])`, "gu"), surnames, linked, taken);
      for (const k of surnames.keys()) taken.add(k);
    }
    linkMentions(root, rx, byName, linked, taken);
    // the people and places of the page: named in the title, on the page's
    // own list, linked by the crew or found above
    const found = new Map();
    const add = (slug) => { const p = hist.index.pages.find((x) => x.slug === slug); if (p && STRIPS.some(([k]) => k === p.kind) && slug !== self) found.set(slug, p); };
    if (title) for (const m of title.matchAll(rx)) { const x = byName.get(m[0]); if (x) add(x.slug); }
    for (const n of people || []) { const p = hist.people.find((x) => x.name === n); if (p) add(p.page); }
    for (const a of root.querySelectorAll("a[data-slug]")) add(a.dataset.slug);
    // an artifact's or an event's page names its people under the title: the
    // people found in the text join that strip rather than making a second
    const top = root.parentElement?.querySelector(".artpeople");
    const chips = (kind, label) => {
      const xs = [...found.values()].filter((p) => p.kind === kind).sort((p, q) => p.title.localeCompare(q.title));
      if (!xs.length) return "";
      if (kind === "person" && top) {
        const have = new Set([...top.querySelectorAll("[data-slug]")].map((a) => a.dataset.slug));
        for (const p of xs) if (!have.has(p.slug)) top.insertAdjacentHTML("beforeend", `<a class="chip small" href="#wiki/${esc(p.slug)}" data-slug="${esc(p.slug)}">${esc(p.title)}</a>`);
        return "";
      }
      return `<div class="onpage"><span class="lbl">${label}</span>${xs.map((p) => `<a href="#wiki/${esc(p.slug)}" data-slug="${esc(p.slug)}">${esc(p.title)}</a>`).join("")}</div>`;
    };
    const strip = STRIPS.map(([k, label]) => chips(k, label)).join("");
    if (strip) root.insertAdjacentHTML("afterend", strip);
  }

  // ---------------------------------------------------------------- the kind pages
  // One page per kind: the people, places, animals and vessels as lists, the
  // Events page as the timeline, and the artifact kinds (images, maps,
  // quotes, texts, objects, tracks) as a grid of collections, one a topic,
  // each a chip with a picture from it, its name and its count; the chip
  // opens the collection. Within it the backend's keywords make sections
  // where the artifacts carry them.
  const pictureOf = (a) => a.thumb || (a.url && /\.(jpe?g|png|gif|webp|bmp)$/i.test(a.url) ? a.url : "");
  const pickOne = (xs) => xs.length ? xs[Math.floor(Math.random() * xs.length)] : null;
  // a collection as a chip: a picture from it (a random one, the topic's own
  // when it has none), its name under a coloured dot, and its count
  function collectionChip({ slug, label, colour, count, word, items = [], fallback = null, sketch = "" }) {
    const a = pickOne(items.filter(pictureOf)) || fallback;
    const pic = sketch || (a ? `<img src="${esc(pictureOf(a))}" alt="" loading="lazy" title="${esc(a.title)}">` : "");
    return `<a class="colchip ${pic ? "" : "nopic"}" href="#wiki/${esc(slug)}" data-slug="${esc(slug)}" title="${esc(label)}">${pic ? `<span class="pic">${pic}</span>` : ""}<span class="body"><b><span class="dot" style="background:${colour}"></span>${esc(label)}</b><span class="n">${count} ${esc(word)}</span></span></a>`;
  }
  // the artifacts by topic, in the index's order
  function collectionsOf(xs) {
    const by = new Map();
    for (const a of xs) { if (!by.has(a.topic)) by.set(a.topic, []); by.get(a.topic).push(a); }
    const known = (hist.index?.topics || []).filter((t) => by.has(t.slug)).map((t) => ({ key: t.slug, label: t.title, colour: topicColour(t.slug), items: by.get(t.slug) }));
    const other = [...by.keys()].filter((k) => !hist.index?.topics.some((t) => t.slug === k)).map((k) => ({ key: k, label: k || "no topic", colour: C.muted, items: by.get(k) }));
    return [...known, ...other];
  }
  // a route's first position: north first on the Tracks page
  const startLat = (a) => a.waypoints?.[0]?.lat ?? a.geometry?.coordinates?.[0]?.[1] ?? a.lat ?? -999;
  const byStartLat = (p, q) => (startLat(q) - startLat(p)) || p.title.localeCompare(q.title);
  const letterList = (xs, name, line) => {
    let letter = "";
    return xs.map((x) => { const L = (name(x)[0] || "?").toUpperCase(); const head = L !== letter ? `<h3>${esc(L)}</h3>` : ""; letter = L; return head + line(x); }).join("");
  };
  function renderKind(el, kind, collection = "") {
    const K = KINDS[kind];
    if (!K) { el.innerHTML = crumb() + `<div class="empty">no such kind</div>`; return; }
    const h2 = (n, label) => `<h2><span class="dot" style="background:${K.colour}"></span><span class="muted">${n}</span> ${esc(label)}</h2>`;
    if (kind === "event") { el.innerHTML = crumb(here("Events", "kind/event")) + eventsHTML(); wireEvents(el); return; }
    if (kind === "people" || kind === "animal" || kind === "vessel") {
      // the names down the left; on the right, the faces the backend has cut
      // from the photographs — of this kind only, in a fixed order that looks
      // like none, so the wall is not the alphabet twice
      const rows = (kind === "people" ? hist.people : kind === "animal" ? hist.animals : hist.vessels).filter(inDomain);
      const faceKind = kind === "people" ? "person" : kind;
      const named = [...rows].sort((a, b) => a.name.localeCompare(b.name));
      const life = (p) => [p.born, p.died].some(Boolean) ? ` <span class="muted mono">${esc(dateLabel(p.born || "?"))}–${esc(dateLabel(p.died || ""))}</span>`
        : [p.built, p.lost].some(Boolean) ? ` <span class="muted mono">${esc(p.built || "")}${p.lost ? " – " + esc(p.lost) : ""}</span>` : "";
      const sub = (p) => [p.kind, p.role].filter(Boolean).join(" · ");
      const list = `<div class="peoplelist">` + letterList(named, (p) => p.name, (p) => `<a class="person ${p.indigenous ? "inuit" : ""}" href="#wiki/${esc(p.page)}" data-slug="${esc(p.page)}"><b>${esc(p.name)}</b>${p.also ? ` <span class="muted">(${esc(p.also)})</span>` : ""}${life(p)}${sub(p) ? `<span class="role">${esc(sub(p))}</span>` : ""}</a>`) + `</div>`;
      const faces = (hist.faces || []).filter((f) => f.file && (f.kind || "person") === faceKind && inDomain(artifactById(f.artifact) || {})).sort((x, y) => mixKey(`${x.artifact}|${x.file}`) - mixKey(`${y.artifact}|${y.file}`));
      const wall = faces.length ? `<div class="faces">${faces.map((f) => `<a class="face" href="#wiki/${esc(f.person_page || f.page)}" data-slug="${esc(f.person_page || f.page)}" title="${esc(f.person || "unidentified")}${f.title ? " · " + esc(f.title) : ""}"><img src="${esc(f.file)}" alt="${esc(f.person || "")}" loading="lazy"></a>`).join("")}</div>` : "";
      el.innerHTML = crumb(here(K.label, `kind/${kind}`)) + h2(named.length, K.label) + `<div class="peoplecols ${wall ? "" : "nofaces"}">${list}${wall}</div>`;
      return;
    }
    if (kind === "place") {
      const places = hist.places.filter(inDomain).sort((a, b) => a.name.localeCompare(b.name));
      const names = (p) => [p.inuktitut, p.historic].filter((n) => n && n !== p.name).join(", ");
      el.innerHTML = crumb(here("Places", "kind/place")) + h2(places.length, "Places") + `<div class="peoplelist">` +
        letterList(places, (p) => p.name, (p) => `<a class="person" href="#wiki/${esc(p.page)}" data-slug="${esc(p.page)}"><b>${esc(p.name)}</b>${names(p) ? ` <span class="muted">(${esc(names(p))})</span>` : ""} <span class="muted small">${esc(p.kind || "")}</span>` +
          (p.lat != null ? ` ${mapLink(p.lat, p.lon, p.name, "place")}` : "") + (p.note ? `<span class="role">${esc(p.note.length > 160 ? p.note.slice(0, 157) + "…" : p.note)}</span>` : "") + `</a>`) + `</div>`;
      return;
    }
    const found = arts().filter((a) => a.type === kind);
    const word = K.label.toLowerCase();
    const pictures = kind === "image" || kind === "map";
    const grid = (xs) => `<div class="artgrid ${pictures ? "pictures" : ""} ${kind === "quote" ? "quotes" : ""}">${xs.map((a) => artifactCard(a, { creator: true })).join("")}</div>`;
    if (COLLECTED.has(kind) && collection) {                        // one collection: its cards, in keyword sections where they have them
      const t = topicOf(collection), xs = found.filter((a) => a.topic === collection);
      const sorted = kind === "track" ? xs.sort(byStartLat) : xs.sort(byYear);
      const body = !xs.length ? `<p class="muted">Nothing of this kind in that collection.</p>` : xs.some((a) => a.keywords?.length) && kind !== "track" ? keywordSections(sorted, grid) : grid(sorted);
      el.innerHTML = crumb(`<a href="#wiki/kind/${esc(kind)}" data-slug="kind/${esc(kind)}">${esc(K.label)}</a>`, here(esc(t?.title || collection), hist.slug)) +
        `<h2><span class="dot" style="background:${topicColour(collection)}"></span><span class="muted">${xs.length}</span> ${esc(K.label)}</h2>` +
        (t ? `<p class="lead">${esc(t.title)}: <a href="#wiki/topic/${esc(t.slug)}" data-topic="${esc(t.slug)}">the topic's page</a>${kind === "track" ? " · the routes from north to south, by where each began" : ""}</p>` : "") + body;
      return;
    }
    if (COLLECTED.has(kind)) {                                      // the collections, one a topic
      const cols = collectionsOf(found);
      const chip = (c) => collectionChip({ slug: `kind/${kind}/${c.key}`, label: c.label, colour: c.colour, count: c.items.length, word, items: kind === "track" ? [] : c.items, fallback: topicImage(c.key),
        sketch: kind === "track" ? trackSketch(pickOne(c.items.filter((a) => a.geometry?.coordinates?.length > 1)) || {}) : "" });
      el.innerHTML = crumb(here(esc(K.label), `kind/${kind}`)) + h2(found.length, K.label) +
        (found.length ? `<p class="lead">${cols.length} collections, one a topic. Open one for its ${esc(word)}.</p><div class="colgrid">${cols.map(chip).join("")}</div>` : `<p class="muted">Nothing of this kind in the record yet.</p>`);
      return;
    }
    el.innerHTML = crumb(here(esc(K.label), `kind/${kind}`)) + h2(found.length, K.label) + (found.length ? grid(found.sort(byYear)) : `<p class="muted">Nothing of this kind in the record yet.</p>`);
  }
  // the backend's keywords are paths ("Ships > Whalers > Dundee fleet"); the
  // page is a section per first word, a heading per second, the rest as tags
  function keywordSections(arts, grid) {
    const paths = (a) => (a.keywords || []).map((k) => Array.isArray(k) ? k : String(k).split(/\s*>\s*/)).filter((k) => k.length);
    const tree = new Map();
    for (const a of arts) {
      const ps = paths(a);
      if (!ps.length) { if (!tree.has("Unsorted")) tree.set("Unsorted", new Map([["", new Set()]])); tree.get("Unsorted").get("").add(a); continue; }
      for (const k of ps) { const top = k[0], sub = k[1] || ""; if (!tree.has(top)) tree.set(top, new Map()); if (!tree.get(top).has(sub)) tree.get(top).set(sub, new Set()); tree.get(top).get(sub).add(a); }
    }
    const tops = [...tree.entries()].sort((x, y) => (x[0] === "Unsorted") - (y[0] === "Unsorted") || x[0].localeCompare(y[0]));
    const nav = `<div class="kwnav">${tops.map(([t, subs]) => `<a class="chip small" href="#kw-${esc(t.replace(/\W+/g, "-"))}">${esc(t)} <span>${[...new Set([...subs.values()].flatMap((x) => [...x]))].length}</span></a>`).join("")}</div>`;
    return nav + tops.map(([t, subs]) => {
      const all = new Set([...subs.values()].flatMap((x) => [...x]));
      return `<h3 id="kw-${esc(t.replace(/\W+/g, "-"))}"><span class="muted">${all.size}</span> ${esc(t)}</h3>` +
        [...subs.entries()].sort((x, y) => x[0].localeCompare(y[0])).map(([sub, xs]) => (sub ? `<h4>${esc(sub)} <span class="muted">${xs.size}</span></h4>` : "") + grid([...xs].sort(byYear))).join("");
    }).join("");
  }

  // ---------------------------------------------------------------- the Events page
  // Every atomic date the crew entered: a chart like the Event Log's timeline
  // (a row per topic, spans as bars, dates as points; click one for its row)
  // over the table. It opens at 1400; the centuries before are a click away.
  function timelineRows() {
    const t = curTopic();
    return (hist.timeline || []).filter((d) => d.topic && inDomain(d) && (!t || d.topic === t) && yearOf(d.date) != null)
      .sort((p, q) => yearOf(p.date) - yearOf(q.date));
  }
  const shortTopic = (slug) => topicOf(slug)?.title.replace(/,.*$/, "") || slug;
  function eventsHTML() {
    const rows = timelineRows();
    const tr = (d, i) => {
      const q = d.qualifier ? `<i>${esc(d.qualifier)}</i> ` : "";
      const when = q + esc(dateLabel(d.date)) + (d.date_end ? ` → ${esc(dateLabel(d.date_end))}` : "") + (d.precision && d.precision !== "day" ? ` <span class="muted">(${esc(d.precision)})</span>` : "");
      const slug = pageFor(d.entity_kind, d.entity_id);
      const what = slug ? `<a href="#wiki/${esc(slug)}" data-slug="${esc(slug)}">${esc(d.label || d.entity_id)}</a>` : esc(d.label || d.entity_id);
      const pin = d.lat != null ? ` <span class="pin" data-lat="${d.lat}" data-lon="${d.lon}" data-label="${esc(d.label || "")}" title="on the map">⌖</span>` : "";
      return `<tr data-key="${i}"${d.lat != null ? ` data-lat="${d.lat}" data-lon="${d.lon}"` : ""}><td class="mono">${when}</td><td>${what}${pin}</td><td>${esc(d.place || "")}</td><td><a href="#wiki/topic/${esc(d.topic)}" data-topic="${esc(d.topic)}"><span class="dot" style="background:${topicColour(d.topic)}"></span>${esc(shortTopic(d.topic))}</a></td></tr>`;
    };
    const spans = rows.filter((d) => d.date_end).length;
    return `<section class="panel card castplot wide solo" data-cp="histplot"><div class="head"><h3>Timeline</h3><div class="tools"><span class="now">${rows.length} dates · ${spans} spans · from ${TIMELINE_FROM}; scroll to zoom, drag to pan, click a point for its row</span><button type="button" class="reset" id="histplotreset" title="the whole record">⟲</button></div></div><div class="plot" id="histplot"></div></section>` +
      `<h2><span class="muted">${rows.length}</span> Events</h2>` +
      `<div class="hscroll" id="histevents"><table class="sched timeline"><thead><tr><th>Date</th><th>What</th><th>Where</th><th>Topic</th></tr></thead><tbody>${rows.map(tr).join("")}</tbody></table></div>`;
  }
  // the year axis: ticks at round years, read as BCE / AD / plain
  function yearTicks(lo, hi) {
    const span = Math.max(1, hi - lo), raw = span / 8;
    const step = [1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 5000].find((x) => x >= raw) || 5000;
    const vals = []; for (let y = Math.ceil(lo / step) * step; y <= hi; y += step) vals.push(y);
    return { tickvals: vals, ticktext: vals.map((y) => y === 0 ? "0" : yearLabel(y)) };
  }
  function drawEvents(el) {
    const gd = el.querySelector("#histplot"); if (!gd) return;
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
      marker: { size: 8, color: pts.map(([d]) => topicColour(d.topic)), line: { color: C.markerLine, width: .5 } } });
    // the chart holds every date but opens on the centuries with most of them
    const pad = Math.max(2, (hi - lo) * .02), from = Math.max(lo - pad, Math.min(TIMELINE_FROM, hi - 10));
    const layout = { ...UW.THEME, margin: { l: fz(150), r: 12, t: fz(8), b: fz(40) }, showlegend: false, dragmode: "pan", barmode: "overlay",
      xaxis: { ...UW.THEME.xaxis, ...yearTicks(lo - pad, hi + pad), range: [from, hi + pad], zeroline: false, title: { text: "year", font: { size: fz(12) } }, tickfont: { size: fz(12) } },
      yaxis: { ...UW.THEME.yaxis, type: "category", categoryorder: "array", categoryarray: cats.slice().reverse(), tickfont: { size: fz(11) }, fixedrange: true } };
    layout.xaxis = { ...layout.xaxis, ...yearTicks(from, hi + pad) };
    Plotly.react(gd, traces, layout, UW.CFG).then((g) => {
      UW.axisZoom(g);
      g.removeAllListeners?.("plotly_click"); g.on("plotly_click", (ev) => { const k = ev.points?.[0]?.customdata; if (k != null) showEventRow(el, k); });
      // the year ticks are ours (BCE, AD): recomputed for whatever span is in view
      g.removeAllListeners?.("plotly_relayout"); g.on("plotly_relayout", () => {
        const r = g._fullLayout?.xaxis?.range; if (!r) return;
        const t = yearTicks(r[0], r[1]);
        if (JSON.stringify(t.tickvals) !== JSON.stringify(g.layout.xaxis.tickvals)) Plotly.relayout(g, { "xaxis.tickvals": t.tickvals, "xaxis.ticktext": t.ticktext });
      });
    });
  }
  function showEventRow(el, key) {
    const host = el.querySelector("#histevents"), row = host?.querySelector(`tr[data-key="${key}"]`);
    if (!row) return;
    for (const x of host.querySelectorAll("tr.on")) x.classList.remove("on");
    row.classList.add("on");
    row.scrollIntoView({ block: "center", behavior: "smooth" });
    if (row.dataset.lat) focusPoint(row.dataset.lat, row.dataset.lon, row.children[1]?.textContent);
  }
  function wireEvents(el) {
    el.querySelector("#histplotreset").onclick = () => { const gd = el.querySelector("#histplot"); if (gd?.data) Plotly.relayout(gd, { "xaxis.autorange": true }); };
    drawEvents(el);
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
  // one spot on the map where several things sit: a chip for each, so the
  // reader picks rather than gets the topmost. Tracks list every call there.
  function renderSite(el) {
    const [lat, lon] = hist.slug.slice(3).split(",").map(Number);
    if (!Number.isFinite(lat) || !Number.isFinite(lon)) { el.innerHTML = crumb() + `<div class="empty">That is not a place on the map.</div>`; return; }
    const items = (hist.sites || siteItems()).get(siteKey(lat, lon)) || [];
    const place = items.find((x) => x.kind === "place")?.p;
    const byPage = new Map();                                        // a track called here more than once: one chip, every visit
    for (const x of items) {
      const slug = x.kind === "place" ? x.p.page : x.a.page;
      if (!byPage.has(slug)) byPage.set(slug, { kind: x.kind, title: x.kind === "place" ? x.p.name : x.a.title, slug, visits: [], note: x.kind === "place" ? [x.p.kind, x.p.note].filter(Boolean).join(" · ") : x.kind === "track" ? "" : fmtDate(x.a) });
      if (x.w) byPage.get(slug).visits.push([dateLabel(x.w.date || ""), x.w.note].filter(Boolean).join(": "));
    }
    const order = { place: 0, track: 1 };
    const chips = [...byPage.values()].sort((p, q) => ((order[p.kind] ?? 2) - (order[q.kind] ?? 2)) || p.title.localeCompare(q.title));
    const title = place ? esc(place.name) : "This spot";
    el.innerHTML = crumb(here(title, hist.slug)) + `<h2>${title} <span class="muted">${coordLink(lat, lon, place?.name || "")}</span></h2>` +
      `<p class="lead">${chips.length} things on the map share this spot. Pick one.</p>` +
      `<div class="pagelist">${chips.map((c) => pageLink({ slug: c.slug, kind: c.kind, title: c.title, summary: c.visits.length ? c.visits.join(" · ") : c.note })).join("")}</div>`;
    focusPoint(lat, lon, place?.name || "");
    el.scrollTop = 0;
  }
  // how the history was made: the project's own account (PROVENANCE.md,
  // written on grid and pulled with the data), with its numbers section
  // replaced by a few of this build's figures and a word on what happens
  // aboard. Short, for readers of the history rather than its makers.
  function renderProvenance(el) {
    const pv = hist.provenance, c = pv?.counts || {}, w = pv?.work || {};
    const n = (x) => (x == null ? "–" : Number(x).toLocaleString("en-CA"));
    const day = (iso) => iso ? new Date(iso).toLocaleDateString("en-CA", { year: "numeric", month: "long", day: "numeric" }) : "";
    const req = w.requests || {}, answered = (req.approved || 0) + (req.denied || 0) + (req.done || 0);
    const flagsNow = hist.flags.size;
    const tile = (v, l) => `<div class="stat"><b>${n(v)}</b><span>${l}</span></div>`;
    const figures = `<div class="stats">${tile(c.pages, "narrative pages")}${tile(c.artifacts, "artifacts")}${tile(c.sources, "works cited")}${tile(c.people, "people named")}</div>` +
      (pv?.generated ? `<p class="muted small">Counted ${day(pv.generated)}${c.pages_draft ? `; ${n(c.pages_draft)} pages are still drafts` : ""}${answered ? `; ${n(answered)} questions answered by a person` : ""}.</p>` : `<p class="muted small">The figures are counted when the history is next published.</p>`);
    const aboard = `## Aboard the ship

Ask Ada answers from these pages with a local model on the ship: it cites the pages it was given and is not itself a source. Anything that looks wrong can be flagged from its card with the small flag in the corner; flags go to the layer's author${flagsNow ? `, and ${n(flagsNow)} ${flagsNow === 1 ? "is" : "are"} flagged now` : ""}.`;
    let body;
    if (pv?.text) {
      // their account, section by section: the H1 is the page's own title
      // and the numbers section is replaced by this build's figures
      const parts = pv.text.replace(/\r/g, "").replace(/^#\s+[^\n]*\n/, "").split(/\n(?=## )/);
      body = parts.map((sec) => /^## The record, in numbers/i.test(sec) ? `<h3>The record, in numbers</h3>${figures}` : markdown(sec)).join("\n");
      if (!/## The record, in numbers/i.test(pv.text)) body += `<h3>The record, in numbers</h3>${figures}`;
    } else {
      body = `<p class="lead">Everything in this tab was researched and written by AI agents from the sources named on each item, then checked; the project's own account travels with the data and is not in this build yet.</p><h3>The record, in numbers</h3>${figures}`;
    }
    el.innerHTML = crumb(here("Provenance", "provenance")) + `<h2>How this history was made</h2><div class="provenance wiki">${body}${markdown(aboard)}</div>`;
  }
  // the works each domain cites: through the artifacts, events, places,
  // people, animals and vessels of its topics, and the natural record's
  // observations and subjects
  function citedBy() {
    const by = { history: new Set(), nature: new Set() };
    const add = (rows) => { for (const r of rows || []) if (r.bibkey) by[r.topic ? topicDomain(r.topic) : "history"]?.add(r.bibkey); };
    for (const rows of [hist.artifacts, hist.events, hist.places, hist.people, hist.animals, hist.vessels]) add(rows);
    for (const k of UW.natureViews?.cited?.() || []) by.nature.add(k);
    return by;
  }
  async function renderBib(el) {
    const bib = await bibliography();
    const by = citedBy(), both = hist.domains.size === DOMAINS_ALL.length;
    // the works the labels on cite; one nobody cites shows while both are on
    const inScope = (e) => domainsOn().some((d) => by[d].has(e.key)) || (both && !by.history.has(e.key) && !by.nature.has(e.key));
    const sorted = [...bib].filter(inScope).sort((a, b) => (a.author || a.title || "").localeCompare(b.author || b.title || ""));
    const cited = (d) => bib.filter((e) => by[d].has(e.key)).length;
    el.innerHTML = crumb(here("Bibliography", "bib")) + `<h2>Bibliography <span class="muted">${sorted.length} works</span></h2>` +
      `<p class="muted small">${cited("history")} works cited by the history, ${cited("nature")} by the natural record${both ? "" : `; the ${domainOn("history") ? "natural record's" : "history's"} are left out while its label is off`}.</p>` +
      `<div class="bibexport"><span class="lbl">Export</span><a class="chip small" href="data/history/references.bib" download title="the build's own BibTeX file">BibTeX</a><button type="button" class="chip small" data-export="ris" title="RIS, for EndNote, Zotero and Mendeley">RIS</button><button type="button" class="chip small" data-export="csl" title="CSL-JSON, for citation processors and Zotero">CSL-JSON</button><button type="button" class="chip small" data-export="txt" title="the MLA list as plain text">Plain text</button></div>` +
      `<ol class="mla">${sorted.map((e) => `<li id="bib-${esc(e.key)}">${mla(e)} <a class="muted small" href="#wiki/source/${esc(e.key)}" data-slug="source/${esc(e.key)}">page</a></li>`).join("")}</ol>`;
    for (const b of el.querySelectorAll("button[data-export]")) b.onclick = () => {
      const stamp = (UW.M.history?.stamp || "").slice(0, 10) || "latest";
      if (b.dataset.export === "ris") download(`arctic-history-${stamp}.ris`, ris(sorted), "application/x-research-info-systems");
      else if (b.dataset.export === "csl") download(`arctic-history-${stamp}.json`, csl(sorted), "application/vnd.citationstyles.csl+json");
      else download(`arctic-history-${stamp}.txt`, sorted.map((e) => { const d = document.createElement("div"); d.innerHTML = mla(e); return d.textContent; }).join("\n\n") + "\n", "text/plain");
    };
  }

  // ---------------------------------------------------------------- the menus
  // The kinds, twice: the pane's Browse dropdown opens a kind's page (or a
  // domain's, or one of the tab's own pages); under the map's layer pills,
  // while a past layer is on, a menu like the header's legs menu filters
  // what the layer shows, a checkbox per kind.
  // a layer's filter menu: built once in its bar, then kept in step with the
  // set (its boxes, its count, its colours), so it stays open through a change
  function renderMenu(bar, word, rows, set, storeKey, layerOn) {
    const keys = rows.filter((r) => r.key).map((r) => r.key);
    if (!bar.querySelector("details")) {
      bar.innerHTML = `<details class="legmenu"><summary></summary><div class="pop">` +
        `<div class="pophead">On the map · <a href="#" data-all>all</a> · <a href="#" data-none>none</a></div><ul>` +
        rows.map((r) => r.key
          ? `<li><label title="${esc(r.hint || "")}"><input type="checkbox" data-k="${r.key}"><span class="dot" data-k="${r.key}"></span><span class="name">${esc(r.label)}</span></label></li>`
          : `<li class="ghead">${esc(r.label)}</li>`).join("") + `</ul></div></details>`;
      const apply = () => { store.set(storeKey, [...set]); renderMenu(bar, word, rows, set, storeKey, layerOn); if (layerOn()) UW.renderMap(); };
      for (const box of bar.querySelectorAll("input[data-k]")) box.onchange = () => { box.checked ? set.add(box.dataset.k) : set.delete(box.dataset.k); apply(); };
      bar.querySelector("[data-all]").onclick = (e) => { e.preventDefault(); for (const k of keys) set.add(k); apply(); };
      bar.querySelector("[data-none]").onclick = (e) => { e.preventDefault(); set.clear(); apply(); };
    }
    for (const box of bar.querySelectorAll("input[data-k]")) box.checked = set.has(box.dataset.k);
    for (const r of rows) if (r.key) bar.querySelector(`.dot[data-k="${r.key}"]`).style.background = r.colour;
    bar.querySelector("summary").textContent = `${word} · ${keys.filter((k) => set.has(k)).length}/${keys.length}`;
  }
  // the artifact kinds in their hierarchy, for the map's menu: a head with
  // no kind of its own (People) is a label over its kinds
  function typeRows() {
    const row = (k) => ({ key: k, label: TYPES[k].label, colour: TYPES[k].colour });
    return GROUPS.flatMap((g) => { const under = g.under.filter((k) => TYPES[k]); return [...(TYPES[g.head] ? [row(g.head)] : under.length ? [{ label: KINDS[g.head].label }] : []), ...under.map(row)]; });
  }
  // the kinds as a dropdown's groups, the one open selected
  function kindOptions(cur) {
    const opt = (slug, label) => `<option value="${slug}" ${cur === slug ? "selected" : ""}>${esc(label)}</option>`;
    return GROUPS.map((g) => g.under.length ? `<optgroup label="${esc(KINDS[g.head].label)}">${opt(`kind/${g.head}`, `All ${KINDS[g.head].label.toLowerCase()}`)}${g.under.map((k) => opt(`kind/${k}`, KINDS[k].label)).join("")}</optgroup>` : opt(`kind/${g.head}`, KINDS[g.head].label)).join("");
  }
  function renderChips() {
    // the Browse dropdown for the whole wiki: its own pages, then the kinds,
    // then (while Nature is on) the natural domains
    const sel = $("#histkindsel");
    if (sel) {
      const opt = (slug, label) => `<option value="${slug}" ${hist.slug === slug ? "selected" : ""}>${esc(label)}</option>`;
      const natv = domainOn("nature") ? UW.natureViews : null;
      const pages = [opt("explore", "Explore"), opt("bib", "Bibliography"), opt("provenance", "Provenance"), ...(natv ? [opt("record", "Observations"), opt("journal", "/Share Photos")] : [])].join("");
      sel.innerHTML = `<option value="" ${!hist.slug || !/^(kind\/|domain\/|subjects\/|explore$|bib$|provenance$|record$|journal$)/.test(hist.slug) ? "selected" : ""}>Browse…</option>` +
        `<optgroup label="Pages">${pages}</optgroup>` +
        kindOptions(hist.slug.split("/").slice(0, 2).join("/")) +
        (natv?.domainOptions ? `<optgroup label="Domains">${natv.domainOptions(hist.slug)}</optgroup>` : "");
      sel.onchange = () => open(sel.value);
    }
    // the map's menus, each after its layer's pill: the kinds after History, the domains after Nature
    const bar = $("#maphistlayers");
    if (bar) {
      renderMenu(bar, "Kinds", typeRows(), hist.types, "hist.types", () => UW.state.history || UW.state.nature);
      bar.hidden = !UW.state.history || !UW.M.history;
    }
    UW.natureViews?.mapMenu?.();
  }
  // who answers the Ask button: Ada for the history, Doc for the nature, the crew's room for both
  const ASK = {
    history: { label: "Ask Ada", room: "ada", title: "open the Library in the chat and ask Ada, the librarian, about the region's past" },
    nature: { label: "Ask Doc", room: "doc", title: "open the Lab in the chat and ask Doc, the naturalist; a sighting told to Doc goes into the journal" },
    both: { label: "Ask both", room: "deck", title: "open the Deck in the chat, where Ada and Doc both answer: Ada from the history, Doc from the nature" },
  };
  const askTarget = () => ASK[hist.domains.size > 1 ? "both" : domainOn("nature") ? "nature" : "history"];
  function renderTools() {
    $("#histhome").classList.toggle("on", !hist.slug && !hist.search);
    $("#histexplore").classList.toggle("on", hist.slug === "explore");
    $("#histback").disabled = !hist.slug && nav.n === 0;
    for (const b of document.querySelectorAll("#wikidomains .dom")) { const on = domainOn(b.dataset.domain); b.classList.toggle("on", on); b.setAttribute("aria-pressed", String(on)); }
    const ask = $("#histask"), t = askTarget(); ask.textContent = t.label; ask.title = t.title;
  }
  // a label switched: the pane lists that domain or not, and on the tab its map layer follows
  function setDomain(d, on) {
    if (!on && hist.domains.size === 1) { UW.toast?.("One of the two stays on; switch the other on first."); return; }
    on ? hist.domains.add(d) : hist.domains.delete(d);
    store.set("wiki.domains", domainsOn());
    if (stashed) { UW.state[d] = on; store.set(d, on); pill(d, on); }
    if (on) ensureAll().then(() => render()).catch(() => {}); else render();
  }
  async function render() {
    ns = NS.wiki;
    renderChips(); renderTools();
    await renderMain();
    if (UW.state.history || UW.state.nature) UW.renderMap();
  }

  // ---------------------------------------------------------------- navigation
  // Every view is a browser history entry (#wiki/<slug>), so the browser's
  // back button walks back through the pages and the pane's Back button does
  // the same; past the first view it goes home rather than off the site. The
  // prefixes of the two tabs of before (#history/, #nature/) still open the
  // wiki: the crew's citations and old links carry them.
  const nav = { n: 0 };                                   // how many views this visit has pushed
  const ALIAS = { timeline: "kind/event", people: "kind/people" };
  const HASH_RX = /^#(wiki|history|nature)\/(.*)$/;
  const hashSlug = () => { const m = HASH_RX.exec(location.hash); return m ? decodeURIComponent(m[2]) : null; };
  function open(slug, opts = {}) {
    slug = ALIAS[slug] || slug;
    hist.slug = slug; store.set("wiki.slug", slug);
    hist.more = { today: false, here: false };
    if (hist.search) { hist.search = ""; const q = $("#histsearch"); if (q) q.value = ""; }
    if (!opts.pop) { nav.n++; try { history.pushState({ hist: slug, n: nav.n }, "", `#wiki/${slug}`); } catch {} }
    if (!opts.quiet && $("#pane-wiki").hidden) UW.showTab("wiki");
    render();
  }
  window.addEventListener("popstate", (e) => {
    const s = e.state?.hist ?? hashSlug();
    if (s == null) { nav.n = 0; if (!$("#pane-wiki").hidden) { hist.slug = ""; store.set("wiki.slug", ""); render(); } return; }
    nav.n = e.state?.n ?? 0;
    open(s, { pop: true });
  });
  function goBack() {
    if (nav.n > 0 && history.state?.hist != null) history.back();
    else open("");
  }
  function focusPoint(lat, lon, label, type = "", layer = "history") {
    if (lat == null) return;
    if (!UW.state[layer]) { UW.state[layer] = true; store.set(layer, true); pill(layer, true); renderChips(); }
    if (type && TYPES[type] && !hist.types.has(type)) { hist.types.add(type); store.set("hist.types", [...hist.types]); renderChips(); }
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
    const flag = e.target.closest(".histpane .flag[data-flag]");
    if (flag) { e.preventDefault(); e.stopPropagation(); toggleFlag(flag.dataset.flag); return; }
    const kw = e.target.closest('.histpane a[href^="#kw-"]');
    if (kw) { e.preventDefault(); document.getElementById(kw.getAttribute("href").slice(1))?.scrollIntoView({ block: "start", behavior: "smooth" }); return; }
    const pin = e.target.closest("#pane-wiki .pin[data-lat]");
    if (pin) { e.preventDefault(); e.stopPropagation(); focusPoint(pin.dataset.lat, pin.dataset.lon, pin.dataset.label, pin.dataset.type, pin.dataset.layer || "history"); return; }
    const a = e.target.closest("#pane-wiki a[data-slug], #pane-wiki a[data-topic]");
    if (!a) return;
    e.preventDefault();
    if (a.classList.contains("vig")) {
      if (a.dataset.lat) { const art = a.dataset.slug ? artifactById(a.dataset.slug.replace(/^artifact\//, "")) : null; if (art) focusArtifact(art); else focusPoint(a.dataset.lat, a.dataset.lon, a.querySelector(".txt")?.textContent || "", "", a.dataset.slug?.startsWith("observation/") ? "nature" : "history"); }
      if (a.dataset.slug) open(a.dataset.slug); else if (a.dataset.topic) open(`topic/${a.dataset.topic}`);
      return;
    }
    if (a.dataset.topic != null) open(`topic/${a.dataset.topic}`);
    else open(a.dataset.slug || "");
  });
  document.addEventListener("keydown", (e) => {
    const flag = (e.key === "Enter" || e.key === " ") && e.target.closest?.(".histpane .flag[data-flag]");
    if (flag) { e.preventDefault(); toggleFlag(flag.dataset.flag); }
  });
  // a click on a spot where several things sit opens the chooser instead
  const crowded = (lat, lon) => lat != null && (hist.sites?.get(siteKey(lat, lon))?.length || 0) > 1;
  UW.onHistoryClick = (id, pt) => {
    // the mark moves to what was clicked; the map keeps its view; the page
    // opens even if it is the one the reader has since left
    if (pt && pt.lat != null) UW.state.focus = { lat: +pt.lat, lon: +pt.lon, label: String(pt.text || "").replace(/<br>.*$/s, "").replace(/<[^>]+>/g, "") };
    if (pt && crowded(pt.lat, pt.lon)) { open(`at/${siteKey(pt.lat, pt.lon)}`); return; }
    if (id.startsWith("place:")) { open(id.slice(6)); return; }
    const a = artifactById(id.split("|")[0]); if (a) open(a.page);
  };
  // the mark sits over the point it marks and takes the click: find what
  // lies under it and open that
  UW.onFocusClick = (pt) => {
    if (!(UW.state.history || UW.state.nature) || !hist.artifacts || pt?.lat == null) return false;
    if (crowded(pt.lat, pt.lon)) { open(`at/${siteKey(pt.lat, pt.lon)}`); return true; }
    const near = (la, lo) => la != null && Math.abs(la - pt.lat) < 1e-6 && Math.abs(lo - pt.lon) < 1e-6;
    let hit = null;
    eachLayer(() => {
      if (hit) return;
      const t = curTopic();
      for (const a of shownArtifacts()) {
        if (a.type === "track" ? (a.waypoints || []).some((w) => near(w.lat, w.lon)) : near(a.lat, a.lon)) { hit = a.page; return; }
      }
      if (hist.types.has("place")) { const p = hist.places.find((p) => inDomain(p) && (!t || p.topic === t) && near(p.lat, p.lon)); if (p) hit = p.page; }
    });
    if (hit) { open(hit); return true; }
    return false;
  };

  // ---------------------------------------------------------------- the map layer
  // Several things can sit on one spot: a cape five patrols called at, and
  // its place pin. The hover then lists every one of them, and a click opens
  // a chooser (the "at/<lat>,<lon>" view) rather than the topmost alone.
  const siteKey = (lat, lon) => `${(+lat).toFixed(3)},${(+lon).toFixed(3)}`;
  const short = (t, n = 60) => esc(t.length > n ? t.slice(0, n - 3) + "…" : t);
  function siteItems() {
    // what the map shows now, grouped by spot: {key → [{kind, a|p, w, label}]}
    const sites = new Map();
    const add = (lat, lon, item) => { if (lat == null || lon == null) return; const k = siteKey(lat, lon); if (!sites.has(k)) sites.set(k, []); sites.get(k).push(item); };
    const shown = shownArtifacts();
    const year = (a) => { const y = yearOf(a.date_start); return y == null ? "" : ` · ${yearLabel(y)}`; };
    for (const a of shown) {
      if (a.type === "track") {
        for (const w of a.waypoints || []) add(w.lat, w.lon, { kind: "track", a, w, label: `${short(a.title)} · ${esc(dateLabel(w.date || ""))}${w.note ? " · " + short(w.note, 50) : ""}` });
      } else add(a.lat, a.lon, { kind: a.type, a, label: `${short(a.title)}${year(a)}` });
    }
    if (hist.types.has("place")) {
      const t = curTopic();
      for (const p of hist.places) if (p.lat != null && inDomain(p) && (!t || p.topic === t)) add(p.lat, p.lon, { kind: "place", p, label: `${esc(p.name)}${p.kind ? " · " + esc(p.kind) : ""}` });
    }
    return sites;
  }
  // the hover for one item: its own label, or every label at its spot, stacked
  const siteText = (sites, lat, lon, own) => { const xs = sites.get(siteKey(lat, lon)); return xs && xs.length > 1 ? xs.map((x) => x.label).join("<br>") : own; };
  const prevExtra = UW.extraMapTraces;
  UW.extraMapTraces = () => {
    const out = prevExtra ? prevExtra() : [];
    const wanted = UW.state.history || UW.state.nature;
    if (!wanted || !hist.artifacts) { if (wanted && !hist.artifacts) ensure().then(() => UW.renderMap()); return out; }
    hist.sites = new Map();
    eachLayer(() => layerTraces(out));
    return out;
  };
  // one past layer's traces: the pane's own artifacts and places, in its namespace
  function layerTraces(out) {
    const shown = shownArtifacts();
    const sites = siteItems();
    for (const [k, v] of sites) hist.sites.set(k, [...(hist.sites.get(k) || []), ...v]);
    const year = (a) => { const y = yearOf(a.date_start); return y == null ? "" : ` · ${yearLabel(y)}`; };
    const hover = (a) => `${short(a.title)}${year(a)}`;
    // tracks are faint until one is open: that one is drawn last, wide and bright
    const tracks = shown.filter((x) => x.type === "track" && x.geometry?.coordinates?.length > 1);
    const cur = hist.slug;
    const picked = cur.startsWith("artifact/") ? tracks.find((x) => x.page === cur) : null;
    for (const a of [...tracks.filter((x) => x !== picked), ...(picked ? [picked] : [])]) {
      const c = a.geometry.coordinates, col = topicColour(a.topic), bold = a === picked;
      out.push({ type: "scattermap", mode: "lines", name: `hist-${a.id}`, showlegend: false, hoverinfo: "text",
        lat: c.map((p) => p[1]), lon: c.map((p) => p[0]), text: c.map(() => hover(a)), customdata: c.map(() => `hist:${a.id}`),
        line: { width: bold ? 4 : 2, color: col }, opacity: bold ? 1 : .3 });
      if (a.waypoints?.length) out.push({ type: "scattermap", mode: "markers", name: `hist-${a.id}-wp`, showlegend: false, hoverinfo: "text",
        lat: a.waypoints.map((w) => w.lat), lon: a.waypoints.map((w) => w.lon),
        text: a.waypoints.map((w) => siteText(sites, w.lat, w.lon, `${esc(dateLabel(w.date || ""))}${w.note ? " · " + short(w.note, 50) : ""}`)),
        customdata: a.waypoints.map((w) => `hist:${a.id}|${w.date || ""}`), marker: { size: bold ? 8 : 6, color: col, opacity: bold ? .9 : .35 } });
    }
    const pins = shown.filter((x) => x.type !== "track" && x.lat != null);
    if (pins.length) out.push({ type: "scattermap", mode: "markers", name: `${ns.key}-artifacts`, showlegend: false, hoverinfo: "text",
      lat: pins.map((a) => a.lat), lon: pins.map((a) => a.lon), text: pins.map((a) => siteText(sites, a.lat, a.lon, hover(a))), customdata: pins.map((a) => `hist:${a.id}`),
      marker: { size: pins.map((a) => a.type === "event" ? 11 : 9), color: pins.map((a) => TYPES[a.type]?.colour || C.muted), opacity: .92 } });
    if (hist.types.has("place")) {
      const t = curTopic(), pl = hist.places.filter((p) => p.lat != null && inDomain(p) && (!t || p.topic === t));
      if (pl.length) out.push({ type: "scattermap", mode: "markers", name: `${ns.key}-places`, showlegend: false, hoverinfo: "text",
        lat: pl.map((p) => p.lat), lon: pl.map((p) => p.lon), text: pl.map((p) => siteText(sites, p.lat, p.lon, `${esc(p.name)}${p.kind ? " · " + esc(p.kind) : ""}`)), customdata: pl.map((p) => `hist:place:${p.page}`),
        marker: { size: 7, color: TYPES.place.colour, opacity: .85 } });
      // the gazetteer's sites with nobody in them (abandoned settlements, camps, stations) belong here
      // rather than on the ship's Places layer, which keeps the settlements with people
      const empty = ns.key === "history" && !t ? (UW.state.communities_data || []).filter((c) => !(c.pop > 0) && c.lat != null) : [];
      if (empty.length) out.push({ type: "scattermap", mode: "markers", name: "history-sites", showlegend: false, hoverinfo: "text",
        lat: empty.map((c) => c.lat), lon: empty.map((c) => c.lon), text: empty.map((c) => `${esc(c.name)}${c.alt?.length ? " · " + esc(c.alt.join(" · ")) : ""}<br>${esc(c.region)}, ${c.cc === "GL" ? "Greenland" : "Canada"} · no one lives here now`),
        marker: { symbol: "square", size: 5, color: TYPES.place.colour, opacity: .55 } });
    }
  }

  // ---------------------------------------------------------------- the crew
  // the page being read, as context for whoever answers in the chat: a
  // page of either half, not a listing
  UW.wikiContext = () => (hist.slug && !["explore", "bib", "provenance", "record", "journal"].includes(hist.slug) && !/^(topic|kind|at|subjects|domain|import)\//.test(hist.slug)) ? hist.slug : "";
  UW.historyContext = UW.natureContext = UW.wikiContext;
  UW.wikiOpen = UW.historyOpen = UW.natureOpen = (slug) => open(slug);

  // ---------------------------------------------------------------- wiring
  const pill = (layer, state) => { const b = document.querySelector(`#maplayers button[data-layer="${layer}"]`); if (b) { b.classList.toggle("on", !!state); b.setAttribute("aria-pressed", String(!!state)); } };
  function wire() {
    $("#histsearch").oninput = debounce((e) => { hist.search = e.target.value; renderMain(); }, 150);
    $("#histback").onclick = goBack;
    $("#histhome").onclick = () => open("");
    $("#histexplore").onclick = () => open(hist.slug === "explore" ? "" : "explore");
    $("#histask").onclick = () => UW.chatRoom?.(askTarget().room);
    for (const b of document.querySelectorAll("#wikidomains .dom")) b.onclick = () => setDomain(b.dataset.domain, !domainOn(b.dataset.domain));
    for (const layer of LAYERS) {
      const p = document.querySelector(`#maplayers button[data-layer="${layer}"]`);
      if (p) { p.hidden = !UW.M.history; p.addEventListener("click", () => setTimeout(renderChips, 0)); }
    }
    hist.slug = ALIAS[hist.slug] || hist.slug;
    // a link into the wiki (#wiki/<slug>, or the prefixes of before) opens that view
    const s = hashSlug();
    if (s != null) { hist.slug = s; store.set("wiki.slug", s); try { history.replaceState({ hist: s, n: 0 }, "", `#wiki/${s}`); } catch {} }
  }
  // On the Wiki tab the map is the wiki's: the ship's own layers step aside
  // and come back when the tab is left; plan and places stay, and the two
  // past layers follow the labels.
  const SHIP_LAYERS = ["stations", "cameras", "events", "track"];
  let stashed = null;
  function historyMap(on) {
    if (on && !stashed) {
      stashed = { sat: UW.state.sat, satAt: UW.state.satAt, history: UW.state.history, nature: UW.state.nature };
      for (const l of SHIP_LAYERS) { stashed[l] = UW.state[l]; UW.state[l] = false; pill(l, false); }
      UW.state.sat = ""; UW.state.satAt = null;
    } else if (!on && stashed) {
      for (const l of SHIP_LAYERS) { UW.state[l] = stashed[l]; pill(l, stashed[l]); }
      UW.state.sat = stashed.sat; UW.state.satAt = stashed.satAt;
      for (const l of LAYERS) { UW.state[l] = stashed[l]; pill(l, stashed[l]); }
      stashed = null;
    }
    if (on) for (const l of LAYERS) { UW.state[l] = domainOn(l); pill(l, domainOn(l)); }
    document.body.classList.toggle("tab-wiki", on);
    renderChips();
  }
  const prevTab = UW.onTab;
  UW.onTab = (name) => {
    prevTab?.(name);
    const on = name === "wiki", was = !!stashed;
    historyMap(on);
    if (on !== was) UW.renderMap();
    if (!on) return;
    ensureAll().then((ok) => { if (ok) render(); else renderMain(); }).catch(() => { UW.setLoadError("Wiki", true); });
  };
  const prevRefresh = UW.refreshExtraData;
  UW.refreshExtraData = () => { prevRefresh?.(); for (const l of LAYERS) { const p = document.querySelector(`#maplayers button[data-layer="${l}"]`); if (p) p.hidden = !UW.M.history; }
    if (!$("#pane-wiki").hidden || UW.state.history || UW.state.nature) ensureAll().then(() => { if (!$("#pane-wiki").hidden) render(); else { renderChips(); UW.renderMap(); } }).catch(() => {}); };
  // the pane's helpers and pieces for the natural half (nature.js, loaded
  // next), so the two halves read alike
  UW.histShared = { ensure, esc, yearOf, yearLabel, dateLabel, km, markdown, crossLink, coordLink, mapLink, facts, sourceRef, peopleStrip,
    artifactCard, artifactById, eventById, topicOf, statusTag, pageLink, whereName, yearTicks, pageDomain, topicDomain, topicColour, topicImage, data: () => hist,
    crumb, here, open, slug: () => hist.slug, rerender: () => renderMain(), refresh: () => render(), menu: renderMenu, KINDS, TYPES, topicCard, collectionChip, backlinksHTML, wireBackmore, letterList, domainOn, focusPoint };
  wire();
  document.addEventListener("uw:theme", () => { if (!$("#pane-wiki").hidden && hist.artifacts) render(); });   // the chips, dots and the timeline take the new colours
  if (hashSlug() != null && $("#pane-wiki").hidden) UW.showTab("wiki");
  else if (document.querySelector("#tabs button.on")?.dataset.tab === "wiki") UW.onTab("wiki");
  else if ((UW.state.history || UW.state.nature) && UW.M.history) ensureAll().then(() => { renderChips(); UW.renderMap(); }).catch(() => {});
})();
