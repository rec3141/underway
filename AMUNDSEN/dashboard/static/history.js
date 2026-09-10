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
  const KINDS = { people: { label: "People", colour: "#ffa198" }, object: TYPES.object, quote: TYPES.quote, text: TYPES.text, place: TYPES.place, track: TYPES.track, map: TYPES.map, event: TYPES.event, image: TYPES.image,
    animal: { label: "Animals", colour: "#e3b341" }, vessel: { label: "Vessels", colour: "#56d364" } };
  // the kinds in their hierarchy, wherever the chips appear: the animals and
  // what people made and said sit under People, the vessels and what lies on
  // the ground under Places
  const GROUPS = [
    { head: "people", under: ["animal", "object", "quote", "text"] },
    { head: "place", under: ["vessel", "track", "map"] },
    { head: "event", under: [] },
    { head: "image", under: [] },
  ];
  const KIND_LABEL = { page: "explore" };                  // a narrative page is an Explore page on the site
  const kindLabel = (k) => KIND_LABEL[k] || k;
  // a page's standing with the crew: draft until they call it good
  const statusTag = (s) => s ? `<span class="status ${esc(s)}" title="${s === "good" ? "the crew have checked this page" : "the crew are still at work on this page"}">${esc(s)}</span>` : "";
  const TOPIC_COLOURS = ["#ffb454", "#5cc8ff", "#7ee787", "#ff7b72", "#d2a8ff", "#f2cc60", "#79c0ff", "#ffa198", "#56d364", "#e3b341", "#a5d6ff", "#ff9bce"];
  const TIMELINE_FROM = 1400;                              // the Events chart opens zoomed to here; the table and the chart hold everything
  const VIG_N = 5;                                         // vignette lines shown before "see more"

  const hist = {
    index: null, artifacts: null, timeline: null, places: [], people: [], events: [], animals: [], vessels: [], bib: null, stamp: null, loading: null,
    flags: new Map(),                                     // artifact id → the flag anyone has raised for review (shared through the server)
    names: null,                                          // every person and place name that has a page, longest first, for the cross-links
    slug: store.get("hist.slug", ""),                     // what is shown: "" home, explore, bib, kind/<k>, topic/<t>, or a page
    types: new Set(store.get("hist.types", Object.keys(TYPES))),   // the kinds the map layer shows
    search: "",
    faces: null,                                          // the face crops the backend publishes, or null
    coast: null,                                          // the coastline (Natural Earth) as lines, for the route sketches
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
      const [index, arts, tl, pl, pe, fa, ev, an, ve] = await Promise.all([
        cachedJSON("index", "data/history/index.json"),
        cachedJSON("artifacts", "data/history/artifacts.json"),
        cachedJSON("timeline", "data/history/timeline.json"),
        maybe("places", "data/history/places.json"),
        maybe("people", "data/history/people.json"),
        maybe("faces", "data/history/faces.json"),
        maybe("events", "data/history/events.json"),
        maybe("animals", "data/history/animals.json"),
        maybe("vessels", "data/history/vessels.json"),
      ]);
      hist.animals = an?.animals || []; hist.vessels = ve?.vessels || [];
      hist.index = index; hist.artifacts = arts.artifacts || []; hist.timeline = tl.timeline || [];
      hist.places = pl?.places || []; hist.people = pe?.people || []; hist.faces = fa?.faces || null; hist.events = ev?.events || [];
      hist.stamp = UW.M.history.stamp; hist.pages = new Map(); hist.bib = null; hist.names = null;
      for (const a of hist.artifacts) a._year = yearOf(a.date_start);
      await loadFlags();
      return true;
    })().finally(() => { hist.loading = null; });
    return hist.loading;
  }
  // the review flags: raised on an artifact's card by anyone, with a note,
  // and kept on the server so every browser shows the same ones; the alerts
  // timer reports them to the keeper. Whoever raised a flag withdraws it
  // while theirs is the only one; once several people have, only an admin.
  const me = () => ({ token: store.get("chat.token", ""), name: store.get("chat.name", "") });
  function takeFlags(r) {
    hist.flags = new Map((r?.flags || []).map((f) => [f.id, f])); hist.admin = !!r?.admin;
    for (const el of document.querySelectorAll("#pane-history .flag[data-flag]")) {
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
  const topicColour = (slug) => TOPIC_COLOURS[Math.max(0, hist.index?.topics.findIndex((t) => t.slug === slug) || 0) % TOPIC_COLOURS.length];
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
    if (kind === "event") return eventSlug(id);
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
          const len = Math.round((Date.UTC(b.y, b.m - 1, b.d) - Date.UTC(a.y, a.m - 1, a.d)) / 864e5) + 1;
          if (day < 7 || len - day <= 7) out.push({ year: y, kind: `day ${day + 1} of ${len}`, label: r.label, place: r.place, topic: r.topic, lat: r.lat, lon: r.lon, slug: pageFor(r.entity_kind, r.entity_id) });
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
    for (const p of [...(hist.people || []), ...(hist.animals || [])]) {
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
    s = s.replace(/!\[([^\]]*)\]\(([^)\s]+)\)/g, (m, alt, src) => {
      const img = `<img src="${src}" alt="${alt}" loading="lazy">`, a = artifactByUrl(src);
      return a ? `<a class="imglink" href="#history/${esc(a.page)}" data-slug="${esc(a.page)}" title="${esc(a.title)}">${img}</a>` : img;
    });
    s = s.replace(/\[([^\]]+)\]\(([^)\s]+)\)/g, (m, label, href) =>
      /^https?:\/\//.test(href) ? `<a href="${href}" target="_blank" rel="noopener">${label}</a>` : `<a href="#history/${href}" data-slug="${href}">${label}</a>`);
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
      if (list || quote.length || table) flush();
      para.push(line.trim());
    }
    flush();
    return out.join("\n");
  }

  // ---------------------------------------------------------------- the pane
  const fmtDate = (a) => dateLabel(a.date_text || a.date_start || "");
  const crumb = (...rest) => `<div class="crumb"><a href="#history/" data-slug="">History</a>${rest.map((r) => ` › ${r}`).join("")}</div>`;
  // the last step of a crumb: the page itself, as its own link (the address to pass on)
  const here = (label, slug) => `<a class="here" href="#history/${esc(slug)}" data-slug="${esc(slug)}" title="this page's address">${label}</a>`;
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
      coast = runs.map((r) => `<polyline points="${r}" fill="none" stroke="#3a4a5c" stroke-width="1"/>`).join("");
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
  function artifactCard(a, opts = {}) {
    const t = TYPES[a.type] || {};
    let media = "";
    const picture = a.url && /\.(jpe?g|png|gif|tiff?|webp|bmp)$/i.test(a.url);
    if (a.type === "track") media = trackSketch(a);
    else if (picture && (a.type === "image" || a.type === "map")) media = `<img class="thumb" src="${esc(a.thumb || a.url)}" alt="" loading="lazy">`;
    else if (a.type === "image" || a.type === "map") media = `<span class="thumb none">${a.url ? esc(a.url.split(".").pop().toUpperCase()) + " · no picture yet" : "interactive resource · no picture yet"}</span>`;
    const quote = a.type === "quote" && a.description ? `<q>${esc(quoteOf(a.description))}</q>` : "";
    const [plat, plon] = a.type === "track" ? trackMid(a) : [a.lat, a.lon];
    return `<a class="artcard ${esc(a.type)}" href="#history/${esc(a.page)}" data-slug="${esc(a.page)}" title="${esc(a.title)}">${flagMark(a)}${media}<span class="dot" style="background:${t.colour || "#8b9bb0"}"></span>` +
      `<span class="kind">${esc(a.type)}</span><b>${esc(a.title)}</b>${quote}<span class="when">${esc(fmtDate(a))}${plat != null ? " " + mapLink(plat, plon, a.title, a.type) : ""}</span>` +
      (opts.creator && a.creator ? `<span class="who">${esc(a.creator)}</span>` : "") + `</a>`;
  }
  function pageLink(p, cls = "") {
    return `<a class="pglink ${cls}" href="#history/${esc(p.slug)}" data-slug="${esc(p.slug)}"><span class="kind">${esc(kindLabel(p.kind))}</span>${esc(p.title)}${statusTag(p.status)}${p.summary ? `<span class="sum">${esc(p.summary)}</span>` : ""}</a>`;
  }
  // a topic as a card with its picture: the home's narrative chips, and Explore
  function topicCard(x, full = false) {
    const im = topicImage(x.slug);
    return `<a class="topiccard ${im ? "" : "noimg"}" href="#history/topic/${esc(x.slug)}" data-topic="${esc(x.slug)}" style="border-left-color:${topicColour(x.slug)}">` +
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
  async function renderMain() {
    const el = $("#histmain");
    el.scrollTop = 0;                                              // a new view opens at its top
    const plot = $("#histplot"); if (plot?.data) Plotly.purge(plot);
    if (!UW.M.history) { el.innerHTML = `<div class="empty">No history has been published yet.</div>`; return; }
    if (!hist.index) { el.innerHTML = `<div class="empty">Loading the history…</div>`; return; }
    loadFlags();                                                   // what other browsers have flagged since; the cards update when it lands
    const q = hist.search.trim().toLowerCase();
    if (q) {                                                        // search: pages and artifacts, in the main area
      const words = q.split(/\s+/).filter(Boolean), t = curTopic();
      const hit = (s) => { const t = String(s || "").toLowerCase(); return words.every((w) => t.includes(w)); };
      const pages = hist.index.pages.filter((p) => (!t || p.topic === t || !p.topic) && (hit(p.title) || hit(p.summary)))
        .sort((a, b) => (hit(b.title) - hit(a.title)) || (a.kind === "page" ? -1 : 1));
      const arts = hist.artifacts.filter((a) => (!t || a.topic === t) && (hit(a.title) || hit(a.description) || hit((a.people || []).join(" ")) || hit((a.tags || []).join(" "))));
      const evs = hist.events.filter((e) => (!t || e.topic === t) && (hit(e.title) || hit(e.detail) || hit(e.place) || hit((e.people || []).join(" "))));
      el.innerHTML = crumb(`search <i>${esc(hist.search.trim())}</i>`) + `<h2>${pages.length} pages · ${arts.length} artifacts · ${evs.length} events</h2>` +
        `<div class="pagelist">${pages.slice(0, 80).map((p) => pageLink(p)).join("")}</div>` +
        (arts.length ? `<div class="artgrid">${arts.slice(0, 80).map((a) => artifactCard(a, { creator: true })).join("")}</div>` : "") +
        (evs.length ? `<div class="pagelist">${evs.slice(0, 80).map((e) => pageLink({ slug: `event/${e.id}`, kind: "event", title: e.title, summary: [dateLabel(e.date_text || e.date_start || ""), e.place].filter(Boolean).join(" · ") })).join("")}</div>` : "");
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
      el.innerHTML = crumb(here("Explore", "explore")) + `<h2>Explore</h2><p class="lead">${t.length} topics, ${n("pages")} narrative pages and ${hist.artifacts.length} artifacts, from the Tuniit to the ships of the last century: voyages as tracks, winterings and besetments as spans on the timeline, people and places as pages that link to one another, every item credited and sourced.</p>` +
        `<div class="topicgrid">${t.map((x) => topicCard(x, true)).join("")}</div>`;
      return;
    }
    if (hist.slug === "kind/track" || hist.slug.startsWith("topic/") || hist.slug.startsWith("artifact/")) await ensureCoast();
    if (hist.slug.startsWith("kind/")) { renderKind(el, hist.slug.slice(5)); return; }
    if (hist.slug === "bib") { await renderBib(el); return; }
    if (hist.slug.startsWith("topic/")) {
      const t = topicOf(hist.slug.slice(6));
      if (!t) { el.innerHTML = `<div class="empty">no such topic</div>`; return; }
      const pages = hist.index.pages.filter((p) => p.topic === t.slug && p.kind === "page");
      // the artifacts of every kind together: first those the narratives
      // mention, in the order they are mentioned, then the rest in a fixed
      // order that looks like none
      const order = await mentionOrder(pages);
      const arts = hist.artifacts.filter((a) => a.topic === t.slug).sort((p, q) => ((order.get(p.page) ?? 1e9) - (order.get(q.page) ?? 1e9)) || byMix(p, q));
      const counts = GROUPS.flatMap((g) => [g.head, ...g.under]).filter((k) => TYPES[k]).map((k) => [k, arts.filter((a) => a.type === k).length]).filter(([, n]) => n)
        .map(([k, n]) => `${n} ${TYPES[k].label.toLowerCase()}`).join(" · ");
      const im = topicImage(t.slug);
      el.innerHTML = crumb(`<a href="#history/explore" data-slug="explore">Explore</a>`, here(esc(t.title), `topic/${t.slug}`)) + `<h2>${esc(t.title)}${statusTag(t.status)}</h2>` +
        (im ? `<figure class="topicfig"><a href="#history/${esc(im.page)}" data-slug="${esc(im.page)}" title="the picture's own page"><img src="${esc(im.url)}" alt=""></a><figcaption><a href="#history/${esc(im.page)}" data-slug="${esc(im.page)}">${esc(im.title)}</a> · ${esc(im.credit)}</figcaption></figure>` : "") +
        `<p class="lead">${esc(t.summary)}</p>` +
        (pages.length ? `<div class="pagelist">${pages.map((p) => pageLink(p)).join("")}</div>` : `<p class="muted">No narrative pages yet; the artifacts below are what the crew has entered so far.</p>`) +
        (arts.length ? `<h3><span class="muted">${arts.length}</span> Artifacts <span class="muted small">${esc(counts)}</span></h3><div class="artgrid">${arts.map((a) => artifactCard(a, { creator: true })).join("")}</div>` : "");
      return;
    }
    // an event's page comes from the publisher; a build without them gets the pane's own
    if (hist.slug.startsWith("event/") && !hist.index.pages.some((x) => x.slug === hist.slug)) { renderEvent(el, hist.slug.slice(6)); return; }
    let p;
    try { p = await page(hist.slug); }
    catch { el.innerHTML = crumb() + `<div class="empty">That page is not in this build.</div>`; return; }
    const t = topicOf(p.topic);
    const a = p.kind === "artifact" ? artifactById(p.ref) : null;
    const pl = p.kind === "place" ? placeByPage(p.slug) : null;
    const ev = p.kind === "event" ? eventById(p.ref) : null;
    const back = (p.backlinks || []).map((s) => hist.index.pages.find((x) => x.slug === s)).filter(Boolean);
    let head = crumb(...(t ? [`<a href="#history/topic/${esc(t.slug)}" data-topic="${esc(t.slug)}">${esc(t.title)}</a>`] : []), here(`<span class="kind">${esc(kindLabel(p.kind))}</span>`, p.slug)) + `<h2>${esc(p.title)}${p.kind === "page" ? statusTag(p.status) : ""}</h2>`;
    if (p.summary && p.kind === "page") head += `<p class="lead">${esc(p.summary)}</p>`;
    let media = "";
    if (a) {
      const picture = a.url && /\.(jpe?g|png|gif|tiff?|webp|bmp)$/i.test(a.url);
      if (picture && (a.type === "image" || a.type === "map")) media = `<figure><a href="${esc(a.url)}" target="_blank" rel="noopener"><img src="${esc(a.url)}" alt="${esc(a.title)}"></a><figcaption>${esc(a.credit)}${a.licence ? " · " + esc(a.licence) : ""}</figcaption></figure>`;
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
      head += `<div class="artmeta"><span class="dot" style="background:${TYPES[a.type]?.colour || "#8b9bb0"}"></span>${esc(a.type)} · ${esc(fmtDate(a))}${a.creator ? " · " + esc(a.creator) : ""}${where}${flagMark(a)}</div>`;
      head += peopleStrip(a.people);
      if (a.type === "track") media = `<figure class="routefig">${trackSketch(a, 480, 300, "sketch large")}</figure>` + media;
      media += track;
    }
    if (pl && pl.lat != null) {
      head += `<div class="artmeta"><span class="dot" style="background:${TYPES.place.colour}"></span>${esc(pl.kind || "place")} · ${mapLink(pl.lat, pl.lon, pl.name, "place")}</div>`;
    }
    if (ev && ev.lat != null) {
      head += `<div class="artmeta"><span class="dot" style="background:${TYPES.event.colour}"></span>event · ${coordLink(ev.lat, ev.lon, ev.title)} · ${mapLink(ev.lat, ev.lon, ev.title, "event")}</div>`;
    }
    if (p.kind === "source") {
      const bib = await bibliography();
      const e = bib.find((x) => x.key === p.slug.split("/").pop());
      if (e) head += `<p class="mlaline"><span class="lbl">MLA</span> ${mla(e)}</p>`;
    }
    el.innerHTML = head + media + `<div class="wiki">${markdown(p.html)}</div>` +
      (back.length ? `<div class="backlinks"><span class="lbl">Mentioned in</span>${back.map((b) => `<a href="#history/${esc(b.slug)}" data-slug="${esc(b.slug)}">${esc(b.title)}</a>`).join("")}</div>` : "");
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
    el.innerHTML = crumb(...(t ? [`<a href="#history/topic/${esc(t.slug)}" data-topic="${esc(t.slug)}">${esc(t.title)}</a>`] : []), here(`<span class="kind">event</span>`, `event/${id}`)) + `<h2>${esc(e.title)}</h2>` +
      `<div class="artmeta"><span class="dot" style="background:${TYPES.event.colour}"></span>event · ${when}${e.place ? " · " + esc(e.place) : ""}${where}</div>` +
      peopleStrip(e.people) +
      `<div class="wiki">${markdown(e.detail || "")}</div>` +
      (e.tags?.length ? `<div class="backlinks"><span class="lbl">Tags</span>${e.tags.map((x) => `<span class="muted">${esc(x)}</span>`).join("")}</div>` : "") +
      `<div class="backlinks">${src ? `<span class="lbl">Source</span><a href="#history/${esc(src.slug)}" data-slug="${esc(src.slug)}">${esc(src.title)}</a>` : ""}<span class="lbl">On the timeline</span><a href="#history/kind/event" data-slug="kind/event">Events</a></div>`;
    crossLink(el.querySelector(".wiki"), `event/${id}`, e.people, "");
    el.scrollTop = 0; window.scrollTo?.(0, 0);
  }
  // the people named on an artifact or an event, each a link to their page
  function peopleStrip(names) {
    if (!names?.length) return "";
    const one = (n) => { const p = hist.people.find((x) => x.name === n); return p ? `<a class="chip small" href="#history/${esc(p.page)}" data-slug="${esc(p.page)}">${esc(n)}</a>` : `<span class="chip small wanted" title="no page yet">${esc(n)}</span>`; };
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
        const a = document.createElement("a"); a.href = `#history/${x.slug}`; a.dataset.slug = x.slug; a.className = "auto"; a.textContent = m[0]; frag.appendChild(a);
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
  const SLUG_RX = /(?<![\w/#-])([a-z][a-z0-9]*(?:-[a-z0-9]+)+|(?:artifact|person|place|event|source|topic|vessel|animal)\/[a-z0-9][\w-]*)(?![\w/-])/g;
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
        const a = document.createElement("a"); a.href = `#history/${p.slug}`; a.dataset.slug = p.slug; a.className = "auto"; a.textContent = p.title; frag.appendChild(a);
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
    const chips = (kind, label) => { const xs = [...found.values()].filter((p) => p.kind === kind).sort((p, q) => p.title.localeCompare(q.title)); return xs.length ? `<div class="onpage"><span class="lbl">${label}</span>${xs.map((p) => `<a href="#history/${esc(p.slug)}" data-slug="${esc(p.slug)}">${esc(p.title)}</a>`).join("")}</div>` : ""; };
    const strip = STRIPS.map(([k, label]) => chips(k, label)).join("");
    if (strip) root.insertAdjacentHTML("afterend", strip);
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
    const h2 = (n, label) => `<h2><span class="dot" style="background:${K.colour}"></span><span class="muted">${n}</span> ${esc(label)}</h2>`;
    if (kind === "event") { el.innerHTML = crumb(here("Events", "kind/event")) + eventsHTML(); wireEvents(el); return; }
    if (kind === "people" || kind === "animal" || kind === "vessel") {
      // the names down the left; on the right, the faces the backend has cut
      // from the photographs — of this kind only, in a fixed order that looks
      // like none, so the wall is not the alphabet twice
      const rows = kind === "people" ? hist.people : kind === "animal" ? hist.animals : hist.vessels;
      const faceKind = kind === "people" ? "person" : kind;
      const named = [...rows].sort((a, b) => a.name.localeCompare(b.name));
      const life = (p) => [p.born, p.died].some(Boolean) ? ` <span class="muted mono">${esc(dateLabel(p.born || "?"))}–${esc(dateLabel(p.died || ""))}</span>`
        : [p.built, p.lost].some(Boolean) ? ` <span class="muted mono">${esc(p.built || "")}${p.lost ? " – " + esc(p.lost) : ""}</span>` : "";
      const sub = (p) => [p.kind, p.role].filter(Boolean).join(" · ");
      const list = `<div class="peoplelist">` + letterList(named, (p) => p.name, (p) => `<a class="person ${p.indigenous ? "inuit" : ""}" href="#history/${esc(p.page)}" data-slug="${esc(p.page)}"><b>${esc(p.name)}</b>${p.also ? ` <span class="muted">(${esc(p.also)})</span>` : ""}${life(p)}${sub(p) ? `<span class="role">${esc(sub(p))}</span>` : ""}</a>`) + `</div>`;
      const faces = (hist.faces || []).filter((f) => f.file && (f.kind || "person") === faceKind).sort((x, y) => mixKey(`${x.artifact}|${x.file}`) - mixKey(`${y.artifact}|${y.file}`));
      const wall = faces.length ? `<div class="faces">${faces.map((f) => `<a class="face" href="#history/${esc(f.person_page || f.page)}" data-slug="${esc(f.person_page || f.page)}" title="${esc(f.person || "unidentified")}${f.title ? " · " + esc(f.title) : ""}"><img src="${esc(f.file)}" alt="${esc(f.person || "")}" loading="lazy"></a>`).join("")}</div>` : "";
      el.innerHTML = crumb(here(K.label, `kind/${kind}`)) + h2(named.length, K.label) + `<div class="peoplecols ${wall ? "" : "nofaces"}">${list}${wall}</div>`;
      return;
    }
    if (kind === "place") {
      const places = [...hist.places].sort((a, b) => a.name.localeCompare(b.name));
      const names = (p) => [p.inuktitut, p.historic].filter((n) => n && n !== p.name).join(", ");
      el.innerHTML = crumb(here("Places", "kind/place")) + h2(places.length, "Places") + `<div class="peoplelist">` +
        letterList(places, (p) => p.name, (p) => `<a class="person" href="#history/${esc(p.page)}" data-slug="${esc(p.page)}"><b>${esc(p.name)}</b>${names(p) ? ` <span class="muted">(${esc(names(p))})</span>` : ""} <span class="muted small">${esc(p.kind || "")}</span>` +
          (p.lat != null ? ` ${mapLink(p.lat, p.lon, p.name, "place")}` : "") + (p.note ? `<span class="role">${esc(p.note.length > 160 ? p.note.slice(0, 157) + "…" : p.note)}</span>` : "") + `</a>`) + `</div>`;
      return;
    }
    const arts = hist.artifacts.filter((a) => a.type === kind).sort(byYear);
    const pictures = kind === "image" || kind === "map";
    const grid = (xs) => `<div class="artgrid ${pictures ? "pictures" : ""} ${kind === "quote" ? "quotes" : ""}">${xs.map((a) => artifactCard(a, { creator: true })).join("")}</div>`;
    let body;
    if (!arts.length) body = `<p class="muted">Nothing of this kind in the record yet.</p>`;
    else if (arts.some((a) => a.keywords?.length)) body = keywordSections(arts, grid);
    else if (kind === "image" || kind === "quote" || kind === "map") {
      // until the keywords come from the backend, the pictures and the words go by topic
      const byTopic = new Map(); for (const a of arts) { if (!byTopic.has(a.topic)) byTopic.set(a.topic, []); byTopic.get(a.topic).push(a); }
      body = [...byTopic.entries()].map(([t, xs]) => `<h3><span class="dot" style="background:${topicColour(t)}"></span><span class="muted">${xs.length}</span> <a href="#history/topic/${esc(t)}" data-topic="${esc(t)}">${esc(topicOf(t)?.title || t)}</a></h3>${grid(xs)}`).join("");
    } else body = grid(arts);
    el.innerHTML = crumb(here(esc(K.label), `kind/${kind}`)) + h2(arts.length, K.label) + body;
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
    return (hist.timeline || []).filter((d) => d.topic && (!t || d.topic === t) && yearOf(d.date) != null)
      .sort((p, q) => yearOf(p.date) - yearOf(q.date));
  }
  const shortTopic = (slug) => topicOf(slug)?.title.replace(/,.*$/, "") || slug;
  function eventsHTML() {
    const rows = timelineRows();
    const tr = (d, i) => {
      const q = d.qualifier ? `<i>${esc(d.qualifier)}</i> ` : "";
      const when = q + esc(dateLabel(d.date)) + (d.date_end ? ` → ${esc(dateLabel(d.date_end))}` : "") + (d.precision && d.precision !== "day" ? ` <span class="muted">(${esc(d.precision)})</span>` : "");
      const slug = pageFor(d.entity_kind, d.entity_id);
      const what = slug ? `<a href="#history/${esc(slug)}" data-slug="${esc(slug)}">${esc(d.label || d.entity_id)}</a>` : esc(d.label || d.entity_id);
      const pin = d.lat != null ? ` <span class="pin" data-lat="${d.lat}" data-lon="${d.lon}" data-label="${esc(d.label || "")}" title="on the map">⌖</span>` : "";
      return `<tr data-key="${i}"${d.lat != null ? ` data-lat="${d.lat}" data-lon="${d.lon}"` : ""}><td class="mono">${when}</td><td>${what}${pin}</td><td>${esc(d.place || "")}</td><td><a href="#history/topic/${esc(d.topic)}" data-topic="${esc(d.topic)}"><span class="dot" style="background:${topicColour(d.topic)}"></span>${esc(shortTopic(d.topic))}</a></td></tr>`;
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
    // the chart holds every date but opens on the centuries with most of them
    const pad = Math.max(2, (hi - lo) * .02), from = Math.max(lo - pad, Math.min(TIMELINE_FROM, hi - 10));
    const layout = { ...UW.THEME, margin: { l: 150, r: 12, t: 8, b: 40 }, showlegend: false, dragmode: "pan", barmode: "overlay",
      xaxis: { ...UW.THEME.xaxis, ...yearTicks(lo - pad, hi + pad), range: [from, hi + pad], zeroline: false, title: { text: "year", font: { size: 12 } }, tickfont: { size: 12 } },
      yaxis: { ...UW.THEME.yaxis, type: "category", categoryorder: "array", categoryarray: cats.slice().reverse(), tickfont: { size: 11 }, fixedrange: true } };
    layout.xaxis = { ...layout.xaxis, ...yearTicks(from, hi + pad) };
    Plotly.react(gd, traces, layout, UW.CFG).then((g) => {
      UW.axisZoom(g);
      g.removeAllListeners?.("plotly_click"); g.on("plotly_click", (ev) => { const k = ev.points?.[0]?.customdata; if (k != null) showEventRow(k); });
      // the year ticks are ours (BCE, AD): recomputed for whatever span is in view
      g.removeAllListeners?.("plotly_relayout"); g.on("plotly_relayout", () => {
        const r = g._fullLayout?.xaxis?.range; if (!r) return;
        const t = yearTicks(r[0], r[1]);
        if (JSON.stringify(t.tickvals) !== JSON.stringify(g.layout.xaxis.tickvals)) Plotly.relayout(g, { "xaxis.tickvals": t.tickvals, "xaxis.ticktext": t.ticktext });
      });
    });
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
    el.innerHTML = crumb(here("Bibliography", "bib")) + `<h2>Bibliography <span class="muted">${sorted.length} works</span></h2>` +
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
  // in their hierarchy: a head with its kinds beside it, or a kind alone; a
  // head the bar has no kind for (People, on the map) is a label
  function chipsHTML(kinds, onOf, cls = "") {
    const chip = (k, extra = "") => { const t = kinds[k]; return t ? `<button type="button" data-t="${k}" class="${cls} ${extra} ${onOf(k) ? "on" : ""}" title="${t.label}"><span class="dot" style="background:${t.colour}"></span>${t.label}</button>` : `<span class="ghead">${KINDS[k].label}</span>`; };
    return GROUPS.filter((g) => kinds[g.head] || g.under.some((k) => kinds[k])).map((g) => g.under.length ? `<span class="kgroup">${chip(g.head, "head")}${g.under.map((k) => chip(k, "sub")).join("")}</span>` : chip(g.head)).join("");
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
    $("#histhome").classList.toggle("on", !hist.slug && !hist.search);
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
    if (hist.search) { hist.search = ""; const q = $("#histsearch"); if (q) q.value = ""; }
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
  function focusPoint(lat, lon, label, type = "") {
    if (lat == null) return;
    if (!UW.state.history) { UW.state.history = true; store.set("history", true); document.querySelector('#maplayers button[data-layer="history"]')?.classList.add("on"); }
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
    const flag = e.target.closest("#pane-history .flag[data-flag]");
    if (flag) { e.preventDefault(); e.stopPropagation(); toggleFlag(flag.dataset.flag); return; }
    const kw = e.target.closest('#pane-history a[href^="#kw-"]');
    if (kw) { e.preventDefault(); document.getElementById(kw.getAttribute("href").slice(1))?.scrollIntoView({ block: "start", behavior: "smooth" }); return; }
    const pin = e.target.closest("#pane-history .pin[data-lat]");
    if (pin) { e.preventDefault(); e.stopPropagation(); focusPoint(pin.dataset.lat, pin.dataset.lon, pin.dataset.label, pin.dataset.type); return; }
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
  document.addEventListener("keydown", (e) => {
    const flag = (e.key === "Enter" || e.key === " ") && e.target.closest?.("#pane-history .flag[data-flag]");
    if (flag) { e.preventDefault(); toggleFlag(flag.dataset.flag); }
  });
  UW.onHistoryClick = (id, pt) => {
    // the mark moves to what was clicked; the map keeps its view; the page
    // opens even if it is the one the reader has since left
    if (pt && pt.lat != null) UW.state.focus = { lat: +pt.lat, lon: +pt.lon, label: String(pt.text || "").replace(/<[^>]+>/g, "") };
    if (id.startsWith("place:")) { open(id.slice(6)); return; }
    const a = artifactById(id.split("|")[0]); if (a) open(a.page);
  };
  // the mark sits over the point it marks and takes the click: find what
  // lies under it and open that
  UW.onFocusClick = (pt) => {
    if (!UW.state.history || !hist.artifacts || pt?.lat == null) return false;
    const near = (la, lo) => la != null && Math.abs(la - pt.lat) < 1e-6 && Math.abs(lo - pt.lon) < 1e-6;
    const t = curTopic();
    for (const a of shownArtifacts()) {
      if (a.type === "track" ? (a.waypoints || []).some((w) => near(w.lat, w.lon)) : near(a.lat, a.lon)) { open(a.page); return true; }
    }
    if (hist.types.has("place")) { const p = hist.places.find((p) => (!t || p.topic === t) && near(p.lat, p.lon)); if (p) { open(p.page); return true; } }
    return false;
  };

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
    if (hist.types.has("place")) {
      const t = curTopic(), pl = hist.places.filter((p) => p.lat != null && (!t || p.topic === t));
      if (pl.length) out.push({ type: "scattermap", mode: "markers", name: "history-places", showlegend: false, hoverinfo: "text",
        lat: pl.map((p) => p.lat), lon: pl.map((p) => p.lon), text: pl.map((p) => `${esc(p.name)}${p.kind ? " · " + esc(p.kind) : ""}`), customdata: pl.map((p) => `hist:place:${p.page}`),
        marker: { size: 7, color: TYPES.place.colour, opacity: .85 } });
    }
    return out;
  };

  // ---------------------------------------------------------------- Ada
  UW.historyContext = () => (hist.slug && !["explore", "bib"].includes(hist.slug) && !/^(topic|kind|event)\//.test(hist.slug)) ? hist.slug : "";
  UW.historyOpen = (slug) => open(slug);

  // ---------------------------------------------------------------- wiring
  function wire() {
    $("#histsearch").oninput = debounce((e) => { hist.search = e.target.value; renderMain(); }, 150);
    $("#histback").onclick = goBack;
    $("#histhome").onclick = () => open("");
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
