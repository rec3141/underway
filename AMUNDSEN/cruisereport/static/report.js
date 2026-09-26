/* Cruise Report Builder: the page keeps one report object (R), which is the
   draft saved on the server and the body sent for previews and the .docx.
   The server does all the data work; this file only edits R and shows
   what the server returns. */
"use strict";

const $ = (s, el = document) => el.querySelector(s);
const $$ = (s, el = document) => [...el.querySelectorAll(s)];
const h = (tag, attrs = {}, ...kids) => {
  const el = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (k === "class") el.className = v;
    else if (k.startsWith("on")) el.addEventListener(k.slice(2), v);
    else if (v === true) el.setAttribute(k, "");
    else if (v !== false && v != null) el.setAttribute(k, v);
  }
  for (const k of kids.flat()) if (k != null) el.append(k.nodeType ? k : document.createTextNode(k));
  return el;
};

const TEXTS = [
  ["intro", "Introduction & Objectives", "Provide a short rationale for your project. Move from broader scientific issues to the specific objectives of your work on board the Amundsen."],
  ["methods", "Methodology", "Briefly describe the field operations conducted and the methodology used to collect and analyze your samples. The station narrative, tables and figures you chose for this section follow your text."],
  ["results", "Preliminary Results", "Present some preliminary results if any, and state explicitly whether your specific objectives were achieved."],
  ["references", "References", "APA-formatted citations, one per line."],
  ["recommendations", "Recommendations", "Actionable, constructive feedback specific to your operations: scientific equipment (laboratories, winches, etc.), life on board, or any other aspect of the cruise."],
  ["publications", "Published & pending work — Publications", "Submitted, accepted or published, using data collected from the ship."],
  ["presentations", "Published & pending work — Presentations", "Oral presentations, posters, public talks."],
  ["in_progress", "Published & pending work — In progress", "Manuscripts currently in preparation."],
];
const SECTIONS = [["methods", "Methodology"], ["results", "Preliminary Results"], ["intro", "Introduction"]];
const PRESETS = {
  conditions: { title: "Conditions at the start of each operation.", rows: "operations",
    columns: ["op.station", "op.label", "op.activity", "op.start_utc", "op.lat", "op.lon", "op.depth_m",
              "op.air_c", "op.wind_dir_deg", "op.wind_kn", "op.sst_c", "op.sss", "op.ice"] },
  stations: { title: "Stations sampled.", rows: "operations",
    columns: ["op.station", "op.label", "op.activity", "op.start_utc", "op.end_utc", "op.lat", "op.lon", "op.depth_m"] },
  bottles: { title: "Rosette bottles sampled by the team.", rows: "bottles",
    columns: ["op.station", "op.label", "bottle.bottle", "bottle.target", "bottle.depth_m",
              "bottle.temperature", "bottle.salinity"] },
  blank: { title: "", rows: "operations", columns: [] },
};
const PROFILE_VARS = ["Temperature", "Salinity", "Fluorescence", "Oxygen", "Transmission", "PAR", "CDOM", "Sigma-t"];
const LOCAL_KEY = "cr:current";

let R = blank();
let INFO = null;                 // /api/leg for R.leg
const LOGS = {};                 // logsheet id -> {name, sheets, match}
let narrWords = 0;

function blank(leg = "") {
  return {
    leg, leg_label: "", team: "",
    header: { title: "", leaders: [{}], participants: [{}] },
    text: {},
    selection: { groups: [], ops: [], teams: [], logsheets: [] },
    digitized: [],
    conditions: { narrative: "summary" },
    tables: [], figures: [],
  };
}

// --- plumbing -----------------------------------------------------------------
async function api(path, body, opts = {}) {
  const r = await fetch(path, body === undefined ? {} : {
    method: opts.method || "POST", body: opts.raw ? body : JSON.stringify(body),
    headers: opts.headers || { "Content-Type": "application/json" },
  });
  if (!r.ok) {
    let msg = r.statusText;
    try { msg = (await r.json()).error || msg; } catch (e) { /* not JSON */ }
    throw new Error(msg);
  }
  return opts.blob ? r.blob() : r.json();
}
let toastTimer;
function toast(msg, err = false) {
  const t = $("#toast");
  t.textContent = msg;
  t.className = "show" + (err ? " err" : "");
  clearTimeout(toastTimer);
  toastTimer = setTimeout(() => (t.className = ""), err ? 7000 : 3000);
}
let figTimer;
function selectionChanged() {
  clearTimeout(figTimer);
  for (const id of R.digitized || []) (DIG[id]?.tables || []).forEach((_, k) => refreshMatches(id, k));
  figTimer = setTimeout(() => { if (R.figures.length) renderFigures(); }, 1200);
}
function persist() {
  try { localStorage.setItem(LOCAL_KEY, JSON.stringify(R)); } catch (e) { /* private mode */ }
  updateWords();
  scheduleNarrWords();
}
const legLabel = (leg) => { const m = /^(\d{4})_LEG_(\d+)$/.exec(leg || ""); return m ? `Leg ${+m[2]}` : leg; };

// --- 1 · team -----------------------------------------------------------------
function renderPeople(kind) {
  const box = $("#" + kind);
  box.replaceChildren();
  R.header[kind].forEach((p, i) => {
    const inp = (key, ph, type = "text") => h("input", { type, placeholder: ph, value: p[key] || "",
      oninput: (e) => { p[key] = e.target.value; persist(); } });
    box.append(h("div", { class: "person" },
      inp("name", "Name"), kind === "leaders" ? inp("email", "Email", "email") : h("span"),
      inp("affiliation", "Affiliation (institute or university)"),
      h("button", { class: "danger small", title: "Remove", onclick: () => {
        R.header[kind].splice(i, 1); if (!R.header[kind].length) R.header[kind].push({});
        renderPeople(kind); persist(); } }, "×")));
  });
}

function renderTeam() {
  $("#team").value = R.team || "";
  $("#title").value = R.header.title || "";
  renderPeople("leaders");
  renderPeople("participants");
  echo();
}
function echo() {
  $("#team-echo").textContent = (R.team || "TeamX").replace(/[^\w-]+/g, "_");
  $("#leg-echo").textContent = legLabel(R.leg) || "Leg X";
  $("#year-echo").textContent = (R.leg || "2026").slice(0, 4);
}

// --- 2 · what you did -----------------------------------------------------------
function renderGroups() {
  const box = $("#groups");
  box.replaceChildren(...INFO.groups.map((g) => {
    const on = R.selection.groups.includes(g.id);
    return h("label", { class: "chip" + (on ? " on" : "") },
      h("input", { type: "checkbox", checked: on, onchange: (e) => toggleGroup(g.id, e.target.checked) }),
      g.label, h("span", { class: "n" }, String(g.count)));
  }));
}
function toggleGroup(id, on) {
  const sel = R.selection;
  const keys = INFO.operations.filter((o) => o.group === id).map((o) => o.key);
  if (on) {
    sel.groups.push(id);
    sel.ops = [...new Set([...sel.ops, ...keys])];
  } else {
    sel.groups = sel.groups.filter((g) => g !== id);
    sel.ops = sel.ops.filter((k) => !keys.includes(k));
  }
  renderGroups(); renderOps(); persist(); selectionChanged();
}

const stem = (s) => s.toLowerCase().replace(/[^a-z]/g, "").slice(0, 5);
function renderTeams() {
  const picked = R.selection.teams;
  const stems = new Set(picked.map(stem));
  const names = Object.entries(INFO.teams);
  $("#teams-box").hidden = !names.length;
  $("#teams").replaceChildren(...names.map(([name, t]) => {
    const on = picked.includes(name);
    const similar = !on && stems.has(stem(name));
    const also = t.spellings.filter((x) => x !== name);
    return h("label", { class: "chip" + (on ? " on" : "") + (similar ? " similar" : ""),
      title: [also.length ? `Includes the spellings ${also.join(", ")}` : "",
              similar ? "Looks like another name you ticked" : ""].filter(Boolean).join(". ") },
      h("input", { type: "checkbox", checked: on, onchange: (e) => {
        R.selection.teams = e.target.checked ? [...picked, name] : picked.filter((t) => t !== name);
        renderTeams(); renderTables(); persist(); selectionChanged(); } }),
      name, also.length ? h("span", { class: "n" }, `+${also.length} spelling${also.length > 1 ? "s" : ""}`) : null,
      h("span", { class: "n" }, String(t.casts)));
  }));
  $("#teams-picked").textContent = picked.length ? picked.join(", ") : "";
}

function logHits() {
  const hits = {};
  for (const lg of Object.values(LOGS)) for (const r of lg.match?.rows || []) if (r.op) hits[r.op] = (hits[r.op] || 0) + 1;
  return hits;
}
function renderOps() {
  const sel = new Set(R.selection.ops);
  const f = $("#ops-filter").value.trim().toLowerCase();
  const hits = logHits();
  const shown = INFO.operations.filter((o) => (!f || [o.station, o.label, o.activity, o.group_label]
    .some((v) => (v || "").toLowerCase().includes(f))) && (sel.has(o.key) || R.selection.groups.includes(o.group) || hits[o.key] || f));
  const t = $("#ops");
  t.replaceChildren(h("thead", {}, h("tr", {}, ...["", "Station", "Label", "Operation", "Instrument", "Start (UTC)", "Duration", "Depth (m)", "Logsheet rows"].map((x) => h("th", {}, x)))),
    h("tbody", {}, ...shown.map((o) => h("tr", { class: sel.has(o.key) ? "" : "off" },
      h("td", {}, h("input", { type: "checkbox", checked: sel.has(o.key), "data-key": o.key, onchange: (e) => {
        const s = new Set(R.selection.ops); e.target.checked ? s.add(o.key) : s.delete(o.key);
        R.selection.ops = [...s]; e.target.closest("tr").className = e.target.checked ? "" : "off";
        $("#ops-count").textContent = `${R.selection.ops.length} ticked`; persist(); selectionChanged(); } })),
      h("td", {}, o.station || "—"), h("td", {}, o.label || "—"), h("td", {}, o.activity),
      h("td", {}, o.group_label), h("td", {}, (o.start_utc || "").replace("T", " ").slice(0, 16)),
      h("td", { class: "num" }, o.duration_min != null ? `${Math.round(o.duration_min)} min` : ""),
      h("td", { class: "num" }, o.depth_m != null ? Math.round(o.depth_m) : ""),
      h("td", { class: "num" }, hits[o.key] ? String(hits[o.key]) : "")))));
  if (!shown.length) t.append(h("tbody", {}, h("tr", {}, h("td", { colspan: 9, class: "hint" },
    "Tick an instrument above, filter by name, or import a logsheet to list operations."))));
  $("#ops-count").textContent = `${R.selection.ops.length} ticked`;
}

// --- logsheets ------------------------------------------------------------------
async function uploadLog(file) {
  try {
    toast(`Reading ${file.name}…`);
    const meta = await api(`api/logsheet?leg=${encodeURIComponent(R.leg)}`, await file.arrayBuffer(),
      { raw: true, headers: { "X-Filename": file.name, "Content-Type": "application/octet-stream" } });
    const sheet = Object.keys(meta.sheets)[0];
    LOGS[meta.id] = { name: meta.name, sheets: meta.sheets };
    R.selection.logsheets.push({ id: meta.id, name: meta.name, sheet, roles: meta.sheets[sheet].roles });
    await matchLog(R.selection.logsheets.at(-1));
  } catch (e) { toast(`Could not read ${file.name}: ${e.message}`, true); }
}
async function matchLog(lg) {
  const m = await api("api/logsheet/match", { id: lg.id, sheet: lg.sheet, roles: lg.roles, leg: R.leg, groups: R.selection.groups });
  LOGS[lg.id] = { ...(LOGS[lg.id] || {}), name: lg.name, match: m };
  renderLogs(); renderOps(); renderTables(); persist();
}
function renderLogs() {
  const box = $("#logsheets");
  box.replaceChildren();
  let total = 0;
  for (const [i, lg] of R.selection.logsheets.entries()) {
    const L = LOGS[lg.id] || {};
    const m = L.match;
    total += m ? m.total : 0;
    const roleSel = (role) => h("label", {}, role,
      h("select", { onchange: (e) => { if (e.target.value) lg.roles[role] = e.target.value; else delete lg.roles[role]; matchLog(lg); } },
        h("option", { value: "" }, "—"), ...(m?.columns || []).map((c) => h("option", { value: c, selected: lg.roles[role] === c }, c))));
    const sheets = Object.keys(L.sheets || {});
    const card = h("div", { class: "tbl" },
      h("div", { class: "row" }, h("b", {}, lg.name), " ",
        sheets.length > 1 ? h("select", { onchange: (e) => { lg.sheet = e.target.value; lg.roles = L.sheets[lg.sheet].roles; matchLog(lg); } },
          ...sheets.map((s) => h("option", { value: s, selected: s === lg.sheet }, s))) : ` · ${lg.sheet}`, " ",
        m ? h("span", { class: "pill" }, `${m.matched} of ${m.total} rows matched`) : "", " ",
        h("button", { class: "ghost small", onclick: () => tickMatched(lg) }, "Tick matched operations"), " ",
        h("button", { class: "danger small", onclick: () => { R.selection.logsheets.splice(i, 1); delete LOGS[lg.id]; renderLogs(); renderOps(); renderTables(); persist(); } }, "Remove")),
      h("p", { class: "hint" }, "Which column holds what? Correct any guess and the rows are matched again."),
      h("div", { class: "cols" }, ...(m?.role_options || []).map(roleSel)));
    if (m) {
      const cols = m.columns.slice(0, 8);
      card.append(h("div", { class: "scroll", style: "max-height:260px" }, h("table", { class: "data" },
        h("thead", {}, h("tr", {}, h("th", {}, "Matched to"), h("th", {}, "How"), ...cols.map((c) => h("th", {}, c)))),
        h("tbody", {}, ...m.rows.slice(0, 200).map((r) => {
          const op = INFO.operations.find((o) => o.key === r.op);
          return h("tr", {}, h("td", {}, op ? `${op.station || ""} ${op.label || ""}` : "—"),
            h("td", {}, h("span", { class: "how" + (r.how ? "" : " none") }, r.how || "unmatched")),
            ...cols.map((c) => h("td", {}, r.cells[c] == null ? "" : String(r.cells[c]).slice(0, 40))));
        })))));
    }
    box.append(card);
  }
  $("#log-count").textContent = total ? `${R.selection.logsheets.length} sheet(s), ${total} rows` : "";
}
function tickMatched(lg) {
  const keys = (LOGS[lg.id]?.match?.rows || []).map((r) => r.op).filter(Boolean);
  R.selection.ops = [...new Set([...R.selection.ops, ...keys])];
  renderOps(); persist(); selectionChanged();
  toast(`${new Set(keys).size} operations ticked from ${lg.name}.`);
}


// --- logbook digitization -------------------------------------------------------
const DIG = {};                  // id -> transcription
const digQueue = [];             // photos waiting: {file, rotate, url}
const digFill = {};              // "id:table" -> carry station/cast/date down (default on)
const fillOf = (id, k) => digFill[`${id}:${k}`] !== false;
// Confidence levels are log10 odds that a cell is right (as in the XLSX).
const LEVELS = { 3: ["near certain", 120], 2: ["very likely", 90], 1: ["likely", 58], 0: ["even odds", 30], "-1": ["a guess", 0] };
const confColour = (c) => `hsl(${(LEVELS[c] || LEVELS[3])[1]} 70% var(--conf-l))`;
const confText = (c) => `${(LEVELS[c] || LEVELS[3])[0]} (log-odds ${c})`;

// The photo at full size in a preview box; rotate (degrees) matches a queued photo's turn.
function openPhoto(src, title, rotate = 0) {
  const dlg = $("#lightbox"), img = $("#lb-img"), body = dlg.querySelector(".lb-body");
  body.classList.remove("zoom");
  img.src = src;
  img.alt = title;
  img.style.transform = rotate ? `rotate(${rotate}deg)` : "";
  $("#lb-title").textContent = title;
  dlg.showModal();
}

// Collapsed or open, per transcribed page; a convenience kept in this browser.
const DIG_OPEN_KEY = "cr:dig-open";
let digOpen = {};
try { digOpen = JSON.parse(localStorage.getItem(DIG_OPEN_KEY)) || {}; } catch (e) { digOpen = {}; }
function saveDigOpen() { try { localStorage.setItem(DIG_OPEN_KEY, JSON.stringify(digOpen)); } catch (e) { /* private mode */ } }

const clock = (ms) => { const s = Math.max(0, Math.round(ms / 1000)); return `${Math.floor(s / 60)}:${String(s % 60).padStart(2, "0")}`; };
function pageStats(doc) {
  const rows = doc.tables.reduce((a, t) => a + t.rows.length, 0);
  const check = doc.tables.reduce((a, t) => a + t.rows.flat().filter((c) => !c.edited && c.c <= 1).length, 0);
  return { rows, check };
}

// Every photo, with its state: those not yet sent first, then the pages on the
// server (queued, transcribing, failed or done) in the order of the results below.
function renderStrip() {
  const local = digQueue.map((q, i) => {
    const label = q.status === "uploading" ? h("span", { class: "st st-working" }, "sending…")
      : q.status === "failed" ? h("span", { class: "st st-failed", title: q.error || "" }, "not sent")
      : h("span", { class: "st st-queued" }, "ready");
    const tools = q.status === "uploading" ? [] : [
      q.status === "failed" ? h("button", { class: "ghost small", title: "Send this photo again", onclick: () => { q.status = "ready"; q.error = ""; renderStrip(); transcribeQueue(); } }, "retry")
        : h("button", { class: "ghost small", title: "Rotate a quarter turn", onclick: () => { q.rotate = (q.rotate + 90) % 360; renderStrip(); } }, "↻"),
      h("button", { class: "danger small", title: "Remove", onclick: () => { URL.revokeObjectURL(q.url); digQueue.splice(i, 1); renderStrip(); } }, "×")];
    return h("div", { class: `dig-thumb ${q.status}` },
      h("img", { src: q.url, alt: q.file.name, style: `transform: rotate(${q.rotate}deg)`, title: "Click to preview",
        onclick: () => openPhoto(q.url, q.file.name, q.rotate) }),
      h("div", { class: "name" }, q.file.name), label,
      q.status === "failed" && q.error ? h("div", { class: "err" }, q.error) : null,
      h("div", { class: "row" }, ...tools));
  });
  const server = R.digitized.filter((id) => DIG[id]).map((id) => {
    const doc = DIG[id], img = `api/digitized/${id}/image`;
    const preview = h("button", { class: "ghost small", title: "Preview the photo", onclick: () => openPhoto(img, doc.name) }, "🔍");
    const remove = h("button", { class: "danger small", title: "Remove from this report", onclick: () => { R.digitized = R.digitized.filter((x) => x !== id); persist(); renderDigitized(); } }, "×");
    if (doc.status === "done") {
      const st = pageStats(doc);
      return h("div", { class: "dig-thumb done", title: "Show this page's tables" },
        h("img", { src: img, alt: doc.name, loading: "lazy", onclick: () => goToPage(id) }),
        h("div", { class: "name" }, doc.name),
        h("span", { class: "st st-done", onclick: () => goToPage(id) }, `done · ${st.rows} rows${st.check ? ` · ${st.check} to check` : ""}`),
        h("div", { class: "row" }, h("button", { class: "ghost small", title: "Show the tables", onclick: () => goToPage(id) }, "results ↓"), preview));
    }
    const label = doc.status === "working"
      ? h("span", { class: "st st-working", "data-started": Math.round((doc.started || Date.now() / 1000) * 1000) }, "transcribing")
      : doc.status === "failed" ? h("span", { class: "st st-failed", title: doc.error || "" }, "failed")
      : h("span", { class: "st st-queued" }, "queued");
    return h("div", { class: `dig-thumb ${doc.status}` },
      h("img", { src: img, alt: doc.name, loading: "lazy", onclick: () => openPhoto(img, doc.name) }),
      h("div", { class: "name" }, doc.name), label,
      doc.status === "failed" && doc.error ? h("div", { class: "err" }, doc.error) : null,
      h("div", { class: "row" },
        doc.status === "failed" ? h("button", { class: "ghost small", title: "Transcribe this photo again",
          onclick: async () => { try { DIG[id] = await api(`api/digitized/${id}/again`, {}); renderStrip(); pollDigitized(); } catch (e) { toast(e.message, true); } } }, "retry") : null,
        preview, remove));
  });
  $("#dig-queue").replaceChildren(...local, ...server);
  $("#dig-actions").hidden = !digQueue.some((q) => q.status === "ready");
  tickClocks();
}
function tickClocks() {
  for (const el of document.querySelectorAll("#dig-queue [data-started]")) el.textContent = `transcribing ${clock(Date.now() - +el.dataset.started)}`;
}
setInterval(tickClocks, 1000);

function goToPage(id) {
  const el = document.getElementById(`dig-${id}`);
  if (!el) return;
  el.open = true;
  digOpen[id] = true; saveDigOpen();
  el.scrollIntoView({ behavior: "smooth", block: "start" });
}

// Sending is quick; the server transcribes in the background, two pages at a time.
let digSending = false;
async function transcribeQueue() {
  if (digSending) return;
  digSending = true;
  $("#dig-go").disabled = true;
  let q;
  while ((q = digQueue.find((x) => x.status === "ready"))) {
    q.status = "uploading"; renderStrip();
    try {
      const doc = await api(`api/digitize?rotate=${q.rotate}`, await q.file.arrayBuffer(),
        { raw: true, headers: { "X-Filename": q.file.name, "Content-Type": "application/octet-stream" } });
      DIG[doc.id] = doc;
      R.digitized.unshift(doc.id);                // newest page on top
      digOpen[doc.id] = true; saveDigOpen();
      URL.revokeObjectURL(q.url);
      digQueue.splice(digQueue.indexOf(q), 1);
      persist();
    } catch (e) {
      q.status = "failed"; q.error = e.message;
    }
    renderStrip();
  }
  $("#dig-go").disabled = false;
  digSending = false;
  pollDigitized();
}

// While any page is queued or being transcribed, ask the server every few seconds.
let digPoll = null;
function pollDigitized() {
  if (digPoll) return;
  digPoll = setInterval(async () => {
    const pending = R.digitized.filter((id) => ["queued", "working"].includes(DIG[id]?.status));
    if (!pending.length) { clearInterval(digPoll); digPoll = null; return; }
    let finished = false;
    for (const id of pending) {
      try {
        const doc = await api(`api/digitized/${id}`);
        if (doc.status === "done" && DIG[id]?.status !== "done") finished = true;
        DIG[id] = doc;
      } catch (e) { /* ask again next time */ }
    }
    if (finished) renderDigitized(); else renderStrip();
  }, 3000);
}

function digTable(doc, k) {
  const t = doc.tables[k];
  const cell = (tag, c, row, col) => {
    const el = h(tag, { contenteditable: "true", spellcheck: "false",
      title: c.edited ? "corrected by hand" : confText(c.c), class: c.edited ? "edited" : "",
      style: tag === "td" ? `background:${confColour(c.c)}` : "" }, c.t);
    el.addEventListener("keydown", (e) => { if (e.key === "Enter") { e.preventDefault(); el.blur(); } });
    el.addEventListener("blur", async () => {
      const text = el.textContent.trim();
      if (text === c.t) return;
      try {
        const res = await api(`api/digitized/${doc.id}/edit`, { table: k, row, col, text });
        const { linked, ...fresh } = res;
        DIG[doc.id] = fresh;
        c.t = text; c.c = 3; c.edited = true;
        el.style.background = tag === "td" ? confColour(3) : ""; el.classList.add("edited"); el.title = "corrected by hand";
        refreshMatches(doc.id, k);
        renderStrip();
        // A logsheet made from this table was rewritten on the server: match it again.
        for (const lg of R.selection.logsheets.filter((x) => (linked || []).includes(x.id))) await matchLog(lg);
      } catch (e) { toast(`Not saved: ${e.message}`, true); el.textContent = c.t; }
    });
    return el;
  };
  const table = h("table", { class: "data dig", "data-dig": `${doc.id}:${k}` },
    h("thead", {}, h("tr", {}, h("th", { title: "The operation each row matches, re-checked after every correction" }, "Matched to"),
      ...t.columns.map((x, j) => cell("th", { t: x, c: 3 }, -1, j)))),
    h("tbody", {}, ...t.rows.map((r, i) => h("tr", {}, h("td", { class: "match" }, "…"), ...r.map((c, j) => cell("td", c, i, j))))));
  return h("div", { class: "scroll", style: "max-height:420px" }, table);
}

// The leg's operations, as searchable options for rows that matched nothing.
function opOptions() {
  let dl = document.getElementById("op-options");
  if (dl && dl.dataset.leg === R.leg && dl.options.length) return dl;
  dl = dl || document.body.appendChild(h("datalist", { id: "op-options" }));
  if (!INFO || INFO.leg !== R.leg) return dl;            // filled once the leg has loaded
  dl.dataset.leg = R.leg;
  dl.replaceChildren(...(INFO?.operations || []).map((o) => h("option", {
    value: [o.station || "—", o.label || o.key, o.activity, (o.start_utc || "").slice(0, 16).replace("T", " ")].join(" · ") })));
  return dl;
}
const opFromOption = (text) => (INFO?.operations || []).find((o) =>
  text === [o.station || "—", o.label || o.key, o.activity, (o.start_utc || "").slice(0, 16).replace("T", " ")].join(" · "));

function matchSearch(id, k, row) {
  opOptions();
  const input = h("input", { list: "op-options", class: "match-search", placeholder: "unmatched: search station, label, date…",
    "aria-label": "Search for the operation this row belongs to" });
  input.addEventListener("change", () => {
    const op = opFromOption(input.value.trim());
    if (op) setMatch(id, k, row, op.key);
    else if (input.value.trim()) toast("Pick one of the listed operations.");
  });
  return input;
}

async function setMatch(id, k, row, op) {
  try {
    const { linked } = await api(`api/digitized/${id}/setmatch`, { table: k, row, op });
    await refreshMatches(id, k);
    for (const lg of R.selection.logsheets.filter((x) => (linked || []).includes(x.id))) await matchLog(lg);
  } catch (e) { toast(`Match not saved: ${e.message}`, true); }
}

async function refreshMatches(id, k) {
  const table = document.querySelector(`table[data-dig="${id}:${k}"]`);
  if (!table || !R.leg) return;
  try {
    const m = await api(`api/digitized/${id}/match`, { table: k, fill_down: fillOf(id, k), leg: R.leg, groups: R.selection.groups });
    const cells = table.querySelectorAll("tbody td.match");
    m.rows.forEach((r, i) => {
      const td = cells[i];
      if (!td) return;
      if (r.op) {
        td.replaceChildren(h("span", {}, `${r.label || r.op} `, h("span", { class: "how" }, r.how),
          r.how === "by hand" ? h("button", { class: "link", title: "Undo this match", onclick: () => setMatch(id, k, i, null) }, "×") : null));
      } else {
        td.replaceChildren(matchSearch(id, k, i));
      }
    });
    const n = m.rows.filter((r) => r.op).length;
    const note = table.closest(".scroll")?.nextElementSibling?.querySelector(".match-count");
    if (note) note.textContent = `${n} of ${m.rows.length} rows matched`;
  } catch (e) { /* the column keeps its last answer */ }
}

function renderDigitized() {
  const box = $("#dig-results");
  const ids = R.digitized.filter((id) => DIG[id]?.status === "done");
  box.replaceChildren(...ids.map((id) => {
    const doc = DIG[id];
    const cells = doc.tables.reduce((a, t) => a + t.rows.length * t.columns.length, 0);
    const low = doc.tables.reduce((a, t) => a + t.rows.flat().filter((c) => !c.edited && c.c <= 1).length, 0);
    const photo = h("img", { class: "page-thumb", src: `api/digitized/${id}/image`, alt: doc.name, loading: "lazy",
      title: "Click to preview the photo", onclick: () => openPhoto(`api/digitized/${id}/image`, doc.name) });
    const stop = (fn) => (e) => { e.preventDefault(); e.stopPropagation(); fn(e); };   // buttons in the header do not fold it
    const page = h("details", { class: "dig-page", id: `dig-${id}`, open: digOpen[id] === true },
      h("summary", { class: "row" },
        h("b", {}, doc.name),
        h("span", { class: "hint" }, `${doc.tables.length} table${doc.tables.length === 1 ? "" : "s"}, ${cells} cells, ${low} to check (likely or less) · ${doc.model}${doc.fallback_reason ? " (fallback)" : ""} · ${doc.seconds}s${doc.usage?.cost != null ? ` · US$${doc.usage.cost.toFixed(3)}` : ""}`),
        h("span", { class: "row" },
          h("button", { class: "ghost small", title: "Send the same photo to the model again; replaces this page's tables and corrections",
            onclick: stop(async () => {
              try { DIG[id] = await api(`api/digitized/${id}/again`, {}); renderDigitized(); pollDigitized(); }
              catch (err) { toast(`Could not transcribe again: ${err.message}`, true); }
            }) }, "Transcribe again"),
          h("button", { class: "danger small", onclick: stop(() => { R.digitized = R.digitized.filter((x) => x !== id); persist(); renderDigitized(); }) }, "Remove"))),
      h("div", { class: "dig-tools" }, photo,
        h("a", { class: "button ghost small xlsx-all", href: `api/digitized.xlsx?ids=${ids.join(",")}`, download: "logbook_transcription.xlsx",
          title: `Every table from all ${ids.length} transcribed page${ids.length === 1 ? "" : "s"}, a sheet each, coloured by confidence` }, "Download all sheets (.xlsx)")),
      ...doc.tables.flatMap((t, k) => {
        const fill = h("input", { type: "checkbox", checked: fillOf(id, k), onchange: (e) => { digFill[`${id}:${k}`] = e.target.checked; refreshMatches(id, k); } });
        return [
          h("h3", {}, t.title || `Table ${k + 1}`),
          digTable(doc, k),
          h("div", { class: "dig-tools" },
            h("span", { class: "pill match-count" }, ""),
            h("a", { class: "button ghost small", href: `api/digitized/${id}/${k}.tsv`, download: "" }, "TSV"),
            h("button", { class: "ghost small", onclick: () => useAsLogsheet(id, k, fill.checked) }, "Use as logsheet"),
            h("label", { class: "inline hint" }, fill, " carry station, cast and date down into blank and ditto (″ ↓) cells")),
        ];
      }),
      doc.notes?.length ? h("div", {}, h("span", { class: "hint" }, "Notes outside the tables:"),
        h("ul", { class: "dig-notes" }, ...doc.notes.map((n) => h("li", { style: `background:${confColour(n.c)}`, title: confText(n.c) }, n.t)))) : null);
    page.addEventListener("toggle", () => { digOpen[id] = page.open; saveDigOpen(); });
    return page;
  }));
  for (const id of ids) DIG[id].tables.forEach((_, k) => refreshMatches(id, k));
  const x = $("#dig-xlsx");
  x.hidden = !ids.length;
  x.href = `api/digitized.xlsx?ids=${ids.join(",")}`;
  x.setAttribute("download", "logbook_transcription.xlsx");
  $("#dig-count").textContent = ids.length ? `${ids.length} page${ids.length === 1 ? "" : "s"}` : "";
  renderStrip();
}

async function useAsLogsheet(id, k, fillDown) {
  try {
    const meta = await api(`api/digitized/${id}/logsheet`, { table: k, fill_down: fillDown });
    const sheet = Object.keys(meta.sheets)[0];
    LOGS[meta.id] = { name: meta.name, sheets: meta.sheets };
    R.selection.logsheets.push({ id: meta.id, name: meta.name, sheet, roles: meta.sheets[sheet].roles });
    await matchLog(R.selection.logsheets.at(-1));
    $("#log-box").open = true;
    $("#log-box").scrollIntoView({ behavior: "smooth", block: "start" });
    toast(`${meta.name} added as a logsheet — check the matches, then “Tick matched operations”.`);
  } catch (e) { toast(`Could not use the table: ${e.message}`, true); }
}

async function loadDigitized() {
  for (const id of R.digitized || []) {
    if (DIG[id]) continue;
    try { DIG[id] = await api(`api/digitized/${id}`); } catch (e) { /* removed on the server */ }
  }
  renderDigitized();
  pollDigitized();
}

// --- 3 · conditions ---------------------------------------------------------------
async function previewNarrative() {
  const box = $("#narratives");
  if (!R.selection.ops.length) { box.replaceChildren(h("p", { class: "hint" }, "Tick some operations first.")); return; }
  box.replaceChildren(h("p", { class: "hint" }, "Reading the ship's records…"));
  try {
    const c = await api("api/conditions", { report: R });
    narrWords = wordsIn(c.narratives.join(" "));
    updateWords();
    box.replaceChildren(...(c.narratives.length ? c.narratives.map((n) => h("p", {}, n))
      : [h("p", { class: "hint" }, "No narrative (switched off).")]));
  } catch (e) { box.replaceChildren(h("p", { class: "hint" }, e.message)); }
}
let narrTimer;
function scheduleNarrWords() {
  clearTimeout(narrTimer);
  narrTimer = setTimeout(async () => {
    if (!R.leg || !R.selection.ops.length || !R.conditions.narrative) { narrWords = 0; updateWords(); return; }
    try { narrWords = (await api("api/conditions", { report: R })).narratives.reduce((a, n) => a + wordsIn(n), 0); updateWords(); }
    catch (e) { /* the counter keeps its last value */ }
  }, 1500);
}
const wordsIn = (s) => (String(s || "").match(/\b\w[\w'’-]*\b/g) || []).length;
function updateWords() {
  const n = Object.values(R.text).reduce((a, t) => a + wordsIn(t), 0) + narrWords;
  const lim = INFO?.word_limit || 3000;
  const el = $("#words");
  el.textContent = `${n.toLocaleString()} / ${lim.toLocaleString()} words`;
  el.classList.toggle("over", n > lim);
}

// --- 4 · tables -----------------------------------------------------------------
function colGroups(t) {
  const c = INFO.columns;
  const groups = [["Operation & conditions", c.op]];
  if (t.rows === "bottles") {
    groups.push(["Bottle", c.bottle]);
    groups.push(["Drawn by team (L)", R.selection.teams.map((x) => ({ id: `draw.${x}`, label: x }))]);
  }
  if (t.rows.startsWith("log:")) {
    const id = t.rows.split(":")[1];
    groups.push([`Logsheet · ${LOGS[id]?.name || id}`, (LOGS[id]?.match?.columns || []).map((x) => ({ id: `log.${x}`, label: x }))]);
  }
  return groups;
}
const colLabel = (id) => {
  for (const list of Object.values(INFO?.columns || {})) { const c = list.find((x) => x.id === id); if (c) return c.label; }
  return id.split(".").slice(1).join(".");
};
function renderTables() {
  if (!INFO) return;
  const box = $("#tables");
  box.replaceChildren(...R.tables.map((t, i) => {
    const sources = [["operations", "One row per operation"], ["bottles", "One row per rosette bottle"],
      ...R.selection.logsheets.map((lg) => [`log:${lg.id}:${lg.sheet}`, `One row per logsheet row · ${lg.name}`])];
    const card = h("div", { class: "tbl" },
      h("div", { class: "tbl-head" },
        h("label", {}, "Caption", h("input", { value: t.title || "", placeholder: "Table caption", oninput: (e) => { t.title = e.target.value; persist(); } })),
        h("label", {}, "Rows", h("select", { onchange: (e) => { t.rows = e.target.value; t.columns = t.columns.filter((c) => c.startsWith("op.")); renderTables(); persist(); } },
          ...sources.map(([v, l]) => h("option", { value: v, selected: t.rows === v }, l)))),
        h("label", {}, "Section", h("select", { onchange: (e) => { t.section = e.target.value; persist(); } },
          ...SECTIONS.map(([v, l]) => h("option", { value: v, selected: (t.section || "methods") === v }, l)))),
        h("button", { class: "ghost small", onclick: () => previewTable(i, card) }, "Preview"),
        h("div", { class: "row" },
          h("button", { class: "ghost small", title: "Move up (earlier in the report)", disabled: i === 0,
            onclick: () => { [R.tables[i - 1], R.tables[i]] = [R.tables[i], R.tables[i - 1]]; renderTables(); persist(); } }, "↑"),
          h("button", { class: "ghost small", title: "Move down (later in the report)", disabled: i === R.tables.length - 1,
            onclick: () => { [R.tables[i + 1], R.tables[i]] = [R.tables[i], R.tables[i + 1]]; renderTables(); persist(); } }, "↓"),
          h("button", { class: "danger small", onclick: () => { R.tables.splice(i, 1); renderTables(); persist(); } }, "Remove"))),
      h("div", { class: "order" }, h("span", { class: "hint" }, "Columns in order (click to remove): "),
        ...t.columns.map((c) => h("span", { class: "pill", title: "Remove", onclick: () => { t.columns = t.columns.filter((x) => x !== c); renderTables(); persist(); } }, colLabel(c) + " ×"))),
      h("div", { class: "cols" }, ...colGroups(t).map(([name, cols]) => h("fieldset", {}, h("legend", {}, name),
        ...(cols.length ? cols.map((c) => h("label", {}, h("input", { type: "checkbox", checked: t.columns.includes(c.id), onchange: (e) => {
          t.columns = e.target.checked ? [...t.columns, c.id] : t.columns.filter((x) => x !== c.id); renderTables(); persist(); } }),
          c.label + (c.unit ? ` (${c.unit})` : ""))) : [h("span", { class: "hint" }, name.startsWith("Drawn") ? "Tick your team's names in step 2." : "—")])))),
      h("div", { class: "preview" }));
    return card;
  }));
}
async function previewTable(i, card) {
  const out = $(".preview", card);
  out.replaceChildren(h("p", { class: "hint" }, "Building…"));
  try {
    const t = await api("api/table", { report: R, index: i });
    out.replaceChildren(h("p", { class: "hint" }, `${t.total} rows${t.total > t.body.length ? `, first ${t.body.length} shown` : ""}.`),
      h("div", { class: "scroll", style: "max-height:320px" }, h("table", { class: "data" },
        h("thead", {}, h("tr", {}, ...t.columns.map((c) => h("th", {}, c.label + (c.unit ? ` (${c.unit})` : ""))))),
        h("tbody", {}, ...t.body.map((r) => h("tr", {}, ...r.map((v) => h("td", {}, v))))))));
  } catch (e) { out.replaceChildren(h("p", { class: "hint" }, e.message)); }
}
function addTable(preset) {
  const p = structuredClone(PRESETS[preset]);
  if (preset === "bottles") p.columns.push(...R.selection.teams.map((x) => `draw.${x}`));
  R.tables.unshift({ ...p, section: "methods" });   // on top, where it is seen; first in Word too
  renderTables(); persist();
  const first = $("#tables .tbl");
  if (first) { first.classList.add("new"); first.scrollIntoView({ behavior: "smooth", block: "nearest" }); }
}

// --- 5 · figures ----------------------------------------------------------------
function renderFigures() {
  const box = $("#figures");
  box.replaceChildren(...R.figures.map((f, i) => {
    f.options = f.options || {};
    const img = h("div", { class: "fig-images" });
    const opts = h("div", { class: "row" });
    let wait;
    const refresh = () => { clearTimeout(wait); wait = setTimeout(() => drawFigure(f, img), 700); };
    if (f.kind === "map") {
      opts.append(
        h("label", {}, h("input", { type: "checkbox", checked: f.options.whole_leg_track !== false, onchange: (e) => { f.options.whole_leg_track = e.target.checked; persist(); refresh(); } }), " whole-leg track"),
        h("label", {}, h("input", { type: "checkbox", checked: f.options.label_stations !== false, onchange: (e) => { f.options.label_stations = e.target.checked; persist(); refresh(); } }), " station names"));
    } else if (f.kind === "profiles") {
      const vars = f.options.variables || ["Temperature", "Salinity", "Fluorescence", "Oxygen"];
      opts.append(...PROFILE_VARS.map((v) => h("label", {}, h("input", { type: "checkbox", checked: vars.includes(v), onchange: (e) => {
        f.options.variables = e.target.checked ? [...vars, v] : vars.filter((x) => x !== v); persist(); refresh(); renderFigures(); } }), " " + v)),
        h("label", {}, "max depth ", h("input", { type: "number", min: 10, step: 10, value: f.options.max_depth || "", style: "width:90px", onchange: (e) => { f.options.max_depth = +e.target.value || null; persist(); refresh(); } })));
    } else if (f.kind === "underway") {
      const all = INFO.underway_panels, ids = new Set(all.map((p) => p.id));
      let panels = (f.options.panels || []).filter((p) => ids.has(p));
      if (!panels.length) panels = INFO.underway_default.slice();
      f.options.panels = panels;
      const groups = [...new Set(all.map((p) => p.group))];
      const note = h("p", { class: "hint", style: "grid-column: 1 / -1; margin: 0" });
      const updateNote = () => {
        const n = f.options.panels.length;
        note.textContent = n > 5 ? `${n} panels: split across ${Math.ceil(n / 5)} images of up to 5, each with its own caption in Word.` : "";
      };
      opts.className = "cols";
      opts.append(note);
      opts.append(...groups.map((g) => h("fieldset", {}, h("legend", {}, g),
        ...all.filter((p) => p.group === g).map((p) => h("label", {}, h("input", { type: "checkbox", checked: panels.includes(p.id), onchange: (e) => {
          f.options.panels = e.target.checked ? [...f.options.panels, p.id] : f.options.panels.filter((x) => x !== p.id);
          updateNote(); persist(); refresh(); } }), " " + p.label + (p.source === "hourly" ? " (hourly)" : ""))))));
      updateNote();
    } else if (f.kind === "ts") {
      const nTeams = R.selection.teams.length;
      opts.append(h("label", {}, h("input", { type: "checkbox", checked: !!f.options.team_bottles, onchange: (e) => { f.options.team_bottles = e.target.checked; persist(); refresh(); } }),
        nTeams ? ` mark the bottles your team sampled (×) — ${R.selection.teams.join(", ")}` : " mark the bottles sampled (×) — tick your team's names in step 2 to show only yours"));
    }
    const card = h("div", { class: "fig" }, img,
      h("label", {}, "Caption", h("textarea", { style: "min-height:50px", oninput: (e) => { f.caption = e.target.value; persist(); } }, f.caption || "")),
      opts,
      h("div", { class: "row" },
        h("label", {}, "Section ", h("select", { onchange: (e) => { f.section = e.target.value; persist(); } },
          ...SECTIONS.map(([v, l]) => h("option", { value: v, selected: (f.section || "methods") === v }, l)))),
        h("button", { class: "ghost small", onclick: refresh }, "Redraw"),
        h("button", { class: "danger small", onclick: () => { R.figures.splice(i, 1); renderFigures(); persist(); } }, "Remove")));
    refresh();
    return card;
  }));
}
async function drawFigure(f, box) {
  if (!R.selection.ops.length) { box.replaceChildren(h("p", { class: "hint" }, "Tick some operations first.")); return; }
  box.style.opacity = .4;
  const ticket = (box.dataset.ticket = String(+(box.dataset.ticket || 0) + 1));   // only the latest request draws
  try {
    const { images } = await api("api/figure", { report: R, spec: f });
    if (box.dataset.ticket !== ticket || !box.isConnected) return;
    box.replaceChildren(...images.map((src, i) => h("figure", {},
      h("img", { src, alt: `${f.caption || f.kind}${images.length > 1 ? ` (${i + 1} of ${images.length})` : ""}` }),
      images.length > 1 ? h("figcaption", { class: "hint" }, `Image ${i + 1} of ${images.length}`) : null)));
  } catch (e) { toast(`Figure: ${e.message}`, true); }
  box.style.opacity = 1;
}
function addFigure(kind) {
  const cap = INFO.figures.find((x) => x.kind === kind)?.caption || "";
  R.figures.push({ kind, caption: cap, section: kind === "map" ? "methods" : "results", options: {} });
  renderFigures(); persist();
}

// --- 6 · text -------------------------------------------------------------------
function renderTexts() {
  $("#texts").replaceChildren(...TEXTS.map(([k, label, ph]) => h("label", {}, h("b", {}, label),
    h("textarea", { placeholder: ph, style: k.length > 12 || ["publications", "presentations", "in_progress"].includes(k) ? "min-height:60px" : "",
      oninput: (e) => { R.text[k] = e.target.value; persist(); } }, R.text[k] || ""))));
}

// --- leg, drafts, download ------------------------------------------------------
async function loadLeg(leg) {
  R.leg = leg; R.leg_label = legLabel(leg);
  $("#ops").replaceChildren(h("tbody", {}, h("tr", {}, h("td", { class: "hint" }, "Loading the event log…"))));
  INFO = await api(`api/leg?leg=${encodeURIComponent(leg)}`);
  const known = new Set(INFO.operations.map((o) => o.key));
  R.selection.ops = R.selection.ops.filter((k) => known.has(k));
  renderGroups(); renderTeams(); renderOps(); renderTables(); renderFigures(); echo(); renderDigitized();
  for (const lg of R.selection.logsheets) matchLog(lg).catch((e) => toast(`${lg.name}: ${e.message}`, true));
  persist();
}
function renderAll() {
  R.digitized = R.digitized || [];
  renderTeam(); renderTexts(); loadDigitized();
  $$('#narr-mode input').forEach((r) => (r.checked = r.value === (R.conditions.narrative || "")));
}
async function refreshDrafts() {
  try {
    const { drafts } = await api("api/drafts");
    $("#draft-list").replaceChildren(h("option", { value: "" }, "— open a saved draft —"),
      ...drafts.map((d) => h("option", { value: d.name }, `${d.name} · ${new Date(d.mtime * 1000).toLocaleString()}`)));
  } catch (e) { /* the list is a convenience */ }
}
const draftName = () => `${(R.team || "team").trim()}-${R.leg || "leg"}`.replace(/[^\w-]+/g, "-");
async function saveDraft() {
  try {
    const { saved } = await api(`api/draft/${encodeURIComponent(draftName())}`, R, { method: "PUT" });
    toast(`Draft saved as “${saved}”. Anyone on the ship network can open it from the Draft list.`);
    refreshDrafts();
  } catch (e) { toast(`Not saved: ${e.message}`, true); }
}
async function openDraft(name) {
  if (!name) return;
  try {
    R = Object.assign(blank(), await api(`api/draft/${encodeURIComponent(name)}`));
    renderAll();
    $("#leg").value = R.leg;
    await loadLeg(R.leg);
    toast(`Opened “${name}”.`);
  } catch (e) { toast(`Could not open: ${e.message}`, true); }
}
async function download(btn) {
  if (!R.selection.ops.length) toast("No operations are ticked, so the report has no generated narrative, tables or figures.");
  btn.disabled = true;
  const label = btn.textContent;
  btn.textContent = "Building…";
  try {
    const blob = await api("api/docx", { report: R }, { blob: true });
    const a = h("a", { href: URL.createObjectURL(blob), download: `Cruise Report_${(R.team || "TeamX").replace(/[^\w-]+/g, "_")}.docx` });
    document.body.append(a); a.click(); a.remove();
    setTimeout(() => URL.revokeObjectURL(a.href), 10000);
  } catch (e) { toast(`Download failed: ${e.message}`, true); }
  btn.disabled = false; btn.textContent = label;
}

// --- wiring ---------------------------------------------------------------------
// One page view for the dashboard's status page (Underway status → Page views).
// On underway.local the dashboard owns /api/; on the ship's IP addresses it sits under /underway/.
function countVisit() {
  const path = location.hostname === "underway.local" ? "/api/usage" : "/underway/api/usage";
  try { navigator.sendBeacon?.(path, "report|en"); } catch (e) { /* counting is best effort */ }
}

async function init() {
  countVisit();
  try { const s = JSON.parse(localStorage.getItem(LOCAL_KEY)); if (s && s.selection) R = Object.assign(blank(), s); }
  catch (e) { /* start blank */ }
  $("#team").addEventListener("input", (e) => { R.team = e.target.value; echo(); persist(); });
  $("#title").addEventListener("input", (e) => { R.header.title = e.target.value; persist(); });
  $$("[data-add]").forEach((b) => b.addEventListener("click", () => { R.header[b.dataset.add].push({}); renderPeople(b.dataset.add); }));
  $("#ops-filter").addEventListener("input", renderOps);
  $("#ops-all").addEventListener("click", () => { const s = new Set(R.selection.ops); $$("#ops input[data-key]").forEach((i) => s.add(i.dataset.key)); R.selection.ops = [...s]; renderOps(); persist(); selectionChanged(); });
  $("#ops-none").addEventListener("click", () => { const off = new Set($$("#ops input[data-key]").map((i) => i.dataset.key)); R.selection.ops = R.selection.ops.filter((k) => !off.has(k)); renderOps(); persist(); selectionChanged(); });
  $("#log-file").addEventListener("change", (e) => { for (const f of e.target.files) uploadLog(f); e.target.value = ""; });
  $("#dig-file").addEventListener("change", (e) => {
    for (const f of e.target.files) digQueue.push({ file: f, rotate: 0, url: URL.createObjectURL(f), status: "ready" });
    e.target.value = ""; renderStrip();
  });
  $("#dig-go").addEventListener("click", () => transcribeQueue());
  const lb = $("#lightbox");
  $("#lb-close").addEventListener("click", () => lb.close());
  lb.addEventListener("click", (e) => { if (e.target === lb) lb.close(); });          // the backdrop
  $("#lb-img").addEventListener("click", () => lb.querySelector(".lb-body").classList.toggle("zoom"));
  $$("#narr-mode input").forEach((r) => r.addEventListener("change", () => { R.conditions.narrative = r.value || null; persist(); previewNarrative(); }));
  $("#cond-preview").addEventListener("click", previewNarrative);
  $$("[data-preset]").forEach((b) => b.addEventListener("click", () => addTable(b.dataset.preset)));
  $$("[data-fig]").forEach((b) => b.addEventListener("click", () => addFigure(b.dataset.fig)));
  $("#save-draft").addEventListener("click", saveDraft);
  $("#save-draft2").addEventListener("click", saveDraft);
  $("#draft-list").addEventListener("change", (e) => openDraft(e.target.value));
  $("#download").addEventListener("click", (e) => download(e.target));
  $("#download2").addEventListener("click", (e) => download(e.target));
  $("#leg").addEventListener("change", (e) => {
    if (R.selection.ops.length && !window.confirm("Switching legs clears the ticked operations. Continue?")) { e.target.value = R.leg; return; }
    R.selection = { ...blank().selection, teams: [] };
    loadLeg(e.target.value).catch((err) => toast(err.message, true));
  });
  renderAll();
  refreshDrafts();
  try {
    const { legs } = await api("api/legs");
    $("#leg").replaceChildren(...legs.map((l) => h("option", { value: l }, `${l.slice(0, 4)} ${legLabel(l)}`)));
    const leg = legs.includes(R.leg) ? R.leg : legs.at(-1);
    $("#leg").value = leg;
    await loadLeg(leg);
  } catch (e) { toast(`The ship's shares could not be read: ${e.message}`, true); }
}
init();
