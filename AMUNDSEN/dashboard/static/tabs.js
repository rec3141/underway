/* Casts, Calendar and Table panes. Loaded after app.js; talks to it through
 * window.UW (state, manifest, helpers, and hooks the map calls back into). */
(() => {
  "use strict";
  const UW = window.UW;
  const $ = (s) => document.querySelector(s);
  const { THEME, C, fz, CFG, fmtTs, fmtVal, dms, store } = UW;
  const esc = (s) => String(s ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  const pal = (i) => C.palette[i % C.palette.length];      // the theme's data palette, round and round
  const getJSON = UW.fetchJSON;
  const cachedJSON = window.UWData.generationCache(getJSON, () => UW.M.generated_utc);
  const debounce = (f, ms) => { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => f(...a), ms); }; };

  // ================================================================ casts
  const casts = {
    idx: null, loadedFor: null,
    sel: new Set(store.get("casts.sel", [])),
    mode: store.get("casts.mode", "profiles"),          // single | profiles (Multi) | section
    kind: store.get("casts.kind", "all"),               // all | CTD | TM | MVP | live (the cast in the water)
    xmode: store.get("casts.xmode", "time"),            // the section's own x axis: time | distance | custom (an order of the user's)
    order: store.get("casts.order", []),                // custom: profile ids in the order they are laid along the section
    variable: store.get("casts.var", "Temperature"),
    bottles: store.get("casts.bottles", false),          // mark the bottle firings on the casts
    smooth: store.get("casts.smooth", true),             // the section smooths the jittery sensors down the profile
    search: "",
  };
  // a profile's variable at a pressure, interpolated between its levels
  const valueAt = (prof, v, pres) => {
    const p = prof.p, x = prof.vars[v]; if (!p?.length || !x) return null;
    if (pres <= p[0]) return x[0]; if (pres >= p[p.length - 1]) return x[p.length - 1];
    let j = 0; while (j < p.length - 2 && p[j + 1] < pres) j++;
    const xa = x[j], xb = x[j + 1]; if (xa == null || xb == null) return xa ?? xb;
    return xa + (xb - xa) * (pres - p[j]) / (p[j + 1] - p[j] || 1);
  };
  const bottleDepth = (b, lat) => b.depth_m ?? (b.p != null ? depthFrom(b.p, lat) : null);
  const bottleText = (b) => `bottle ${b.bottle}${b.time ? " · " + String(b.time).replace("T", " ").slice(11, 16) : ""}`;
  if (casts.mode === "live") { casts.kind = "live"; casts.mode = "single"; store.set("casts.kind", "live"); store.set("casts.mode", "single"); }   // Live is a kind now
  // selection ids: a cast or tow id, or "<towid>#<dip index>" for one dip
  const parentId = (id) => id.split("#")[0];
  const castById = (id) => casts.idx?.casts.find((c) => c.id === parentId(id));
  const dipSel = (towId) => [...casts.sel].filter((s) => s.startsWith(towId + "#")).map((s) => +s.split("#")[1]).sort((a, b) => a - b);
  casts.open = new Set(store.get("casts.open", []));
  const castLabel = (c) => c.kind === "LIVE" ? "Live cast" : c.kind === "MVP" ? `MVP tow ${c.cast}${c.n_profiles ? ` · ${c.n_profiles} dips` : ""}`
    : `${c.kind === "TM" ? "TM cast" : "Cast"} ${c.cast}${c.station ? " · " + c.station : ""}`;
  const castDate = (c) => c.time ? c.time.replace("T", " ").slice(0, 16) + (c.time_end ? "–" + c.time_end.replace("T", " ").slice(11, 16) : "") : "";
  // a tow bundle expands into its dips — only the selected ones when dips were
  // picked individually, all of them when the tow was selected as a whole; a
  // CTD cast is one profile
  const profilesOf = (d) => {
    if (!d.profiles?.length) return [{ ...d, label: castLabel(d), parent: d }];
    const picked = new Set(dipSel(d.id));
    return d.profiles.map((p, i) => ({ ...p, units: d.units, label: `${castLabel(d)} #${i + 1}`, parent: d, index: i }))
      .filter((p) => !picked.size || picked.has(p.index));
  };

  UW.selectedCastKeys = () => new Set([...casts.sel].map(parentId));
  // a station click on the map toggles its cast and opens the Casts tab; a
  // quiet call (the stations table) only makes sure it is selected
  UW.onStationClick = async (key, opts = {}) => {
    if (key.startsWith("ev:")) {                          // a station without a cast: its events on the Agenda
      const st = (UW.M.stations || []).find((s) => s.kind === "event" && `ev:${s.leg}:${s.station}` === key);
      if (!st || opts.quiet) return;
      UW.focusMap(st.lat, st.lon, st.station);
      stn.search = st.station; const box = $("#stnsearch"); if (box) box.value = st.station;
      UW.showTab("stations"); renderStations(); return;
    }
    if (!casts.idx) {
      if (!opts.quiet) return;
      try { await ensureCastIndex(); }
      catch { UW.setLoadError("Casts", true); return; }
    }
    if (!castById(key)) return;
    if (opts.quiet) { if (opts.toggle || !casts.sel.has(key)) toggleCast(key); return; }
    toggleCast(key);
    if ($("#pane-casts").hidden) UW.showTab("casts");
  };
  UW.extraMapTraces = () => {
    const out = [];
    if (!casts.idx || casts.kind === "CTD") return out;
    // each MVP tow is one dataset: its track as a line, with a clickable
    // marker at the start (the whole line also selects it)
    const f = UW.spanFilter();
    const tows = casts.idx.casts.filter((c) => c.kind === "MVP" && c.track?.length && (UW.inFilter(c.leg, c.time_end || c.time, f) || UW.inFilter(c.leg, c.time, f)));
    const lat = [], lon = [], cd = [], txt = [];
    for (const c of tows) {
      for (const [la, lo] of c.track) { lat.push(la); lon.push(lo); cd.push(c.id); txt.push(`<b>${castLabel(c)}</b><br>${castDate(c)}<br>to ${maxDepth(c)}`); }
      lat.push(null); lon.push(null); cd.push(null); txt.push("");
    }
    if (tows.length) {
      out.push({ type: "scattermap", mode: "lines", name: "MVP tows", showlegend: false, hoverinfo: "skip", connectgaps: false,
                 lat, lon, line: { width: 3, color: "rgba(126,231,135,.55)" } });
      // selected tows drawn brighter on top; individually picked dips as dots
      const sel = tows.filter((c) => casts.sel.has(c.id));
      if (sel.length) out.push({ type: "scattermap", mode: "lines", name: "selected tows", showlegend: false, hoverinfo: "skip", connectgaps: false,
        lat: sel.flatMap((c) => [...c.track.map((t) => t[0]), null]), lon: sel.flatMap((c) => [...c.track.map((t) => t[1]), null]),
        line: { width: 4, color: C.accent2 } });
      const dips = tows.flatMap((c) => dipSel(c.id).map((i) => ({ c, i })));
      if (dips.length) out.push({ type: "scattermap", mode: "markers", name: "selected dips", showlegend: false, hoverinfo: "text",
        lat: dips.map(({ c, i }) => c.track[i]?.[0]), lon: dips.map(({ c, i }) => c.track[i]?.[1]),
        text: dips.map(({ c, i }) => `${castLabel(c)} · dip ${i + 1}`), marker: { size: 9, color: C.accent2 } });
      out.push({ type: "scattermap", mode: "markers", name: "MVP tow starts", showlegend: false, hoverinfo: "text",
        lat: tows.map((c) => c.lat), lon: tows.map((c) => c.lon), customdata: tows.map((c) => c.id),
        text: tows.map((c) => `<b>${castLabel(c)}</b><br>${castDate(c)}<br>to ${maxDepth(c)} · click to select the tow`),
        marker: { size: tows.map((c) => isSelected(c) ? 11 : 7), color: tows.map((c) => isSelected(c) ? C.accent2 : C.ok), symbol: "circle" } });
      // generous click target for tow starts (drawn beneath the station targets)
      out.push({ type: "scattermap", mode: "markers", name: "tow hit targets", showlegend: false, hoverinfo: "skip",
        lat: tows.map((c) => c.lat), lon: tows.map((c) => c.lon), customdata: tows.map((c) => c.id),
        marker: { size: 22, color: "rgba(126,231,135,0.02)" } });
    }
    const sel = orderedSelection().filter((c) => c.lat != null);
    if (casts.mode === "section" && sel.length > 1) out.push({
      type: "scattermap", mode: "lines", name: "section", showlegend: false, hoverinfo: "skip",
      lat: sel.map((c) => c.lat), lon: sel.map((c) => c.lon), line: { width: 2, color: "rgba(255,180,84,.6)" },
    });
    return out;
  };

  function orderedSelection() {
    const seen = new Set();
    return [...casts.sel].map(castById).filter((c) => c && !seen.has(c.id) && seen.add(c.id))
      .sort((a, b) => (a.time || "").localeCompare(b.time || ""));
  }
  const isSelected = (c) => casts.sel.has(c.id) || dipSel(c.id).length > 0;
  function toggleCast(id) {
    if (id.includes("#")) {
      // a dip: selecting one turns a whole-tow selection into a dip selection
      const tow = parentId(id);
      casts.sel.delete(tow);
      casts.sel.has(id) ? casts.sel.delete(id) : casts.sel.add(id);
    } else {
      const wasOn = isSelected(castById(id) || { id });
      for (const s of [...casts.sel]) if (parentId(s) === id) casts.sel.delete(s);
      if (!wasOn) casts.sel.add(id);
    }
    store.set("casts.sel", [...casts.sel]);
    renderCastList(); renderCastPlots(); UW.renderMap();
  }

  async function ensureCastIndex() {
    const stamp = UW.M.generated_utc;
    if (casts.idx && casts.loadedFor === stamp) return;
    const idx = await cachedJSON("cast-index", UW.M.casts.index);
    casts.idx = idx; casts.loadedFor = stamp;
    fillCastVars();
  }
  async function castData(id) {
    const m = castById(id); if (!m) return null;
    return cachedJSON(`cast:${id}`, m.file);
  }

  // the variable menu offers what the selected casts actually carry (every
  // variable in the index when nothing is selected); the choice survives
  // when it is still available, else Temperature or the first
  function fillCastVars() {
    if (!casts.idx) return;
    const chosen = [...casts.sel].map(castById).filter(Boolean);
    const vars = chosen.length ? orderVars(new Set(chosen.flatMap((c) => c.vars || []))) : orderVars(casts.idx.variables);
    const sel = $("#castvar"); sel.innerHTML = "";
    for (const v of vars) { const o = document.createElement("option"); o.value = v; o.textContent = v; sel.appendChild(o); }
    if (!vars.includes(casts.variable)) casts.variable = vars.includes("Temperature") ? "Temperature" : (vars[0] || "Temperature");
    sel.value = casts.variable;
  }
  // A table the legs menu has trimmed says so in its first row: how many
  // rows belong to legs not shown, with a link that shows every leg.
  function spanNote(kept, all, f, tag = "tr", colspan = 1) {
    const n = all - kept; if (n <= 0) return "";
    const text = `${n.toLocaleString()} ${n === 1 ? "row" : "rows"} from legs not shown · <a href="#" class="spanall">show all legs</a>`;
    return tag === "li" ? `<li class="spannote">${text}</li>` : `<tr class="spannote"><td colspan="${colspan}">${text}</td></tr>`;
  }
  document.addEventListener("click", (e) => {
    const a = e.target.closest("a.spanall"); if (!a) return;
    e.preventDefault(); UW.showAllLegs();
  });
  // the casts as a sortable table like the Stations and Underway ones: a
  // row per cast (a tow unfolds into its dips), a click selects it
  const CAST_COLS = [["sel", ""], ["kind", "type"], ["cast", "cast"], ["station", "station"], ["label", "label"], ["time", "time (ship)"], ["depth", "max depth (m)"], ["bottles", "bottles"], ["leg", "leg"]];
  casts.sort = store.get("casts.sort", { key: "time", dir: -1 });
  function castRows() {
    if (!casts.idx) return { rows: [], inLegs: [] };
    const q = casts.search.toLowerCase();
    const f = UW.currentFilter();
    const inLegs = casts.idx.casts
      .filter((c) => f.legs.has(c.leg))
      .filter((c) => casts.kind === "all" || c.kind === casts.kind)
      .filter((c) => !q || `${c.cast} ${c.station} ${c.label} ${c.time} ${c.leg}`.toLowerCase().includes(q));
    const rows = inLegs.filter((c) => UW.inFilter(c.leg, c.time_end || c.time, f) || UW.inFilter(c.leg, c.time, f))
      .map((c) => ({ ...c, legLabel: UW.legById(c.leg)?.label || c.leg, depth: c.max_p != null ? Math.round(depthFrom(c.max_p, c.lat)) : null, bottles: c.n_bottles ?? null }));
    const k = casts.sort.key, dir = casts.sort.dir;
    const val = (r) => k === "leg" ? r.legLabel : k === "cast" ? +r.cast : k === "sel" ? (casts.sel.has(r.id) ? 1 : 0) : r[k];
    rows.sort((a, b) => { const x = val(a), y = val(b); if (x == null || x === "") return 1; if (y == null || y === "") return -1; return (x < y ? -1 : x > y ? 1 : 0) * dir; });
    return { rows, inLegs, f };
  }
  function renderCastList() {
    const tbl = $("#casttable"); if (!casts.idx) return;
    if (casts.kind === "live") {                                   // the table box holds the Seasave setup instead
      if (!tbl.querySelector("#livecfgbox")) tbl.innerHTML = `<tbody><tr class="livesetup"><td colspan="${CAST_COLS.length}"><div id="livecfgbox"></div></td></tr></tbody>`;
      if (live.data) liveCfgForm(tbl, live.data);
      $("#castclear").textContent = "clear selection"; $("#castclear").classList.remove("has"); return;
    }
    const { rows, inLegs, f } = castRows();
    const arrow = (k) => casts.sort.key === k ? (casts.sort.dir > 0 ? " ▲" : " ▼") : "";
    const head = CAST_COLS.map(([k, l]) => `<th data-k="${esc(k)}" title="sort">${esc(l)}${arrow(k)}</th>`).join("");
    const row = (c) => {
      const dips = dipSel(c.id), whole = casts.sel.has(c.id), part = dips.length > 0;
      const isTow = c.kind === "MVP" && c.n_profiles;
      let html = `<tr class="${whole ? "sel" : part ? "part" : ""}" data-id="${esc(c.id)}">
        <td class="sel">${isTow ? `<button class="tog" data-tow="${esc(c.id)}" title="show dips">${casts.open.has(c.id) ? "▾" : "▸"}</button>` : ""}</td>
        <td><span class="kind ${c.kind}">${c.kind === "CTD" ? "ROS" : c.kind}</span></td><td class="mono">${esc(c.cast)}</td>
        <td>${esc(c.station || "")}${isTow && c.n_profiles ? ` <small>${part ? `${dips.length}/` : ""}${c.n_profiles} dips</small>` : ""}</td><td>${esc(c.label || "")}</td>
        <td class="mono">${esc(castDate(c))}</td><td class="mono">${c.depth ?? ""}</td><td class="mono">${c.bottles ?? ""}</td><td>${esc(c.legLabel)}</td></tr>`;
      if (isTow && casts.open.has(c.id)) {
        const picked = new Set(dips);
        html += c.track.map((t, i) => `<tr class="dip ${picked.has(i) ? "sel" : ""}" data-id="${esc(c.id)}#${i}">
          <td class="sel"></td><td><span class="kind dip">#${i + 1}</span></td><td></td>
          <td>dip ${i + 1}</td><td class="mono">${t[0] != null ? `${t[0].toFixed(3)}, ${t[1].toFixed(3)}` : ""}</td><td></td><td></td><td></td><td></td></tr>`).join("");
      }
      return html;
    };
    tbl.innerHTML = `<thead><tr>${head}</tr></thead><tbody>${spanNote(rows.length, inLegs.length, f, "tr", CAST_COLS.length)}${rows.map(row).join("")}${!rows.length ? `<tr><td colspan="${CAST_COLS.length}" class="muted">no casts match</td></tr>` : ""}</tbody>`;
    topScroll($("#castlist"));
    for (const th of tbl.querySelectorAll("th")) th.onclick = () => {
      const k = th.dataset.k; casts.sort = { key: k, dir: casts.sort.key === k ? -casts.sort.dir : (k === "time" ? -1 : 1) }; store.set("casts.sort", casts.sort); renderCastList();
    };
    for (const tr of tbl.querySelectorAll("tbody tr[data-id]")) tr.onclick = (e) => {
      if (e.target.closest(".tog") || e.target.closest("a")) return;
      e.preventDefault(); toggleCast(tr.dataset.id);
    };
    for (const b of tbl.querySelectorAll("button.tog")) b.onclick = (e) => {
      e.stopPropagation();
      const id = b.dataset.tow; casts.open.has(id) ? casts.open.delete(id) : casts.open.add(id);
      store.set("casts.open", [...casts.open]); renderCastList();
    };
    const nsel = new Set([...casts.sel].map(parentId)).size;
    $("#castclear").textContent = nsel ? `clear selection (${nsel})` : "clear selection";
    $("#castclear").classList.toggle("has", nsel > 0);
  }
  function downloadCastsTSV() {
    const { rows } = castRows();
    saveTSV("casts.tsv", ["kind", "cast", "station", "label", "time_utc", "time_end_utc", "lat", "lon", "max_depth_m", "bottles", "leg", "selected"],
      rows.map((r) => [r.kind, r.cast, r.station, r.label, r.time, r.time_end, r.lat, r.lon, r.depth, r.bottles, r.legLabel, casts.sel.has(r.id) ? 1 : 0]));
  }

  // ------------------------------------------------------------ overlay views
  // Live: the cast in the water (or the last one back on deck) from the deck
  // PC's Seasave feed via api/live, polled every 2 s while on screen. Single:
  // one selected cast from the archive. Both draw the chosen variables over
  // depth on their own x axes (bottom, top, then outwards).
  const live = { vars: store.get("casts.live.vars", ["temperature", "salinity"]), which: "current", data: null, timer: null, showCfg: false };
  const single = { vars: store.get("casts.single.vars", ["Temperature", "Salinity"]), id: null, dip: null };
  const LIVE_SKIP = new Set(["scan", "time", "t", "pressure", "prdm", "prm", "pr", "p", "depth", "depsm", "depth_m"]);
  const liveVisible = () => casts.kind === "live" && !$("#pane-casts").hidden;

  // spec: { depth[], vars: {name: values}, units: {name}, splitAt (index of the deepest point, or null), nowDepth, sub }
  function drawOverlay(body, plotId, title, spec, chosen) {
    if (!body.querySelector(`#${plotId}`)) body.innerHTML = castPanelHtml(plotId, title, "", false, false, true, false).replace('class="panel card castplot', 'class="panel card castplot solo wide tall');
    const vars = chosen.filter((v) => spec.vars[v]);
    const gdEl = $(`#${plotId}`);
    // each extra axis needs ~64 px of ticks and title: the canvas grows by
    // that much per axis beyond the first at the top and the bottom, so the
    // profile keeps its height; the margins hold the outermost axes
    const nb = Math.ceil(vars.length / 2), nt = Math.floor(vars.length / 2);
    const extra = Math.max(0, nb - 1) + Math.max(0, nt - 1);
    gdEl.style.height = extra ? `calc(var(--tallh) + ${Math.round(extra * fz(64))}px)` : "";
    const H = Math.max(360, gdEl.clientHeight || 500), step = fz(64) / H;
    const y0 = step * Math.max(0, nb - 1), y1 = 1 - step * Math.max(0, nt - 1);
    const traces = [], layout = { ...castLayout(), hovermode: "closest", margin: { l: fz(56), r: 16, t: fz(64), b: fz(64) }, showlegend: false };
    const maxD = Math.max(1, ...spec.depth.filter((x) => x != null));
    layout.yaxis = depthAxis(maxD * 1.04, { domain: [y0, y1] });
    const yv = spec.depth.map(yT);
    vars.forEach((v, i) => {
      const ax = i === 0 ? "x" : `x${i + 1}`, key = i === 0 ? "xaxis" : `xaxis${i + 1}`, color = pal(i);
      const bottom = i % 2 === 0, k = Math.floor(i / 2);
      const unit = spec.units?.[v] ? ` (${spec.units[v]})` : "";
      // each axis carries a baseline in its colour, ticks tight against it and the
      // title tight against the ticks, so the stacked axes read as groups
      layout[key] = { ...THEME.xaxis, title: { text: v + unit, font: { size: fz(12), color }, standoff: 2 }, tickfont: { size: fz(11), color }, ticks: "outside", ticklen: 3, tickcolor: color,
        showline: true, linecolor: color, linewidth: 1.5, showgrid: i === 0, side: bottom ? "bottom" : "top",
        ...(i === 0 ? { anchor: "y" } : { overlaying: "x", anchor: k === 0 ? "y" : "free", position: k === 0 ? undefined : (bottom ? y0 - step * k : y1 + step * k) }) };
      const seg = (from, to, dash) => traces.push({ type: "scatter", mode: "lines", name: `${v}${dash ? " up" : ""}`, xaxis: ax, yaxis: "y",
        x: spec.vars[v].slice(from, to), y: yv.slice(from, to), customdata: spec.depth.slice(from, to), connectgaps: false, line: { color, width: dash ? 1.2 : 1.8, dash: dash ? "dot" : "solid" },
        hovertemplate: `${esc(v)} %{x:.3~f}${esc(unit)}<br>%{customdata:.1f} m<extra>${dash ? "up" : ""}</extra>` });
      if (spec.splitAt != null && spec.splitAt < spec.depth.length - 1) { seg(0, spec.splitAt + 1, false); seg(spec.splitAt, spec.depth.length, true); }
      else seg(0, spec.depth.length, false);
    });
    if (spec.nowDepth != null && vars.length) {
      const li = spec.depth.length - 1;
      traces.push({ type: "scatter", mode: "markers", xaxis: "x", yaxis: "y", x: [spec.vars[vars[0]][li]], y: [yT(spec.nowDepth)], marker: { size: 11, color: C.accent2, symbol: "diamond" }, hoverinfo: "skip", name: "now" });
    }
    // the bottle firings: white dots down the right edge, on an axis of their own
    if (casts.bottles && spec.bottles?.length && vars.length) {
      layout.xaxis20 = { overlaying: "x", range: [0, 1], visible: false, fixedrange: true };
      traces.push({ type: "scatter", mode: "markers", xaxis: "x20", yaxis: "y", name: "bottles", x: spec.bottles.map(() => 0.975), y: spec.bottles.map((b) => yT(bottleDepth(b, spec.lat))),
        text: spec.bottles.map((b) => `${bottleText(b)}<br>${Math.round(bottleDepth(b, spec.lat))} m`), hoverinfo: "text", marker: { size: 8, color: C.marker, line: { color: C.markerLine, width: 1 } } });
    }
    if (!vars.length) { body.innerHTML = '<div class="empty">Tick at least one variable above.</div>'; return; }
    Plotly.react($(`#${plotId}`), traces, layout, CFG).then((gd) => UW.axisZoom(gd, { x: false }));
    const sub = body.querySelector(`#${plotId}`)?.closest(".castplot")?.querySelector(".now");
    if (sub) sub.textContent = spec.sub || "";
  }
  const varChips = (names, on, cls) => names.map((c) => `<button type="button" class="chip ${cls} ${on.includes(c) ? "on" : ""}" data-v="${esc(c)}">${esc(c)}</button>`).join("");

  // ---- live
  async function pollLive() {
    clearTimeout(live.timer);
    if (!liveVisible()) return;
    try { live.data = await getJSON(`api/live?t=${Date.now()}`); } catch { live.data = null; }
    drawLive($("#castplots"));
    live.timer = setTimeout(pollLive, 2000);
  }
  function renderLive(host) {
    host.innerHTML = `<div class="livebar" id="livebar"><div id="livestatus"></div></div><div id="livebody"></div>`;
    pollLive();
  }
  // the Seasave setup, in the cast-list box while Live is the kind: the
  // source form is built once and left alone while the page refreshes, so an
  // edit in progress survives; the field list and raw scans under it follow
  // every poll
  function liveCfgForm(host, d) {
    const box = host.querySelector("#livecfgbox");
    if (!box) return;
    const fieldsSummary = `Seasave's field list (SBE_ConvertedDataSettings)${d.fields?.length ? ` · ${d.fields.length} fields` : ""}`;
    const fieldsText = d.fields?.length ? d.fields.map((n, i) => `${(d.columns || [])[i] || ""}  ←  ${n}`).join("\n") : "none received yet — Seasave sends it when the connection opens";
    const rawText = (d.raw || []).join("\n") || "none yet";
    const details = `<details id="livefields"><summary>${esc(fieldsSummary)}</summary><pre class="mono">${esc(fieldsText)}</pre></details>
        <details id="liveraw"><summary>last raw scans</summary><pre class="mono">${esc(rawText)}</pre></details>`;
    if (box.querySelector("form")) {                     // refresh the texts, not the elements: an open panel stays open
      const set = (id, summary, text) => { const el = box.querySelector(id); if (!el) return; if (summary) el.querySelector("summary").textContent = summary; el.querySelector("pre").textContent = text; };
      set("#livefields", fieldsSummary, fieldsText); set("#liveraw", null, rawText);
      const stl = box.querySelector("#livesetupstatus"); if (stl) stl.innerHTML = live.statusHtml || "";
      return;
    }
    box.innerHTML = `<div class="livesetup-title">Live cast</div><div id="livesetupstatus">${live.statusHtml || ""}</div><form class="livecfgform" id="livecfgform"><label>Seasave TCP/IP out (host:port) <input name="tcp" value="${esc(d.tcp || "")}" size="18"></label><button type="submit">apply</button>
        <span role="alert" id="livecfgerror"></span></form><div id="livedetails">${details}</div>`;
    box.querySelector("form").onsubmit = async (ev) => { ev.preventDefault(); const f = new FormData(ev.target);
      const button = ev.target.querySelector('[type="submit"]'), error = box.querySelector("#livecfgerror");
      button.disabled = true; error.textContent = "";
      const controller = new AbortController(), timeout = setTimeout(() => controller.abort(), 10000);
      try {
        const response = await fetch("api/live", { method: "POST", signal: controller.signal, headers: { "Content-Type": "application/json" }, body: JSON.stringify({ tcp: f.get("tcp") }) });
        const result = await response.json();
        if (!response.ok) throw new Error(result.error || `Configuration failed (${response.status})`);
        live.data = result; drawLive($("#castplots")); pollLive();
      } catch (e) { error.textContent = e.name === "AbortError" ? "Request timed out; check settings before retrying." : `Could not apply settings: ${e.message}`; }
      finally { clearTimeout(timeout); button.disabled = false; }
    };
  }
  function drawLive(host) {
    const st = host.querySelector("#livestatus"), body = host.querySelector("#livebody");
    if (!st || !body) return;
    const d = live.data;
    if (!d) { st.innerHTML = `<span class="muted">live feed unavailable (server not reachable)</span>`; return; }
    const cast = live.which === "last" ? (d.last || d.current) : (d.current || d.last);
    const which = cast === d.current ? "in the water" : cast === d.last ? (cast.end_reason ? `last cast (${esc(cast.end_reason)})` : "last cast") : null;
    // the subtitle: the time now, when data last arrived, and LIVE while
    // scans are coming in (connected but idle: CONNECTED; otherwise OFFLINE)
    const age = d.last_packet_age_s;
    const flowing = d.tcp_state === "connected" && age != null && age < 10;
    const announced = !!d.fields?.length;                          // Seasave sends its field list when it is really serving
    const state = !d.tcp ? "OFF" : flowing ? "LIVE" : d.tcp_state === "connected" ? (announced ? "CONNECTED" : "PORT OPEN") : "OFFLINE";
    const updated = d.packets && age != null ? `last updated ${age < 60 ? age.toFixed(0) + " s" : (age / 60).toFixed(0) + " min"} ago` : "no data yet";
    const why = !d.tcp ? "no source set" : state === "PORT OPEN" ? `Seasave at ${d.tcp} accepts the connection but has sent nothing, not even its field list: acquisition is probably stopped or TCP/IP Out is off` : `Seasave at ${d.tcp}: ${d.tcp_state}`;
    const feed = `${stampL(Date.now())} ${tzAbbr()} · ${updated} · <span class="livestate ${state.toLowerCase().replace(" ", "-")}" title="${esc(why)}">${state}</span>`;
    const cols = (cast?.columns || d.columns || []).filter((c) => !LIVE_SKIP.has(c.toLowerCase()));
    live.statusHtml = `<div class="livestatus"><span class="dot ${flowing ? "on" : ""}"></span><span>${feed}</span>
        ${cast ? `<span class="muted">· ${which} · ${cast.n.toLocaleString()} scans kept${cast.max_p ? ` · max ${cast.max_p.toFixed(0)} ${cast.depth_like ? "m" : "dbar"}` : ""}${cast.direction ? ` · ${cast.direction === "down" ? "↓ descending" : cast.direction === "up" ? "↑ ascending" : "holding"}` : ""}</span>` : ""}
        </div>`;
    st.innerHTML = `<div class="livevars">${varChips(cols, live.vars, "livevar")}${d.current && d.last ? ` <span class="muted">show:</span> <button type="button" class="chip ${live.which === "current" ? "on" : ""}" data-w="current">in water</button><button type="button" class="chip ${live.which === "last" ? "on" : ""}" data-w="last">last</button>` : ""}</div>`;
    for (const b of st.querySelectorAll(".livevar")) b.onclick = () => { live.vars = live.vars.includes(b.dataset.v) ? live.vars.filter((x) => x !== b.dataset.v) : [...live.vars, b.dataset.v]; store.set("casts.live.vars", live.vars); drawLive(host); };
    for (const b of st.querySelectorAll("[data-w]")) b.onclick = () => { live.which = b.dataset.w; drawLive(host); };
    liveCfgForm($("#casttable"), d);
    if (!cast || !cast.t.length) {
      body.innerHTML = `<div class="empty">${d.tcp_state === "connected" && !announced ? `Seasave at ${esc(d.tcp)} accepts the connection but is not sending: start acquisition (and check Configure Outputs › TCP/IP Out).` : d.no_pressure ? "Seasave's TCP/IP output carries no pressure or package depth (its \"Depth, NMEA\" is the echosounder's bottom depth). In Seasave: Configure Outputs › TCP/IP Out › Select Variables, add Pressure [db] or Depth [salt water, m]." : d.packets ? "Scans arrive but no cast is in the water yet — the plot starts when the package passes 2 m." : d.tcp_state === "connected" ? "Connected to Seasave; the plot begins when acquisition starts and the package goes in." : d.tcp ? `Seasave at ${esc(d.tcp)} is not answering (${esc(d.tcp_state)}); retrying.` : "No Seasave source set — use ⚙."}</div>`;
      return;
    }
    const P = cast.cols[cast.pressure_col || d.pressure_col] || [], lat = UW.M.latest?.lat ?? 70;
    const depth = cast.depth_like ? P : P.map((p) => p == null ? null : depthFrom(p, lat));
    let imax = 0; for (let i = 0; i < P.length; i++) if (P[i] != null && P[i] > (P[imax] ?? -1)) imax = i;
    const li = depth.length - 1;
    if (casts.mode === "profiles") {
      // Multi: the live cast as one graph per variable, like archived casts
      const vars = Object.fromEntries(Object.entries(cast.cols).filter(([k]) => !LIVE_SKIP.has(k.toLowerCase())));
      const pseudo = { id: "live", kind: "LIVE", cast: "live", station: "", time: cast.started ? new Date(cast.started * 1000).toISOString() : "",
        units: Object.fromEntries(Object.keys(vars).map((k) => [k, ""])), vars, depth, lat };
      renderProfiles(body, [pseudo]);
      return;
    }
    drawOverlay(body, "live-plot", "Live cast", { depth, vars: cast.cols, units: {}, splitAt: imax, nowDepth: depth[li],
      sub: `${depth[li] != null ? depth[li].toFixed(1) + " m now" : ""}${cast.started ? " · started " + fmtTs(cast.started * 1000).slice(11) : ""}` }, live.vars);
    wireCastPanels(host, () => drawLive(host));
  }

  // ---- single: one selected cast in the same overlay
  async function renderSingle(host, data) {
    const pick = data.find((d) => d.id === single.id) || data[data.length - 1];
    if (!pick) { host.innerHTML = '<div class="empty">Select a cast from the list or the map.</div>'; return; }
    single.id = pick.id;
    const profs = profilesOf(pick);                        // a tow's selected dips, or the one profile
    const prof = profs.find((p) => p.index === single.dip) || profs[0];
    const vars = orderVars(Object.keys(prof.vars));
    host.innerHTML = `<div class="livebar"><div class="livevars">${varChips(vars, single.vars, "singlevar")}</div>
      ${data.length > 1 ? `<div class="livevars"><span class="muted">cast:</span> ${data.map((d) => `<button type="button" class="chip ${d.id === pick.id ? "on" : ""}" data-id="${esc(d.id)}">${esc(castLabel(d))}</button>`).join("")}</div>` : ""}
      ${profs.length > 1 ? `<div class="livevars"><span class="muted">dip:</span> ${profs.map((p) => `<button type="button" class="chip ${p === prof ? "on" : ""}" data-dip="${p.index}" title="${esc(p.time || "")}">#${p.index + 1}</button>`).join("")}</div>` : ""}</div><div id="singlebody"></div>`;
    for (const b of host.querySelectorAll(".singlevar")) b.onclick = () => { single.vars = single.vars.includes(b.dataset.v) ? single.vars.filter((x) => x !== b.dataset.v) : [...single.vars, b.dataset.v]; store.set("casts.single.vars", single.vars); renderSingle(host, data); };
    for (const b of host.querySelectorAll("[data-id]")) b.onclick = () => { single.id = b.dataset.id; single.dip = null; renderSingle(host, data); };
    for (const b of host.querySelectorAll("[data-dip]")) b.onclick = () => { single.dip = +b.dataset.dip; renderSingle(host, data); };
    const when = prof.time ? String(prof.time).replace("T", " ").slice(0, 16) : castDate(pick);
    drawOverlay(host.querySelector("#singlebody"), "single-plot", profs.length > 1 ? `${castLabel(pick)} · dip #${prof.index + 1}` : castLabel(pick),
      { depth: depths(prof), vars: Object.fromEntries(Object.keys(prof.vars).map((v) => [v, drawn(prof, v)])), units: pick.units || {}, splitAt: null, nowDepth: null, bottles: prof.bottles || pick.bottles, lat: prof.lat ?? pick.lat,
        sub: `${when}${(prof.bottom_m || pick.bottom_m) ? ` · bottom ${Math.round(prof.bottom_m || pick.bottom_m)} m` : ""}` }, single.vars);
    wireCastPanels(host, () => renderSingle(host, data));
  }

  let plotSeq = 0;
  async function renderCastPlots() {
    const seq = ++plotSeq, stamp = UW.M.generated_utc;
    fillCastVars();
    if (casts.kind !== "live") clearTimeout(live.timer);
    const host = $("#castplots");
    const sel = orderedSelection();
    // the live cast draws as Single or Multi; a section needs casts from the archive
    $("#castmode button[data-m=section]").disabled = casts.kind === "live";
    if (casts.kind === "live" && casts.mode === "section") { casts.mode = "single"; store.set("casts.mode", casts.mode); for (const x of $("#castmode").querySelectorAll("button")) x.classList.toggle("on", x.dataset.m === casts.mode); }
    $("#castrow2").hidden = casts.mode !== "section";           // the section's own x axis and the variable it colours by
    if (casts.kind === "live") { $("#castmeta").textContent = ""; return renderLive(host); }
    if (!sel.length) { host.innerHTML = '<div class="empty">Select casts from the list, or click stations and tow tracks on the map.</div>'; $("#castmeta").textContent = ""; return; }
    let data;
    try {
      data = (await Promise.all(sel.map((c) => castData(c.id)))).filter(Boolean);
    } catch {
      if (seq === plotSeq) { UW.setLoadError("Casts", true); refreshedTab = null; }
      return false;
    }
    if (seq !== plotSeq || stamp !== UW.M.generated_utc) return false;
    UW.setLoadError("Casts", false);
    const dips = data.reduce((n, d) => n + profilesOf(d).length, 0);
    $("#castmeta").textContent = `${data.length} selected · ${dips} profile${dips === 1 ? "" : "s"}`;
    if (casts.mode === "profiles") renderProfiles(host, data); else if (casts.mode === "single") renderSingle(host, data); else renderSection(host, data);
  }

  // colour for dip i of n within a tow: a light-to-dark ramp of the tow's hue
  function towShade(base, i, n) {
    const t = n > 1 ? i / (n - 1) : 0;
    const [r, g, b] = base.match(/\w\w/g).map((h) => parseInt(h, 16));
    const k = 1 - 0.55 * t;
    return `rgb(${Math.round(r * k)},${Math.round(g * k)},${Math.round(b * k)})`;
  }

  // UNESCO (1983) pressure -> depth, metres; latitude in degrees
  function depthFrom(p, lat) {
    const x = Math.sin((lat ?? 70) * Math.PI / 180) ** 2;
    const g = 9.780318 * (1 + (5.2788e-3 + 2.36e-5 * x) * x) + 1.092e-6 * p;
    return (((-1.82e-15 * p + 2.279e-10) * p - 2.2512e-5) * p + 9.72659) * p / g;
  }
  const depths = (prof) => prof.depth || prof.p.map((p) => depthFrom(p, prof.lat ?? prof.parent?.lat));   // a live cast carries depth already
  // Depth axes are linear or compressed (square root of depth, so the upper
  // water column gets room); the switch is shared by every cast view. yT maps
  // a depth onto the axis, depthAxis labels it in metres.
  casts.dscale = store.get("casts.dscale", "linear");
  const yT = (d) => d == null ? null : casts.dscale === "sqrt" ? Math.sqrt(Math.max(0, d)) : d;
  const DEPTH_TICKS = [0, 5, 10, 20, 30, 50, 75, 100, 150, 200, 300, 400, 500, 750, 1000, 1500, 2000, 3000, 4000, 5000];
  function depthAxis(maxD, extra = {}) {
    const ax = { ...THEME.yaxis, title: { text: casts.dscale === "sqrt" ? "depth (m, compressed)" : "depth (m)", font: { size: fz(12) }, standoff: 2 }, tickfont: { size: fz(12) },
      autorange: false, range: [yT(maxD), 0], ...extra };
    if (casts.dscale === "sqrt") { const t = DEPTH_TICKS.filter((d) => d <= maxD); ax.tickvals = t.map(yT); ax.ticktext = t.map(String); }
    return ax;
  }
  const maxDepth = (c) => c.max_p != null ? `${Math.round(depthFrom(c.max_p, c.lat))} m` : "";
  // Temperature, salinity and density lead; the rest in a stable order
  const VAR_ORDER = ["Temperature", "Salinity", "Sigma-t", "Oxygen", "Oxygen saturation", "Fluorescence", "CDOM", "PAR", "Transmission", "Buoyancy frequency", "Sound velocity"];
  const orderVars = (vs) => [...vs].sort((a, b) => (VAR_ORDER.indexOf(a) + 1 || 99) - (VAR_ORDER.indexOf(b) + 1 || 99) || a.localeCompare(b));
  // the profile panels can be reordered by drag, widened, and minimised to a
  // chip row, like the underway panels; remembered on the device
  const castPanelState = { wide: new Set(store.get("casts.wide", [])), min: new Set(store.get("casts.min", [])), order: store.get("casts.panelorder", []), focus: store.get("casts.focus", "Temperature") };
  const saveCastPanels = () => { store.set("casts.wide", [...castPanelState.wide]); store.set("casts.min", [...castPanelState.min]); store.set("casts.panelorder", castPanelState.order); };
  const castOrder = (vars) => { const o = castPanelState.order.filter((v) => vars.includes(v)); return [...o, ...vars.filter((v) => !o.includes(v))]; };

  // same frame and controls as the underway panels
  // toolbar order everywhere: reset, compress (depth graphs only), minimise, maximise
  // A cast panel is "on" (blue frame) when it is the one whose axes answer
  // shift/ctrl + scroll; Single and Section have one panel, always on; in
  // Multi a click on a title picks the panel, Temperature to begin with.
  function castPanelHtml(id, title, unit, wideable = true, movable = false, depth = true, on = true) {
    return `<section class="panel card castplot ${castPanelState.wide.has(id) ? "wide" : ""} ${on ? "on" : ""}" data-cp="${esc(id)}" data-var="${esc(title)}" ${movable ? 'draggable="true"' : ""}>
      <div class="head">${movable ? '<span class="handle" title="drag onto another graph to swap places">⋮⋮</span>' : ""}<h3 ${movable ? 'title="click to select this graph: the selected one zooms with shift + scroll (x) and ctrl + scroll (depth); drag to pan any of them"' : ""}>${esc(title)}</h3><div class="tools"><span class="now">${esc(unit)}</span>
        <button class="reset" title="reset zoom">⟲</button>${depth ? `<button class="dscale ${casts.dscale === "sqrt" ? "on" : ""}" title="compress the depth axis (square root) — applies to every cast graph">⇅</button>` : ""}${movable ? '<button class="min" title="minimise to the bottom bar">—</button>' : ""}${wideable ? '<button class="wide" title="expand">⤢</button>' : ""}</div></div>
      <div class="plot" id="${esc(id)}"></div></section>`;
  }
  function wireCastPanels(host, rerender, vars = []) {
    for (const sec of host.querySelectorAll(".castplot")) {
      const id = sec.dataset.cp, v = sec.dataset.var;
      const rs = sec.querySelector(".reset");
      if (rs) rs.onclick = () => Plotly.relayout(sec.querySelector(".plot"), { "xaxis.autorange": true, "yaxis.autorange": true });
      if (sec.querySelector(".dscale")) sec.querySelector(".dscale").onclick = () => { casts.dscale = casts.dscale === "sqrt" ? "linear" : "sqrt"; store.set("casts.dscale", casts.dscale); renderCastPlots(); };
      sec.querySelector(".wide")?.addEventListener("click", () => {
        castPanelState.wide.has(id) ? castPanelState.wide.delete(id) : castPanelState.wide.add(id);
        saveCastPanels(); rerender();
      });
      sec.querySelector(".min")?.addEventListener("click", () => { castPanelState.min.add(v); saveCastPanels(); rerender(); });
      if (sec.draggable) {
        sec.addEventListener("dragstart", (e) => { e.dataTransfer.setData("text/plain", v); sec.classList.add("dragging"); });
        sec.addEventListener("dragend", () => sec.classList.remove("dragging"));
        sec.addEventListener("dragover", (e) => { e.preventDefault(); sec.classList.add("over"); });
        sec.addEventListener("dragleave", () => sec.classList.remove("over"));
        sec.addEventListener("drop", (e) => {
          e.preventDefault(); sec.classList.remove("over");
          const from = e.dataTransfer.getData("text/plain");
          if (!from || from === v) return;
          // the dragged graph and the one it lands on trade places, wherever
          // in the card it was dropped
          const order = castOrder(vars);
          const i = order.indexOf(from), j = order.indexOf(v);
          if (i < 0 || j < 0) return;
          [order[i], order[j]] = [order[j], order[i]];
          castPanelState.order = order; saveCastPanels(); rerender();
        });
        if (v !== LEGEND) sec.querySelector("h3").onclick = () => {
          castPanelState.focus = castPanelState.focus === v ? null : v;         // the selected title clicked again: none selected
          store.set("casts.focus", castPanelState.focus);
          for (const x of host.querySelectorAll(".castplot")) x.classList.toggle("on", !!castPanelState.focus && x.dataset.var === castPanelState.focus);
        };
      }
    }
    for (const chip of host.querySelectorAll(".dock .chip")) chip.onclick = () => { castPanelState.min.delete(chip.dataset.var); saveCastPanels(); rerender(); };
  }
  const castLayout = () => ({ ...THEME, margin: { l: fz(52), r: 8, t: fz(6), b: fz(36) }, showlegend: false, dragmode: "pan" });   // read at draw time: the theme may have changed

  function renderProfiles(host, data) {
    // the legend is a panel like the graphs (movable, minimisable), named LEGEND in the order
    const all = castOrder([...orderVars(new Set(data.flatMap((d) => Object.keys(d.units)))), LEGEND]);
    const vars = all.filter((v) => !castPanelState.min.has(v));
    const minimised = all.filter((v) => castPanelState.min.has(v));
    if (castPanelState.focus && !vars.includes(castPanelState.focus)) castPanelState.focus = null;
    const legendHtml = () => castPanelHtml("cp-legend", LEGEND, `${data.length} cast${data.length === 1 ? "" : "s"}`, true, true, false, false)
      .replace('class="panel card castplot', 'class="panel card castplot legendpanel').replace(/<button class="reset"[^>]*>⟲<\/button>/, "")
      .replace('<div class="plot" id="cp-legend"></div>', `<div class="legendbody">${data.map((d, i) => `<span><i style="background:${pal(i)}"></i>${esc(castLabel(d))}<small>${esc(castDate(d))}</small></span>`).join("")}</div>`);
    host.innerHTML = vars.map((v) => v === LEGEND ? legendHtml() : castPanelHtml(`cp-${v.replace(/\W+/g, "_")}`, v, data.find((d) => d.units[v])?.units[v] || "", true, true, true, v === castPanelState.focus)).join("") +
      (minimised.length ? `<div class="dock castdock">${minimised.map((v) => `<button class="chip" data-var="${esc(v)}" title="restore">${esc(v)} <span>▲</span></button>`).join("")}</div>` : "");
    for (const v of vars) {
      if (v === LEGEND) continue;
      const traces = [];
      data.forEach((d, i) => {
        const ps = profilesOf(d);
        ps.forEach((p, j) => {
          if (!p.vars[v]) return;
          const colour = ps.length > 1 ? towShade(pal(i), j, ps.length) : pal(i);
          traces.push({
            type: "scatter", mode: "lines", name: p.label, x: drawn(p, v), y: depths(p).map(yT), customdata: depths(p), connectgaps: false,
            line: { width: ps.length > 1 ? 1 : 1.6, color: colour },
            opacity: ps.length > 1 ? 0.8 : 1,
            hovertemplate: `${esc(p.label)}<br>%{x:.3~f} ${esc(d.units[v] || "")} at %{customdata:.0f} m<extra></extra>`,
          });
          const bts = casts.bottles ? (p.bottles || []).filter((b) => b.p != null || b.depth_m != null) : [];
          if (bts.length) traces.push({
            type: "scatter", mode: "markers", name: `${p.label} bottles`, showlegend: false,
            x: bts.map((b) => valueAt(p, v, b.p ?? b.depth_m)), y: bts.map((b) => yT(bottleDepth(b, p.lat ?? d.lat))),
            text: bts.map((b) => `${esc(p.label)}<br>${bottleText(b)} · ${Math.round(bottleDepth(b, p.lat ?? d.lat))} m`), hoverinfo: "text",
            marker: { size: 7, color: colour, line: { color: C.marker, width: 1 } },
          });
        });
      });
      const layout = { ...castLayout(), hovermode: "closest",
        xaxis: { ...THEME.xaxis, title: { text: data.find((d) => d.units[v])?.units[v] || "", font: { size: fz(12) }, standoff: 4 }, tickfont: { size: fz(12) } },
        yaxis: depthAxis(Math.max(1, ...data.flatMap((d) => profilesOf(d).flatMap((p) => p.vars[v] ? depths(p) : []))) * 1.02) };
      Plotly.react(host.querySelector(`#cp-${v.replace(/\W+/g, "_")}`), traces, layout, CFG).then((gd) => { UW.axisZoom(gd); syncDepthAxes(host, gd); });
    }
    wireCastPanels(host, () => renderProfiles(host, data), all);
  }
  const LEGEND = "Legend";
  // the Multi graphs share their depth axis: a zoom or pan of one (or its
  // reset) is applied to the others; the flag keeps the echoes from looping
  let depthSyncing = false;
  function syncDepthAxes(host, gd) {
    gd.removeAllListeners?.("plotly_relayout");
    gd.on("plotly_relayout", (ev) => {
      if (depthSyncing) return;
      let upd = null;
      if (ev["yaxis.autorange"]) upd = { "yaxis.autorange": true };
      else if (ev["yaxis.range[0]"] != null) upd = { "yaxis.range": [ev["yaxis.range[0]"], ev["yaxis.range[1]"]] };
      else if (Array.isArray(ev["yaxis.range"])) upd = { "yaxis.range": ev["yaxis.range"] };
      if (!upd) return;
      depthSyncing = true;
      const others = [...host.querySelectorAll(".castplot:not(.legendpanel) .plot")].filter((p) => p !== gd && p.data);
      Promise.all(others.map((p) => Plotly.relayout(p, upd).catch(() => {}))).finally(() => { depthSyncing = false; });
    });
  }

  // interpolate a cast's variable onto a common pressure grid
  function onGrid(d, v, grid) {
    const p = d.p, x = d.vars[v]; const out = new Array(grid.length).fill(null);
    if (!x) return out;
    let j = 0;
    for (let i = 0; i < grid.length; i++) {
      const g = grid[i];
      while (j < p.length - 1 && p[j + 1] < g) j++;
      if (g < p[0] || g > p[p.length - 1]) continue;
      const a = p[j], b = p[j + 1] ?? p[j], xa = x[j], xb = x[j + 1] ?? x[j];
      if (xa == null || xb == null) continue;
      out[i] = b === a ? xa : xa + (xb - xa) * (g - a) / (b - a);
    }
    return out;
  }
  const haversine = (a, b) => { const R = 6371, r = Math.PI / 180, dl = (b.lat - a.lat) * r, dn = (b.lon - a.lon) * r;
    const h = Math.sin(dl / 2) ** 2 + Math.cos(a.lat * r) * Math.cos(b.lat * r) * Math.sin(dn / 2) ** 2; return 2 * R * Math.asin(Math.sqrt(h)); };

  // a profile's id for the custom order: the cast's, plus the dip for a tow
  const profileId = (d) => `${d.parent?.id || d.id}${d.index != null ? `#${d.index}` : ""}`;
  // the custom order: the remembered ids first (those still selected), then
  // anything new in time order
  const customOrder = (profiles) => {
    const byId = new Map(profiles.map((d) => [profileId(d), d]));
    const out = casts.order.filter((id) => byId.has(id)).map((id) => byId.get(id));
    const seen = new Set(out.map(profileId));
    return [...out, ...profiles.filter((d) => !seen.has(profileId(d)))];
  };
  const saveOrder = (profiles) => { casts.order = profiles.map(profileId); store.set("casts.order", casts.order); };
  // Smoothing goes by the data, not by name: a profile's variable is rough
  // when what a 7-sample running mean removes has an RMS above ROUGH of the
  // variable's 2–98 % range. Temperature, salinity and oxygen sit near 0.01,
  // the optical and chemical sensors at 0.03 and up.
  const ROUGH = 0.02;
  const roughness = (x) => {
    const sm = runningMean(x, 7); let n = 0, ss = 0; const vals = [];
    for (let i = 0; i < x.length; i++) if (x[i] != null && sm[i] != null) { ss += (x[i] - sm[i]) ** 2; n++; vals.push(x[i]); }
    if (n < 30) return 0;
    vals.sort((a, b) => a - b); const rng = vals[Math.floor(vals.length * .98)] - vals[Math.floor(vals.length * .02)];
    return rng > 0 ? Math.sqrt(ss / n) / rng : 0;
  };
  const roughCache = new WeakMap();
  const isRough = (prof, v) => { let m = roughCache.get(prof); if (!m) { m = new Map(); roughCache.set(prof, m); } if (!m.has(v)) m.set(v, prof.vars[v] ? roughness(prof.vars[v]) > ROUGH : false); return m.get(v); };
  // the smoothing window (samples ≈ dbar) for a profile reaching maxD
  const smoothWindow = (maxD) => maxD > 1500 ? 21 : maxD > 400 ? 11 : 7;
  // a profile's variable as drawn: smoothed when it is rough and Smooth is on, else as stored
  const drawn = (prof, v) => (casts.smooth && prof.vars[v] && isRough(prof, v)) ? runningMean(prof.vars[v], smoothWindow(prof.p?.length ? prof.p[prof.p.length - 1] : 0)) : prof.vars[v];
  // a centred running mean over w samples, nulls left out of the average and
  // kept as gaps where the window holds nothing
  function runningMean(x, w) {
    if (!x) return x;
    const h = Math.floor(w / 2), out = new Array(x.length).fill(null);
    for (let i = 0; i < x.length; i++) {
      let sum = 0, n = 0;
      for (let j = Math.max(0, i - h); j <= Math.min(x.length - 1, i + h); j++) if (x[j] != null) { sum += x[j]; n++; }
      out[i] = n ? sum / n : null;
    }
    return out;
  }
  function renderSection(host, data) {
    const v = casts.variable;
    // tows contribute every dip; everything is ordered by time, or in
    // custom mode as the user has arranged the legend
    const custom = casts.xmode === "custom", az = casts.xmode === "az";
    let withVar = data.flatMap(profilesOf).filter((d) => d.vars[v]).sort((a, b) => (a.time || "").localeCompare(b.time || ""));
    if (custom) withVar = customOrder(withVar);
    // A–Z: by station id, numbers in order (CardS-2 before CardS-10), then time
    const stationOf = (d) => String(d.station ?? d.parent?.station ?? d.label ?? "");
    if (az) withVar = [...withVar].sort((a, b) => stationOf(a).localeCompare(stationOf(b), undefined, { numeric: true, sensitivity: "base" }) || (a.time || "").localeCompare(b.time || ""));
    if (withVar.length < 2) { host.innerHTML = `<div class="empty">A section needs at least two profiles with ${esc(v)} — ${withVar.length} selected.</div>`; return; }
    // depth grid (metres) shared by every profile
    const maxD = Math.max(...withVar.map((d) => depthFrom(d.p[d.p.length - 1], d.lat ?? d.parent?.lat)));
    const step = maxD > 1500 ? 5 : maxD > 400 ? 2 : 1;
    const grid = []; for (let d = 0; d <= maxD; d += step) grid.push(d);
    // the jittery optical and chemical sensors are smoothed down the profile
    // (a centred running mean, a window that grows with the depth range)
    // before gridding, since a section is about the big picture
    // a section smooths a variable when it is rough in most of its profiles
    const smoothW = casts.smooth && withVar.filter((d) => isRough(d, v)).length * 2 > withVar.length ? smoothWindow(maxD) : 0;
    const onDepthGrid = (prof) => onGrid({ p: depths(prof), vars: smoothW ? { [v]: runningMean(prof.vars[v], smoothW) } : prof.vars }, v, grid);
    // x follows the header's Time/Distance switch: distance is cumulative
    // along the profiles in time order, time is each profile's own
    const byTime = casts.xmode === "time";                       // the section's axis, apart from the underway one
    const km = [0];
    for (let i = 1; i < withVar.length; i++) {
      const a = withVar[i - 1], b = withVar[i];
      km.push(km[i - 1] + (a.lat != null && b.lat != null ? haversine(a, b) : 1));
    }
    const tms = withVar.map((d, i) => d.time ? Date.parse(d.time + (d.time.endsWith("Z") ? "" : "Z")) : i);
    const xs = byTime ? tms.map((t) => UW.shipAxis(t)) : km;
    const xTitle = byTime ? `ship time (${UW.tzAbbr()})` : custom ? "distance along the custom order (km)" : az ? "distance along the stations A–Z (km)" : "distance along section (km)";
    const xFmt = (i) => byTime ? fmtTs(tms[i]) : `${km[i].toFixed(0)} km`;
    const unit = withVar[0].units[v] || "";
    // Resample onto a regular x grid so the section interpolates between
    // profiles in both modes (a heatmap on irregular x only smooths in pixels).
    const cols = withVar.map(onDepthGrid);
    const NX = 240;
    const x0 = Math.min(...xs), x1 = Math.max(...xs), span = x1 - x0 || 1;
    const xg = Array.from({ length: NX }, (_, i) => x0 + span * i / (NX - 1));
    const order = xs.map((_, i) => i).sort((a, b) => xs[a] - xs[b]);
    const z = grid.map((_, gi) => xg.map((xv) => {
      let k = 0; while (k < order.length - 1 && xs[order[k + 1]] < xv) k++;
      const a = order[k], b = order[Math.min(k + 1, order.length - 1)];
      const za = cols[a][gi], zb = cols[b][gi];
      if (a === b || xs[b] === xs[a]) return za;
      const t = (xv - xs[a]) / (xs[b] - xs[a]);
      if (za == null || zb == null) return t < 0.5 ? za : zb;      // no bridging into a gap
      return za + (zb - za) * t;
    }));
    const xPlot = byTime ? xg.map((t) => new Date(t)) : xg;
    const xPts = byTime ? xs.map((t) => new Date(t)) : km;          // the same ship-time shift as the heatmap
    const dense = withVar.length > 24;      // a tow: label only every few dips
    // the legend: a column of chips to the left of the plot, always
    // movable (drag, or ▲ ▼); moving one switches the axis to custom and
    // keeps that order, and a link restores time order
    const entry = (d, i) => `<span class="chip reorder" draggable="true" data-i="${i}" title="drag, or ▲ ▼, to lay the profiles in your own order"><b>${i + 1}</b><span class="lbl">${esc(d.label)}<small>${esc(xFmt(i))}${byTime ? ` · ${km[i].toFixed(0)} km` : ""}</small></span><span class="nudges"><button type="button" class="nudge" data-d="-1" title="move up" ${i === 0 ? "disabled" : ""}>▲</button><button type="button" class="nudge" data-d="1" title="move down" ${i === withVar.length - 1 ? "disabled" : ""}>▼</button></span></span>`;
    host.innerHTML = `<div class="sectionwrap"><div class="castlegend vertical">${withVar.map(entry).join("")}${custom ? '<a href="#" class="timeorder">↺ time order</a>' : ""}</div>` +
      castPanelHtml("cs-plot", `${v} section`, `${withVar.length} profiles · ${km.at(-1).toFixed(0)} km · ${unit}${smoothW ? ` · smoothed over ${smoothW} m` : ""}`, false, false, true, false).replace('class="panel card castplot', 'class="panel card castplot solo wide') + "</div>";
    const move = (from, to) => {
      const arr = [...withVar]; const [x] = arr.splice(from, 1); arr.splice(to, 0, x); saveOrder(arr);
      if (casts.xmode !== "custom") { casts.xmode = "custom"; store.set("casts.xmode", "custom"); $("#castxmode .xcycle").textContent = "Custom"; }
      renderSection(host, data);
    };
    for (const b of host.querySelectorAll(".castlegend .nudge")) b.onclick = (ev) => { ev.preventDefault(); ev.stopPropagation(); const i = +b.closest(".reorder").dataset.i; move(i, i + (+b.dataset.d)); };
    for (const el of host.querySelectorAll(".castlegend .reorder")) {
      el.ondragstart = (ev) => { ev.dataTransfer.setData("text/plain", el.dataset.i); ev.dataTransfer.effectAllowed = "move"; };
      el.ondragover = (ev) => { ev.preventDefault(); el.classList.add("over"); };
      el.ondragleave = () => el.classList.remove("over");
      el.ondrop = (ev) => { ev.preventDefault(); el.classList.remove("over"); const from = +ev.dataTransfer.getData("text/plain"), to = +el.dataset.i; if (!isNaN(from) && from !== to) move(from, to); };
    }
    const to = host.querySelector(".castlegend .timeorder");
    if (to) to.onclick = (ev) => { ev.preventDefault(); saveOrder([]); casts.xmode = "time"; store.set("casts.xmode", "time"); $("#castxmode .xcycle").textContent = "Time"; renderCastPlots(); };
    const traces = [
      { type: "heatmap", x: xPlot, y: grid.map(yT), z, customdata: grid.map((g) => xg.map(() => g)), colorscale: "Viridis", connectgaps: false, zsmooth: "best",
        colorbar: { title: { text: unit, side: "right" }, thickness: 12, len: .8, tickfont: { size: fz(12) }, outlinewidth: 0 },
        hovertemplate: (byTime ? "%{x|%m-%d %H:%M}" : "%{x:.1f} km") + ` · %{customdata:.0f} m<br><b>%{z:.3~f} ${esc(unit)}</b><extra></extra>` },
      { type: "scatter", mode: dense ? "markers" : "markers+text", x: xPts, y: withVar.map(() => 0), text: withVar.map((_, i) => String(i + 1)), textposition: "top center",
        textfont: { size: fz(11), color: THEME.font.color }, marker: { symbol: "triangle-down", size: dense ? 5 : 9, color: C.accent2 },
        hovertext: withVar.map((d, i) => `${d.label}<br>${d.time ? fmtTs(tms[i]) + " " + UW.tzAbbr() : ""}`), hoverinfo: "text", cliponaxis: false },
    ];
    // echo-sounder bottom where there is one, else the deepest sample; the
    // fill is clipped to the frame so a bottom far below the casts stays out of it
    // seabed: the echo-sounder bottom logged with each profile (a marker), or
    // the deepest sample where none was logged; straight segments between
    // profiles, clipped to the frame
    const sounded = withVar.map((d) => d.bottom_m > 0);
    const bottoms = withVar.map((d, i) => Math.min(maxD + step, sounded[i] ? d.bottom_m : depthFrom(d.p[d.p.length - 1], d.lat ?? d.parent?.lat)));
    traces.push({ type: "scatter", mode: "lines", x: xPts, y: bottoms.map(() => yT(maxD + step)), line: { width: 0 }, hoverinfo: "skip", showlegend: false });
    traces.push({ type: "scatter", mode: "lines+markers", x: xPts, y: bottoms.map(yT), name: "bottom",
      line: { color: C.floorLine, width: 1.5, shape: "linear" }, fill: "tonexty", fillcolor: C.floor,
      marker: { size: sounded.map((b) => b ? 5 : 0), color: C.muted, symbol: "diamond" },
      hovertext: withVar.map((d, i) => sounded[i] ? `${d.label}<br>bottom ${Math.round(d.bottom_m)} m` : `${d.label}<br>deepest sample ${Math.round(bottoms[i])} m`), hoverinfo: "text" });
    // the bottle firings, after the bottom: its fill runs to the trace before it
    if (casts.bottles) {
      const bx = [], by = [], bt = [];
      withVar.forEach((d, i) => { for (const b of d.bottles || []) { const dep = bottleDepth(b, d.lat ?? d.parent?.lat); if (dep == null) continue; bx.push(xPts[i]); by.push(yT(dep)); bt.push(`${d.label}<br>${bottleText(b)} · ${Math.round(dep)} m`); } });
      if (bx.length) traces.push({ type: "scatter", mode: "markers", name: "bottles", x: bx, y: by, text: bt, hoverinfo: "text", marker: { size: 6, color: C.marker, line: { color: C.markerLine, width: 1 } } });
    }
    const layout = { ...castLayout(), margin: { l: fz(54), r: 8, t: fz(18), b: fz(40) },
      xaxis: { ...THEME.xaxis, title: { text: xTitle, font: { size: fz(12) }, standoff: 4 }, tickfont: { size: fz(12) }, type: byTime ? "date" : "linear" },
      yaxis: depthAxis(maxD + step) };
    Plotly.react($("#cs-plot"), traces, layout, CFG).then((gd) => UW.axisZoom(gd));
    wireCastPanels(host, () => renderSection(host, data));
  }

  function wireCasts() {
    for (const b of $("#castmode").querySelectorAll("button")) b.onclick = () => {
      casts.mode = b.dataset.m; store.set("casts.mode", casts.mode);
      for (const x of $("#castmode").querySelectorAll("button")) x.classList.toggle("on", x === b);
      renderCastPlots(); UW.renderMap();
    };
    for (const b of $("#castmode").querySelectorAll("button")) b.classList.toggle("on", b.dataset.m === casts.mode);
    $("#castvar").onchange = (e) => { casts.variable = e.target.value; store.set("casts.var", casts.variable); renderCastPlots(); };
    const sx = $("#castxmode .xcycle"), XMODES = ["time", "distance", "az", "custom"], XWORD = { time: "Time", distance: "Distance", az: "A–Z", custom: "Custom" };
    if (!XMODES.includes(casts.xmode)) casts.xmode = "time";
    sx.textContent = XWORD[casts.xmode];
    sx.onclick = () => { casts.xmode = XMODES[(XMODES.indexOf(casts.xmode) + 1) % XMODES.length]; store.set("casts.xmode", casts.xmode); sx.textContent = XWORD[casts.xmode]; renderCastPlots(); };
    for (const b of $("#castkind").querySelectorAll("button")) {
      b.classList.toggle("on", b.dataset.k === casts.kind);
      // the live view (and the poll that feeds its setup box) is started by the
      // plots renderer, so a change to or from Live redraws the plots too
      b.onclick = () => { const was = casts.kind; casts.kind = b.dataset.k; store.set("casts.kind", casts.kind); for (const x of $("#castkind").querySelectorAll("button")) x.classList.toggle("on", x === b); renderCastList(); if (casts.kind === "live" || was === "live") renderCastPlots(); UW.renderMap(); };
    }
    $("#castsearch").oninput = debounce((e) => { casts.search = e.target.value; renderCastList(); }, 150);
    $("#castclear").onclick = () => { casts.sel.clear(); store.set("casts.sel", []); renderCastList(); renderCastPlots(); UW.renderMap(); };
    $("#castcsv").onclick = downloadCastsTSV;
    const sm = $("#castsmooth");
    sm.classList.toggle("on", casts.smooth);
    sm.onclick = () => { casts.smooth = !casts.smooth; store.set("casts.smooth", casts.smooth); sm.classList.toggle("on", casts.smooth); renderCastPlots(); };
    const bb = $("#castbottles");
    bb.classList.toggle("on", casts.bottles);
    bb.onclick = () => { casts.bottles = !casts.bottles; store.set("casts.bottles", casts.bottles); bb.classList.toggle("on", casts.bottles); renderCastPlots(); };
  }

  // ================================================================ calendar
  // The ship stays on Quebec time, so every time a person reads on this tab
  // is ship time (SITE.local_tz); the instants underneath stay UTC.
  const LTZ = UW.SITE.local_tz;
  const _lparts = new Intl.DateTimeFormat("en-CA", { timeZone: LTZ, year: "numeric", month: "2-digit", day: "2-digit", hour: "2-digit", minute: "2-digit", hourCycle: "h23" });
  const localParts = (t) => { const o = {}; for (const p of _lparts.formatToParts(new Date(t))) o[p.type] = p.value; return o; };
  const dayL = (t) => { const p = localParts(t); return `${p.year}-${p.month}-${p.day}`; };            // YYYY-MM-DD, ship time
  const hmL = (t) => { const p = localParts(t); return `${p.hour}:${p.minute}`; };                       // HH:MM, ship time
  const tzAbbr = (t = Date.now()) => (new Intl.DateTimeFormat("en-US", { timeZone: LTZ, timeZoneName: "short" }).formatToParts(new Date(t)).find((p) => p.type === "timeZoneName") || {}).value || LTZ;
  const offsetMs = (t) => { const p = localParts(t); return Date.UTC(+p.year, +p.month - 1, +p.day, +p.hour, +p.minute) - Math.floor(t / 60000) * 60000; };  // ship time minus UTC
  const localMidnight = (key) => { const g = Date.parse(key + "T00:00:00Z"); return g - offsetMs(g); }; // the instant the ship day begins
  const nextDay = (key) => dayL(localMidnight(key) + 36 * 3600e3);
  const stampL = (t) => `${dayL(t)} ${hmL(t)}`;
  const cal = { data: null, loadedFor: null, view: store.get("cal.view", "agenda"), search: "", month: store.get("cal.month", dayL(Date.now()).slice(0, 7)),
    span: store.get("cal.span", "days"), day: store.get("cal.day", dayL(Date.now())), openDays: new Set(), openDone: new Set() };
  // The calendar file carries the current legs; the legs before live in an
  // archive (M.calendar.archive) fetched only when a shown leg is in it or
  // the span or the month view reaches back before it, and kept across
  // builds by its content stamp.
  const arch = { stamp: null, data: null, pending: null, failedGen: null };
  const archiveMeta = () => UW.M.calendar?.archive || null;
  function archiveWanted() {
    const a = archiveMeta(); if (!a) return false;
    const f = UW.currentFilter();
    if ((a.legs || []).some((id) => f.legs.has(id))) return true;
    const before = UW.tms(a.before);
    if (!isNaN(before) && UW.legsStart() < before) return true;
    return cal.view === "month" && !!a.before && cal.month < a.before.slice(0, 7);
  }
  function loadArchive() {
    const a = archiveMeta();
    if (arch.data && arch.stamp === a.stamp) return Promise.resolve(arch.data);
    if (arch.pending && arch.stamp === a.stamp) return arch.pending;
    arch.stamp = a.stamp; arch.data = null;
    arch.pending = getJSON(`${a.file}?v=${encodeURIComponent(a.stamp)}`)
      .then((d) => { arch.data = d; return d; })
      .catch((e) => { arch.failedGen = UW.M.generated_utc; throw e; })      // tried again with the next build
      .finally(() => { arch.pending = null; });
    return arch.pending;
  }
  // the archive's events and feed items ahead of the current file's, so the
  // rest of the tab reads one calendar
  function withArchive(current, archive) {
    if (!archive) return current;
    const feeds = (current.gcal || []).map((f) => {
      const old = (archive.gcal || []).find((g) => g.key === f.key);
      return old ? { ...f, events: [...(old.events || []), ...(f.events || [])] } : f;
    });
    const events = [...(archive.events || []), ...(current.events || [])].sort((x, y) => UW.tms(x.time_utc) - UW.tms(y.time_utc));
    return { ...current, events, gcal: feeds };
  }
  async function ensureCalendar() {
    const stamp = UW.M.generated_utc;
    const want = archiveWanted() && arch.failedGen !== stamp;
    if (cal.data && cal.loadedFor === stamp && (!want || cal.archiveStamp === archiveMeta()?.stamp)) return;
    const data = await cachedJSON("calendar", UW.M.calendar.file);
    let archive = null;
    if (want) { try { archive = await loadArchive(); } catch { /* the current legs still show */ } }
    cal.data = withArchive(data, archive); cal.loadedFor = stamp; cal.archiveStamp = archive ? archiveMeta()?.stamp : null;
  }
  function renderCalendar() {
    const host = $("#calendar"); if (!cal.data) return;
    // a month or a leg stepped back into the archive: draw what is here, then again with it
    if (archiveWanted() && cal.archiveStamp !== archiveMeta()?.stamp && arch.failedGen !== UW.M.generated_utc && !cal.archiveLoading) {
      cal.archiveLoading = true;
      ensureCalendar().then(() => renderCalendar()).catch(() => {}).finally(() => { cal.archiveLoading = false; });
    }
    const s = cal.data.schedule || {};
    const q = cal.search.toLowerCase();
    const f = UW.currentFilter();
    const evAll = cal.data.events.filter((e) => f.legs.has(e.leg)).filter((e) => !q || JSON.stringify(e).toLowerCase().includes(q));
    const evs = evAll.filter((e) => UW.inFilter(e.leg, e.time_utc, f));
    if (cal.view === "timeline") return renderTimeline(host, evs, s, evAll.length - evs.length);
    if (cal.view === "month") return renderMonth(host, q);
    const isoDay = (r) => { const t = UW.tms(r.start_utc); return isNaN(t) ? "" : dayL(t); };
    // the rows in page order under a header row per ship day; a day's
    // finished operations (completed or canceled) fold under that header
    const shown = (s.rows || []).filter((r) => !(isFinished(r) && !r.start_utc))
      .filter((r) => !q || `${r.station} ${r.operation} ${r.status} ${r.comment} ${isoDay(r)}`.toLowerCase().includes(q));
    const byDayT = new Map();
    for (const r of shown) { const d = isoDay(r) || "undated"; if (!byDayT.has(d)) byDayT.set(d, []); byDayT.get(d).push(r); }
    const sched = [...byDayT.entries()].map(([d, rs]) => {
      const nDone = rs.filter(isFinished).length, open = cal.openDone.has(d);
      return dayHead(d) + (nDone ? `<tr class="fold"><td colspan="7"><button type="button" class="dayfold-done" data-day="${esc(d)}">${open ? "▾ hide" : "▸ show"} ${nDone} finished</button></td></tr>` : "") +
        rs.map((r) => schedRow(r, rowKey(r), false).replace("<tr ", isFinished(r) && !open ? "<tr hidden " : "<tr ")).join("");
    }).join("");
    let html = `<section class="card block"><h3>Operations schedule ${esc(s.title || "")}</h3>` +
      (s.whiteboard ? `<p class="whiteboard">📋 ${esc(s.whiteboard)}</p>` : "") +
      (sched ? `<div class="hscroll"><table class="sched">${SCHED_HEAD(false)}${sched}</table></div>` : '<p class="muted">no scheduled operations listed</p>') +
      `<p class="muted small">Ship intranet: ${(UW.M.intranet || []).map((l) => `<a href="${esc(l.url)}" target="_blank" rel="noopener">${esc(l.label)}</a>`).join(" · ")}` +
      ` &nbsp;·&nbsp; calendars: ${(UW.M.links || []).map((l) => `<a href="${esc(l.url)}" target="_blank" rel="noopener">${esc(l.label)}</a>`).join(" · ")}</p>` +
      alertsHtml() + `</section>`;
    const wasOpen = host.querySelector("#alerts")?.open;
    host.innerHTML = html;
    const det = host.querySelector("#alerts"); if (det && wasOpen) det.open = true;
    wireAlerts(host); wireBells(host);
    for (const b of host.querySelectorAll(".dayfold-done")) b.onclick = () => foldDone(host, b.dataset.day, !cal.openDone.has(b.dataset.day));
  }
  const isFinished = (r) => /^(completed|cancell?ed)$/i.test((r.status || "").trim());
  function foldDone(host, d, open) {
    if (open) cal.openDone.add(d); else cal.openDone.delete(d);
    for (const tr of host.querySelectorAll(`tr.sched.done[data-day="${CSS.escape(d)}"]`)) tr.hidden = !open;
    const b = host.querySelector(`.dayfold-done[data-day="${CSS.escape(d)}"]`); if (b) b.textContent = b.textContent.replace(/^\S+ (show|hide)/, open ? "▾ hide" : "▸ show");
  }
  // ---- one table for both views. The Today table and the event log share
  // the columns time · station · operation · alerts · status · dur. · comment
  // and a coloured header row per ship day; the log adds the logged events
  // (their depth in the dur. column) and folds each day's scheduled rows.
  const SCHED_HEAD = (depth) => `<tr><th title="🔔 this operation · 📢 every operation of this kind">alerts</th><th>time</th><th title="now · next · later · done · canceled · was scheduled · logged">status</th><th>station</th><th>operation</th><th>dur.</th>${depth ? "<th>depth</th>" : ""}<th>comment</th></tr>`;
  // the status in a word: now (in progress), next (up next), later (upcoming), done, canceled, was (scheduled once), logged
  const statusWord = (r, next) => { const st = (r.status || "").trim().toLowerCase();
    return r.former ? ["was", "was scheduled"] : st === "in progress" ? ["now", "now"] : st === "completed" ? ["done", "done"] : /^cancel/.test(st) ? ["canceled", "canceled"] : st === "coming soon" ? ["soon", "soon"] : st && st !== "scheduled" ? ["later", st] : next ? ["next", "next"] : ["later", "later"]; };
  const NCOLS = (depth) => depth ? 8 : 7;
  const dayHead = (d, note, depth) => `<tr class="dayhead"><td colspan="${NCOLS(depth)}">${esc(d)}${d === dayL(Date.now()) ? " · today" : ""}${note ? ` <small>${note}</small>` : ""}</td></tr>`;
  const rowKey = (r) => `r:${r.key || `${r.station}|${r.operation}`}`;
  function schedRow(r, k, depth) {
    const nextKey = UW.M.calendar?.now?.next?.key, next = !r.former && r.key === nextKey;
    const t0 = UW.tms(r.start_utc), t1 = UW.tms(r.end_utc);
    const when = r.start ? `${esc(r.start)}–${esc(r.end || "")}` : `${isNaN(t0) ? "" : hmL(t0)}–${isNaN(t1) ? "" : hmL(t1)}`;
    const [cls, word] = statusWord(r, next);
    return `<tr class="sched ${r.former ? "former" : ""} ${isFinished(r) ? "done" : ""} ${statusClass(r.status || "upcoming")} ${next ? "next" : ""}" data-key="${esc(k)}" data-day="${esc(isNaN(t0) ? "" : dayL(t0))}">` +
      `<td class="alerts-cell">${r.former ? "" : bellHtml(r)}</td><td class="mono">${when}</td><td><span class="status s-${cls}" title="${esc(word)}">${esc(word)}</span></td>` +
      `<td>${r.station ? `<a href="#" class="stnlink" data-station="${esc(r.station)}" title="show ${esc(r.station)} on the map">${esc(r.station)}</a>` : ""}</td><td>${esc(r.operation || "")}</td><td>${r.duration_h != null ? r.duration_h.toFixed(1) + " h" : ""}</td>` +
      `${depth ? "<td></td>" : ""}<td class="muted">${esc(r.comment || "")}</td></tr>`;
  }
  // the event log: logged events and scheduled operations (current and
  // former) by ship day, newest first; every row carries the key its
  // timeline point uses (e:<index> / r:<row key>). Every source has its own
  // time format (the event log writes 2026/09/03 11:23:12, the schedule
  // ISO), so group and sort on instants.
  function eventListHtml(evs, s, q, f, nHidden = 0) {
    const hm = (t) => isNaN(t) ? "" : hmL(t);
    const byDay = new Map();
    const add = (t, x) => { const d = isNaN(t) ? "undated" : dayL(t); if (!byDay.has(d)) byDay.set(d, []); byDay.get(d).push({ t: isNaN(t) ? 0 : t, ...x }); };
    evs.forEach((e, i) => add(UW.tms(e.time_utc), { e, k: `e:${i}` }));
    for (const r of scheduledRows(s)) if ((!q || JSON.stringify(r).toLowerCase().includes(q)) && (UW.inFilter(null, r.start_utc, f) || UW.inFilter(null, r.end_utc, f) || UW.tms(r.start_utc) > f.end)) add(UW.tms(r.start_utc), { r, k: rowKey(r) });
    const days = [...byDay.keys()].sort().reverse().slice(0, 60);
    // a logged CTD event links to its cast when the logbook has one at that
    // station within a few hours (the key the Casts tab selects by)
    const castFor = (e) => {
      if (!/ctd|rosette/i.test(e.activity || "") || !e.station) return null;
      const t = UW.tms(e.time_utc), st = String(e.station).trim().toLowerCase();
      let best = null;
      for (const c of UW.M.stations || []) {
        if (c.kind === "event" || c.leg !== e.leg || String(c.station).trim().toLowerCase() !== st) continue;
        const dt = Math.abs(UW.tms(c.time) - t);
        if (dt < 6 * 3600e3 && (!best || dt < best.dt)) best = { dt, key: `${c.leg}:CTD_${String(c.cast).padStart(3, "0")}`, cast: c.cast };
      }
      return best;
    };
    const evRow = (e, k, d) => { const c = castFor(e); return `<tr class="logged" data-key="${esc(k)}" data-day="${esc(d)}" data-lat="${e.lat ?? ""}" data-lon="${e.lon ?? ""}" title="${e.lat != null ? "show on map" : ""}">` +
      `<td class="alerts-cell">${c ? `<button type="button" class="viewdata" data-cast="${esc(c.key)}" title="open cast ${esc(c.cast)} on the Casts tab">view data</button>` : ""}</td>` +
      `<td class="mono">${hm(UW.tms(e.time_utc))}</td><td><span class="status s-logged" title="logged">logged</span></td><td>${esc(e.station || "")}</td><td>${esc(e.activity || "")}${e.event ? " · " + esc(e.event) : ""}${e.label ? ` <code>${esc(e.label)}</code>` : ""}</td>` +
      `<td></td><td>${e.depth_m != null ? Math.round(+e.depth_m) + " m" : ""}</td><td class="muted">${esc(e.comment || "")}</td></tr>`; };
    // scheduled rows show; the formerly scheduled ones (off the intranet page now) fold per day
    const body = days.map((d) => {
      const items = byDay.get(d).sort((a, b) => b.t - a.t);
      const nEv = items.filter((x) => x.e).length, nSched = items.filter((x) => x.r && !x.r.former).length, nFormer = items.filter((x) => x.r?.former).length, open = cal.openDays.has(d);
      const first = items.find((x) => x.e)?.e, leg = UW.legById(first?.leg)?.label || items.find((x) => x.r)?.r.leg || "";
      return dayHead(d, `${nEv} events · ${nSched} scheduled${nFormer ? ` · ${nFormer} formerly` : ""}${leg ? " · " + esc(leg) : ""}`, true) +
        (nFormer ? `<tr class="fold"><td colspan="8"><button type="button" class="dayfold" data-day="${esc(d)}">${open ? "▾ hide" : "▸ show"} ${nFormer} formerly scheduled</button></td></tr>` : "") +
        items.map((x) => x.e ? evRow(x.e, x.k, d) : schedRow(x.r, x.k, true).replace("<tr ", open || !x.r.former ? "<tr " : "<tr hidden ")).join("");
    }).join("");
    return `<div class="evlog" id="evlog"><table class="sched">${SCHED_HEAD(true)}${spanNote(evs.length, evs.length + nHidden, f, "tr", NCOLS(true))}${body}</table></div>`;
  }
  function wireEventList(host) {
    for (const tr of host.querySelectorAll("tr.logged[data-lat]")) tr.onclick = () => { if (tr.dataset.lat) UW.focusMap(tr.dataset.lat, tr.dataset.lon, tr.children[1]?.textContent); };
    for (const b of host.querySelectorAll(".viewdata")) b.onclick = async (ev) => {
      ev.stopPropagation();
      try { await ensureCastIndex(); } catch { UW.setLoadError("Casts", true); return; }
      UW.onStationClick?.(b.dataset.cast);
    };
    for (const b of host.querySelectorAll(".dayfold")) b.onclick = () => foldDay(host, b.dataset.day, !cal.openDays.has(b.dataset.day));
    wireBells(host);
  }
  // a day's scheduled rows shown or hidden in place, remembered for the session
  function foldDay(host, d, open) {
    if (open) cal.openDays.add(d); else cal.openDays.delete(d);
    for (const tr of host.querySelectorAll(`tr.sched.former[data-day="${CSS.escape(d)}"]`)) tr.hidden = !open;
    const b = host.querySelector(`.dayfold[data-day="${CSS.escape(d)}"]`); if (b) b.textContent = b.textContent.replace(/^\S+ (show|hide)/, open ? "▾ hide" : "▸ show");
  }
  // a click on the timeline scrolls the log to that row (the log's own scroll, not the page's)
  function showLogRow(host, key) {
    const log = host.querySelector("#evlog"), row = log?.querySelector(`tr[data-key="${CSS.escape(key)}"]`);
    if (!log || !row) return;
    if (row.hidden && row.dataset.day) foldDay(host, row.dataset.day, true);
    for (const x of log.querySelectorAll("tr.on")) x.classList.remove("on");
    row.classList.add("on");
    log.scrollTo({ top: row.offsetTop - log.clientHeight / 3, behavior: "smooth" });
    if (row.dataset.lat) UW.focusMap(row.dataset.lat, row.dataset.lon, row.children[1]?.textContent);
  }
  // the bell beside an operation: follow just that one, by email (the address
  // the page remembers) or by Telegram (a t.me link carrying the row); a
  // followed row gets a 15-minute heads-up and every change
  // rows followed by email (bells.rows, for the saved address) and in this
  // browser (bells.web, by its own id); a bell lights for either
  const bells = { rows: new Set(), web: new Set(), for: null, webFor: null, pendingRow: null };
  const followEmail = () => store.get("alerts.email", "");
  const followed = (key) => bells.rows.has(key) || bells.web.has(key);
  const encodeRow = (key) => btoa(unescape(encodeURIComponent(key))).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "").slice(0, 64);
  // two bells: this operation, and every operation of this kind (any station)
  const bellTitle = (kind, on) => on ? `you follow ${kind} — click to stop` : `follow ${kind}: 15 min heads-up and every change`;
  function bellHtml(r) {
    const a = UW.M.alerts || {};
    if (!a.email && !a.telegram_bot) return "";
    const key = r.key || `${r.station}|${r.operation}`, on = followed(key);
    const kind = /transit|steam/i.test(r.operation || "") ? "Transit" : (r.operation || "");     // every transit is one kind, whatever the destination
    const opKey = `op:${kind}`, opOn = followed(opKey), kindName = kind === "Transit" ? "every transit" : `every ${kind}`;
    return `<button type="button" class="bell ${on ? "on" : ""}" data-key="${esc(key)}" data-name="${esc(`${r.station || ""} — ${r.operation || ""}`.trim())}" title="${esc(bellTitle("this operation", on))}">🔔</button>` +
      (kind ? `<button type="button" class="bell kind ${opOn ? "on" : ""}" data-key="${esc(opKey)}" data-name="${esc(kindName)}" title="${esc(bellTitle(kindName, opOn))}">📢</button>` : "");
  }
  async function refreshBells(host) {
    const to = followEmail(), wid = store.get("alerts.webid", "");
    if (!to) { bells.rows = new Set(); bells.for = null; }
    if (to && bells.for !== to) {
      try { const j = await UW.fetchJSON(`api/alerts/following?channel=email&to=${encodeURIComponent(to)}&t=${Date.now()}`); bells.rows = new Set(j.rows || []); bells.for = to; }
      catch { /* bells stay unlit */ }
    }
    if (wid && bells.webFor !== wid) {
      try { const j = await UW.fetchJSON(`api/alerts/following?channel=web&to=${encodeURIComponent(wid)}&t=${Date.now()}`); bells.web = new Set(j.rows || []); bells.webFor = wid; }
      catch { /* likewise */ }
    }
    for (const b of host.querySelectorAll(".bell")) b.classList.toggle("on", followed(b.dataset.key));
  }
  // a station named in a schedule row: found among the casts and event-log
  // stations, else the cruise plan's, and shown on the map
  function stationPosition(name) {
    const want = String(name || "").trim().toLowerCase().replace(/\s+/g, " ");
    if (!want) return null;
    const same = (x) => String(x || "").trim().toLowerCase().replace(/\s+/g, " ") === want;
    const st = (UW.M.stations || []).filter((s) => same(s.station) && s.lat != null).sort((a, b) => String(b.time).localeCompare(String(a.time)))[0];
    if (st) return { lat: st.lat, lon: st.lon };
    for (const pl of UW.plansShown?.() || []) { const p = pl.stations.find((x) => same(x.name)); if (p) return { lat: p.lat, lon: p.lon }; }
    return null;
  }
  function wireStationLinks(host) {
    for (const a of host.querySelectorAll("a.stnlink")) a.onclick = (ev) => {
      ev.preventDefault(); ev.stopPropagation();
      const pos = stationPosition(a.dataset.station);
      if (pos) UW.focusMap(pos.lat, pos.lon, a.dataset.station); else UW.toast?.(`${a.dataset.station}: no position known yet`);
    };
  }
  function wireBells(host) {
    wireStationLinks(host);
    refreshBells(host);
    for (const b of host.querySelectorAll(".bell")) b.onclick = (ev) => { ev.stopPropagation(); bellMenu(host, b); };
  }
  // follow (or drop) a row by email or in this browser; the bells relight
  async function followVia(host, channel, key, name, remove) {
    const to = channel === "web" ? UW.webId() : followEmail();
    const r = await fetch("api/alerts/row", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify({ channel, to, key, name, remove }) });
    const j = await r.json();
    if (!r.ok) throw new Error(j.error || r.status);
    if (channel === "web") { bells.web = new Set(j.rows || []); bells.webFor = to; askNotify(); } else { bells.rows = new Set(j.rows || []); bells.for = to; }
    for (const b of host.querySelectorAll(".bell")) { const on = followed(b.dataset.key); b.classList.toggle("on", on); b.title = bellTitle(b.classList.contains("kind") ? b.dataset.name : "this operation", on); }
  }
  const followByEmail = (host, key, name, remove) => followVia(host, "email", key, name, remove);
  // a browser that shows alerts in its header bar may as well notify too
  const askNotify = () => { try { if (window.Notification && Notification.permission === "default") Notification.requestPermission(); } catch { /* not every browser */ } };
  function bellMenu(host, b) {
    host.querySelector(".bellmenu")?.remove();
    const a = UW.M.alerts || {}, key = b.dataset.key, name = b.dataset.name, to = followEmail(), on = bells.rows.has(key), webOn = bells.web.has(key);
    const m = document.createElement("div"); m.className = "bellmenu";
    m.innerHTML = `<div class="bm-title">${esc(name)}</div>` +
      (a.web ? `<button type="button" class="bm-web">${webOn ? "🖥 stop showing" : "🖥 show"} in this browser's header bar</button>` : "") +
      (a.email ? (to ? `<button type="button" class="bm-email">${on ? "✉ stop emailing" : "✉ email"} ${esc(to)}</button>` : `<button type="button" class="bm-email">✉ email me… (enter an address below)</button>`) : "") +
      (a.telegram_bot ? `<a class="bm-tg" href="https://t.me/${esc(a.telegram_bot)}?start=${encodeRow(key)}" target="_blank" rel="noopener">✈ Telegram @${esc(a.telegram_bot)}</a>` : "") +
      `<div class="bm-note">15 min heads-up and every change${key.startsWith("op:") ? `, for ${esc(name)}${key === "op:Transit" ? " (whatever the destination)" : " at any station"}` : " to this operation"}</div>`;
    const rect = b.getBoundingClientRect(), hostRect = host.getBoundingClientRect();
    m.style.left = `${Math.max(0, rect.left - hostRect.left)}px`; m.style.top = `${rect.bottom - hostRect.top + host.scrollTop + 4}px`;
    host.style.position = host.style.position || "relative";
    host.appendChild(m);
    const close = () => { m.remove(); document.removeEventListener("click", close); };
    setTimeout(() => document.addEventListener("click", close), 0);
    const e = m.querySelector(".bm-email");
    if (e) e.onclick = async (ev) => {
      ev.stopPropagation();
      if (!to) { bells.pendingRow = { key, name }; close(); const det = host.querySelector("#alerts"); if (det) { det.open = true; det.querySelector("input[name=to]")?.focus(); det.querySelector("#alertmsg").textContent = `enter your email to follow ${name}`; } return; }
      try { await followByEmail(host, key, name, on); close(); } catch (err) { m.querySelector(".bm-note").textContent = `not saved: ${err.message}`; }
    };
    const w = m.querySelector(".bm-web");
    if (w) w.onclick = async (ev) => { ev.stopPropagation(); try { await followVia(host, "web", key, name, webOn); close(); } catch (err) { m.querySelector(".bm-note").textContent = `not saved: ${err.message}`; } };
    if (m.querySelector(".bm-tg")) m.querySelector(".bm-tg").onclick = () => setTimeout(close, 100);
  }

  // alerts: subscribe here to this browser's header bar or by email, or
  // through the Telegram bot
  function alertsHtml() {
    const a = UW.M.alerts || {};
    if (!a.email && !a.telegram_bot && !a.web) return "";
    const saved = store.get("alerts.email", ""), viaWeb = !saved || store.get("alerts.web", false);
    return `<details class="alerts" id="alerts"><summary>🔔 Get alerts for scheduled operations</summary>
      ${a.email || a.web ? `<form id="alertform" class="alertform">
        <label>via <select name="channel">${a.web ? `<option value="web" ${viaWeb ? "selected" : ""}>this browser's header bar</option>` : ""}${a.email ? `<option value="email" ${viaWeb ? "" : "selected"}>email</option>` : ""}</select></label>
        <label class="emailfield" ${viaWeb ? "hidden" : ""}>email <input type="email" name="to" ${viaWeb ? "" : "required"} value="${esc(saved)}" placeholder="you@example.org"></label>
        <label>only operations matching <input name="match" placeholder="e.g. CardS-3, CTD — blank for everything" size="34"></label>
        <label>warn <select name="lead_min"><option value="15">15 min</option><option value="30" selected>30 min</option><option value="60">1 h</option><option value="120">2 h</option></select> ahead</label>
        <span class="evs"><label><input type="checkbox" name="events" value="upcoming" checked> starting soon</label><label><input type="checkbox" name="events" value="started" checked> started</label><label><input type="checkbox" name="events" value="finished"> finished</label><label><input type="checkbox" name="events" value="moved" checked> time changed</label></span>
        <button type="submit">subscribe</button><span class="muted" id="alertmsg"></span></form>` : ""}
      ${a.telegram_bot ? `<p class="muted small">Telegram: message <a href="https://t.me/${esc(a.telegram_bot)}" target="_blank" rel="noopener">@${esc(a.telegram_bot)}</a> with /start, then /only CardS-3 or /lead 60 to tune it.</p>` : ""}
      <p class="muted small">Header-bar alerts stay in this browser and clear with ✕. Every alert email carries an unsubscribe link. Times are ship time.</p></details>`;
  }
  function wireAlerts(host) {
    const f = host.querySelector("#alertform"); if (!f) return;
    const ch = f.querySelector("select[name=channel]"), ef = f.querySelector(".emailfield"), ei = f.querySelector("input[name=to]");
    const viaWeb = () => ch.value === "web";
    ch.onchange = () => { ef.hidden = viaWeb(); ei.required = !viaWeb(); };
    f.onsubmit = async (ev) => {
      ev.preventDefault();
      const fd = new FormData(f), msg = f.querySelector("#alertmsg"), channel = viaWeb() ? "web" : "email";
      const body = { channel, to: channel === "web" ? UW.webId() : fd.get("to"), match: fd.get("match"), lead_min: fd.get("lead_min"), events: fd.getAll("events") };
      msg.textContent = "saving…";
      try {
        const r = await fetch("api/alerts", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body) });
        const j = await r.json();
        if (!r.ok) throw new Error(j.error || r.status);
        if (channel === "web") { store.set("alerts.web", true); askNotify(); } else store.set("alerts.email", j.to);
        msg.textContent = `subscribed ${channel === "web" ? "this browser" : j.to}: ${j.match || "everything"}, ${j.lead_min} min ahead`;
        const hostEl = f.closest("#calendar") || document;
        if (bells.pendingRow) { const pr = bells.pendingRow; bells.pendingRow = null; bells.for = null; bells.webFor = null; try { await followVia(hostEl, channel, pr.key, pr.name, false); msg.textContent += ` · following ${pr.name}`; } catch { /* the bell shows the truth */ } }
        else { bells.for = null; bells.webFor = null; refreshBells(hostEl); }
        if (channel === "web") UW.pollInapp?.();
      } catch (e) { msg.textContent = `not saved: ${e.message}`; }
    };
  }
  // current rows plus the former ones the history remembers, each with UTC instants
  const scheduledRows = (s) => [...(s.rows || []), ...(s.former || [])].filter((r) => r.start_utc && r.end_utc);
  // "In progress" -> st-in-progress, for the agenda, the schedule table and the calendar blocks
  const statusClass = (status) => status ? "st-" + String(status).toLowerCase().replace(/\s+/g, "-") : "";
  // one colour per kind of operation, shared by the timeline's event-log
  // rows and its scheduled bars so a scheduled CTD and a logged CTD match
  const OP_KINDS = [["transit", /transit|steam/i], ["ctd", /ctd|rosette/i], ["core", /core|corer/i], ["net", /net|ikmt|hydrobios|tucker|monster|catcher|pump/i],
    ["camera", /cam|rov|subocean|hydroscat|c-ops/i], ["boat", /zodiac|barge|community|tour|open house/i], ["mapping", /mapping|survey|multibeam/i], ["break", /break|lunch|dinner|meal/i]];
  const OP_COLOUR = { get transit() { return C.muted; }, get ctd() { return C.accent; }, get core() { return C.gold; }, get net() { return C.ok; },
    get camera() { return C.purple; }, get boat() { return C.pink; }, get mapping() { return pal(6); }, get break() { return C.muted; } };
  const opKind = (name) => (OP_KINDS.find(([, re]) => re.test(name || "")) || [""])[0];
  // the instrument or process behind a free-text activity or operation name,
  // so the timeline has one row for "Box Core", "Box Core - GEO" and "Box core
  // bio", one "Transit" for every "Transit to <station>", one "Baited Cam" for
  // its deployment and recovery
  const OP_NAMES = [["Transit", /transit|steam/i], ["CTD Rosette", /ctd.?rosette|classic.?rosette|^rosette/i], ["TM Rosette", /tm.?rosette/i], ["CTD", /^ctd\b/i],
    ["Box Core", /box.?core/i], ["Gravity Core", /gravity.?core/i], ["Multicorer", /multi.?corer/i], ["Piston Core", /piston/i],
    ["Baited Cam", /baited.?cam/i], ["Drop Camera", /drop.?cam/i], ["ROV", /\brov\b/i], ["SubOcean", /subocean/i],
    ["Tucker Net", /tucker/i], ["Monster Net", /monster/i], ["IKMT", /ikmt/i], ["Hydrobios", /hydrobios/i], ["Snow Catcher", /snow.?catcher/i], ["Agassiz Trawl", /agassiz/i], ["Plankton Net", /plankton|bongo|wp2/i],
    ["In-situ Pumps", /in.?situ.?pump/i], ["Zodiac", /zodiac/i], ["Barge", /barge/i], ["Helicopter", /helicopter|heli\b/i], ["Mooring", /mooring/i], ["Lander", /lander/i],
    ["Mapping", /mapping|multibeam|survey/i], ["Break", /break|lunch|dinner|meal/i], ["Crew Change", /crew.?change/i], ["Community Visit", /community|ship.?tour|open.?house/i],
    ["C-OPS", /c-?ops/i], ["Hydroscat", /hydroscat/i], ["SCUBA", /scuba|dive/i], ["Ice Station", /ice.?station|ice.?work/i]];
  const opName = (name) => {
    const hit = OP_NAMES.find(([, re]) => re.test(name || ""));
    if (hit) return hit[0];
    // otherwise: drop a leading verb, a trailing count or size, and tidy the case
    const s = String(name || "other").replace(/^(deploy|recover|retrieve|launch|drop|start|end|stop)\s+/i, "").replace(/\s+(\d+\s*(x\s*\d+\s*m)?|[A-Z]|bio|geo)\s*$/i, "").replace(/\s*[-–]\s*(geo|bio|p)\s*$/i, "").trim();
    return s ? s.replace(/\w\S*/g, (w) => w[0].toUpperCase() + w.slice(1)) : "other";
  };
  const opColour = (name, fallback) => OP_COLOUR[opKind(name)] || fallback;
  function renderTimeline(host, evs, s, nHidden = 0) {
    const now = Date.now();
    const shifted = (t) => new Date(t + offsetMs(t));      // the axis reads as ship time
    const when = (e) => shifted(UW.tms(e.time_utc));
    const recent = evs;                                   // already the legs and span on display
    const counts = new Map(); for (const e of recent) counts.set(opName(e.activity), (counts.get(opName(e.activity)) || 0) + 1);
    const top = [...counts.entries()].sort((a, b) => b[1] - a[1]).slice(0, 19).map(([t]) => t);
    const typeOf = (e) => top.includes(opName(e.activity)) ? opName(e.activity) : "other";
    const types = [...new Set(recent.map(typeOf))];
    const traces = types.map((t, i) => {
      const es = recent.map((e, j) => [e, j]).filter(([e]) => typeOf(e) === t).map(([e]) => e);
      const idx = recent.map((e, j) => [e, j]).filter(([e]) => typeOf(e) === t).map(([, j]) => `e:${j}`);
      return { type: "scatter", mode: "markers", name: t, x: es.map(when), y: es.map(() => t), customdata: idx,
        text: es.map((e) => `${esc(e.station || "")} · ${esc(e.activity || "")} · ${esc(e.event || "")} ${esc(e.label || "")}`), hovertemplate: "%{x|%Y-%m-%d %H:%M} " + tzAbbr() + "<br>%{text}<extra>" + esc(t) + "</extra>",
        marker: { size: 8, color: opColour(t, pal(i)) } };
    });
    // scheduled operations as bars on their own row, coloured like the
    // event-log row of the same kind: current ones bright, former ones (off
    // the intranet page now) dimmer
    const f = UW.currentFilter();
    const rows = scheduledRows(s).map((r) => ({ r, d0: shifted(UW.tms(r.start_utc)), d1: shifted(UW.tms(r.end_utc)) })).filter((b) => b.d1 - offsetMs(b.d1) >= f.start);
    const rgba = (hex, a) => `rgba(${parseInt(hex.slice(1, 3), 16)},${parseInt(hex.slice(3, 5), 16)},${parseInt(hex.slice(5, 7), 16)},${a})`;
    const barColour = (b) => opColour(b.r.operation, pal((b.r.operation || "").length));
    if (rows.length) traces.push({ type: "bar", orientation: "h", name: "scheduled", base: rows.map((b) => b.d0), x: rows.map((b) => b.d1 - b.d0), y: rows.map(() => "scheduled"), customdata: rows.map((b) => rowKey(b.r)),
      text: rows.map((b) => `${esc(b.r.station)} · ${esc(b.r.operation)} (${esc(b.r.status)})${b.r.former ? " · was scheduled" : ""}<br>${stampL(UW.tms(b.r.start_utc))}–${hmL(UW.tms(b.r.end_utc))} ${tzAbbr()}`),
      hovertemplate: "%{text}<extra></extra>", textposition: "none", marker: { color: rows.map((b) => rgba(barColour(b), b.r.former ? .3 : .8)), line: { color: rows.map(barColour), width: 1 } }, width: .5 });
    host.innerHTML = castPanelHtml("cal-plot", "Timeline", `${recent.length} events · ${rows.length} scheduled · ${f.label} span · click a point for its log entry`, false, false, false).replace('class="panel card castplot', 'class="panel card castplot wide') +
      eventListHtml(evs, s, cal.search.toLowerCase(), f, nHidden);
    wireEventList(host);
    const layout = { ...castLayout(), margin: { l: fz(130), r: 10, t: fz(28), b: fz(58) }, barmode: "overlay",
      xaxis: { ...THEME.xaxis, type: "date", title: { text: `ship time (${tzAbbr()})`, font: { size: fz(12) } }, tickfont: { size: fz(12) },
               ...(isFinite(UW.spanFilter().start) ? { range: [shifted(UW.spanFilter().start), shifted(UW.spanFilter().end + 3600e3)], autorange: false } : {}) },   // opens on the span; the log runs on before it
      yaxis: { ...THEME.yaxis, type: "category", categoryorder: "array", categoryarray: ["scheduled", ...types.slice().reverse()], tickfont: { size: fz(12) }, fixedrange: true },
      shapes: [{ type: "line", xref: "x", x0: shifted(now), x1: shifted(now), yref: "paper", y0: 0, y1: 1, line: { color: C.now, width: 2 } }],
      annotations: [{ xref: "x", x: shifted(now), yref: "paper", y: 1, yanchor: "bottom", text: `now ${hmL(now)}`, showarrow: false, font: { size: fz(11), color: C.now } }] };
    Plotly.react($("#cal-plot"), traces, layout, CFG).then((gd) => { UW.axisZoom(gd); gd.removeAllListeners?.("plotly_click"); gd.on("plotly_click", (ev) => { const k = ev.points?.[0]?.customdata; if (k) showLogRow(host, k); }); });
    wireCastPanels(host, () => renderTimeline(host, evs, s));
  }
  // Calendar view: a month grid or three days centred on a day, from the
  // Google calendars (imported at build time) and the intranet schedule.
  // Click an entry for its details.
  const GCAL_COLOUR = { get schedule() { return C.accent; }, get surprise() { return C.accent2; }, get intranet() { return C.muted; } };
  function calendarItems(q) {
    const items = [];
    for (const f of cal.data.gcal || []) for (const e of f.events || []) items.push({ ...e, cal: f.key, label: f.label });
    const pump = cal.data.pump_events || [];
    for (let i = items.length - 1; i >= 0; i--) if (items[i].cal === "surprise" &&
      items[i].summary === "TSG pump off / low intake flow" && pump.some(e => UW.tms(e.time_utc) === UW.tms(items[i].start))) items.splice(i, 1);
    for (const e of pump) items.push({start:e.time_utc,end:e.end_utc,summary:e.event,
      description:e.comment,cal:"pump",label:"TSG intake",leg:e.leg});
    // the intranet rows are also pushed to the Amundsen Schedule calendar,
    // with "Operation: …" and "Station: …" lines in the description; the row
    // itself is the fresher copy (the public feed lags the push by minutes to
    // hours), so a feed event that names one of our rows gives way to the row
    // whatever time it still shows
    const rows = scheduledRows(cal.data.schedule || {});
    const rowName = (station, op) => `${station || ""} — ${op || ""}`.replace(/\s+/g, " ").trim();
    const rowNames = new Set(rows.map((r) => rowName(r.station, r.operation)));
    const descLine = (e, label) => ((e.description || "").match(new RegExp(`^${label}: ?(.*)$`, "m")) || [])[1] || "";
    const isRowCopy = (e) => e.cal === "schedule" && !/^\[EventLog\]/.test(e.summary || "") && !!descLine(e, "Operation") &&
      rowNames.has(rowName(descLine(e, "Station"), descLine(e, "Operation")));
    for (let i = items.length - 1; i >= 0; i--) if (isRowCopy(items[i])) items.splice(i, 1);
    for (const r of rows) items.push({ start: r.start_utc, end: r.end_utc, summary: `${r.former ? "was scheduled" : "scheduled"} · ${r.station} — ${r.operation} (${r.status})`,
      description: [r.comment, `${r.duration_h != null ? r.duration_h.toFixed(1) + " h" : ""}`].filter(Boolean).join("\n"), cal: "intranet", label: "intranet schedule", status: r.status, key: r.key });
    items.forEach((e, i) => { e.id = i; });
    return items.filter((e) => !q || `${e.summary} ${e.description || ""}`.toLowerCase().includes(q));
  }
  const evStart = (e) => new Date(e.all_day ? localMidnight(e.start) : e.start);
  const evEnd = (e) => e.end ? new Date(e.all_day ? localMidnight(e.end) : e.end) : evStart(e);
  // block text without the prefixes the layout already conveys: a leading
  // "[status] ", "scheduled · " / "was scheduled · ", and a trailing " at <date>"
  const shortSummary = (t) => String(t || "").replace(/^\[[^\]]*\]\s*/, "").replace(/^(was )?scheduled · /, "").replace(/ at \d{4}-\d{2}-\d{2}[^,;]*$/, "");
  function entryHtml(e, cont) {
    return `<div class="mev" data-id="${e.id}" style="border-color:${GCAL_COLOUR[e.cal] || C.muted}" title="${esc(e.label)}\n${esc(e.summary)}">` +
      `<span class="mt">${cont || e.all_day ? "" : hmL(evStart(e))}</span> ${esc(e.summary || "")}</div>`;
  }
  function detailHtml(e) {
    const t0 = evStart(e), t1 = evEnd(e);
    const when = e.all_day ? `${e.start}${e.end && e.end !== e.start ? " → " + e.end : ""} (all day)` :
      `${stampL(t0)} → ${stampL(t1)} ${tzAbbr(t1)} · ${((t1 - t0) / 3600e3).toFixed(1)} h`;
    const pos = /Position:\s*([\d.]+)°([NS]),\s*([\d.]+)°([EW])/.exec(e.description || "");
    return `<div class="mdetail"><button class="mclose" title="close">✕</button>
      <div class="mdlabel" style="color:${GCAL_COLOUR[e.cal] || C.muted}">${esc(e.label)}</div>
      <h4>${esc(e.summary || "")}</h4>
      <div class="mdwhen">${esc(when)}</div>
      ${e.description ? `<pre class="mddesc">${esc(e.description)}</pre>` : ""}
      ${pos ? `<button class="mdmap">show on map</button>` : ""}</div>`;
  }
  function wireEntries(host, items) {
    const byId = new Map(items.map((e) => [e.id, e]));
    const box = host.querySelector("#mdetailbox");
    const show = (e) => {
      box.innerHTML = detailHtml(e); box.hidden = false;
      for (const x of host.querySelectorAll(".mev.on")) x.classList.remove("on");
      host.querySelector(`.mev[data-id="${e.id}"]`)?.classList.add("on");
      box.querySelector(".mclose").onclick = () => { box.hidden = true; for (const x of host.querySelectorAll(".mev.on")) x.classList.remove("on"); };
      const mb = box.querySelector(".mdmap");
      if (mb) mb.onclick = () => { const m = /Position:\s*([\d.]+)°([NS]),\s*([\d.]+)°([EW])/.exec(e.description);
        UW.focusMap((m[2] === "S" ? -1 : 1) * +m[1], (m[4] === "W" ? -1 : 1) * +m[3], e.summary); };
    };
    for (const el of host.querySelectorAll(".mev")) el.onclick = () => show(byId.get(+el.dataset.id));
  }
  function calFrame(host, title, body, items, navShift) {
    const feeds = cal.data.gcal || [];
    host.innerHTML = `<section class="card block month">
      <div class="mhead"><div class="group seg small" id="calspan"><button data-s="days" ${cal.span === "days" ? 'class="on"' : ""}>3 days</button><button data-s="month" ${cal.span === "month" ? 'class="on"' : ""}>Month</button></div>
        <button id="mprev" title="previous">‹</button><h3>${esc(title)}</h3><button id="mnext" title="next">›</button><button id="mtoday">today</button>
        <span class="mlegend">${feeds.map((f) => `<i style="border-color:${GCAL_COLOUR[f.key] || C.muted}"></i>${esc(f.label)}${f.stale ? " (cached)" : ""} · ${(f.events || []).length}`).join(" &nbsp; ")} &nbsp; <i style="border-color:${C.muted}"></i>intranet schedule</span></div>
      <div id="mdetailbox" hidden></div>
      ${body}
      <p class="muted small">Times are ship time (${tzAbbr()}). Open in Google Calendar: ${(UW.M.links || []).map((l) => `<a href="${esc(l.url)}" target="_blank" rel="noopener">${esc(l.label)}</a>`).join(" · ")}</p></section>`;
    for (const b of host.querySelectorAll("#calspan button")) b.onclick = () => { cal.span = b.dataset.s; store.set("cal.span", cal.span); renderCalendar(); };
    $("#mprev").onclick = () => navShift(-1); $("#mnext").onclick = () => navShift(1);
    $("#mtoday").onclick = () => { cal.day = dayL(Date.now()); cal.month = cal.day.slice(0, 7); store.set("cal.day", cal.day); store.set("cal.month", cal.month); renderCalendar(); };
    wireEntries(host, items);
  }
  function renderMonth(host, q) {
    if (cal.span === "days") return renderDays(host, q);
    const items = calendarItems(q);
    const first = new Date(cal.month + "-01T00:00:00Z");
    const y = first.getUTCFullYear(), m = first.getUTCMonth();
    const days = new Date(Date.UTC(y, m + 1, 0)).getUTCDate();
    const lead = (first.getUTCDay() + 6) % 7;                 // Monday first
    const byDay = new Map();
    for (const e of items) {
      const d0 = evStart(e).getTime(), d1 = evEnd(e).getTime();
      const seen = new Set();
      // every ship day the event touches (half-day steps survive a clock change)
      for (let t = d0; (t < d1 || t === d0) && t - d0 < 62 * 86400e3; t += 43200e3) {
        if (e.all_day && e.end && t >= d1) break;              // all-day ends are exclusive
        const k = dayL(t);
        if (seen.has(k)) continue; seen.add(k);
        if (!byDay.has(k)) byDay.set(k, []); byDay.get(k).push({ e, cont: seen.size > 1 });
      }
    }
    const today = dayL(Date.now());
    const cells = [];
    for (let i = 0; i < lead; i++) cells.push('<div class="mcell pad"></div>');
    for (let d = 1; d <= days; d++) {
      const k = `${cal.month}-${String(d).padStart(2, "0")}`;
      const evs = (byDay.get(k) || []).sort((a, b) => evStart(a.e) - evStart(b.e));
      cells.push(`<div class="mcell ${k === today ? "today" : ""}" data-day="${k}"><div class="mday">${d}</div>` +
        evs.slice(0, 6).map(({ e, cont }) => entryHtml(e, cont)).join("") +
        (evs.length > 6 ? `<div class="mmore">+${evs.length - 6} more</div>` : "") + `</div>`);
    }
    const label = first.toLocaleString(undefined, { month: "long", year: "numeric", timeZone: LTZ });
    calFrame(host, label, `<div class="mgrid">${["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"].map((d) => `<div class="mdow">${d}</div>`).join("")}${cells.join("")}</div>`, items,
      (n) => { const d = new Date(Date.UTC(y, m + n, 1)); cal.month = d.toISOString().slice(0, 7); store.set("cal.month", cal.month); renderCalendar(); });
    // a day number opens that day in the 3-day view
    for (const c of host.querySelectorAll(".mcell[data-day] .mday")) c.onclick = () => { cal.day = c.parentElement.dataset.day; cal.span = "days"; store.set("cal.day", cal.day); store.set("cal.span", "days"); renderCalendar(); };
  }
  // three ship days centred on cal.day, with a time axis; timed entries are
  // blocks, all-day ones sit at the top
  function renderDays(host, q) {
    const items = calendarItems(q);
    const centre = localMidnight(cal.day);
    const keys = [-1, 0, 1].map((n) => dayL(centre + n * 86400e3 + 43200e3));
    const today = dayL(Date.now());
    const now = Date.now();
    const cols = keys.map((k) => {
      const d0 = localMidnight(k), d1 = localMidnight(nextDay(k)), hours = (d1 - d0) / 3600e3;
      const here = items.filter((e) => evStart(e) < d1 && evEnd(e) > d0 || (e.all_day && e.start === k));
      const allDay = here.filter((e) => e.all_day), timed = here.filter((e) => !e.all_day);
      // lay overlapping blocks side by side
      // longer blocks first so a short one drawn later sits on top of a long
      // one it overlaps; overlapping blocks share the column side by side
      const sorted = timed.sort((a, b) => (evStart(a) - evStart(b)) || ((evEnd(b) - evStart(b)) - (evEnd(a) - evStart(a))));
      // overlapping blocks share the column width equally: a block alone is
      // full width, two together half each, three a third; the lanes are
      // dealt within each cluster of mutually overlapping blocks
      const lanes = [];
      let cluster = [], clusterEnd = -Infinity;
      const close = () => { const nl = Math.max(1, ...cluster.map((e) => e._lane + 1)); for (const e of cluster) e._nl = nl; cluster = []; lanes.length = 0; };
      for (const e of sorted) {
        if (cluster.length && evStart(e) >= clusterEnd) close();
        let l = 0; while (lanes[l] && lanes[l] > evStart(e)) l++;
        lanes[l] = evEnd(e); e._lane = l; cluster.push(e); clusterEnd = Math.max(clusterEnd, evEnd(e));
      }
      close();
      const blocks = sorted.map((e, i) => {
        const s = Math.max(0, (evStart(e) - d0) / 3600e3), t = Math.min(hours, (evEnd(e) - d0) / 3600e3);
        // a short event still gets one readable line: the block is at least ~40 min tall
        return `<div class="dblock mev ${statusClass(e.status)}" data-id="${e.id}" style="top:${(s / hours * 100).toFixed(2)}%;height:${Math.max(2.9, (t - s) / hours * 100).toFixed(2)}%;left:${(e._lane / e._nl * 100).toFixed(1)}%;width:${(100 / e._nl - 1).toFixed(1)}%;z-index:${2 + i};border-color:${GCAL_COLOUR[e.cal] || C.muted}" title="${esc(e.label)}\n${hmL(evStart(e))} ${esc(e.summary)}">${esc(shortSummary(e.summary))}</div>`;
      }).join("");
      const nowLine = k === today ? `<div class="dnow" style="top:${((now - d0) / (d1 - d0) * 100).toFixed(2)}%"><span>${hmL(now)}</span></div>` : "";
      const head = new Date(d0 + 43200e3).toLocaleDateString(undefined, { weekday: "short", month: "short", day: "numeric", timeZone: LTZ });
      return `<div class="dcol ${k === today ? "today" : ""}"><div class="dhead">${head}${k === today ? " · today" : ""}</div>
        <div class="dallday">${allDay.map((e) => entryHtml(e, false)).join("")}</div>
        <div class="dbody">${Array.from({ length: Math.round(hours) }, (_, h) => `<div class="dhour" style="top:${(h / hours * 100).toFixed(2)}%"></div>`).join("")}${blocks}${nowLine}</div></div>`;
    }).join("");
    const axis = `<div class="daxis"><div class="dhead">${tzAbbr()}</div><div class="dallday"></div><div class="dbody">${Array.from({ length: 24 }, (_, h) => `<div class="dhl" style="top:${(h / 24 * 100).toFixed(2)}%">${String(h).padStart(2, "0")}</div>`).join("")}</div></div>`;
    const dayLabel = (k, opts) => new Date(localMidnight(k) + 43200e3).toLocaleDateString(undefined, { ...opts, timeZone: LTZ });
    const label = `${dayLabel(keys[0], { month: "short", day: "numeric" })} – ${dayLabel(keys[2], { month: "short", day: "numeric", year: "numeric" })}`;
    calFrame(host, label, `<div class="dgrid">${axis}${cols}</div>`, items,
      (n) => { cal.day = dayL(centre + n * 86400e3 + 43200e3); cal.month = cal.day.slice(0, 7); store.set("cal.day", cal.day); store.set("cal.month", cal.month); renderCalendar(); });
  }
  function wireCalendar() {
    for (const b of $("#calview").querySelectorAll("button")) {
      b.classList.toggle("on", b.dataset.v === cal.view);
      b.onclick = () => { cal.view = b.dataset.v; store.set("cal.view", cal.view); for (const x of $("#calview").querySelectorAll("button")) x.classList.toggle("on", x === b); renderCalendar(); };
    }
    $("#calsearch").oninput = debounce((e) => { cal.search = e.target.value; renderCalendar(); }, 150);
  }

  // a wide table gets a scroll bar above it too, kept in step with the one
  // below, so the far columns can be reached without scrolling to the bottom
  function topScroll(wrap) {
    if (!wrap) return;
    let top = wrap.previousElementSibling?.classList.contains("topscroll") ? wrap.previousElementSibling : null;
    if (!top) {
      top = document.createElement("div"); top.className = "topscroll"; top.innerHTML = "<div></div>";
      wrap.parentElement.insertBefore(top, wrap);
      top.onscroll = () => { if (wrap.scrollLeft !== top.scrollLeft) wrap.scrollLeft = top.scrollLeft; };
      wrap.addEventListener("scroll", () => { if (top.scrollLeft !== wrap.scrollLeft) top.scrollLeft = wrap.scrollLeft; });
    }
    const table = wrap.querySelector("table");
    const fit = () => { top.firstElementChild.style.width = `${table?.scrollWidth || 0}px`; top.hidden = !table || table.scrollWidth <= wrap.clientWidth + 1; };
    fit();
    if (!wrap._topObs) { wrap._topObs = new ResizeObserver(fit); wrap._topObs.observe(wrap); }
    if (table && !table._topObs) { table._topObs = new ResizeObserver(fit); table._topObs.observe(table); }
  }

  // ================================================================ stations
  // one row per CTD cast from the logbook, and one per station the event
  // log records without a cast (kind "event", with what was done there)
  const stn = { sort: store.get("stn.sort", { key: "time", dir: -1 }), search: "" };
  const STATION_COLS = [["time", "time (ship)"], ["leg", "leg"], ["kind", "source"], ["cast", "cast"], ["station", "station"], ["label", "label"], ["type", "type"], ["activities", "activities"], ["lat", "lat"], ["lon", "lon"], ["bottom_m", "bottom (m)"], ["depth_m", "cast depth (m)"], ["comments", "comments"]];
  function stationRows() {
    const q = stn.search.toLowerCase();
    const f = UW.currentFilter();
    let all = (UW.M.stations || []).filter((s) => f.legs.has(s.leg)).map((s) => ({ ...s, legLabel: UW.legById(s.leg)?.label || s.leg, kind: s.kind === "event" ? "event log" : "CTD logbook", activities: (s.activities || []).join(", ") }));
    // the planned stations (the cruise plan KMZ): no time or leg, so never filtered out
    for (const pl of UW.plansShown?.() || []) all = all.concat(pl.stations.map((st) => ({ station: st.name, kind: `kmz ${pl.imported}`, legLabel: st.group, type: st.type || "", bottom_m: st.depth_m ?? null, activities: st.ops || "", label: st.region || "", comments: st.desc || "", lat: st.lat, lon: st.lon, time: null, leg: null })));
    if (q) all = all.filter((r) => `${r.time} ${r.legLabel} ${r.kind} ${r.station} ${r.label} ${r.type} ${r.activities} ${r.comments}`.toLowerCase().includes(q));
    const rows = all.filter((s) => UW.inFilter(s.leg, s.time, f));
    stn.hidden = all.length - rows.length;
    const k = stn.sort.key, dir = stn.sort.dir;
    const val = (r) => k === "leg" ? r.legLabel : k === "cast" ? +r.cast : r[k];
    rows.sort((a, b) => { const x = val(a), y = val(b); if (x == null || x === "") return 1; if (y == null || y === "") return -1; return (x < y ? -1 : x > y ? 1 : 0) * dir; });
    return rows;
  }
  function renderStations() {
    const rows = stationRows();
    const arrow = (k) => stn.sort.key === k ? (stn.sort.dir > 0 ? " ▲" : " ▼") : "";
    const head = STATION_COLS.map(([k, l]) => `<th data-k="${esc(k)}" title="sort">${esc(l)}${arrow(k)}</th>`).join("");
    const cell = (r, k) => k === "leg" ? esc(r.legLabel) : k === "time" ? esc(fmtTs(UW.tms(r.time))) :
      k === "lat" || k === "lon" ? (r[k] != null ? (+r[k]).toFixed(4) : "") : k === "bottom_m" || k === "depth_m" ? (r[k] != null ? Math.round(+r[k]) : "") : esc(r[k] ?? "");
    const body = rows.map((r) => `<tr class="${r.cast && casts.sel.has(`${r.leg}:CTD_${String(r.cast).padStart(3, "0")}`) ? "sel" : ""} ${r.cast ? "" : "evst"}">${STATION_COLS.map(([k]) => `<td class="${["time", "lat", "lon", "bottom_m", "depth_m", "cast"].includes(k) ? "mono" : ""}">${cell(r, k)}</td>`).join("")}</tr>`).join("");
    $("#stationtable").innerHTML = `<thead><tr>${head}</tr></thead><tbody>${spanNote(rows.length, rows.length + stn.hidden, UW.currentFilter(), "tr", STATION_COLS.length)}${body}</tbody>`;
    const nsel0 = new Set([...casts.sel].map(parentId)).size;
    $("#stnclear").textContent = nsel0 ? `clear selection (${nsel0})` : "clear selection";
    $("#stnclear").classList.toggle("has", nsel0 > 0);
    topScroll($("#stationtable").closest(".tablewrap"));
    const nsel = rows.filter((r) => r.cast && casts.sel.has(`${r.leg}:CTD_${String(r.cast).padStart(3, "0")}`)).length;
    const nev = rows.filter((r) => !r.cast).length;
    $("#stnmeta").textContent = `${rows.length.toLocaleString()} stations${nev ? ` (${nev} without a cast)` : ""}${nsel ? ` · ${nsel} selected for the Casts tab` : ""} · click a row to select its cast (again to deselect), or to find a station on the map`;
    for (const th of $("#stationtable").querySelectorAll("th")) th.onclick = () => {
      const k = th.dataset.k; stn.sort = { key: k, dir: stn.sort.key === k ? -stn.sort.dir : (k === "time" ? -1 : 1) }; store.set("stn.sort", stn.sort); renderStations();
    };
    // a click selects the cast (and shows the station on the map); a click on
    // a selected row deselects it
    for (const [i, tr] of [...$("#stationtable").querySelectorAll("tbody tr:not(.spannote)")].entries()) tr.onclick = async () => {
      const r = rows[i], key = `${r.leg}:CTD_${String(r.cast).padStart(3, "0")}`;
      if (!r.cast) {                                      // a station without a cast: just find it on the map
        for (const x of $("#stationtable").querySelectorAll("tbody tr.on")) x.classList.remove("on");
        tr.classList.add("on"); UW.focusMap(r.lat, r.lon, r.station); return;
      }
      const was = casts.sel.has(key);
      if (!was) UW.focusMap(r.lat, r.lon, `Cast ${r.cast} ${r.station}`);
      await UW.onStationClick?.(key, { quiet: true, toggle: true });
      renderStations();
      if (!was) $("#stationtable").querySelectorAll("tbody tr:not(.spannote)")[i]?.classList.add("on");
    };
  }
  // exports are tab-separated text: a cell never holds a tab or a line break
  const tsvCell = (s) => String(s ?? "").replace(/[\t\r\n]+/g, " ");
  function saveTSV(name, head, rows) {
    const lines = [head.map(tsvCell).join("\t")].concat(rows.map((r) => r.map(tsvCell).join("\t")));
    const blob = new Blob([lines.join("\n") + "\n"], { type: "text/tab-separated-values" });
    const a = document.createElement("a"); a.href = URL.createObjectURL(blob); a.download = name; a.click();
    setTimeout(() => URL.revokeObjectURL(a.href), 5000);
  }
  function downloadStationsCSV() {
    saveTSV("stations.tsv", STATION_COLS.map(([k]) => k), stationRows().map((r) => STATION_COLS.map(([k]) => k === "leg" ? r.legLabel : r[k])));
  }
  function wireStations() {
    $("#stnsearch").oninput = debounce((e) => { stn.search = e.target.value; renderStations(); }, 150);
    $("#stnclear").onclick = () => { casts.sel.clear(); store.set("casts.sel", []); UW.clearFocus?.(); renderStations(); renderCastList(); if (!$("#pane-casts").hidden) renderCastPlots(); UW.renderMap(); };
    $("#stncsv").onclick = downloadStationsCSV;
  }

  // ================================================================ table
  // hourly or daily aggregates of the underway record
  const tbl = { rule: store.get("tbl.rule", "1h"), stat: +store.get("tbl.stat", 0), sort: store.get("tbl.sort", { key: "t", dir: -1 }), search: "", data: {}, loadedFor: null };
  if (!["1h", "1d"].includes(tbl.rule)) tbl.rule = "1h";
  async function ensureAgg() {
    const stamp = UW.M.generated_utc;
    const rule = tbl.rule;
    const data = await cachedJSON(`aggregate:${rule}`, UW.M.aggregates[rule].file);
    if (tbl.loadedFor !== stamp) tbl.data = {};
    tbl.data[rule] = data; tbl.loadedFor = stamp; tbl.legs = UW.M.legs;
  }
  function currentRows() {
    const d = tbl.data[tbl.rule]; if (!d) return [];
    const q = tbl.search.toLowerCase();
    const f = UW.currentFilter();
    // A failed refresh can leave older table data visible; its numeric leg
    // codes must still be interpreted with the matching manifest.
    let all = d.rows.filter((r) => f.legs.has(tbl.legs[r.leg]?.id)).map((r) => ({ ...r, legLabel: tbl.legs[r.leg]?.label || "" }));
    if (q) all = all.filter((r) => `${fmtTs(r.t)} ${r.legLabel}`.toLowerCase().includes(q));
    const rows = all.filter((r) => UW.inFilter(tbl.legs[r.leg]?.id, r.t, f));
    tbl.hidden = all.length - rows.length;
    const k = tbl.sort.key, dir = tbl.sort.dir;
    const val = (r) => k === "t" ? r.t : k === "leg" ? r.legLabel : k === "lat" || k === "lon" ? r[k] : (r[k] ? r[k][tbl.stat] : null);
    rows.sort((a, b) => { const x = val(a), y = val(b); if (x == null) return 1; if (y == null) return -1; return (x < y ? -1 : x > y ? 1 : 0) * dir; });
    return rows;
  }
  function renderTable() {
    const d = tbl.data[tbl.rule]; if (!d) return;
    const rows = currentRows();
    const stat = ["mean", "min", "max", "n"][tbl.stat];
    const cols = [["t", "time (ship)"], ["leg", "leg"], ["lat", "lat"], ["lon", "lon"], ...d.variables.map((v) => [v, v])];
    const arrow = (k) => tbl.sort.key === k ? (tbl.sort.dir > 0 ? " ▲" : " ▼") : "";
    const head = cols.map(([k, l]) => `<th data-k="${esc(k)}" title="sort">${esc(l)}${arrow(k)}</th>`).join("");
    const body = rows.slice(0, 2000).map((r) => `<tr><td class="mono">${fmtTs(r.t)}</td><td>${esc(r.legLabel)}</td><td class="mono">${r.lat ?? ""}</td><td class="mono">${r.lon ?? ""}</td>` +
      d.variables.map((v) => `<td class="mono">${r[v] ? (tbl.stat === 3 ? r[v][3] : fmtVal(r[v][tbl.stat], "")) : ""}</td>`).join("") + "</tr>").join("");
    $("#aggtable").innerHTML = `<thead><tr>${head}</tr></thead><tbody>${spanNote(rows.length, rows.length + tbl.hidden, UW.currentFilter(), "tr", cols.length)}${body}</tbody>`;
    topScroll($("#aggtable").closest(".tablewrap"));
    const shown = rows.slice(0, 2000);
    for (const [i, tr] of [...$("#aggtable").querySelectorAll("tbody tr:not(.spannote)")].entries()) tr.onclick = () => {
      const r = shown[i]; if (r.lat == null) return;
      for (const x of $("#aggtable").querySelectorAll("tbody tr.on")) x.classList.remove("on");
      tr.classList.add("on"); UW.focusMap(r.lat, r.lon, `${fmtTs(r.t)} · ${r.legLabel}`);
    };
    $("#tblmeta").textContent = `${rows.length.toLocaleString()} rows · ${stat}${rows.length > 2000 ? " · showing first 2000" : ""}`;
    for (const th of $("#aggtable").querySelectorAll("th")) th.onclick = () => {
      const k = th.dataset.k; tbl.sort = { key: k, dir: tbl.sort.key === k ? -tbl.sort.dir : (k === "t" ? -1 : 1) }; store.set("tbl.sort", tbl.sort); renderTable();
    };
  }
  function downloadCSV() {
    const d = tbl.data[tbl.rule]; if (!d) return;
    const stat = ["mean", "min", "max", "n"][tbl.stat];
    const rows = currentRows();
    const head = ["time_utc", "leg", "lat", "lon", ...d.variables.map((v) => `${v} (${stat})`)];
    saveTSV(`underway_${tbl.rule}_${stat}.tsv`, head, rows.map((r) => [new Date(r.t).toISOString(), r.legLabel, r.lat, r.lon, ...d.variables.map((v) => r[v] ? r[v][tbl.stat] : "")]));
  }
  function wireTable() {
    for (const b of $("#aggrule").querySelectorAll("button")) {
      b.classList.toggle("on", b.dataset.r === tbl.rule);
      b.onclick = () => { tbl.rule = b.dataset.r; store.set("tbl.rule", tbl.rule); for (const x of $("#aggrule").querySelectorAll("button")) x.classList.toggle("on", x === b); refreshActiveTab(true); };
    }
    $("#aggstat").value = String(tbl.stat);
    $("#aggstat").onchange = (e) => { tbl.stat = +e.target.value; store.set("tbl.stat", tbl.stat); renderTable(); };
    $("#tblsearch").oninput = debounce((e) => { tbl.search = e.target.value; renderTable(); }, 150);
    $("#tblcsv").onclick = downloadCSV;
  }

  // ================================================================ glue
  UW.onXMode = () => {};                                        // the section keeps its own x axis
  UW.onFilter = () => {
    // a leg switched off takes its casts out of the selection
    if (casts.idx) {
      const f = UW.currentFilter();
      const gone = [...casts.sel].filter((k) => { const c = castById(k); return c && !f.legs.has(c.leg); });
      if (gone.length) { for (const k of gone) casts.sel.delete(k); store.set("casts.sel", [...casts.sel]); if (!$("#pane-casts").hidden) renderCastPlots(); UW.renderMap(); }
    }
    refreshActiveTab(true);
  };
  let refreshedTab = null, tabSeq = 0;
  const activeTab = () => [...document.querySelectorAll("#tabs button.on")].find((b) => b.dataset.tab !== "chat")?.dataset.tab;
  async function refreshActiveTab(force = false) {
    const name = activeTab();
    if (!["casts", "stations", "calendar", "table"].includes(name)) return;
    const stamp = UW.M.generated_utc;
    const key = `${name}:${stamp}:${name === "table" ? tbl.rule : ""}`;
    if (!force && refreshedTab === key) return;
    const seq = ++tabSeq;
    const scope = { casts: "Casts", stations: "Stations", calendar: "Schedule", table: "Table" }[name];
    try {
      if (name === "casts") await ensureCastIndex();
      if (name === "calendar") await ensureCalendar();
      if (name === "table") await ensureAgg();
      if (seq !== tabSeq || name !== activeTab() || stamp !== UW.M.generated_utc) return;
      if (name === "casts") {
        renderCastList(); UW.renderMap();
        if (await renderCastPlots() === false) return;
      }
      if (name === "stations") renderStations();
      if (name === "calendar") renderCalendar();
      if (name === "table") renderTable();
      if (seq !== tabSeq || stamp !== UW.M.generated_utc) return;
      refreshedTab = key; UW.setLoadError(scope, false);
    } catch {
      if (seq === tabSeq) { refreshedTab = null; UW.setLoadError(scope, true); }
    }
  }
  UW.refreshActiveTab = refreshActiveTab;
  UW.onTab = (name) => {
    ++tabSeq; ++plotSeq; // invalidate work belonging to the tab being left
    for (const scope of ["Casts", "Stations", "Schedule", "Table"]) UW.setLoadError(scope, false);
    if (name !== "casts") clearTimeout(live.timer);
    refreshActiveTab(true);
  };
  wireCasts(); wireStations(); wireCalendar(); wireTable();
  const active = document.querySelector("#tabs button.on")?.dataset.tab;
  if (active && active !== "underway") UW.onTab(active);
})();
