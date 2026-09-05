/* Casts, Calendar and Table panes. Loaded after app.js; talks to it through
 * window.UW (state, manifest, helpers, and hooks the map calls back into). */
(() => {
  "use strict";
  const UW = window.UW;
  const $ = (s) => document.querySelector(s);
  const { THEME, CFG, fmtUTC, fmtVal, dms, store } = UW;
  const esc = (s) => String(s ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  const PALETTE = ["#5cc8ff", "#ffb454", "#7ee787", "#ff7b72", "#d2a8ff", "#f2cc60", "#79c0ff", "#ffa198", "#56d364", "#e3b341", "#a5d6ff", "#ff9bce"];
  const getJSON = UW.fetchJSON;
  const cachedJSON = window.UWData.generationCache(getJSON, () => UW.M.generated_utc);
  const debounce = (f, ms) => { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => f(...a), ms); }; };

  // ================================================================ casts
  const casts = {
    idx: null, loadedFor: null,
    sel: new Set(store.get("casts.sel", [])),
    mode: store.get("casts.mode", "profiles"),
    kind: store.get("casts.kind", "all"),
    variable: store.get("casts.var", "Temperature"),
    search: "",
  };
  // selection ids: a cast or tow id, or "<towid>#<dip index>" for one dip
  const parentId = (id) => id.split("#")[0];
  const castById = (id) => casts.idx?.casts.find((c) => c.id === parentId(id));
  const dipSel = (towId) => [...casts.sel].filter((s) => s.startsWith(towId + "#")).map((s) => +s.split("#")[1]).sort((a, b) => a - b);
  casts.open = new Set(store.get("casts.open", []));
  const castLabel = (c) => c.kind === "MVP" ? `MVP tow ${c.cast}${c.n_profiles ? ` · ${c.n_profiles} dips` : ""}`
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
    const f = UW.currentFilter();
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
        line: { width: 4, color: "#ffb454" } });
      const dips = tows.flatMap((c) => dipSel(c.id).map((i) => ({ c, i })));
      if (dips.length) out.push({ type: "scattermap", mode: "markers", name: "selected dips", showlegend: false, hoverinfo: "text",
        lat: dips.map(({ c, i }) => c.track[i]?.[0]), lon: dips.map(({ c, i }) => c.track[i]?.[1]),
        text: dips.map(({ c, i }) => `${castLabel(c)} · dip ${i + 1}`), marker: { size: 9, color: "#ffb454" } });
      out.push({ type: "scattermap", mode: "markers", name: "MVP tow starts", showlegend: false, hoverinfo: "text",
        lat: tows.map((c) => c.lat), lon: tows.map((c) => c.lon), customdata: tows.map((c) => c.id),
        text: tows.map((c) => `<b>${castLabel(c)}</b><br>${castDate(c)}<br>to ${maxDepth(c)} · click to select the tow`),
        marker: { size: tows.map((c) => isSelected(c) ? 11 : 7), color: tows.map((c) => isSelected(c) ? "#ffb454" : "#7ee787"), symbol: "circle" } });
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
  function renderCastList() {
    const ul = $("#castlist"); if (!casts.idx) return;
    const q = casts.search.toLowerCase();
    const f = UW.currentFilter();
    const rows = casts.idx.casts
      .filter((c) => UW.inFilter(c.leg, c.time_end || c.time, f) || UW.inFilter(c.leg, c.time, f))
      .filter((c) => casts.kind === "all" || c.kind === casts.kind)
      .filter((c) => !q || `${c.cast} ${c.station} ${c.label} ${c.time} ${c.leg}`.toLowerCase().includes(q))
      .sort((a, b) => (b.time || "").localeCompare(a.time || ""));
    const row = (c) => {
      const dips = dipSel(c.id), whole = casts.sel.has(c.id), part = dips.length > 0;
      const isTow = c.kind === "MVP" && c.n_profiles;
      const state = whole ? "on" : part ? "part" : "";
      let html = `<li class="${state}" data-id="${esc(c.id)}">
        ${isTow ? `<button class="tog" data-tow="${esc(c.id)}" title="show dips">${casts.open.has(c.id) ? "▾" : "▸"}</button>` : '<span class="tog"></span>'}
        <input type="checkbox" ${whole ? "checked" : ""} ${part ? 'class="partial"' : ""} title="${isTow ? "whole tow" : "select"}">
        <span class="kind ${c.kind}">${c.kind === "CTD" ? "ROS" : c.kind}</span>
        <span class="name">${esc(castLabel(c))}${part ? ` <small>${dips.length}/${c.n_profiles} dips</small>` : ""}</span>
        <span class="meta-row"><span class="when">${esc(castDate(c))}</span><span class="depth">${maxDepth(c)}</span><span class="leg">${esc(UW.legById(c.leg)?.label || c.leg)}</span></span></li>`;
      if (isTow && casts.open.has(c.id)) {
        const picked = new Set(dips);
        html += c.track.map((t, i) => `<li class="dip ${picked.has(i) ? "on" : ""}" data-id="${esc(c.id)}#${i}">
          <span class="tog"></span><input type="checkbox" ${picked.has(i) ? "checked" : ""}>
          <span class="kind dip">#${i + 1}</span><span class="name">dip ${i + 1}</span>
          <span class="meta-row"><span class="when">${t[0] != null ? `${t[0].toFixed(3)}, ${t[1].toFixed(3)}` : ""}</span></span></li>`).join("");
      }
      return html;
    };
    ul.innerHTML = rows.map(row).join("") + (!rows.length ? '<li class="more">no casts match</li>' : "");
    for (const li of ul.querySelectorAll("li[data-id]")) li.onclick = (e) => {
      if (e.target.closest(".tog")) return;
      e.preventDefault(); toggleCast(li.dataset.id);
    };
    for (const b of ul.querySelectorAll("button.tog")) b.onclick = (e) => {
      e.stopPropagation();
      const id = b.dataset.tow; casts.open.has(id) ? casts.open.delete(id) : casts.open.add(id);
      store.set("casts.open", [...casts.open]); renderCastList();
    };
    const nsel = new Set([...casts.sel].map(parentId)).size;
    $("#castclear").textContent = nsel ? `clear (${nsel})` : "clear";
  }

  // ------------------------------------------------------------ overlay views
  // Live: the cast in the water (or the last one back on deck) from the deck
  // unit's UDP feed via api/live, polled every 2 s while on screen. Single:
  // one selected cast from the archive. Both draw the chosen variables over
  // depth on their own x axes (bottom, top, then outwards).
  const live = { vars: store.get("casts.live.vars", ["temperature", "salinity"]), which: "current", data: null, timer: null, showCfg: false };
  const single = { vars: store.get("casts.single.vars", ["Temperature", "Salinity"]), id: null, dip: null };
  const LIVE_SKIP = new Set(["scan", "time", "t", "pressure", "prdm", "prm", "pr", "p", "depth", "depsm", "depth_m"]);
  const liveVisible = () => casts.mode === "live" && !$("#pane-casts").hidden;

  // spec: { depth[], vars: {name: values}, units: {name}, splitAt (index of the deepest point, or null), nowDepth, sub }
  function drawOverlay(body, plotId, title, spec, chosen) {
    if (!body.querySelector(`#${plotId}`)) body.innerHTML = castPanelHtml(plotId, title, "", false).replace('class="panel card castplot', 'class="panel card castplot wide tall');
    const vars = chosen.filter((v) => spec.vars[v]);
    const gdEl = $(`#${plotId}`);
    // each extra axis needs ~58 px of ticks and title: the plot area gives up
    // that much per axis and the margins hold the outermost ones
    const H = Math.max(360, gdEl.clientHeight || 500), step = 64 / H;
    const nb = Math.ceil(vars.length / 2), nt = Math.floor(vars.length / 2);
    const y0 = step * Math.max(0, nb - 1), y1 = 1 - step * Math.max(0, nt - 1);
    const traces = [], layout = { ...CAST_LAYOUT, hovermode: "closest", margin: { l: 56, r: 16, t: 64, b: 64 }, showlegend: false };
    const maxD = Math.max(1, ...spec.depth.filter((x) => x != null));
    layout.yaxis = depthAxis(maxD * 1.04, { domain: [y0, y1] });
    const yv = spec.depth.map(yT);
    vars.forEach((v, i) => {
      const ax = i === 0 ? "x" : `x${i + 1}`, key = i === 0 ? "xaxis" : `xaxis${i + 1}`, color = PALETTE[i % PALETTE.length];
      const bottom = i % 2 === 0, k = Math.floor(i / 2);
      const unit = spec.units?.[v] ? ` (${spec.units[v]})` : "";
      // each axis carries a baseline in its colour, ticks tight against it and the
      // title tight against the ticks, so the stacked axes read as groups
      layout[key] = { ...THEME.xaxis, title: { text: v + unit, font: { size: 12, color }, standoff: 2 }, tickfont: { size: 11, color }, ticks: "outside", ticklen: 3, tickcolor: color,
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
      traces.push({ type: "scatter", mode: "markers", xaxis: "x", yaxis: "y", x: [spec.vars[vars[0]][li]], y: [yT(spec.nowDepth)], marker: { size: 11, color: "#ffb454", symbol: "diamond" }, hoverinfo: "skip", name: "now" });
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
    host.innerHTML = `<div class="livebar" id="livebar"><div id="livestatus"></div><div id="livecfgbox"></div></div><div id="livebody"></div>`;
    pollLive();
  }
  // the settings form is built once when opened and left alone while the
  // page refreshes, so an edit in progress survives
  function liveCfgForm(host, d) {
    const box = host.querySelector("#livecfgbox");
    if (!live.showCfg) { box.innerHTML = ""; return; }
    if (box.querySelector("form")) { const pre = box.querySelector("pre"); if (pre) pre.textContent = (d.raw || []).join("\n"); return; }
    box.innerHTML = `<form class="livecfgform" id="livecfgform"><label>UDP port <input name="port" value="${d.port || ""}" size="6"></label>
        <label>columns, in Seasave's output order <input name="columns" value="${esc((d.columns || []).join(","))}" size="60"></label><button type="submit">apply</button>
        <button type="button" class="chip" id="livecfgclose">close</button><span role="alert" id="livecfgerror"></span>
        <details><summary>last raw packets</summary><pre class="mono">${esc((d.raw || []).join("\n"))}</pre></details></form>`;
    box.querySelector("#livecfgclose").onclick = () => { live.showCfg = false; liveCfgForm(host, d); };
    box.querySelector("form").onsubmit = async (ev) => { ev.preventDefault(); const f = new FormData(ev.target);
      const button = ev.target.querySelector('[type="submit"]'), error = box.querySelector("#livecfgerror");
      button.disabled = true; error.textContent = "";
      const controller = new AbortController(), timeout = setTimeout(() => controller.abort(), 10000);
      try {
        const response = await fetch("api/live", { method: "POST", signal: controller.signal, headers: { "Content-Type": "application/json" }, body: JSON.stringify({ port: f.get("port").trim() || "0", columns: f.get("columns") }) });
        const result = await response.json();
        if (!response.ok) throw new Error(result.error || `Configuration failed (${response.status})`);
        live.data = result; live.showCfg = false; drawLive(host); pollLive();
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
    const age = d.last_packet_age_s;
    const feed = d.port ? `listening on UDP ${d.port}${d.packets ? ` · ${d.packets.toLocaleString()} packets · ${d.rate_hz} Hz · last ${age != null ? age.toFixed(0) + " s ago" : "—"}${d.source ? " from " + esc(d.source) : ""}` : " · nothing received yet"}` : "feed off — set the UDP port with ⚙";
    const cols = (cast?.columns || d.columns || []).filter((c) => !LIVE_SKIP.has(c.toLowerCase()));
    st.innerHTML = `<div class="livestatus"><span class="dot ${d.packets && age != null && age < 10 ? "on" : ""}"></span><span>${feed}</span>
        ${cast ? `<span class="muted">· ${which} · ${cast.n.toLocaleString()} scans kept${cast.max_p ? ` · max ${cast.max_p.toFixed(0)} ${cast.depth_like ? "m" : "dbar"}` : ""}${cast.direction ? ` · ${cast.direction === "down" ? "↓ descending" : cast.direction === "up" ? "↑ ascending" : "holding"}` : ""}</span>` : ""}
        <button type="button" class="chip" id="livecfg" title="port and column names">⚙</button></div>
      <div class="livevars">${varChips(cols, live.vars, "livevar")}${d.current && d.last ? ` <span class="muted">show:</span> <button type="button" class="chip ${live.which === "current" ? "on" : ""}" data-w="current">in water</button><button type="button" class="chip ${live.which === "last" ? "on" : ""}" data-w="last">last</button>` : ""}</div>`;
    for (const b of st.querySelectorAll(".livevar")) b.onclick = () => { live.vars = live.vars.includes(b.dataset.v) ? live.vars.filter((x) => x !== b.dataset.v) : [...live.vars, b.dataset.v]; store.set("casts.live.vars", live.vars); drawLive(host); };
    for (const b of st.querySelectorAll("[data-w]")) b.onclick = () => { live.which = b.dataset.w; drawLive(host); };
    st.querySelector("#livecfg").onclick = () => { live.showCfg = !live.showCfg; liveCfgForm(host, d); };
    liveCfgForm(host, d);
    if (!cast || !cast.t.length) {
      body.innerHTML = `<div class="empty">${d.packets ? "Packets arrive but no cast is in the water yet — the plot starts when pressure passes 2 dbar." : "Waiting for the deck unit. Start acquisition in Seasave; the plot begins when the package goes in."}</div>`;
      return;
    }
    const P = cast.cols[cast.pressure_col || d.pressure_col] || [], lat = UW.M.latest?.lat ?? 70;
    const depth = cast.depth_like ? P : P.map((p) => p == null ? null : depthFrom(p, lat));
    let imax = 0; for (let i = 0; i < P.length; i++) if (P[i] != null && P[i] > (P[imax] ?? -1)) imax = i;
    const li = depth.length - 1;
    drawOverlay(body, "live-plot", "Live cast", { depth, vars: cast.cols, units: {}, splitAt: imax, nowDepth: depth[li],
      sub: `${depth[li] != null ? depth[li].toFixed(1) + " m now" : ""}${cast.started ? " · started " + new Date(cast.started * 1000).toISOString().slice(11, 16) + "Z" : ""}` }, live.vars);
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
      { depth: depths(prof), vars: prof.vars, units: pick.units || {}, splitAt: null, nowDepth: null,
        sub: `${when}${(prof.bottom_m || pick.bottom_m) ? ` · bottom ${Math.round(prof.bottom_m || pick.bottom_m)} m` : ""}` }, single.vars);
    wireCastPanels(host, () => renderSingle(host, data));
  }

  let plotSeq = 0;
  async function renderCastPlots() {
    const seq = ++plotSeq, stamp = UW.M.generated_utc;
    fillCastVars();
    if (casts.mode !== "live") clearTimeout(live.timer);
    const host = $("#castplots");
    const sel = orderedSelection();
    $("#castvarwrap").hidden = casts.mode !== "section";
    $("#castxmode").hidden = casts.mode !== "section";          // the section's x axis follows Time/Distance
    if (casts.mode === "live") { $("#castmeta").textContent = "the cast in the water, from the deck unit"; return renderLive(host); }
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
  const depths = (prof) => prof.p.map((p) => depthFrom(p, prof.lat ?? prof.parent?.lat));
  // Depth axes are linear or compressed (square root of depth, so the upper
  // water column gets room); the switch is shared by every cast view. yT maps
  // a depth onto the axis, depthAxis labels it in metres.
  casts.dscale = store.get("casts.dscale", "linear");
  const yT = (d) => d == null ? null : casts.dscale === "sqrt" ? Math.sqrt(Math.max(0, d)) : d;
  const DEPTH_TICKS = [0, 5, 10, 20, 30, 50, 75, 100, 150, 200, 300, 400, 500, 750, 1000, 1500, 2000, 3000, 4000, 5000];
  function depthAxis(maxD, extra = {}) {
    const ax = { ...THEME.yaxis, title: { text: casts.dscale === "sqrt" ? "depth (m, compressed)" : "depth (m)", font: { size: 12 }, standoff: 2 }, tickfont: { size: 12 },
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
  const castPanelState = { wide: new Set(store.get("casts.wide", [])), min: new Set(store.get("casts.min", [])), order: store.get("casts.order", []) };
  const saveCastPanels = () => { store.set("casts.wide", [...castPanelState.wide]); store.set("casts.min", [...castPanelState.min]); store.set("casts.order", castPanelState.order); };
  const castOrder = (vars) => { const o = castPanelState.order.filter((v) => vars.includes(v)); return [...o, ...vars.filter((v) => !o.includes(v))]; };

  // same frame and controls as the underway panels
  // toolbar order everywhere: reset, compress (depth graphs only), minimise, maximise
  function castPanelHtml(id, title, unit, wideable = true, movable = false, depth = true) {
    return `<section class="panel card castplot ${castPanelState.wide.has(id) ? "wide" : ""} ${title === "Temperature" ? "on" : ""}" data-cp="${esc(id)}" data-var="${esc(title)}" ${movable ? 'draggable="true"' : ""}>
      <div class="head">${movable ? '<span class="handle" title="drag to reorder">⋮⋮</span>' : ""}<h3>${esc(title)}</h3><div class="tools"><span class="now">${esc(unit)}</span>
        <button class="reset" title="reset zoom">⟲</button>${depth ? `<button class="dscale ${casts.dscale === "sqrt" ? "on" : ""}" title="compress the depth axis (square root) — applies to every cast graph">⇅</button>` : ""}${movable ? '<button class="min" title="minimise to the bottom bar">—</button>' : ""}${wideable ? '<button class="wide" title="expand">⤢</button>' : ""}</div></div>
      <div class="plot" id="${esc(id)}"></div></section>`;
  }
  function wireCastPanels(host, rerender, vars = []) {
    for (const sec of host.querySelectorAll(".castplot")) {
      const id = sec.dataset.cp, v = sec.dataset.var;
      sec.querySelector(".reset").onclick = () => Plotly.relayout(sec.querySelector(".plot"), { "xaxis.autorange": true, "yaxis.autorange": true });
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
          const order = castOrder(vars).filter((x) => x !== from);
          const at = order.indexOf(v);
          order.splice(e.offsetX < sec.clientWidth / 2 ? at : at + 1, 0, from);
          castPanelState.order = order; saveCastPanels(); rerender();
        });
      }
    }
    for (const chip of host.querySelectorAll(".dock .chip")) chip.onclick = () => { castPanelState.min.delete(chip.dataset.var); saveCastPanels(); rerender(); };
  }
  const CAST_LAYOUT = { ...THEME, margin: { l: 52, r: 8, t: 6, b: 36 }, showlegend: false, dragmode: "pan" };

  function renderProfiles(host, data) {
    const all = castOrder(orderVars(new Set(data.flatMap((d) => Object.keys(d.units)))));
    const vars = all.filter((v) => !castPanelState.min.has(v));
    const minimised = all.filter((v) => castPanelState.min.has(v));
    host.innerHTML = vars.map((v) => castPanelHtml(`cp-${v.replace(/\W+/g, "_")}`, v, data.find((d) => d.units[v])?.units[v] || "", true, true)).join("") +
      (minimised.length ? `<div class="dock castdock">${minimised.map((v) => `<button class="chip" data-var="${esc(v)}" title="restore">${esc(v)} <span>▲</span></button>`).join("")}</div>` : "") +
      `<div class="castlegend">${data.map((d, i) => `<span><i style="background:${PALETTE[i % PALETTE.length]}"></i>${esc(castLabel(d))} <small>${esc(castDate(d))}</small></span>`).join("")}</div>`;
    for (const v of vars) {
      const traces = [];
      data.forEach((d, i) => {
        const ps = profilesOf(d);
        ps.forEach((p, j) => {
          if (!p.vars[v]) return;
          traces.push({
            type: "scatter", mode: "lines", name: p.label, x: p.vars[v], y: depths(p).map(yT), customdata: depths(p), connectgaps: false,
            line: { width: ps.length > 1 ? 1 : 1.6, color: ps.length > 1 ? towShade(PALETTE[i % PALETTE.length], j, ps.length) : PALETTE[i % PALETTE.length] },
            opacity: ps.length > 1 ? 0.8 : 1,
            hovertemplate: `${esc(p.label)}<br>%{x:.3~f} ${esc(d.units[v] || "")} at %{customdata:.0f} m<extra></extra>`,
          });
        });
      });
      const layout = { ...CAST_LAYOUT, hovermode: "closest",
        xaxis: { ...THEME.xaxis, title: { text: data.find((d) => d.units[v])?.units[v] || "", font: { size: 12 }, standoff: 4 }, tickfont: { size: 12 } },
        yaxis: depthAxis(Math.max(1, ...data.flatMap((d) => profilesOf(d).flatMap((p) => p.vars[v] ? depths(p) : []))) * 1.02) };
      Plotly.react(host.querySelector(`#cp-${v.replace(/\W+/g, "_")}`), traces, layout, CFG).then((gd) => UW.axisZoom(gd));
    }
    wireCastPanels(host, () => renderProfiles(host, data), all);
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

  function renderSection(host, data) {
    const v = casts.variable;
    // tows contribute every dip; everything is ordered by time
    const withVar = data.flatMap(profilesOf).filter((d) => d.vars[v]).sort((a, b) => (a.time || "").localeCompare(b.time || ""));
    if (withVar.length < 2) { host.innerHTML = `<div class="empty">A section needs at least two profiles with ${esc(v)} — ${withVar.length} selected.</div>`; return; }
    // depth grid (metres) shared by every profile
    const maxD = Math.max(...withVar.map((d) => depthFrom(d.p[d.p.length - 1], d.lat ?? d.parent?.lat)));
    const step = maxD > 1500 ? 5 : maxD > 400 ? 2 : 1;
    const grid = []; for (let d = 0; d <= maxD; d += step) grid.push(d);
    const onDepthGrid = (prof) => onGrid({ p: depths(prof), vars: prof.vars }, v, grid);
    // x follows the header's Time/Distance switch: distance is cumulative
    // along the profiles in time order, time is each profile's own
    const byTime = UW.state.xmode === "time";
    const km = [0];
    for (let i = 1; i < withVar.length; i++) {
      const a = withVar[i - 1], b = withVar[i];
      km.push(km[i - 1] + (a.lat != null && b.lat != null ? haversine(a, b) : 1));
    }
    const tms = withVar.map((d, i) => d.time ? Date.parse(d.time + (d.time.endsWith("Z") ? "" : "Z")) : i);
    const xs = byTime ? tms : km;
    const xTitle = byTime ? "time (UTC)" : "distance along section (km)";
    const xFmt = (i) => byTime ? (withVar[i].time || "").replace("T", " ").slice(0, 16) : `${km[i].toFixed(0)} km`;
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
    const xPts = byTime ? tms.map((t) => new Date(t)) : km;
    const dense = withVar.length > 24;      // a tow: label only every few dips
    host.innerHTML = castPanelHtml("cs-plot", `${v} section`, `${withVar.length} profiles · ${km.at(-1).toFixed(0)} km · ${unit}`, false).replace('class="panel card castplot', 'class="panel card castplot wide') +
      (dense ? "" : `<div class="castlegend">${withVar.map((d, i) => `<span><b>${i + 1}</b> ${esc(d.label)} <small>${esc(xFmt(i))}${byTime ? ` · ${km[i].toFixed(0)} km` : ""}</small></span>`).join("")}</div>`);
    const traces = [
      { type: "heatmap", x: xPlot, y: grid.map(yT), z, customdata: grid.map((g) => xg.map(() => g)), colorscale: "Viridis", connectgaps: false, zsmooth: "best",
        colorbar: { title: { text: unit, side: "right" }, thickness: 12, len: .8, tickfont: { size: 12 }, outlinewidth: 0 },
        hovertemplate: (byTime ? "%{x|%m-%d %H:%M}Z" : "%{x:.1f} km") + ` · %{customdata:.0f} m<br><b>%{z:.3~f} ${esc(unit)}</b><extra></extra>` },
      { type: "scatter", mode: dense ? "markers" : "markers+text", x: xPts, y: withVar.map(() => 0), text: withVar.map((_, i) => String(i + 1)), textposition: "top center",
        textfont: { size: 10, color: "#c9d4e0" }, marker: { symbol: "triangle-down", size: dense ? 5 : 9, color: "#ffb454" },
        hovertext: withVar.map((d) => `${d.label}<br>${d.time ? d.time.replace("T", " ").slice(0, 16) : ""}`), hoverinfo: "text", cliponaxis: false },
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
      line: { color: "#3b4658", width: 1.5, shape: "linear" }, fill: "tonexty", fillcolor: "rgba(43,52,65,.92)",
      marker: { size: sounded.map((b) => b ? 5 : 0), color: "#8ea3ba", symbol: "diamond" },
      hovertext: withVar.map((d, i) => sounded[i] ? `${d.label}<br>bottom ${Math.round(d.bottom_m)} m` : `${d.label}<br>deepest sample ${Math.round(bottoms[i])} m`), hoverinfo: "text" });
    const layout = { ...CAST_LAYOUT, margin: { l: 54, r: 8, t: 18, b: 40 },
      xaxis: { ...THEME.xaxis, title: { text: xTitle, font: { size: 12 }, standoff: 4 }, tickfont: { size: 12 }, type: byTime ? "date" : "linear" },
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
    for (const b of $("#castkind").querySelectorAll("button")) {
      b.classList.toggle("on", b.dataset.k === casts.kind);
      b.onclick = () => { casts.kind = b.dataset.k; store.set("casts.kind", casts.kind); for (const x of $("#castkind").querySelectorAll("button")) x.classList.toggle("on", x === b); renderCastList(); UW.renderMap(); };
    }
    $("#castsearch").oninput = debounce((e) => { casts.search = e.target.value; renderCastList(); }, 150);
    $("#castclear").onclick = () => { casts.sel.clear(); store.set("casts.sel", []); renderCastList(); renderCastPlots(); UW.renderMap(); };
  }

  // ================================================================ calendar
  const cal = { data: null, loadedFor: null, view: store.get("cal.view", "agenda"), search: "", month: store.get("cal.month", new Date().toISOString().slice(0, 7)),
    span: store.get("cal.span", "days"), day: store.get("cal.day", new Date().toISOString().slice(0, 10)) };
  async function ensureCalendar() {
    const stamp = UW.M.generated_utc;
    if (cal.data && cal.loadedFor === stamp) return;
    const data = await cachedJSON("calendar", UW.M.calendar.file);
    cal.data = data; cal.loadedFor = stamp;
  }
  function renderCalendar() {
    const host = $("#calendar"); if (!cal.data) return;
    const s = cal.data.schedule || {};
    $("#calmeta").textContent = `${cal.data.events.length} logged events · schedule ${s.updated ? "updated " + s.updated : "unavailable"}${s.stale ? " (cached copy)" : ""}`;
    const q = cal.search.toLowerCase();
    const f = UW.currentFilter();
    const evs = cal.data.events.filter((e) => UW.inFilter(e.leg, e.time_utc, f)).filter((e) => !q || JSON.stringify(e).toLowerCase().includes(q));
    if (cal.view === "timeline") return renderTimeline(host, evs, s);
    if (cal.view === "month") return renderMonth(host, q);
    const isoDay = (r) => { const t = UW.tms(r.start_utc); return isNaN(t) ? esc(r.date) : new Date(t).toISOString().slice(0, 10); };
    const sched = (s.rows || []).map((r) => `<tr class="st-${esc((r.status || "").toLowerCase().replace(/\s+/g, "-"))}"><td>${isoDay(r)}</td><td>${esc(r.start)}–${esc(r.end)}</td><td>${esc(r.station)}</td><td>${esc(r.operation)}</td><td><span class="status">${esc(r.status)}</span></td><td>${r.duration_h != null ? r.duration_h.toFixed(1) + " h" : ""}</td><td class="muted">${esc(r.comment)}</td></tr>`).join("");
    let html = `<section class="card block"><h3>Operations schedule ${esc(s.title || "")}</h3>` +
      (sched ? `<table class="sched"><tr><th>date</th><th>time</th><th>station</th><th>operation</th><th>status</th><th>dur.</th><th>comment</th></tr>${sched}</table>` : '<p class="muted">no scheduled operations listed</p>') +
      (s.whiteboard ? `<p class="whiteboard">📋 ${esc(s.whiteboard)}</p>` : "") +
      `<p class="muted small">Ship intranet: ${(UW.M.intranet || []).map((l) => `<a href="${esc(l.url)}" target="_blank" rel="noopener">${esc(l.label)}</a>`).join(" · ")}` +
      ` &nbsp;·&nbsp; calendars: ${(UW.M.links || []).map((l) => `<a href="${esc(l.url)}" target="_blank" rel="noopener">${esc(l.label)}</a>`).join(" · ")}</p></section>`;
    // agenda: logged events and scheduled operations (current and former)
    // grouped by UTC day, newest first
    // every source has its own time format (the event log writes 2026/09/03
    // 11:23:12, the schedule ISO), so group and sort on instants
    const byDay = new Map();
    const iso = (t) => isNaN(t) ? "" : new Date(t).toISOString();
    const hm = (t) => iso(t).slice(11, 16);
    const add = (t, x) => { const d = iso(t).slice(0, 10) || "undated"; if (!byDay.has(d)) byDay.set(d, []); byDay.get(d).push({ t: isNaN(t) ? 0 : t, ...x }); };
    for (const e of evs) add(UW.tms(e.time_utc), { e });
    for (const r of scheduledRows(s)) if ((!q || JSON.stringify(r).toLowerCase().includes(q)) && (UW.inFilter(null, r.start_utc, f) || UW.inFilter(null, r.end_utc, f) || UW.tms(r.start_utc) > f.end)) add(UW.tms(r.start_utc), { r });
    const days = [...byDay.keys()].sort().reverse().slice(0, 60);
    const evHtml = (e) => `<div class="ev" data-lat="${e.lat ?? ""}" data-lon="${e.lon ?? ""}" title="show on map">
          <span class="t">${hm(UW.tms(e.time_utc))}Z</span>
          <span class="st">${esc(e.station || "")}</span>
          <span class="what">${esc(e.activity || "")}${e.event ? " · " + esc(e.event) : ""}${e.label ? ` <code>${esc(e.label)}</code>` : ""}</span>
          <span class="pos muted">${e.lat != null && e.lon != null ? dms(+e.lat, +e.lon) : ""}${e.depth_m != null ? " · " + Math.round(+e.depth_m) + " m" : ""}</span>
          ${e.comment ? `<span class="cm muted">${esc(e.comment)}</span>` : ""}</div>`;
    const schedHtml = (r) => `<div class="ev sched ${r.former ? "former" : ""}">
          <span class="t">${hm(UW.tms(r.start_utc))}Z</span>
          <span class="st">${esc(r.station || "")}</span>
          <span class="what">${esc(r.operation || "")} <span class="badge">${r.former ? "was scheduled" : "scheduled"}</span> <span class="status">${esc(r.status || "")}</span></span>
          <span class="pos muted">${hm(UW.tms(r.start_utc))}–${hm(UW.tms(r.end_utc))}Z${r.duration_h != null ? " · " + r.duration_h.toFixed(1) + " h" : ""}</span>
          ${r.comment ? `<span class="cm muted">${esc(r.comment)}</span>` : ""}</div>`;
    html += `<section class="agenda">` + days.map((d) => { const items = byDay.get(d).sort((a, b) => b.t - a.t); const first = items.find((x) => x.e)?.e;
      return `<div class="day"><h4>${esc(d)} <small>${items.filter((x) => x.e).length} events · ${items.filter((x) => x.r).length} scheduled · ${esc(UW.legById(first?.leg)?.label || items.find((x) => x.r)?.r.leg || "")}</small></h4>` +
        items.map((x) => x.e ? evHtml(x.e) : schedHtml(x.r)).join("") + `</div>`; }).join("") + `</section>`;
    host.innerHTML = html;
    for (const el of host.querySelectorAll(".ev[data-lat]")) el.onclick = () => { if (el.dataset.lat) UW.focusMap(el.dataset.lat, el.dataset.lon, el.querySelector(".st")?.textContent); };
  }
  // current rows plus the former ones the history remembers, each with UTC instants
  const scheduledRows = (s) => [...(s.rows || []), ...(s.former || [])].filter((r) => r.start_utc && r.end_utc);
  function renderTimeline(host, evs, s) {
    const now = Date.now();
    const when = (e) => new Date(UW.tms(e.time_utc));
    const recent = evs;                                   // already the legs and span on display
    const counts = new Map(); for (const e of recent) counts.set(e.activity || "other", (counts.get(e.activity || "other") || 0) + 1);
    const top = [...counts.entries()].sort((a, b) => b[1] - a[1]).slice(0, 19).map(([t]) => t);
    const typeOf = (e) => top.includes(e.activity || "other") ? (e.activity || "other") : "other";
    const types = [...new Set(recent.map(typeOf))];
    const traces = types.map((t, i) => {
      const es = recent.filter((e) => typeOf(e) === t);
      return { type: "scatter", mode: "markers", name: t, x: es.map(when), y: es.map(() => t),
        text: es.map((e) => `${esc(e.station || "")} · ${esc(e.event || "")} ${esc(e.label || "")}`), hovertemplate: "%{x|%Y-%m-%d %H:%M}Z<br>%{text}<extra>" + esc(t) + "</extra>",
        marker: { size: 8, color: PALETTE[i % PALETTE.length] } };
    });
    // scheduled operations as bars on their own row: current ones bright,
    // former ones (off the intranet page now) dimmer
    const f = UW.currentFilter();
    const rows = scheduledRows(s).map((r) => ({ r, d0: new Date(r.start_utc), d1: new Date(r.end_utc) })).filter((b) => b.d1 >= f.start);
    if (rows.length) traces.push({ type: "bar", orientation: "h", name: "scheduled", base: rows.map((b) => b.d0), x: rows.map((b) => b.d1 - b.d0), y: rows.map(() => "scheduled"),
      text: rows.map((b) => `${esc(b.r.station)} · ${esc(b.r.operation)} (${esc(b.r.status)})${b.r.former ? " · was scheduled" : ""}<br>${new Date(UW.tms(b.r.start_utc)).toISOString().slice(0, 16).replace("T", " ")}–${new Date(UW.tms(b.r.end_utc)).toISOString().slice(11, 16)}Z`),
      hovertemplate: "%{text}<extra></extra>", textposition: "none", marker: { color: rows.map((b) => b.r.former ? "rgba(255,180,84,.35)" : "rgba(255,180,84,.8)"), line: { color: "#ffb454", width: 1 } }, width: .5 });
    host.innerHTML = castPanelHtml("cal-plot", "Timeline", `${recent.length} events · ${rows.length} scheduled · ${f.label} span`, false, false, false).replace('class="panel card castplot', 'class="panel card castplot wide');
    const layout = { ...CAST_LAYOUT, margin: { l: 130, r: 10, t: 10, b: 40 }, barmode: "overlay",
      xaxis: { ...THEME.xaxis, type: "date", title: { text: "UTC", font: { size: 12 } }, tickfont: { size: 12 } },
      yaxis: { ...THEME.yaxis, type: "category", categoryorder: "array", categoryarray: ["scheduled", ...types.slice().reverse()], tickfont: { size: 12 }, fixedrange: true },
      shapes: [{ type: "line", xref: "x", x0: new Date(now), x1: new Date(now), yref: "paper", y0: 0, y1: 1, line: { color: "#7ee787", width: 1.5, dash: "dot" } }] };
    Plotly.react($("#cal-plot"), traces, layout, CFG).then((gd) => UW.axisZoom(gd));
    wireCastPanels(host, () => renderTimeline(host, evs, s));
  }
  // Calendar view: a month grid or three days centred on a day, from the
  // Google calendars (imported at build time) and the intranet schedule.
  // Click an entry for its details.
  const GCAL_COLOUR = { schedule: "#5cc8ff", surprise: "#ffb454", intranet: "#8ea3ba" };
  function calendarItems(q) {
    const items = [];
    for (const f of cal.data.gcal || []) for (const e of f.events || []) items.push({ ...e, cal: f.key, label: f.label });
    // the intranet rows are also pushed to the Amundsen Schedule calendar; the
    // row itself is the fresher copy (the feed lags the push by minutes), so
    // a feed event that is one of our rows gives way to the row
    const rows = scheduledRows(cal.data.schedule || {});
    const isRowCopy = (e) => e.cal === "schedule" && rows.some((r) => Math.abs(UW.tms(e.start) - UW.tms(r.start_utc)) < 90e3 && (e.summary || "").includes(r.station || "\u0000"));
    for (let i = items.length - 1; i >= 0; i--) if (isRowCopy(items[i])) items.splice(i, 1);
    for (const r of rows) items.push({ start: r.start_utc, end: r.end_utc, summary: `${r.former ? "was scheduled" : "scheduled"} · ${r.station} — ${r.operation} (${r.status})`,
      description: [r.comment, `${r.duration_h != null ? r.duration_h.toFixed(1) + " h" : ""}`].filter(Boolean).join("\n"), cal: "intranet", label: "intranet schedule" });
    items.forEach((e, i) => { e.id = i; });
    return items.filter((e) => !q || `${e.summary} ${e.description || ""}`.toLowerCase().includes(q));
  }
  const dayKey = (d) => d.toISOString().slice(0, 10);
  const evStart = (e) => new Date(e.all_day ? e.start + "T00:00:00Z" : e.start);
  const evEnd = (e) => e.end ? new Date(e.all_day ? e.end + "T00:00:00Z" : e.end) : evStart(e);
  // block text without the prefixes the layout already conveys: a leading
  // "[status] ", "scheduled · " / "was scheduled · ", and a trailing " at <date>"
  const shortSummary = (t) => String(t || "").replace(/^\[[^\]]*\]\s*/, "").replace(/^(was )?scheduled · /, "").replace(/ at \d{4}-\d{2}-\d{2}[^,;]*$/, "");
  function entryHtml(e, cont) {
    return `<div class="mev" data-id="${e.id}" style="border-color:${GCAL_COLOUR[e.cal] || "#8ea3ba"}" title="${esc(e.label)}\n${esc(e.summary)}">` +
      `<span class="mt">${cont || e.all_day ? "" : evStart(e).toISOString().slice(11, 16) + "Z"}</span> ${esc(e.summary || "")}</div>`;
  }
  function detailHtml(e) {
    const t0 = evStart(e), t1 = evEnd(e);
    const when = e.all_day ? `${e.start}${e.end && e.end !== e.start ? " → " + e.end : ""} (all day)` :
      `${t0.toISOString().slice(0, 16).replace("T", " ")}Z → ${t1.toISOString().slice(0, 16).replace("T", " ")}Z · ${((t1 - t0) / 3600e3).toFixed(1)} h`;
    const pos = /Position:\s*([\d.]+)°([NS]),\s*([\d.]+)°([EW])/.exec(e.description || "");
    return `<div class="mdetail"><button class="mclose" title="close">✕</button>
      <div class="mdlabel" style="color:${GCAL_COLOUR[e.cal] || "#8ea3ba"}">${esc(e.label)}</div>
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
        <span class="mlegend">${feeds.map((f) => `<i style="border-color:${GCAL_COLOUR[f.key] || "#8ea3ba"}"></i>${esc(f.label)}${f.stale ? " (cached)" : ""} · ${(f.events || []).length}`).join(" &nbsp; ")} &nbsp; <i style="border-color:#8ea3ba"></i>intranet schedule</span></div>
      <div id="mdetailbox" hidden></div>
      ${body}
      <p class="muted small">Times UTC. Open in Google Calendar: ${(UW.M.links || []).map((l) => `<a href="${esc(l.url)}" target="_blank" rel="noopener">${esc(l.label)}</a>`).join(" · ")}</p></section>`;
    for (const b of host.querySelectorAll("#calspan button")) b.onclick = () => { cal.span = b.dataset.s; store.set("cal.span", cal.span); renderCalendar(); };
    $("#mprev").onclick = () => navShift(-1); $("#mnext").onclick = () => navShift(1);
    $("#mtoday").onclick = () => { cal.day = dayKey(new Date()); cal.month = cal.day.slice(0, 7); store.set("cal.day", cal.day); store.set("cal.month", cal.month); renderCalendar(); };
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
      const d0 = evStart(e), d1 = evEnd(e);
      for (let d = new Date(Date.UTC(d0.getUTCFullYear(), d0.getUTCMonth(), d0.getUTCDate())); d <= d1 && d - d0 < 62 * 86400e3; d = new Date(d.getTime() + 86400e3)) {
        if (e.all_day && e.end && d >= d1) break;              // all-day ends are exclusive
        const k = dayKey(d);
        if (!byDay.has(k)) byDay.set(k, []); byDay.get(k).push({ e, cont: (d - d0) >= 86400e3 });
      }
    }
    const today = dayKey(new Date());
    const cells = [];
    for (let i = 0; i < lead; i++) cells.push('<div class="mcell pad"></div>');
    for (let d = 1; d <= days; d++) {
      const k = `${cal.month}-${String(d).padStart(2, "0")}`;
      const evs = (byDay.get(k) || []).sort((a, b) => a.e.start.localeCompare(b.e.start));
      cells.push(`<div class="mcell ${k === today ? "today" : ""}" data-day="${k}"><div class="mday">${d}</div>` +
        evs.slice(0, 6).map(({ e, cont }) => entryHtml(e, cont)).join("") +
        (evs.length > 6 ? `<div class="mmore">+${evs.length - 6} more</div>` : "") + `</div>`);
    }
    const label = first.toLocaleString(undefined, { month: "long", year: "numeric", timeZone: "UTC" });
    calFrame(host, label, `<div class="mgrid">${["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"].map((d) => `<div class="mdow">${d}</div>`).join("")}${cells.join("")}</div>`, items,
      (n) => { const d = new Date(Date.UTC(y, m + n, 1)); cal.month = d.toISOString().slice(0, 7); store.set("cal.month", cal.month); renderCalendar(); });
    // a day number opens that day in the 3-day view
    for (const c of host.querySelectorAll(".mcell[data-day] .mday")) c.onclick = () => { cal.day = c.parentElement.dataset.day; cal.span = "days"; store.set("cal.day", cal.day); store.set("cal.span", "days"); renderCalendar(); };
  }
  // three days centred on cal.day, with a time axis; timed entries are
  // blocks, all-day ones sit at the top
  function renderDays(host, q) {
    const items = calendarItems(q);
    const centre = new Date(cal.day + "T00:00:00Z");
    const days = [-1, 0, 1].map((n) => new Date(centre.getTime() + n * 86400e3));
    const today = dayKey(new Date());
    const now = new Date();
    const cols = days.map((d) => {
      const k = dayKey(d), d0 = d, d1 = new Date(d.getTime() + 86400e3);
      const here = items.filter((e) => evStart(e) < d1 && evEnd(e) > d0 || (e.all_day && e.start === k));
      const allDay = here.filter((e) => e.all_day), timed = here.filter((e) => !e.all_day);
      // lay overlapping blocks side by side
      // longer blocks first so a short one drawn later sits on top of a long
      // one it overlaps; overlapping blocks share the column side by side
      const sorted = timed.sort((a, b) => (evStart(a) - evStart(b)) || ((evEnd(b) - evStart(b)) - (evEnd(a) - evStart(a))));
      const lanes = [];
      for (const e of sorted) { let l = 0; while (lanes[l] && lanes[l] > evStart(e)) l++; lanes[l] = evEnd(e); e._lane = l; }
      const nl = Math.max(1, lanes.length);
      const blocks = sorted.map((e, i) => {
        const s = Math.max(0, (evStart(e) - d0) / 3600e3), t = Math.min(24, (evEnd(e) - d0) / 3600e3);
        // a short event still gets one readable line: the block is at least ~40 min tall
        return `<div class="dblock mev" data-id="${e.id}" style="top:${(s / 24 * 100).toFixed(2)}%;height:${Math.max(2.9, (t - s) / 24 * 100).toFixed(2)}%;left:${(e._lane / nl * 100).toFixed(1)}%;width:${(100 / nl - 1).toFixed(1)}%;z-index:${2 + i};border-color:${GCAL_COLOUR[e.cal] || "#8ea3ba"}" title="${esc(e.label)}\n${evStart(e).toISOString().slice(11, 16)}Z ${esc(e.summary)}">${esc(shortSummary(e.summary))}</div>`;
      }).join("");
      const nowLine = k === today ? `<div class="dnow" style="top:${((now.getUTCHours() + now.getUTCMinutes() / 60) / 24 * 100).toFixed(2)}%"></div>` : "";
      return `<div class="dcol ${k === today ? "today" : ""}"><div class="dhead">${d.toLocaleDateString(undefined, { weekday: "short", month: "short", day: "numeric", timeZone: "UTC" })}</div>
        <div class="dallday">${allDay.map((e) => entryHtml(e, false)).join("")}</div>
        <div class="dbody">${Array.from({ length: 24 }, (_, h) => `<div class="dhour" style="top:${(h / 24 * 100).toFixed(2)}%"></div>`).join("")}${blocks}${nowLine}</div></div>`;
    }).join("");
    const axis = `<div class="daxis"><div class="dhead"></div><div class="dallday"></div><div class="dbody">${Array.from({ length: 24 }, (_, h) => `<div class="dhl" style="top:${(h / 24 * 100).toFixed(2)}%">${String(h).padStart(2, "0")}</div>`).join("")}</div></div>`;
    const label = `${days[0].toLocaleDateString(undefined, { month: "short", day: "numeric", timeZone: "UTC" })} – ${days[2].toLocaleDateString(undefined, { month: "short", day: "numeric", year: "numeric", timeZone: "UTC" })}`;
    calFrame(host, label, `<div class="dgrid">${axis}${cols}</div>`, items,
      (n) => { cal.day = dayKey(new Date(centre.getTime() + n * 86400e3)); cal.month = cal.day.slice(0, 7); store.set("cal.day", cal.day); store.set("cal.month", cal.month); renderCalendar(); });
  }
  function wireCalendar() {
    for (const b of $("#calview").querySelectorAll("button")) {
      b.classList.toggle("on", b.dataset.v === cal.view);
      b.onclick = () => { cal.view = b.dataset.v; store.set("cal.view", cal.view); for (const x of $("#calview").querySelectorAll("button")) x.classList.toggle("on", x === b); renderCalendar(); };
    }
    $("#calsearch").oninput = debounce((e) => { cal.search = e.target.value; renderCalendar(); }, 150);
  }

  // ================================================================ table
  const tbl = { rule: store.get("tbl.rule", "stations"), stat: +store.get("tbl.stat", 0), sort: store.get("tbl.sort", { key: "t", dir: -1 }), search: "", data: {}, loadedFor: null };
  async function ensureAgg() {
    const stamp = UW.M.generated_utc;
    if (tbl.rule === "stations") return;
    const rule = tbl.rule;
    const data = await cachedJSON(`aggregate:${rule}`, UW.M.aggregates[rule].file);
    if (tbl.loadedFor !== stamp) tbl.data = {};
    tbl.data[rule] = data; tbl.loadedFor = stamp; tbl.legs = UW.M.legs;
  }
  // stations: the CTD station list, one row per cast
  const STATION_COLS = [["time", "time (UTC)"], ["leg", "leg"], ["cast", "cast"], ["station", "station"], ["label", "label"], ["type", "type"], ["lat", "lat"], ["lon", "lon"], ["bottom_m", "bottom (m)"], ["depth_m", "cast depth (m)"], ["comments", "comments"]];
  function stationRows() {
    const q = tbl.search.toLowerCase();
    const f = UW.currentFilter();
    let rows = (UW.M.stations || []).filter((s) => UW.inFilter(s.leg, s.time, f)).map((s) => ({ ...s, legLabel: UW.legById(s.leg)?.label || s.leg }));
    if (q) rows = rows.filter((r) => `${r.time} ${r.legLabel} ${r.station} ${r.label} ${r.type} ${r.comments}`.toLowerCase().includes(q));
    const k = tbl.sort.key in { t: 1 } ? "time" : tbl.sort.key, dir = tbl.sort.dir;
    const val = (r) => k === "leg" ? r.legLabel : k === "cast" ? +r.cast : r[k];
    rows.sort((a, b) => { const x = val(a), y = val(b); if (x == null || x === "") return 1; if (y == null || y === "") return -1; return (x < y ? -1 : x > y ? 1 : 0) * dir; });
    return rows;
  }
  function renderStations() {
    const rows = stationRows();
    const arrow = (k) => (tbl.sort.key === k || (k === "time" && tbl.sort.key === "t")) ? (tbl.sort.dir > 0 ? " ▲" : " ▼") : "";
    const head = STATION_COLS.map(([k, l]) => `<th data-k="${esc(k)}" title="sort">${esc(l)}${arrow(k)}</th>`).join("");
    const cell = (r, k) => k === "leg" ? esc(r.legLabel) : k === "time" ? esc((r.time || "").replace("T", " ").slice(0, 16)) :
      k === "lat" || k === "lon" ? (r[k] != null ? (+r[k]).toFixed(4) : "") : k === "bottom_m" || k === "depth_m" ? (r[k] != null ? Math.round(+r[k]) : "") : esc(r[k] ?? "");
    const body = rows.map((r) => `<tr class="${casts.sel.has(`${r.leg}:CTD_${String(r.cast).padStart(3, "0")}`) ? "sel" : ""}">${STATION_COLS.map(([k]) => `<td class="${["time", "lat", "lon", "bottom_m", "depth_m", "cast"].includes(k) ? "mono" : ""}">${cell(r, k)}</td>`).join("")}</tr>`).join("");
    $("#aggtable").innerHTML = `<thead><tr>${head}</tr></thead><tbody>${body}</tbody>`;
    const nsel = rows.filter((r) => casts.sel.has(`${r.leg}:CTD_${String(r.cast).padStart(3, "0")}`)).length;
    $("#tblmeta").textContent = `${rows.length.toLocaleString()} stations${nsel ? ` · ${nsel} selected for the Casts tab` : ""} · click a row to select its cast (again to deselect)`;
    for (const th of $("#aggtable").querySelectorAll("th")) th.onclick = () => {
      const k = th.dataset.k; tbl.sort = { key: k, dir: tbl.sort.key === k ? -tbl.sort.dir : (k === "time" ? -1 : 1) }; store.set("tbl.sort", tbl.sort); renderStations();
    };
    // a click selects the cast (and shows the station on the map); a click on
    // a selected row deselects it
    for (const [i, tr] of [...$("#aggtable").querySelectorAll("tbody tr")].entries()) tr.onclick = async () => {
      const r = rows[i], key = `${r.leg}:CTD_${String(r.cast).padStart(3, "0")}`;
      const was = casts.sel.has(key);
      if (!was) UW.focusMap(r.lat, r.lon, `Cast ${r.cast} ${r.station}`);
      await UW.onStationClick?.(key, { quiet: true, toggle: true });
      renderStations();
      if (!was) $("#aggtable").querySelectorAll("tbody tr")[i]?.classList.add("on");
    };
  }
  function currentRows() {
    const d = tbl.data[tbl.rule]; if (!d) return [];
    const q = tbl.search.toLowerCase();
    const f = UW.currentFilter();
    // A failed refresh can leave older table data visible; its numeric leg
    // codes must still be interpreted with the matching manifest.
    let rows = d.rows.filter((r) => UW.inFilter(tbl.legs[r.leg]?.id, r.t, f)).map((r) => ({ ...r, legLabel: tbl.legs[r.leg]?.label || "" }));
    if (q) rows = rows.filter((r) => `${fmtUTC(r.t)} ${r.legLabel}`.toLowerCase().includes(q));
    const k = tbl.sort.key, dir = tbl.sort.dir;
    const val = (r) => k === "t" ? r.t : k === "leg" ? r.legLabel : k === "lat" || k === "lon" ? r[k] : (r[k] ? r[k][tbl.stat] : null);
    rows.sort((a, b) => { const x = val(a), y = val(b); if (x == null) return 1; if (y == null) return -1; return (x < y ? -1 : x > y ? 1 : 0) * dir; });
    return rows;
  }
  function renderTable() {
    $("#aggstat").parentElement.hidden = tbl.rule === "stations";
    if (tbl.rule === "stations") return renderStations();
    const d = tbl.data[tbl.rule]; if (!d) return;
    const rows = currentRows();
    const stat = ["mean", "min", "max", "n"][tbl.stat];
    const cols = [["t", "time (UTC)"], ["leg", "leg"], ["lat", "lat"], ["lon", "lon"], ...d.variables.map((v) => [v, v])];
    const arrow = (k) => tbl.sort.key === k ? (tbl.sort.dir > 0 ? " ▲" : " ▼") : "";
    const head = cols.map(([k, l]) => `<th data-k="${esc(k)}" title="sort">${esc(l)}${arrow(k)}</th>`).join("");
    const body = rows.slice(0, 2000).map((r) => `<tr><td class="mono">${fmtUTC(r.t)}</td><td>${esc(r.legLabel)}</td><td class="mono">${r.lat ?? ""}</td><td class="mono">${r.lon ?? ""}</td>` +
      d.variables.map((v) => `<td class="mono">${r[v] ? (tbl.stat === 3 ? r[v][3] : fmtVal(r[v][tbl.stat], "")) : ""}</td>`).join("") + "</tr>").join("");
    $("#aggtable").innerHTML = `<thead><tr>${head}</tr></thead><tbody>${body}</tbody>`;
    const shown = rows.slice(0, 2000);
    for (const [i, tr] of [...$("#aggtable").querySelectorAll("tbody tr")].entries()) tr.onclick = () => {
      const r = shown[i]; if (r.lat == null) return;
      for (const x of $("#aggtable").querySelectorAll("tbody tr.on")) x.classList.remove("on");
      tr.classList.add("on"); UW.focusMap(r.lat, r.lon, `${fmtUTC(r.t)} · ${r.legLabel}`);
    };
    $("#tblmeta").textContent = `${rows.length.toLocaleString()} rows · ${stat}${rows.length > 2000 ? " · showing first 2000" : ""}`;
    for (const th of $("#aggtable").querySelectorAll("th")) th.onclick = () => {
      const k = th.dataset.k; tbl.sort = { key: k, dir: tbl.sort.key === k ? -tbl.sort.dir : (k === "t" ? -1 : 1) }; store.set("tbl.sort", tbl.sort); renderTable();
    };
  }
  function downloadCSV() {
    if (tbl.rule === "stations") {
      const q = (s) => `"${String(s ?? "").replace(/"/g, '""')}"`;
      const rows = stationRows();
      const lines = [STATION_COLS.map(([k]) => q(k)).join(",")].concat(rows.map((r) => STATION_COLS.map(([k]) => q(k === "leg" ? r.legLabel : r[k])).join(",")));
      const blob = new Blob([lines.join("\n")], { type: "text/csv" });
      const a = document.createElement("a"); a.href = URL.createObjectURL(blob); a.download = "stations.csv"; a.click();
      setTimeout(() => URL.revokeObjectURL(a.href), 5000);
      return;
    }
    const d = tbl.data[tbl.rule]; if (!d) return;
    const stat = ["mean", "min", "max", "n"][tbl.stat];
    const rows = currentRows();
    const head = ["time_utc", "leg", "lat", "lon", ...d.variables.map((v) => `${v} (${stat})`)];
    const q = (s) => `"${String(s ?? "").replace(/"/g, '""')}"`;
    const lines = [head.map(q).join(",")].concat(rows.map((r) => [new Date(r.t).toISOString(), r.legLabel, r.lat, r.lon, ...d.variables.map((v) => r[v] ? r[v][tbl.stat] : "")].map(q).join(",")));
    const blob = new Blob([lines.join("\n")], { type: "text/csv" });
    const a = document.createElement("a"); a.href = URL.createObjectURL(blob); a.download = `underway_${tbl.rule}_${stat}.csv`; a.click();
    setTimeout(() => URL.revokeObjectURL(a.href), 5000);
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
  UW.onXMode = () => { if (!$("#pane-casts").hidden && casts.mode === "section") renderCastPlots(); };
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
    if (!["casts", "calendar", "table"].includes(name)) return;
    const stamp = UW.M.generated_utc;
    const key = `${name}:${stamp}:${name === "table" ? tbl.rule : ""}`;
    if (!force && refreshedTab === key) return;
    const seq = ++tabSeq;
    const scope = { casts: "Casts", calendar: "Agenda", table: "Table" }[name];
    try {
      if (name === "casts") await ensureCastIndex();
      if (name === "calendar") await ensureCalendar();
      if (name === "table") await ensureAgg();
      if (seq !== tabSeq || name !== activeTab() || stamp !== UW.M.generated_utc) return;
      if (name === "casts") {
        renderCastList(); UW.renderMap();
        if (await renderCastPlots() === false) return;
      }
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
    for (const scope of ["Casts", "Agenda", "Table"]) UW.setLoadError(scope, false);
    if (name !== "casts") clearTimeout(live.timer);
    refreshActiveTab(true);
  };
  wireCasts(); wireCalendar(); wireTable();
  const active = document.querySelector("#tabs button.on")?.dataset.tab;
  if (active && active !== "underway") UW.onTab(active);
})();
