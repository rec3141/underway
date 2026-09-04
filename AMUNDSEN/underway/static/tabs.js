/* Casts, Calendar and Table panes. Loaded after app.js; talks to it through
 * window.UW (state, manifest, helpers, and hooks the map calls back into). */
(() => {
  "use strict";
  const UW = window.UW;
  const $ = (s) => document.querySelector(s);
  const { THEME, CFG, fmtUTC, fmtVal, dms, store } = UW;
  const esc = (s) => String(s ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  const PALETTE = ["#5cc8ff", "#ffb454", "#7ee787", "#ff7b72", "#d2a8ff", "#f2cc60", "#79c0ff", "#ffa198", "#56d364", "#e3b341", "#a5d6ff", "#ff9bce"];
  async function getJSON(url) { const r = await fetch(url, { cache: "no-store" }); if (!r.ok) throw new Error(`${r.status} ${url}`); return r.json(); }
  const debounce = (f, ms) => { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => f(...a), ms); }; };

  // ================================================================ casts
  const casts = {
    idx: null, loadedFor: null, cache: {},
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
  UW.onStationClick = (key) => {
    if (!casts.idx) return;
    if (!castById(key)) return;
    toggleCast(key);
    if ($("#pane-casts").hidden) UW.showTab("casts");
  };
  UW.extraMapTraces = () => {
    const out = [];
    if (!casts.idx || casts.kind === "CTD") return out;
    // each MVP tow is one dataset: its track as a line, with a clickable
    // marker at the start (the whole line also selects it)
    const tows = casts.idx.casts.filter((c) => c.kind === "MVP" && c.track?.length);
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
    try { casts.idx = await getJSON(`${UW.M.casts.index}?v=${encodeURIComponent(stamp)}`); casts.loadedFor = stamp; }
    catch { casts.idx = { casts: [], variables: [] }; }
    const sel = $("#castvar"); sel.innerHTML = "";
    for (const v of casts.idx.variables) { const o = document.createElement("option"); o.value = v; o.textContent = v; sel.appendChild(o); }
    if (!casts.idx.variables.includes(casts.variable)) casts.variable = casts.idx.variables[0] || "Temperature";
    sel.value = casts.variable;
  }
  async function castData(id) {
    if (casts.cache[id]) return casts.cache[id];
    const m = castById(id); if (!m) return null;
    try { return (casts.cache[id] = await getJSON(`${m.file}?v=${encodeURIComponent(UW.M.generated_utc)}`)); } catch { return null; }
  }

  function renderCastList() {
    const ul = $("#castlist"); if (!casts.idx) return;
    const q = casts.search.toLowerCase();
    const rows = casts.idx.casts
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

  async function renderCastPlots() {
    const host = $("#castplots");
    const sel = orderedSelection();
    $("#castvarwrap").hidden = casts.mode !== "section";
    if (!sel.length) { host.innerHTML = '<div class="empty">Select casts from the list, or click stations and tow tracks on the map.</div>'; $("#castmeta").textContent = ""; return; }
    const data = (await Promise.all(sel.map((c) => castData(c.id)))).filter(Boolean);
    const dips = data.reduce((n, d) => n + profilesOf(d).length, 0);
    $("#castmeta").textContent = `${data.length} selected · ${dips} profile${dips === 1 ? "" : "s"}`;
    if (casts.mode === "profiles") renderProfiles(host, data); else renderSection(host, data);
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
  const maxDepth = (c) => c.max_p != null ? `${Math.round(depthFrom(c.max_p, c.lat))} m` : "";
  // Temperature, salinity and density lead; the rest in a stable order
  const VAR_ORDER = ["Temperature", "Salinity", "Sigma-t", "Oxygen", "Oxygen saturation", "Fluorescence", "CDOM", "PAR", "Transmission", "Buoyancy frequency", "Sound velocity"];
  const orderVars = (vs) => [...vs].sort((a, b) => (VAR_ORDER.indexOf(a) + 1 || 99) - (VAR_ORDER.indexOf(b) + 1 || 99) || a.localeCompare(b));
  const castPanelState = { wide: new Set(store.get("casts.wide", [])) };

  // same frame and controls as the underway panels
  function castPanelHtml(id, title, unit, wideable = true) {
    return `<section class="panel card castplot ${castPanelState.wide.has(id) ? "wide" : ""} ${title === "Temperature" ? "on" : ""}" data-cp="${esc(id)}">
      <div class="head"><h3>${esc(title)}</h3><div class="tools"><span class="now">${esc(unit)}</span>
        <button class="reset" title="reset zoom">⟲</button>${wideable ? '<button class="wide" title="expand">⤢</button>' : ""}</div></div>
      <div class="plot" id="${esc(id)}"></div></section>`;
  }
  function wireCastPanels(host, rerender) {
    for (const sec of host.querySelectorAll(".castplot")) {
      const id = sec.dataset.cp;
      sec.querySelector(".reset").onclick = () => Plotly.relayout(sec.querySelector(".plot"), { "xaxis.autorange": true, "yaxis.autorange": true });
      sec.querySelector(".wide")?.addEventListener("click", () => {
        castPanelState.wide.has(id) ? castPanelState.wide.delete(id) : castPanelState.wide.add(id);
        store.set("casts.wide", [...castPanelState.wide]); rerender();
      });
    }
  }
  const CAST_LAYOUT = { ...THEME, margin: { l: 46, r: 8, t: 6, b: 30 }, showlegend: false, dragmode: "pan" };

  function renderProfiles(host, data) {
    const vars = orderVars(new Set(data.flatMap((d) => Object.keys(d.units))));
    host.innerHTML = vars.map((v) => castPanelHtml(`cp-${v.replace(/\W+/g, "_")}`, v, data.find((d) => d.units[v])?.units[v] || "")).join("") +
      `<div class="castlegend">${data.map((d, i) => `<span><i style="background:${PALETTE[i % PALETTE.length]}"></i>${esc(castLabel(d))} <small>${esc(castDate(d))}</small></span>`).join("")}</div>`;
    for (const v of vars) {
      const traces = [];
      data.forEach((d, i) => {
        const ps = profilesOf(d);
        ps.forEach((p, j) => {
          if (!p.vars[v]) return;
          traces.push({
            type: "scatter", mode: "lines", name: p.label, x: p.vars[v], y: depths(p), connectgaps: false,
            line: { width: ps.length > 1 ? 1 : 1.6, color: ps.length > 1 ? towShade(PALETTE[i % PALETTE.length], j, ps.length) : PALETTE[i % PALETTE.length] },
            opacity: ps.length > 1 ? 0.8 : 1,
            hovertemplate: `${esc(p.label)}<br>%{x:.3~f} ${esc(d.units[v] || "")} at %{y:.0f} m<extra></extra>`,
          });
        });
      });
      const layout = { ...CAST_LAYOUT, hovermode: "closest",
        xaxis: { ...THEME.xaxis, title: { text: data.find((d) => d.units[v])?.units[v] || "", font: { size: 10 }, standoff: 4 }, tickfont: { size: 10 } },
        yaxis: { ...THEME.yaxis, autorange: "reversed", title: { text: "depth (m)", font: { size: 10 }, standoff: 2 }, tickfont: { size: 10 } } };
      Plotly.react(host.querySelector(`#cp-${v.replace(/\W+/g, "_")}`), traces, layout, CFG);
    }
    wireCastPanels(host, () => renderProfiles(host, data));
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
      { type: "heatmap", x: xPlot, y: grid, z, colorscale: "Viridis", connectgaps: false, zsmooth: "best",
        colorbar: { title: { text: unit, side: "right" }, thickness: 12, len: .8, tickfont: { size: 10 }, outlinewidth: 0 },
        hovertemplate: (byTime ? "%{x|%m-%d %H:%M}Z" : "%{x:.1f} km") + ` · %{y:.0f} m<br><b>%{z:.3~f} ${esc(unit)}</b><extra></extra>` },
      { type: "scatter", mode: dense ? "markers" : "markers+text", x: xPts, y: withVar.map(() => 0), text: withVar.map((_, i) => String(i + 1)), textposition: "top center",
        textfont: { size: 10, color: "#c9d4e0" }, marker: { symbol: "triangle-down", size: dense ? 5 : 9, color: "#ffb454" },
        hovertext: withVar.map((d) => `${d.label}<br>${d.time ? d.time.replace("T", " ").slice(0, 16) : ""}`), hoverinfo: "text", cliponaxis: false },
    ];
    // echo-sounder bottom where there is one, else the deepest sample; the
    // fill is clipped to the frame so a bottom far below the casts stays out of it
    const bottoms = withVar.map((d) => Math.min(maxD + step, d.bottom_m > 0 ? d.bottom_m : depthFrom(d.p[d.p.length - 1], d.lat ?? d.parent?.lat)));
    traces.push({ type: "scatter", mode: "lines", x: xPts, y: bottoms, line: { color: "#2b3441", width: 2, shape: "spline" }, fill: "tonexty", fillcolor: "rgba(43,52,65,.92)", hoverinfo: "skip", name: "bottom" });
    traces.push({ type: "scatter", mode: "lines", x: xPts, y: bottoms.map(() => maxD + step), line: { width: 0 }, hoverinfo: "skip", showlegend: false });
    const layout = { ...CAST_LAYOUT, margin: { l: 50, r: 8, t: 18, b: 34 },
      xaxis: { ...THEME.xaxis, title: { text: xTitle, font: { size: 10 }, standoff: 4 }, tickfont: { size: 10 }, type: byTime ? "date" : "linear" },
      yaxis: { ...THEME.yaxis, autorange: false, title: { text: "depth (m)", font: { size: 10 }, standoff: 2 }, tickfont: { size: 10 }, range: [maxD + step, 0] } };
    Plotly.react($("#cs-plot"), traces, layout, CFG);
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
  const cal = { data: null, loadedFor: null, view: store.get("cal.view", "agenda"), search: "" };
  async function ensureCalendar() {
    const stamp = UW.M.generated_utc;
    if (cal.data && cal.loadedFor === stamp) return;
    try { cal.data = await getJSON(`${UW.M.calendar.file}?v=${encodeURIComponent(stamp)}`); cal.loadedFor = stamp; }
    catch { cal.data = { events: [], schedule: { rows: [] } }; }
  }
  function renderCalendar() {
    const host = $("#calendar"); if (!cal.data) return;
    const s = cal.data.schedule || {};
    $("#calmeta").textContent = `${cal.data.events.length} logged events · schedule ${s.updated ? "updated " + s.updated : "unavailable"}${s.stale ? " (cached copy)" : ""}`;
    const q = cal.search.toLowerCase();
    const evs = cal.data.events.filter((e) => !q || JSON.stringify(e).toLowerCase().includes(q));
    if (cal.view === "timeline") return renderTimeline(host, evs, s);
    const sched = (s.rows || []).map((r) => `<tr class="st-${esc((r.status || "").toLowerCase().replace(/\s+/g, "-"))}"><td>${esc(r.date)}</td><td>${esc(r.start)}–${esc(r.end)}</td><td>${esc(r.station)}</td><td>${esc(r.operation)}</td><td><span class="status">${esc(r.status)}</span></td><td>${r.duration_h != null ? r.duration_h.toFixed(1) + " h" : ""}</td><td class="muted">${esc(r.comment)}</td></tr>`).join("");
    let html = `<section class="card block"><h3>Operations schedule ${esc(s.title || "")}</h3>` +
      (sched ? `<table class="sched"><tr><th>date</th><th>time</th><th>station</th><th>operation</th><th>status</th><th>dur.</th><th>comment</th></tr>${sched}</table>` : '<p class="muted">no scheduled operations listed</p>') +
      (s.whiteboard ? `<p class="whiteboard">📋 ${esc(s.whiteboard)}</p>` : "") +
      `<p class="muted small">Source: ship intranet Schedule page · calendars: ${(UW.M.links || []).map((l) => `<a href="${esc(l.url)}" target="_blank" rel="noopener">${esc(l.label)}</a>`).join(" · ")}</p></section>`;
    // agenda: events grouped by UTC day, newest first
    const byDay = new Map();
    for (const e of evs) { const d = (e.time_utc || "").slice(0, 10); if (!byDay.has(d)) byDay.set(d, []); byDay.get(d).push(e); }
    const days = [...byDay.keys()].sort().reverse().slice(0, 60);
    html += `<section class="agenda">` + days.map((d) => `<div class="day"><h4>${esc(d)} <small>${byDay.get(d).length} events · ${esc(UW.legById(byDay.get(d)[0].leg)?.label || "")}</small></h4>` +
      byDay.get(d).sort((a, b) => (b.time_utc || "").localeCompare(a.time_utc || "")).map((e) => `<div class="ev">
          <span class="t">${esc((e.time_utc || "").slice(11, 16))}Z</span>
          <span class="st">${esc(e.station || "")}</span>
          <span class="what">${esc(e.activity || "")}${e.event ? " · " + esc(e.event) : ""}${e.label ? ` <code>${esc(e.label)}</code>` : ""}</span>
          <span class="pos muted">${e.lat != null && e.lon != null ? dms(+e.lat, +e.lon) : ""}${e.depth_m != null ? " · " + Math.round(+e.depth_m) + " m" : ""}</span>
          ${e.comment ? `<span class="cm muted">${esc(e.comment)}</span>` : ""}</div>`).join("") + `</div>`).join("") + `</section>`;
    host.innerHTML = html;
  }
  function renderTimeline(host, evs, s) {
    host.innerHTML = '<div class="castplot card wide"><div class="plot tall" id="cal-plot"></div></div>';
    const now = Date.now();
    const recent = evs.filter((e) => now - new Date(e.time_utc.replace(" ", "T") + (e.time_utc.includes("T") || e.time_utc.length < 19 ? "" : "Z")) < 14 * 86400e3);
    const types = [...new Set(recent.map((e) => e.activity || "other"))].slice(0, 20);
    const traces = types.map((t, i) => {
      const es = recent.filter((e) => (e.activity || "other") === t);
      return { type: "scatter", mode: "markers", name: t, x: es.map((e) => new Date(e.time_utc.replace(" ", "T") + "Z")), y: es.map(() => t),
        text: es.map((e) => `${esc(e.station || "")} · ${esc(e.event || "")} ${esc(e.label || "")}`), hovertemplate: "%{x|%m-%d %H:%M}Z<br>%{text}<extra>" + esc(t) + "</extra>",
        marker: { size: 8, color: PALETTE[i % PALETTE.length] } };
    });
    // scheduled operations as bars on their own row
    const bars = (s.rows || []).map((r) => {
      const m = /^(\d{2})\/(\d{2})\/(\d{2})$/.exec(r.date || ""); if (!m) return null;
      const d0 = new Date(`20${m[3]}-${m[2]}-${m[1]}T${r.start || "00:00"}:00Z`); let d1 = new Date(`20${m[3]}-${m[2]}-${m[1]}T${r.end || "23:59"}:00Z`);
      if (d1 < d0) d1 = new Date(d1.getTime() + 86400e3);
      return { r, d0, d1 };
    }).filter(Boolean);
    if (bars.length) traces.push({ type: "bar", orientation: "h", name: "scheduled", base: bars.map((b) => b.d0), x: bars.map((b) => b.d1 - b.d0), y: bars.map(() => "scheduled"),
      text: bars.map((b) => `${esc(b.r.station)} · ${esc(b.r.operation)} (${esc(b.r.status)})`), hovertemplate: "%{text}<extra></extra>", marker: { color: "rgba(255,180,84,.7)" }, width: .6 });
    const layout = { ...THEME, margin: { l: 130, r: 10, t: 10, b: 40 }, showlegend: false, barmode: "overlay",
      xaxis: { ...THEME.xaxis, type: "date", title: { text: "UTC", font: { size: 10 } }, tickfont: { size: 10 } },
      yaxis: { ...THEME.yaxis, type: "category", categoryorder: "array", categoryarray: ["scheduled", ...types.slice().reverse()], tickfont: { size: 10 } },
      shapes: [{ type: "line", xref: "x", x0: new Date(now), x1: new Date(now), yref: "paper", y0: 0, y1: 1, line: { color: "#7ee787", width: 1.5, dash: "dot" } }] };
    Plotly.react($("#cal-plot"), traces, layout, CFG);
  }
  function wireCalendar() {
    for (const b of $("#calview").querySelectorAll("button")) {
      b.classList.toggle("on", b.dataset.v === cal.view);
      b.onclick = () => { cal.view = b.dataset.v; store.set("cal.view", cal.view); for (const x of $("#calview").querySelectorAll("button")) x.classList.toggle("on", x === b); renderCalendar(); };
    }
    $("#calsearch").oninput = debounce((e) => { cal.search = e.target.value; renderCalendar(); }, 150);
  }

  // ================================================================ table
  const tbl = { rule: store.get("tbl.rule", "1h"), stat: +store.get("tbl.stat", 0), sort: store.get("tbl.sort", { key: "t", dir: -1 }), search: "", data: {}, loadedFor: null };
  async function ensureAgg() {
    const stamp = UW.M.generated_utc;
    if (tbl.loadedFor !== stamp) { tbl.data = {}; tbl.loadedFor = stamp; }
    if (!tbl.data[tbl.rule]) {
      try { tbl.data[tbl.rule] = await getJSON(`${UW.M.aggregates[tbl.rule].file}?v=${encodeURIComponent(stamp)}`); } catch { tbl.data[tbl.rule] = { variables: [], rows: [] }; }
    }
  }
  function currentRows() {
    const d = tbl.data[tbl.rule]; if (!d) return [];
    const q = tbl.search.toLowerCase();
    let rows = d.rows.map((r) => ({ ...r, legLabel: UW.legById(UW.M.legs[r.leg]?.id)?.label || "" }));
    if (q) rows = rows.filter((r) => `${fmtUTC(r.t)} ${r.legLabel}`.toLowerCase().includes(q));
    const k = tbl.sort.key, dir = tbl.sort.dir;
    const val = (r) => k === "t" ? r.t : k === "leg" ? r.legLabel : k === "lat" || k === "lon" ? r[k] : (r[k] ? r[k][tbl.stat] : null);
    rows.sort((a, b) => { const x = val(a), y = val(b); if (x == null) return 1; if (y == null) return -1; return (x < y ? -1 : x > y ? 1 : 0) * dir; });
    return rows;
  }
  function renderTable() {
    const d = tbl.data[tbl.rule]; if (!d) return;
    const rows = currentRows();
    const stat = ["mean", "min", "max", "n"][tbl.stat];
    const cols = [["t", "time (UTC)"], ["leg", "leg"], ["lat", "lat"], ["lon", "lon"], ...d.variables.map((v) => [v, v])];
    const arrow = (k) => tbl.sort.key === k ? (tbl.sort.dir > 0 ? " ▲" : " ▼") : "";
    const head = cols.map(([k, l]) => `<th data-k="${esc(k)}" title="sort">${esc(l)}${arrow(k)}</th>`).join("");
    const body = rows.slice(0, 2000).map((r) => `<tr><td class="mono">${fmtUTC(r.t)}</td><td>${esc(r.legLabel)}</td><td class="mono">${r.lat ?? ""}</td><td class="mono">${r.lon ?? ""}</td>` +
      d.variables.map((v) => `<td class="mono">${r[v] ? (tbl.stat === 3 ? r[v][3] : fmtVal(r[v][tbl.stat], "")) : ""}</td>`).join("") + "</tr>").join("");
    $("#aggtable").innerHTML = `<thead><tr>${head}</tr></thead><tbody>${body}</tbody>`;
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
    const q = (s) => `"${String(s ?? "").replace(/"/g, '""')}"`;
    const lines = [head.map(q).join(",")].concat(rows.map((r) => [new Date(r.t).toISOString(), r.legLabel, r.lat, r.lon, ...d.variables.map((v) => r[v] ? r[v][tbl.stat] : "")].map(q).join(",")));
    const blob = new Blob([lines.join("\n")], { type: "text/csv" });
    const a = document.createElement("a"); a.href = URL.createObjectURL(blob); a.download = `underway_${tbl.rule}_${stat}.csv`; a.click();
    setTimeout(() => URL.revokeObjectURL(a.href), 5000);
  }
  function wireTable() {
    for (const b of $("#aggrule").querySelectorAll("button")) {
      b.classList.toggle("on", b.dataset.r === tbl.rule);
      b.onclick = async () => { tbl.rule = b.dataset.r; store.set("tbl.rule", tbl.rule); for (const x of $("#aggrule").querySelectorAll("button")) x.classList.toggle("on", x === b); await ensureAgg(); renderTable(); };
    }
    $("#aggstat").value = String(tbl.stat);
    $("#aggstat").onchange = (e) => { tbl.stat = +e.target.value; store.set("tbl.stat", tbl.stat); renderTable(); };
    $("#tblsearch").oninput = debounce((e) => { tbl.search = e.target.value; renderTable(); }, 150);
    $("#tblcsv").onclick = downloadCSV;
  }

  // ================================================================ glue
  UW.onXMode = () => { if (!$("#pane-casts").hidden && casts.mode === "section") renderCastPlots(); };
  UW.onTab = async (name) => {
    if (name === "casts") { await ensureCastIndex(); renderCastList(); renderCastPlots(); UW.renderMap(); }
    if (name === "calendar") { await ensureCalendar(); renderCalendar(); }
    if (name === "table") { await ensureAgg(); renderTable(); }
  };
  wireCasts(); wireCalendar(); wireTable();
  const active = document.querySelector("#tabs button.on")?.dataset.tab;
  if (active && active !== "underway") UW.onTab(active);
})();
