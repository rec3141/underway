/* Amundsen underway dashboard — renders the JSON produced by the Python build.
 * Self-contained: Plotly is bundled, and the basemap is Natural Earth GeoJSON
 * served from static/geo/ and drawn by Plotly's MapLibre map with no tiles.
 *
 * One record spans every leg. A window is a span back from the latest data;
 * each point carries its leg, and the leg list filters what is shown. */
(() => {
  "use strict";

  const SITE = window.__SITE__;
  let M = window.__MANIFEST__;
  const $ = (s) => document.querySelector(s);
  const store = {
    get(k, d) { try { return JSON.parse(localStorage.getItem("uw:" + k)) ?? d; } catch { return d; } },
    set(k, v) { try { localStorage.setItem("uw:" + k, JSON.stringify(v)); } catch { /* private mode */ } },
  };

  const state = {
    hidden: new Set(store.get("hiddenLegs", [])),   // leg ids switched off; default: everything shown
    win: store.get("win", M.default_window),
    xmode: store.get("xmode", "time"),
    colour: store.get("colour", "SST (°C)"),
    log: store.get("log", {}),
    bathy: store.get("bathy", true),
    stations: store.get("stations", true),
    order: store.get("order", []),
    panel: store.get("panel", {}),                    // name -> "min" | "wide" | null (a key the user has set)
    raw: null,                                        // window payload as built
    data: null,                                       // same, filtered to shown legs
    geo: null,
    view: null,                                       // user's pan/zoom
  };

  const NOT_PANELS = new Set(["Time elapsed (h)", "Distance travelled (km)"]);
  // the per-scale surprise series feed the one surprise panel, which shows
  // the scale matching the span on display (holding at the longest scale)
  const SURPRISE = "Surprise (−log10 p)";
  for (const v of M.variables) if (v.name.startsWith("Surprise ·")) NOT_PANELS.add(v.name);
  function surpriseScale() {
    const scales = M.surprise?.scales || [];
    const hours = (M.windows.find((w) => w.label === state.win) || {}).hours || 1;
    let pick = scales[0];
    for (const sc of scales) if (sc[1] <= hours * 60) pick = sc;
    return pick ? pick[0] : null;
  }
  let VAR = Object.fromEntries(M.variables.map((v) => [v.name, v]));

  const THEME = {
    paper_bgcolor: "rgba(0,0,0,0)", plot_bgcolor: "#121920",
    font: { color: "#c9d4e0", family: "system-ui, -apple-system, Segoe UI, Roboto, sans-serif", size: 11 },
    xaxis: { gridcolor: "#243040", zerolinecolor: "#243040", linecolor: "#34435a" },
    yaxis: { gridcolor: "#243040", zerolinecolor: "#243040", linecolor: "#34435a" },
    hoverlabel: { bgcolor: "#1b242e", bordercolor: "#5cc8ff", font: { color: "#e6ecf2", size: 12 } },
  };
  const CFG = { displayModeBar: false, responsive: true, scrollZoom: true, doubleClick: "reset" };

  // Shift+scroll zooms the x axis alone, Ctrl+scroll the y axis alone, about
  // the cursor; a plain scroll keeps Plotly's zoom of both. Listens in the
  // capture phase so Plotly's own wheel handler never sees the modified event.
  function axisZoom(gd) {
    if (gd._axisZoom) return;
    gd._axisZoom = true;
    gd.addEventListener("wheel", (ev) => {
      const fl = gd._fullLayout;
      if (!(ev.shiftKey || ev.ctrlKey) || !fl || !fl.xaxis || !fl.yaxis) return;
      ev.preventDefault(); ev.stopPropagation();
      const rect = gd.getBoundingClientRect();
      const ax = ev.shiftKey ? fl.xaxis : fl.yaxis;
      const px = ev.shiftKey ? ev.clientX - rect.left - ax._offset : ev.clientY - rect.top - ax._offset;
      const c = ax.p2l(px);
      const k = Math.exp(ev.deltaY * 0.0015);
      const r0 = ax.r2l(ax.range[0]), r1 = ax.r2l(ax.range[1]);
      const lo = c + (r0 - c) * k, hi = c + (r1 - c) * k;
      Plotly.relayout(gd, { [`${ax._name}.range`]: [ax.l2r(lo), ax.l2r(hi)], [`${ax._name}.autorange`]: false });
    }, { passive: false, capture: true });
  }

  // ------------------------------------------------------------ helpers
  const fmtUTC = (ms) => new Date(ms).toISOString().replace("T", " ").slice(0, 16) + "Z";
  const fmtLocal = (iso) => new Date(iso).toLocaleString(undefined, { timeZone: SITE.local_tz,
    month: "short", day: "numeric", hour: "2-digit", minute: "2-digit" });
  const ago = (iso) => {
    const s = Math.max(0, (Date.now() - new Date(iso)) / 1000);
    if (s < 90) return `${Math.round(s)} s ago`;
    if (s < 5400) return `${Math.round(s / 60)} min ago`;
    if (s < 48 * 3600) return `${(s / 3600).toFixed(1)} h ago`;
    return `${Math.round(s / 86400)} d ago`;
  };
  const lastFinite = (arr) => { for (let i = arr.length - 1; i >= 0; i--) if (arr[i] != null) return arr[i]; return null; };
  const fmtVal = (v, unit) => v == null ? "—" : `${Math.abs(v) >= 100 ? v.toFixed(0) : v.toFixed(2)}${unit ? " " + unit : ""}`;
  const dms = (lat, lon) => `${Math.abs(lat).toFixed(4)}°${lat >= 0 ? "N" : "S"}, ${Math.abs(lon).toFixed(4)}°${lon >= 0 ? "E" : "W"}`;
  const xvals = (d) => state.xmode === "time" ? d.t.map((ms) => new Date(ms)) : d.dist_km;
  const xTitle = () => state.xmode === "time" ? "UTC" : "distance along track (km)";
  const minmax = (a) => { let lo = Infinity, hi = -Infinity; for (const x of a) if (x != null) { if (x < lo) lo = x; if (x > hi) hi = x; } return [lo, hi]; };
  const cssId = (s) => s.replace(/[^a-z0-9]+/gi, "_");
  const legById = (id) => M.legs.find((l) => l.id === id);
  const legByIndex = (i) => M.legs[i];
  const shownLegs = () => M.legs.filter((l) => !state.hidden.has(l.id));

  // ------------------------------------------------------------ leg filter
  // Rows of hidden legs become null so lines break there instead of bridging
  // across a leg that is switched off.
  function applyLegFilter(raw) {
    const hiddenIdx = new Set(M.legs.filter((l) => state.hidden.has(l.id)).map((l) => l.index));
    if (!hiddenIdx.size) return raw;
    const mask = raw.leg.map((c) => c == null || !hiddenIdx.has(c));
    const nul = (arr) => arr.map((v, i) => (mask[i] ? v : null));
    const vars = Object.fromEntries(Object.entries(raw.vars).map(([k, v]) => [k, nul(v)]));
    const shown = mask.filter(Boolean).length;
    return { ...raw, lat: nul(raw.lat), lon: nul(raw.lon), dist_km: nul(raw.dist_km), vars, shown,
             limits: Object.fromEntries(Object.entries(vars).map(([k, v]) => [k, quantileLimits(v)])) };
  }

  function quantileLimits(vals) {
    const a = vals.filter((x) => x != null).sort((x, y) => x - y);
    if (a.length < 2) return null;
    const q = (p) => a[Math.min(a.length - 1, Math.floor(p * (a.length - 1)))];
    let lo = q(0.05), hi = q(0.95);
    if (lo === hi) { lo -= 0.5; hi += 0.5; }
    return [lo, hi];
  }

  function renderLegMenu() {
    const ul = $("#leglist");
    ul.innerHTML = "";
    // which legs actually have points in the current window
    const inWindow = new Set((state.raw?.leg || []).filter((c) => c != null));
    for (const l of [...M.legs].sort((a, b) => (b.year * 100 + b.number) - (a.year * 100 + a.number))) {
      const li = document.createElement("li");
      const span = l.first_date && l.last_date
        ? `${l.first_date.slice(4, 6)}/${l.first_date.slice(6)} – ${l.last_date.slice(4, 6)}/${l.last_date.slice(6)}` : "";
      li.innerHTML = `<label class="${inWindow.has(l.index) ? "" : "outside"}"><input type="checkbox" ${state.hidden.has(l.id) ? "" : "checked"}>
        <span class="name">${l.label}</span>${l.live ? '<span class="live">live</span>' : ""}
        <span class="span">${span}</span><span class="n">${l.files} d</span>
        ${inWindow.has(l.index) ? "" : '<span class="pend">outside span</span>'}</label>`;
      li.querySelector("input").onchange = (e) => {
        e.target.checked ? state.hidden.delete(l.id) : state.hidden.add(l.id);
        store.set("hiddenLegs", [...state.hidden]);
        state.view = null;
        applyAndRender();
      };
      ul.appendChild(li);
    }
    $("#legsummary").textContent = `Legs · ${shownLegs().length}/${M.legs.length}`;
    $("#legfoot").textContent = `${inWindow.size} leg${inWindow.size === 1 ? "" : "s"} fall within the current span`;
    $("#legall").onclick = (e) => { e.preventDefault(); state.hidden.clear(); store.set("hiddenLegs", []); requestFit(); applyAndRender(); };
    $("#legnone").onclick = (e) => { e.preventDefault(); state.hidden = new Set(M.legs.map((l) => l.id)); store.set("hiddenLegs", [...state.hidden]); applyAndRender(); };
  }

  // ------------------------------------------------------------ header
  function renderStatus() {
    const d = state.data;
    const end = M.data_range.end;
    const stale = (Date.now() - new Date(end)) > 30 * 60 * 1000;
    const live = legById(M.live);
    const pos = M.latest ? dms(M.latest.lat, M.latest.lon) : "position unknown";
    $("#status").innerHTML =
      `${live ? `<b>${live.label}</b> <span class="live">live</span> · ` : ""}latest data <b>${fmtLocal(end)}</b> ship time` +
      ` (${stale ? `<span class="stale">${ago(end)}</span>` : ago(end)}) · <b>${pos}</b>` +
      (d ? ` · <b>${(d.shown ?? d.n).toLocaleString()}</b> points @ ${d.step_s}s` : "");
    $("#gen").textContent = M.generated_utc.replace("T", " ").slice(0, 16) + "Z";
  }

  function renderControls() {
    const r = $("#span"), ticks = $("#spanticks");
    const labels = M.windows.map((w) => w.label);
    r.max = labels.length - 1;
    ticks.innerHTML = labels.map((l, i) => `<option value="${i}" label="${l}"></option>`).join("");
    let idx = labels.indexOf(state.win);
    if (idx < 0) idx = Math.max(0, labels.indexOf(M.default_window));
    r.value = idx;
    $("#spanlabel").textContent = labels[idx];
    r.oninput = () => { $("#spanlabel").textContent = labels[r.value]; };
    r.onchange = () => { state.win = labels[r.value]; store.set("win", state.win); requestFit(); loadWindow(); };

    for (const b of $("#xmode").querySelectorAll("button")) {
      b.classList.toggle("on", b.dataset.x === state.xmode);
      b.onclick = () => { state.xmode = b.dataset.x; store.set("xmode", state.xmode); renderControls(); renderPanels(); window.UW?.onXMode?.(); };
    }
    const sel = $("#colour");
    sel.innerHTML = "";
    for (const v of M.variables) {
      if (!v.resolved) continue;
      const o = document.createElement("option");
      o.value = v.name; o.textContent = v.name;
      sel.appendChild(o);
    }
    if (!VAR[state.colour]?.resolved) state.colour = M.variables.find((v) => v.resolved && !v.derived)?.name || M.variables[0].name;
    sel.value = state.colour;
    sel.onchange = () => { state.colour = sel.value; store.set("colour", sel.value); render(); };

    $("#bathy").checked = state.bathy;
    $("#bathy").onchange = (e) => { state.bathy = e.target.checked; store.set("bathy", state.bathy); renderMap(); };
    $("#stations").checked = state.stations;
    $("#stations").onchange = (e) => { state.stations = e.target.checked; store.set("stations", state.stations); renderMap(); };
    $("#mapreset").onclick = () => { requestFit(); state.focus = null; renderMap(); };
  }

  // ------------------------------------------------------------ basemap
  const DEPTH_FILL = [[0, "#28556f"], [200, "#234b66"], [1000, "#1d405b"], [2000, "#183650"], [3000, "#142d45"],
    [4000, "#10253a"], [5000, "#0d1e30"], [6000, "#0a1828"], [7000, "#081422"], [8000, "#07111d"], [9000, "#060e19"], [10000, "#050c15"]];

  async function loadGeo() {
    if (state.geo || !SITE.geo_layers?.length) return;
    const get = async (name) => {
      try { const r = await fetch(`static/geo/${name}`); return r.ok ? await r.json() : null; } catch { return null; }
    };
    const [bathy, land, glac, coast, isl] = await Promise.all(
      ["bathymetry.geojson", "land.geojson", "glaciated_areas.geojson", "coastline.geojson", "minor_islands.geojson"].map(get));
    const layers = [];
    if (bathy) {
      const byDepth = {};
      for (const f of bathy.features) (byDepth[f.properties.depth] ||= []).push(f);
      for (const [depth, color] of DEPTH_FILL) {
        if (!byDepth[depth]) continue;
        layers.push({ sourcetype: "geojson", source: { type: "FeatureCollection", features: byDepth[depth] },
          type: "fill", color, opacity: 1, below: "traces", name: "bathy" });
      }
    }
    if (land) layers.push({ sourcetype: "geojson", source: land, type: "fill", color: "#2b3441", below: "traces", name: "land" });
    if (isl) layers.push({ sourcetype: "geojson", source: isl, type: "fill", color: "#2b3441", below: "traces", name: "land" });
    if (glac) layers.push({ sourcetype: "geojson", source: glac, type: "fill", color: "#dfe7ef", opacity: .9, below: "traces", name: "ice" });
    if (coast) layers.push({ sourcetype: "geojson", source: coast, type: "line", color: "#8ea3ba", line: { width: 1 }, below: "traces", name: "coast" });
    state.geo = layers;
  }

  // The GEBCO pyramid goes into the style as a proper source so MapLibre knows
  // its maxzoom and scales the deepest tiles at closer zooms; a Plotly layer
  // shorthand cannot say that, and the raster simply vanished past zoom 9.
  function mapStyle(withRaster) {
    const style = { version: 8, sources: {}, layers: [{ id: "bg", type: "background", paint: { "background-color": "#0b1620" } }] };
    if (withRaster && SITE.raster) {
      const base = location.origin + location.pathname.replace(/[^/]*$/, "");
      style.sources.gebco = { type: "raster", tiles: [base + SITE.raster.url], tileSize: 256,
                              minzoom: SITE.raster.minzoom, maxzoom: SITE.raster.maxzoom, attribution: SITE.raster.attribution };
      style.layers.push({ id: "gebco", type: "raster", source: "gebco", paint: { "raster-opacity": 1, "raster-resampling": "linear" } });
    }
    return style;
  }

  // Web-Mercator zoom that fits a lat/lon box into the map element, minus a margin.
  function fitView(lats, lons) {
    const el = $("#map");
    const W = Math.max(200, el.clientWidth), H = Math.max(200, el.clientHeight);
    let [lat0, lat1] = minmax(lats), [lon0, lon1] = minmax(lons);
    if (!isFinite(lat0) || !isFinite(lon0)) return { center: { lat: 70, lon: -90 }, zoom: 3 };
    const minSpan = 0.05;                                       // a stationary ship still gets a sensible box
    if (lat1 - lat0 < minSpan) { lat0 -= minSpan / 2; lat1 += minSpan / 2; }
    if (lon1 - lon0 < minSpan) { lon0 -= minSpan / 2; lon1 += minSpan / 2; }
    const mercY = (lat) => { const r = lat * Math.PI / 180; return Math.log(Math.tan(Math.PI / 4 + r / 2)) / (2 * Math.PI); };
    const zLon = Math.log2((W / 512) * 360 / (lon1 - lon0));
    const zLat = Math.log2((H / 512) / (mercY(lat1) - mercY(lat0)));
    const zoom = Math.min(zLon, zLat) - 0.35;
    const cLat = Math.atan(Math.sinh(Math.PI * ((mercY(lat0) + mercY(lat1))))) * 180 / Math.PI;
    return { center: { lat: cLat, lon: (lon0 + lon1) / 2 }, zoom: Math.max(1, Math.min(14, zoom)) };
  }

  // ------------------------------------------------------------ map
  // Ask the next render to fit the track. Stray relayout events (a page
  // reflow fires one on phones) must not put a stale view back before then.
  function requestFit() { state.view = null; state.fitPending = true; }
  // Centre the map on a point (a table row, an event) and mark it.
  function focusMap(lat, lon, label) {
    if (lat == null || lon == null) return;
    const zoom = Math.max(state.view?.zoom ?? fitView(state.data?.lat || [lat], state.data?.lon || [lon]).zoom, 6);
    state.view = { center: { lat: +lat, lon: +lon }, zoom }; state.fitPending = false;
    state.focus = { lat: +lat, lon: +lon, label: label || "" };
    renderMap();
  }
  function mapMessage(text) { const m = $("#mapmsg"); m.hidden = !text; m.textContent = text || ""; }

  function renderMap() {
    const d = state.data;
    const el = $("#map");
    if (!d || !(d.shown ?? d.n)) { Plotly.purge(el); mapMessage(d ? "nothing to show: no legs selected in this span" : "no data"); $("#mapfoot").textContent = ""; return; }
    mapMessage("");

    const v = VAR[state.colour];
    const c = d.vars[state.colour] || [];
    const lim = d.limits[state.colour] || minmax(c);
    const hover = d.t.map((ms, i) => d.lat[i] == null ? "" :
      `<b>${legByIndex(d.leg[i])?.label || ""}</b> · ${fmtUTC(ms)}<br>${state.colour}: <b>${fmtVal(c[i], v?.unit)}</b>` +
      `<br>${dms(d.lat[i], d.lon[i])}<br>${(d.dist_km[i] ?? 0).toFixed(1)} km along track`);

    // draw order is click order: MVP tows from the cast tab go under the
    // track, and the station markers stay on top so they get the clicks
    const traces = [...(window.UW?.extraMapTraces?.() || [])];
    traces.push({
      type: "scattermap", mode: "lines+markers", name: "track",
      lat: d.lat, lon: d.lon, text: hover, hoverinfo: "text", connectgaps: false,
      line: { width: 1.4, color: "rgba(200,215,230,.5)" },
      marker: { size: 6, color: c, colorscale: v?.cmap || "Viridis", cmin: lim?.[0], cmax: lim?.[1], showscale: true,
                colorbar: { title: { text: state.colour, side: "right" }, thickness: 12, len: .55, x: 1.0,
                  tickfont: { size: 10 }, outlinewidth: 0, bgcolor: "rgba(15,20,25,.6)" } },
    });
    const li = (() => { for (let i = d.lat.length - 1; i >= 0; i--) if (d.lat[i] != null) return i; return -1; })();
    if (li >= 0) traces.push({
      type: "scattermap", mode: "markers", name: "latest", showlegend: false,
      lat: [d.lat[li]], lon: [d.lon[li]], hoverinfo: "text", text: [`latest · ${fmtUTC(d.t[li])}`],
      marker: { size: 16, color: "#ffb454", opacity: .95 },
    });
    const shownIds = new Set(shownLegs().map((l) => l.id));
    const st = state.stations ? (M.stations || []).filter((s) => shownIds.has(s.leg)) : [];
    const selected = window.UW?.selectedCastKeys?.() || new Set();
    if (st.length) traces.push({
      type: "scattermap", mode: "markers", name: "CTD stations", showlegend: false,
      lat: st.map((s) => s.lat), lon: st.map((s) => s.lon), hoverinfo: "text",
      customdata: st.map((s) => `${s.leg}:CTD_${String(s.cast).padStart(3, "0")}`),
      text: st.map((s) => `<b>Cast ${s.cast}</b> ${s.station}${s.label ? " · " + s.label : ""} · ${legById(s.leg)?.label || s.leg}` +
        `<br>${s.time || ""}${s.type ? "<br>" + s.type : ""}${s.bottom_m != null ? `<br>bottom ${s.bottom_m} m` : ""}` +
        `${s.comments ? "<br><i>" + s.comments + "</i>" : ""}`),
      marker: { size: st.map((s) => selected.has(`${s.leg}:CTD_${String(s.cast).padStart(3, "0")}`) ? 14 : 9),
                color: st.map((s) => selected.has(`${s.leg}:CTD_${String(s.cast).padStart(3, "0")}`) ? "#ffb454" : "rgba(255,255,255,.9)"),
                opacity: .95 },
    });
    // an all-but-invisible oversized copy on top gives each station a generous
    // click target without changing how it looks
    if (st.length) traces.push({
      type: "scattermap", mode: "markers", name: "station hit targets", showlegend: false, hoverinfo: "skip",
      lat: st.map((s) => s.lat), lon: st.map((s) => s.lon),
      customdata: st.map((s) => `${s.leg}:CTD_${String(s.cast).padStart(3, "0")}`),
      marker: { size: 26, color: "rgba(255,255,255,0.02)" },
    });

    if (state.focus) traces.push({
      type: "scattermap", mode: "markers", name: "focus", showlegend: false, hoverinfo: "text", text: [state.focus.label],
      lat: [state.focus.lat, state.focus.lat], lon: [state.focus.lon, state.focus.lon],
      marker: { size: [22, 12], color: ["#5cc8ff", "#0f1419"], opacity: [.9, 1] },
    });

    const view = (!state.fitPending && state.view) || fitView(d.lat, d.lon);
    // With a GEBCO tile pyramid the shaded raster carries both bathymetry and
    // land relief, so the Natural Earth depth bands and land fills stay out of
    // the way (glaciers become a light wash, coastlines stay); without it the
    // bands are what the toggle controls.
    const relief = state.bathy && SITE.raster;
    const layers = [];
    for (const l of (state.geo || [])) {
      if (l.name === "bathy" && (!state.bathy || SITE.raster)) continue;
      if (l.name === "land" && relief) continue;
      layers.push(l.name === "ice" && relief ? { ...l, opacity: .35 } : l);
    }
    const layout = { ...THEME, margin: { l: 0, r: 0, t: 0, b: 0 }, showlegend: false, dragmode: "pan",
                     map: { style: mapStyle(state.bathy), center: view.center, zoom: view.zoom, layers } };
    Plotly.react(el, traces, layout, CFG).then(() => {
      state.fitPending = false;
      el.removeAllListeners?.("plotly_relayout");
      el.on("plotly_relayout", (ev) => {
        if (state.fitPending) return;
        const c2 = ev["map.center"], z = ev["map.zoom"];
        if (c2 || z != null) state.view = { center: c2 || state.view?.center || view.center, zoom: z ?? state.view?.zoom ?? view.zoom };
      });
      el.removeAllListeners?.("plotly_click");
      el.on("plotly_click", (ev) => {
        const p = ev.points?.[0];
        if (p?.customdata) window.UW?.onStationClick?.(p.customdata);
      });
    });

    const km = lastFinite(d.dist_km) ?? 0;
    const spd = lastFinite(d.vars["Ship speed (kn)"] || []);
    const legsIn = new Set(d.leg.filter((x, i) => x != null && d.lat[i] != null)).size;
    $("#mapfoot").innerHTML =
      `<span><b>${d.label}</b> span · <b>${km.toFixed(0)} km</b> travelled · ${legsIn} leg${legsIn === 1 ? "" : "s"}</span>` +
      (spd != null ? `<span>speed <b>${spd.toFixed(1)} kn</b></span>` : "") +
      (st.length ? `<span><b>${st.length}</b> CTD casts</span>` : "") +
      `<span class="mono">${d.start.slice(0, 16)}Z → ${d.end.slice(0, 16)}Z</span>` +
      `<span class="hint">scroll to zoom · drag to pan · ⟲ fits</span>`;
  }

  // ------------------------------------------------------------ panels
  function panelNames() {
    const all = M.variables.map((v) => v.name).filter((n) => !NOT_PANELS.has(n));
    const ordered = state.order.filter((n) => all.includes(n));
    return [...ordered, ...all.filter((n) => !ordered.includes(n))];
  }

  function panelEl(name) {
    let el = document.getElementById("p-" + cssId(name));
    if (el) return el;
    const v = VAR[name];
    el = document.createElement("section");
    el.className = "panel card"; el.id = "p-" + cssId(name); el.dataset.name = name; el.draggable = true;
    if (name.startsWith("Surprise")) el.classList.add("surprise");
    el.innerHTML = `<div class="head">
        <span class="handle" title="drag to reorder">⋮⋮</span>
        <h3 title="colour everything by this variable">${name}</h3>
        <div class="tools"><span class="now"></span>
          ${v?.log_ok ? '<button class="log" title="log10 y-axis">log</button>' : ""}
          <button class="reset" title="reset zoom">⟲</button>
          <button class="wide" title="expand">⤢</button>
          <button class="min" title="minimise to the bottom bar">—</button>
        </div></div><div class="plot"></div>`;
    el.querySelector("h3").onclick = () => { state.colour = name; store.set("colour", name); $("#colour").value = name; render(); };
    el.querySelector(".log")?.addEventListener("click", () => { state.log[name] = !state.log[name]; store.set("log", state.log); renderPanel(name); });
    el.querySelector(".reset").onclick = () => Plotly.relayout(el.querySelector(".plot"), { "xaxis.autorange": true, "yaxis.autorange": true });
    el.querySelector(".wide").onclick = () => setPanelState(name, state.panel[name] === "wide" ? null : "wide");
    el.querySelector(".min").onclick = () => setPanelState(name, "min");
    el.addEventListener("dragstart", (e) => { e.dataTransfer.setData("text/plain", name); el.classList.add("dragging"); });
    el.addEventListener("dragend", () => el.classList.remove("dragging"));
    el.addEventListener("dragover", (e) => { e.preventDefault(); el.classList.add("over"); });
    el.addEventListener("dragleave", () => el.classList.remove("over"));
    el.addEventListener("drop", (e) => {
      e.preventDefault(); el.classList.remove("over");
      const from = e.dataTransfer.getData("text/plain");
      if (!from || from === name) return;
      const order = panelNames().filter((n) => n !== from);
      const at = order.indexOf(name);
      order.splice(e.offsetX < el.clientWidth / 2 ? at : at + 1, 0, from);
      state.order = order; store.set("order", order);
      layoutPanels();
    });
    return el;
  }

  function setPanelState(name, s) {
    if (s) state.panel[name] = s; else delete state.panel[name];
    store.set("panel", state.panel);
    layoutPanels();
    renderPanel(name);
  }

  function layoutPanels() {
    const grid = $("#panels"), dock = $("#dock");
    for (const name of panelNames()) {
      const el = panelEl(name);
      const s = state.panel[name];
      el.classList.toggle("wide", s === "wide");
      if (s === "min") {
        let chip = document.getElementById("c-" + cssId(name));
        if (!chip) {
          chip = document.createElement("button");
          chip.className = "chip"; chip.id = "c-" + cssId(name);
          chip.onclick = () => setPanelState(name, null);
          dock.appendChild(chip);
        }
        const y = state.data?.vars[name];
        chip.innerHTML = `${name} <b>${fmtVal(y ? lastFinite(y) : null, VAR[name]?.unit)}</b> <span>▲</span>`;
        if (el.parentElement) el.remove();
      } else {
        document.getElementById("c-" + cssId(name))?.remove();
        grid.appendChild(el);
      }
    }
    dock.hidden = !dock.children.length;
    for (const el of grid.children) { const p = el.querySelector(".plot"); if (p?.data) Plotly.Plots.resize(p); }
  }

  function renderPanel(name) {
    if (state.panel[name] === "min") { layoutPanels(); return; }
    const d = state.data, v = VAR[name], el = panelEl(name);
    const plot = el.querySelector(".plot");
    el.classList.toggle("on", name === state.colour);
    el.classList.toggle("unresolved", !v.resolved);
    el.querySelector(".log")?.classList.toggle("on", !!state.log[name]);
    el.querySelector(".wide").classList.toggle("on", state.panel[name] === "wide");
    let title = name;
    let y = d?.vars[name];
    if (name === SURPRISE) {
      const sc = surpriseScale();
      if (sc && d?.vars[`Surprise · ${sc}`]) { y = d.vars[`Surprise · ${sc}`]; title = `Surprise · ${sc}`; }
    }
    el.querySelector("h3").textContent = title;
    const empty = (msg) => { Plotly.purge(plot); plot.className = "empty"; plot.textContent = msg; el.querySelector(".now").textContent = ""; };
    if (!v.resolved) return empty("source column not found in any leg");
    if (!d || !y || !y.some((x) => x != null)) return empty("no data in this span for the selected legs");
    plot.className = "plot";
    el.querySelector(".now").textContent = fmtVal(lastFinite(y), v.unit);

    const cv = VAR[state.colour];
    const c = d.vars[state.colour] || [];
    const lim = d.limits[state.colour] || minmax(c);
    // SVG, not WebGL: a dozen scattergl panels plus the map exceed the
    // browser's WebGL context limit (Safari's is 8) and the map is what gets
    // dropped. Panels never carry more than a few thousand points.
    const trace = {
      x: xvals(d), y, type: "scatter", mode: v.circular ? "markers" : "lines+markers", name,
      line: { width: 1, color: "rgba(160,180,200,.45)" }, connectgaps: false,
      marker: { size: v.circular ? 4 : 3.5, color: c, colorscale: cv?.cmap || "Viridis", cmin: lim?.[0], cmax: lim?.[1], showscale: false },
      text: d.leg.map((i) => legByIndex(i)?.label || ""),
      hovertemplate: `%{y:.3~f} ${v.unit}<br>%{x}<br>%{text}<extra></extra>`,
    };
    const useLog = !!state.log[name] && y.some((q) => q > 0);
    const layout = {
      ...THEME, margin: { l: 46, r: 8, t: 6, b: 28 }, showlegend: false, hovermode: "closest", hoverdistance: 14, dragmode: "pan",
      xaxis: { ...THEME.xaxis, title: { text: xTitle(), font: { size: 10 }, standoff: 4 }, tickfont: { size: 10 },
               type: state.xmode === "time" ? "date" : "linear",
               hoverformat: state.xmode === "time" ? "%Y-%m-%d %H:%M:%SZ" : ".1f",
               ticksuffix: state.xmode === "time" ? "" : " km" },
      yaxis: { ...THEME.yaxis, title: { text: v.unit, font: { size: 10 }, standoff: 2 }, tickfont: { size: 10 },
               type: useLog ? "log" : "linear", ...(v.circular ? { range: [0, 360], dtick: 90 } : {}) },
    };
    if (name.startsWith("Surprise")) {
      const top = Math.max(3.5, minmax(y)[1] * 1.08);
      layout.yaxis.range = [0, top];
      layout.shapes = [{ type: "rect", xref: "paper", x0: 0, x1: 1, yref: "y", y0: 3, y1: top,
                         fillcolor: "rgba(255,180,84,.10)", line: { width: 0 } }];
    }
    Plotly.react(plot, [trace], layout, CFG).then(() => axisZoom(plot));
  }

  function renderPanels() {
    layoutPanels();
    for (const name of panelNames()) renderPanel(name);
  }

  // ------------------------------------------------------------ provenance
  function renderProvenance() {
    const legCols = M.legs.map((l) => `<th title="${l.label}">${l.year % 100}·${l.number}</th>`).join("");
    const rows = M.variables.map((v) =>
      `<tr><td>${v.name}</td><td class="src ${v.resolved ? "" : "bad"}">${v.derived ? "<i>derived</i>" : (v.source || "not found")}</td>` +
      M.legs.map((l) => `<td class="cov">${v.derived ? "·" : (v.coverage?.[l.id] ? "✓" : '<span class="bad">–</span>')}</td>`).join("") + "</tr>");
    $("#sources").innerHTML = `<tr><th>panel</th><th>source column (Instrument — Variable)</th>${legCols}</tr>${rows.join("")}`;
    const f = M.files;
    $("#notes").innerHTML =
      `<p><b>Surprise</b>: ${M.surprise.note || "not computed"}. Each scale is −log10 of the χ² p-value of the Mahalanobis distance from an exponentially weighted mean and covariance of the minutes before (capped at 6); the combined score is the mean over scales. Above 3 is shaded.</p>` +
      `<p><b>Zooming</b>: scroll zooms a graph, Shift+scroll its x axis only, Ctrl+scroll its y axis only; double-click resets.</p>` +
      `<p><b>Inputs</b>: ${f.total} daily files across ${M.legs.length} legs; latest <code>${f.latest}</code>.</p>` +
      `<p><b>Record</b>: ${M.data_range.start.slice(0, 16)}Z → ${M.data_range.end.slice(0, 16)}Z. ${M.columns_seen.length} distinct columns seen; ` +
      `the per-leg columns show where a source column exists.</p>` +
      `<p>Axes are UTC; the header shows ship time (${SITE.local_tz}). Gaps in lines are missing data, not interpolation. ` +
      `Basemap: ${SITE.raster ? "GEBCO 2024 shaded relief — bathymetry and land (15 arc-second grid) — and " : ""}Natural Earth 10 m coastline, land and glaciers${SITE.raster ? "" : " and depth bands"}, all served locally; Web Mercator.</p>`;
  }

  // ------------------------------------------------------------ data flow
  async function fetchJSON(url) {
    const r = await fetch(url, { cache: "no-store" });
    if (!r.ok) throw new Error(`${r.status} ${url}`);
    return r.json();
  }

  function applyAndRender() {
    if (!state.raw) return;
    state.data = applyLegFilter(state.raw);
    renderLegMenu();
    render();
  }

  async function loadWindow() {
    const w = M.windows.find((x) => x.label === state.win) || M.windows.find((x) => x.label === M.default_window) || M.windows[0];
    state.win = w.label;
    renderControls();
    state.raw = await fetchJSON(`${w.file}?v=${encodeURIComponent(M.generated_utc)}`);
    applyAndRender();
  }

  function render() {
    renderStatus();
    renderMap();
    renderPanels();
  }

  async function checkForUpdate() {
    try {
      const m = await fetchJSON(`data/manifest.json?t=${Date.now()}`);
      if (m.generated_utc !== M.generated_utc) {
        M = m; VAR = Object.fromEntries(M.variables.map((v) => [v.name, v]));
        renderProvenance(); await loadWindow(); renderAlert();
      } else renderStatus();
    } catch { /* offline or mid-write; next tick */ }
  }

  // the latest change to the intranet schedule or whiteboard, until dismissed
  function renderAlert() {
    const u = M.calendar?.update;
    const bar = $("#alert");
    if (!u || !u.text || store.get("alert.seen") === u.changed_utc) { bar.hidden = true; return; }
    $("#alerttext").innerHTML = `<b>${fmtLocal(u.changed_utc)}</b> · ${u.text.replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]))}`;
    bar.hidden = false;
    $("#alertclose").onclick = () => { store.set("alert.seen", u.changed_utc); bar.hidden = true; };
    $("#alertgo").onclick = () => { showTab("calendar"); };
  }

  // ------------------------------------------------------------ tabs
  // The map stays; the right-hand pane and the header controls swap.
  function showTab(name) {
    for (const b of $("#tabs").querySelectorAll("button")) b.classList.toggle("on", b.dataset.tab === name);
    for (const p of document.querySelectorAll(".pane")) p.hidden = p.id !== "pane-" + name;
    document.querySelector("main").className = "tab-" + name;
    // the underway controls stay for the cast tab too: its section view
    // follows the Time/Distance switch
    $("#controls-underway").hidden = !(name === "underway" || name === "casts");
    for (const el of $("#controls-underway").querySelectorAll(".legmenu, .span, .select")) el.hidden = name === "casts";
    $("#maphint").hidden = name !== "casts";
    store.set("tab", name);
    window.UW?.onTab?.(name);
    if (name === "underway") setTimeout(() => { for (const el of $("#panels").children) { const p = el.querySelector(".plot"); if (p?.data) Plotly.Plots.resize(p); } }, 0);
  }
  for (const b of $("#tabs").querySelectorAll("button")) b.onclick = () => showTab(b.dataset.tab);

  // hooks for tabs.js
  window.UW = Object.assign(window.UW || {}, {
    get M() { return M; }, get state() { return state; }, SITE, THEME, CFG,
    fmtUTC, fmtVal, dms, legById, minmax, store,
    renderMap, showTab, focusMap, requestFit, axisZoom,
  });

  (async () => {
    renderControls();
    renderProvenance();
    showTab(store.get("tab", "underway"));
    await loadGeo();
    await loadWindow(); renderAlert();
    setInterval(checkForUpdate, 30 * 1000);
    window.addEventListener("resize", () => { Plotly.Plots.resize($("#map")); });
    document.addEventListener("click", (e) => { const m = $("#legmenu"); if (m.open && !m.contains(e.target)) m.open = false; });
  })();
})();
