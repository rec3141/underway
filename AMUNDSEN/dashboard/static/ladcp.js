/* Station current vectors from processed lowered ADCP profiles. */
(() => {
  "use strict";
  const finite = (v) => typeof v === "number" && Number.isFinite(v);
  function sampleAt(profile, depth) {
    const ds = profile.depth || [];
    if (!finite(depth) || !ds.length || depth < ds[0] || depth > ds.at(-1)) return null;
    const steps = ds.slice(1).map((d, i) => d - ds[i]).filter((d) => d > 0).sort((a, b) => a - b);
    const spacing = steps[Math.floor(steps.length / 2)] || 0;
    let i = 0;
    for (let j = 1; j < ds.length; j++) if (Math.abs(ds[j] - depth) < Math.abs(ds[i] - depth)) i = j;
    if (Math.abs(ds[i] - depth) > spacing / 2 + 0.1) return null;
    const u = profile.vars?.["Eastward current"]?.[i], v = profile.vars?.["Northward current"]?.[i];
    if (!finite(u) || !finite(v)) return null;
    const speed = Math.hypot(u, v), error = profile.vars?.["Current error"]?.[i];
    return { depth: ds[i], u, v, speed, direction: speed > 0 ? (Math.atan2(u, v) * 180 / Math.PI + 360) % 360 : null,
      error: finite(error) ? error : null };
  }

  // An arrow's length on screen in pixels: 0.1 m/s is 20 px, whatever the
  // zoom; the slowest keep a stub and the fastest stop at 120 px.
  const PX_PER_MPS = 200;
  const arrowLength = (speed) => Math.min(120, Math.max(6, speed * PX_PER_MPS));
  // The depth slider is quadratic, like the compressed depth axes: its
  // position is the square root of depth over the deepest, so the upper
  // water column, where most bins and most change are, gets most of its travel.
  const depthFromSlider = (position, top) => Math.round(top * position * position);
  const sliderFromDepth = (depth, top) => top > 0 ? Math.sqrt(Math.min(1, Math.max(0, depth / top))) : 0;
  window.UWLadcp = { sampleAt, arrowLength, depthFromSlider, sliderFromDepth };
  const UW = window.UW, host = typeof document !== "undefined" && document.querySelector("#ladcp-controls");
  if (!UW || !host) return;
  const esc = (s) => String(s ?? "").replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));
  let enabled = UW.store.get("ladcp.enabled", false), depth = UW.store.get("ladcp.depth", 50);
  if (!finite(depth) || depth < 0) depth = 50;
  let profiles = [], loaded = null, loading = null, failedAt = 0, failed = false;
  const KEY = "ADCP currents at the chosen depth, for the selected legs and span. Arrows point downstream; 0.1 m/s = 20 px.";
  host.innerHTML = `<button id="ladcp-toggle" type="button" aria-pressed="false" title="${esc(KEY)}">ADCP</button>
    <input id="ladcp-depth-slider" type="range" min="0" max="1" step="0.001" aria-label="Current depth in metres" hidden>
    <label id="ladcp-depth-label" hidden><input id="ladcp-depth" type="number" min="0" max="12000" step="1" aria-label="Current depth in metres"> m</label>`;
  const button = host.querySelector("#ladcp-toggle"), input = host.querySelector("#ladcp-depth");
  // what the arrows show, or why there are none, goes in the button's tooltip
  const status = (text) => { button.title = text ? `${text}\n${KEY}` : KEY; };
  input.value = depth;
  const slider = host.querySelector("#ladcp-depth-slider");
  let redraw = null, top = 50;                     // top: the depth at the slider's far end
  function syncDepth(visible = profiles) {
    const deepest = visible.reduce((max, p) => Math.max(max, p.depth?.at(-1) || 0), 0);
    top = Math.max(50, Math.ceil(deepest / 50) * 50, depth);
    slider.value = sliderFromDepth(depth, top);
    slider.setAttribute("aria-valuetext", `${depth} metres`);
    input.value = depth;
  }
  syncDepth();
  function controls() {
    button.classList.toggle("on", enabled); button.setAttribute("aria-pressed", String(enabled));
    for (const id of ["#ladcp-depth-label", "#ladcp-depth-slider"]) host.querySelector(id).hidden = !enabled;
  }
  button.onclick = () => { enabled = !enabled; UW.store.set("ladcp.enabled", enabled); controls(); if (!enabled) UW.setLoadError("LADCP", false); UW.renderMap(); };
  input.onchange = () => {
    const next = input.valueAsNumber;
    if (!finite(next) || next < 0 || next > 12000) { input.value = depth; return; }
    depth = next; syncDepth(); UW.store.set("ladcp.depth", depth); UW.renderMap();
  };
  slider.oninput = () => {
    depth = depthFromSlider(slider.valueAsNumber, top); input.value = depth;
    slider.setAttribute("aria-valuetext", `${depth} metres`);
    UW.store.set("ladcp.depth", depth);
    if (redraw == null) redraw = requestAnimationFrame(() => { redraw = null; UW.renderMap(); });
  };
  controls();
  async function load() {
    const generation = UW.M.generated_utc;
    if (loaded === generation || loading || Date.now() - failedAt < 15000) return;
    loading = generation; status("Loading station currents…");
    try {
      const index = await UW.fetchJSON(`${UW.M.casts?.index || "data/casts/index.json"}?v=${encodeURIComponent(generation)}`);
      const data = index.ladcp_file ? await UW.fetchJSON(`${index.ladcp_file}?v=${encodeURIComponent(generation)}`) : { casts: [] };
      if (!Array.isArray(data.casts)) throw new Error("Invalid LADCP index");
      if (UW.M.generated_utc === generation) { profiles = data.casts; loaded = generation; failed = false; UW.setLoadError("LADCP", false); }
    } catch { failedAt = Date.now(); failed = true; UW.setLoadError("LADCP", true); }
    finally { loading = null; UW.renderMap(); }
  }
  UW.ladcpMapTraces = () => {
    if (!enabled) return [];
    void load();
    const f = UW.spanFilter(), visible = profiles.filter((p) => finite(p.lat) && finite(p.lon) && UW.inFilter(p.leg, p.time, f));
    syncDepth(visible);
    const samples = visible.map((p) => ({ p, s: sampleAt(p, depth) })).filter(({ s }) => s);
    status(!visible.length && !loading && !failed ? (profiles.length ? "No ADCP casts in this time span" : "No ADCP profiles available")
      : `${samples.length}/${visible.length} casts at ${depth} m${failed ? " · update unavailable" : loading ? " · updating…" : ""}`);
    const C = UW.C || {}, selected = UW.selectedCastKeys?.() || new Set();
    const arrows = { plain: { lat: [], lon: [], text: [], customdata: [], angle: [], size: [] }, picked: { lat: [], lon: [], text: [], customdata: [], angle: [], size: [] } };
    for (const { p, s } of samples) {
      const text = `<b>ADCP cast ${esc(p.cast)}${p.station ? " · " + esc(p.station) : ""}</b><br>${esc(p.leg)} · ${esc(p.time)} UTC` +
        `<br>${s.depth.toFixed(1)} m · ${s.speed.toFixed(3)} m/s${s.direction == null ? " · calm" : ` toward ${s.direction.toFixed(0)}° true`}` +
        `<br>East ${s.u.toFixed(3)} · North ${s.v.toFixed(3)} m/s<br>Error velocity ${s.error == null ? "unavailable" : s.error.toFixed(3) + " m/s"}` +
        `<br>Click for current profile`;
      const a = arrows[selected.has(p.id) ? "picked" : "plain"];
      a.lat.push(p.lat); a.lon.push(p.lon); a.text.push(text); a.customdata.push(p.id);
      a.angle.push(s.direction ?? 0); a.size.push(s.direction == null ? 4 : arrowLength(s.speed));
    }
    // plain arrows in the map's arrow ink; a selected cast's arrow haloed in the accent
    const out = [];
    for (const [key, halo] of [["plain", C.mapArrowHalo], ["picked", C.accent]]) {
      const a = arrows[key];
      if (a.lat.length) out.push({ type: "scattermap", mode: "markers", name: "ADCP currents", showlegend: false, hoverinfo: "text",
        lat: a.lat, lon: a.lon, text: a.text, customdata: a.customdata,
        marker: { symbol: "arrow", angle: a.angle, size: a.size, color: C.mapArrow, line: { color: halo } } });
    }
    return out;
  };
  UW.renderMap();
})();
