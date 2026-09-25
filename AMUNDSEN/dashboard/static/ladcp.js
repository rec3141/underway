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

  // Mercator offsets keep a 0.5 m/s vector 36 pixels long at the current zoom.
  function arrowPoints(lat, lon, u, v, zoom) {
    const speed = Math.hypot(u, v);
    if (!speed) return [];
    const scale = 72 * 360 / (512 * 2 ** zoom), dx = u * scale, dy = v * scale;
    const mercY = Math.log(Math.tan(Math.PI / 4 + lat * Math.PI / 360)) * 180 / Math.PI;
    const point = (x, y) => [lon + x, (2 * Math.atan(Math.exp((mercY + y) * Math.PI / 180)) - Math.PI / 2) * 180 / Math.PI];
    const head = 0.28;
    return [point(0, 0), point(dx, dy), point(dx - head * dx - head * dy, dy - head * dy + head * dx),
      point(dx, dy), point(dx - head * dx + head * dy, dy - head * dy - head * dx)];
  }
  const speedColour = (speed) => speed < 0.1 ? "#38bdf8" : speed < 0.3 ? "#fbbf24" : "#f472b6";
  window.UWLadcp = { sampleAt, arrowPoints, speedColour };
  const UW = window.UW, host = typeof document !== "undefined" && document.querySelector("#ladcp-controls");
  if (!UW || !host) return;
  const esc = (s) => String(s ?? "").replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));
  let enabled = UW.store.get("ladcp.enabled", false), depth = UW.store.get("ladcp.depth", 50);
  if (!finite(depth) || depth < 0) depth = 50;
  let profiles = [], loaded = null, loading = null, failedAt = 0, failed = false;
  host.innerHTML = `<button id="ladcp-toggle" type="button" aria-pressed="false" title="Current vectors at sampled CTD stations, for the selected legs and time span">LADCP currents</button>
    <label id="ladcp-depth-label" hidden>Depth <input id="ladcp-depth" type="number" min="0" max="12000" step="1" aria-label="Current depth in metres"> m</label>
    <input id="ladcp-depth-slider" type="range" min="0" max="1000" step="1" aria-label="Current depth in metres" hidden>
    <span id="ladcp-status" class="hint" role="status" hidden></span>
    <span id="ladcp-key" class="hint" hidden>Arrows point in the direction of flow · 0.5 m/s = 36 px · <span style="color:#38bdf8">&lt;0.1</span> / <span style="color:#fbbf24">0.1–0.3</span> / <span style="color:#f472b6">≥0.3 m/s</span></span>`;
  const button = host.querySelector("#ladcp-toggle"), input = host.querySelector("#ladcp-depth"), status = host.querySelector("#ladcp-status");
  input.value = depth;
  const slider = host.querySelector("#ladcp-depth-slider");
  let redraw = null;
  function syncDepth(visible = profiles) {
    const deepest = visible.reduce((max, p) => Math.max(max, p.depth?.at(-1) || 0), 0);
    slider.max = Math.max(50, Math.ceil(deepest / 50) * 50, depth);
    slider.value = depth;
    slider.setAttribute("aria-valuetext", `${depth} metres`);
    input.value = depth;
  }
  syncDepth();
  function controls() {
    button.classList.toggle("on", enabled); button.setAttribute("aria-pressed", String(enabled));
    for (const id of ["#ladcp-depth-label", "#ladcp-depth-slider", "#ladcp-status", "#ladcp-key"]) host.querySelector(id).hidden = !enabled;
  }
  button.onclick = () => { enabled = !enabled; UW.store.set("ladcp.enabled", enabled); controls(); if (!enabled) UW.setLoadError("LADCP", false); UW.renderMap(); };
  input.onchange = () => {
    const next = input.valueAsNumber;
    if (!finite(next) || next < 0 || next > 12000) { input.value = depth; return; }
    depth = next; syncDepth(); UW.store.set("ladcp.depth", depth); UW.renderMap();
  };
  slider.oninput = () => {
    depth = slider.valueAsNumber; input.value = depth;
    slider.setAttribute("aria-valuetext", `${depth} metres`);
    UW.store.set("ladcp.depth", depth);
    if (redraw == null) redraw = requestAnimationFrame(() => { redraw = null; UW.renderMap(); });
  };
  controls();
  async function load() {
    const generation = UW.M.generated_utc;
    if (loaded === generation || loading || Date.now() - failedAt < 15000) return;
    loading = generation; status.textContent = "Loading station currents…";
    try {
      const index = await UW.fetchJSON(`${UW.M.casts?.index || "data/casts/index.json"}?v=${encodeURIComponent(generation)}`);
      const data = index.ladcp_file ? await UW.fetchJSON(`${index.ladcp_file}?v=${encodeURIComponent(generation)}`) : { casts: [] };
      if (!Array.isArray(data.casts)) throw new Error("Invalid LADCP index");
      if (UW.M.generated_utc === generation) { profiles = data.casts; loaded = generation; failed = false; UW.setLoadError("LADCP", false); }
    } catch { failedAt = Date.now(); failed = true; UW.setLoadError("LADCP", true); }
    finally { loading = null; UW.renderMap(); }
  }
  UW.ladcpMapTraces = (zoom = 6) => {
    if (!enabled) return [];
    void load();
    const f = UW.spanFilter(), visible = profiles.filter((p) => finite(p.lat) && finite(p.lon) && UW.inFilter(p.leg, p.time, f));
    syncDepth(visible);
    const samples = visible.map((p) => ({ p, s: sampleAt(p, depth) })).filter(({ s }) => s);
    status.textContent = `${samples.length}/${visible.length} casts at ${depth} m (nearest measured bin)${failed ? " · update unavailable" : loading ? " · updating…" : ""}`;
    if (!visible.length && !loading && !failed) status.textContent = profiles.length ? "No LADCP casts in this time span; expand the span or legs." : "No LADCP profiles available.";
    const out = [], selected = UW.selectedCastKeys?.() || new Set();
    for (const { p, s } of samples) {
      const points = arrowPoints(p.lat, p.lon, s.u, s.v, zoom);
      const text = `<b>LADCP cast ${esc(p.cast)}${p.station ? " · " + esc(p.station) : ""}</b><br>${esc(p.leg)} · ${esc(p.time)} UTC` +
        `<br>${s.depth.toFixed(1)} m · ${s.speed.toFixed(3)} m/s${s.direction == null ? " · calm" : ` toward ${s.direction.toFixed(0)}° true`}` +
        `<br>East ${s.u.toFixed(3)} · North ${s.v.toFixed(3)} m/s<br>Error velocity ${s.error == null ? "unavailable" : s.error.toFixed(3) + " m/s"}` +
        `<br>Click for current profile`;
      if (points.length) out.push({ type: "scattermap", mode: "lines", name: "LADCP currents", showlegend: false,
        lat: points.map((q) => q[1]), lon: points.map((q) => q[0]), text: points.map(() => text), customdata: points.map(() => p.id), hoverinfo: "text",
        line: { color: speedColour(s.speed), width: selected.has(p.id) ? 4 : 2.5 } });
      out.push({ type: "scattermap", mode: "markers", name: "LADCP stations", showlegend: false,
        lat: [p.lat], lon: [p.lon], text: [text], customdata: [p.id], hoverinfo: "text",
        marker: { size: selected.has(p.id) ? 11 : 8, color: speedColour(s.speed) } });
    }
    return out;
  };
  UW.ladcpEnabled = () => enabled;
  UW.renderMap();
})();
