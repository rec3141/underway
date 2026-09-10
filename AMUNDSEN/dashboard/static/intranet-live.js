// The ship intranet's live tables (navigation, atmosphere, sea-water surface,
// winches) as panels among the underway graphs: same card, same drag handle
// and minimise/maximise buttons, so they can be arranged like any other.
// The server polls the page every few seconds (api/intranet); this polls the
// server every 5 s while the Underway tab shows.
(() => {
  const UW = window.UW;
  if (!UW?.registerPanel) return;
  const TABLES = [
    ["Live · Navigation", /navigation/i, "position, speed, heading, track and bottom depth from the intranet's live page"],
    ["Live · Atmosphere", /atmospheric/i, "the met tower at 21.6 m: wind, pressure, air temperature, humidity"],
    ["Live · Sea water surface", /sea water/i, "the thermosalinograph intake at 7 m"],
    ["Live · Winches", /rosette|500hp/i, "rosette depth and rate; 500HP cable length and rate"],
  ];
  const esc = (x) => String(x ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  let latest = null;
  const els = new Map();                    // panel name -> its card, once rendered
  function draw(name, re, el, plot) {
    plot.className = "plot table"; if (plot.data) Plotly.purge(plot);
    const j = latest;
    const rows = (j?.sections || []).filter((s) => re.test(s.title)).flatMap((s) => s.rows);
    const old = !j || j.age_s == null || j.age_s > 30;
    el.querySelector(".now").textContent = j?.age_s != null ? `${j.age_s.toFixed(0)} s ago` : "";
    if (!rows.length) { plot.innerHTML = `<div class="stale">${esc(j?.error || "waiting for the intranet live page…")}</div>`; return; }
    const items = rows.map(([k, v]) => {
      if (/^time \(utc\)/i.test(k)) { const ms = UW.tms(v); return [`Time (${UW.tzAbbr()})`, isNaN(ms) ? v : UW.fmtTs(ms).slice(11)]; }
      return [k.replace(/^500HP /, "500HP · ").replace(/^Rosette /, "Rosette · "), /^(nan|null|none|)$/i.test(String(v).trim()) ? "—" : v];   // an idle instrument reports NaN
    });
    plot.innerHTML = `<dl>${items.map(([k, v]) => `<dt>${esc(k)}</dt><dd>${esc(v)}</dd>`).join("")}</dl>` + (old ? `<div class="stale">${esc(j.error || "not refreshed for a while")}</div>` : "");
  }
  for (const [name, re, description] of TABLES) {
    UW.registerPanel(name, { unit: "", resolved: true, log_ok: false, description, layoutRevision: "intranet-live-v1",
      render(el, plot) { els.set(name, [el, plot, re]); draw(name, re, el, plot); } });
  }
  // "76° 24.9565' N" -> 76.4159; "89° 12.6412' W" -> -89.2107
  const ddm = (s) => { const m = /(\d+)\D+([\d.]+)'?\s*([NSEW])/.exec(s || ""); if (!m) return null; const v = +m[1] + +m[2] / 60; return /[SW]/.test(m[3]) ? -v : v; };
  // the ship's position from the Navigation table, for the map's marker on every tab
  function shipFromLive(j) {
    const nav = (j?.sections || []).find((s) => /navigation/i.test(s.title));
    if (!nav || j.age_s == null || j.age_s > 120) { UW.shipLive = null; return; }
    const row = (k) => (nav.rows.find(([label]) => new RegExp(k, "i").test(label)) || [])[1];
    const lat = ddm(row("^latitude")), lon = ddm(row("^longitude")), t = UW.tms(row("^time"));
    if (lat == null || lon == null || isNaN(t)) { UW.shipLive = null; return; }
    UW.shipLive = { lat, lon, t, heading: +row("^heading") || null, speed: isNaN(+row("^speed")) ? null : +row("^speed") };
    UW.moveShip?.();
  }
  let timer = null;
  async function poll() {
    clearTimeout(timer);
    if (!document.hidden) {                                          // the map is on every tab, so the poll runs on every tab
      try { latest = await UW.fetchJSON(`api/intranet?t=${Date.now()}`); } catch { latest = { sections: [], error: "server not reachable", age_s: null }; }
      UW.intranetLatest = latest;                                    // the subtitle's source list reads the poll time from here
      shipFromLive(latest);
      if (!document.getElementById("pane-underway")?.hidden) for (const [name, [el, plot, re]] of els) if (el.isConnected) draw(name, re, el, plot);
    }
    timer = setTimeout(poll, 5000);
  }
  poll();
})();
