// The live feed supplies the ship marker. All group summaries now use
// chart-backed data; the server still records these snapshots for history.
(() => {
  const UW = window.UW;
  if (!UW?.registerPanel) return;
  let latest = null;
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
    }
    timer = setTimeout(poll, 5000);
  }
  poll();
})();
