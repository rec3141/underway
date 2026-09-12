/* Shared, bounded JSON requests and caches scoped to a published generation. */
(() => {
  "use strict";
  async function fetchJSON(url, options = {}) {
    const controller = new AbortController();
    const timer = setTimeout(() => controller.abort(), 30000);
    try {
      const r = await fetch(url, { cache: "no-store", ...options, signal: controller.signal });
      if (!r.ok) throw new Error(`${r.status} ${url}`);
      return await r.json();
    } finally { clearTimeout(timer); }
  }

  function generationCache(load, generation) {
    let stamp;
    let values = new Map(), pending = new Map();
    return async (key, url) => {
      const requested = generation();
      if (stamp !== requested) { stamp = requested; values = new Map(); pending = new Map(); }
      if (values.has(key)) return values.get(key);
      if (pending.has(key)) return pending.get(key);
      const request = (async () => {
        const value = await load(`${url}?v=${encodeURIComponent(requested)}`);
        if (generation() !== requested || stamp !== requested) throw new Error("Data changed during download; retrying");
        values.set(key, value);
        return value;
      })();
      pending.set(key, request);
      try { return await request; }
      finally { if (pending.get(key) === request) pending.delete(key); }
    };
  }
  // Remember explicit axis edits outside the plot element: some views rebuild
  // their DOM on refresh. Match axes by meaning so reordering does not move
  // a temperature range onto salinity. Untouched axes continue to follow data.
  function plotState(Plotly) {
    const scopes = new Map();
    let revision = 0;
    const axes = (layout) => Object.keys(layout).filter(k => /^[xy]axis\d*$/.test(k));
    const identity = (key, axis) => JSON.stringify([key[0], axis.title?.text || key, axis.type || 'linear']);
    return async (gd, data, layout, config, scope) => {
      let saved = scopes.get(scope);
      if (!saved) {
        saved = new Map(); scopes.set(scope, saved);
        if (scopes.size > 150) scopes.delete(scopes.keys().next().value);
      }
      // Ranges are restored explicitly below. Disable Plotly's separate UI
      // memory so an intentional reset cannot resurrect an old range.
      const next = { ...layout, uirevision: ++revision };
      for (const key of axes(layout)) {
        next[key] = { ...layout[key], uirevision: revision };
        const range = saved.get(identity(key, next[key]));
        if (range) { next[key].range = [...range]; next[key].autorange = false; }
      }
      if (gd._rememberAxes) gd.removeListener('plotly_relayout', gd._rememberAxes);
      const plot = await Plotly.react(gd, data, next, config);
      plot._rememberAxes = (event) => {
        for (const key of axes(next)) {
          const id = identity(key, next[key]);
          if (event[`${key}.autorange`] === true || event[`${key}.autorange`] === 'reversed') saved.delete(id);
          else if (Object.keys(event).some(k => k === `${key}.range` || k.startsWith(`${key}.range[`))) {
            const range = plot._fullLayout?.[key]?.range;
            if (range) saved.set(id, [...range]);
          }
        }
      };
      plot.on('plotly_relayout', plot._rememberAxes);
      return plot;
    };
  }
  window.UWData = { fetchJSON, generationCache, plotState };
})();
