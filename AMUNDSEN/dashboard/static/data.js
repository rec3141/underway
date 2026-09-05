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
  window.UWData = { fetchJSON, generationCache };
})();
