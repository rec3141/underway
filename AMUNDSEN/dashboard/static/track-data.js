/* Viewport-only track observations. Files are immutable and content addressed. */
(() => {
  'use strict';
  const fields = ['t', 'lat', 'lon', 'dist_km', 'leg', 'pump_low', 'provisional'];
  const abortError = () => Object.assign(new Error('Track request superseded'), {name: 'AbortError'});
  function longitudeRanges(w, e) {
    if (Math.abs(e - w) >= 360) return [[-180, 180]];
    const wrap = x => ((x + 180) % 360 + 360) % 360 - 180;
    w = wrap(w); e = wrap(e);
    return w <= e ? [[w, e]] : [[w, 180], [-180, e]];
  }
  function intersects(a, b) {
    if (!a || !b) return true;
    return a[1] <= b[3] && b[1] <= a[3] && longitudeRanges(a[0], a[2]).some(x =>
      longitudeRanges(b[0], b[2]).some(y => x[0] <= y[1] && y[0] <= x[1]));
  }
  function segmentIntersects(lon1, lat1, lon2, lat2, bounds) {
    if (!bounds) return true;
    const wrap = x => ((x + 180) % 360 + 360) % 360 - 180;
    lon1 = wrap(lon1); lon2 = lon1 + wrap(lon2 - lon1);
    for (const [west, east] of longitudeRanges(bounds[0], bounds[2])) {
      for (const shift of [-360, 0, 360]) {
        let lo = 0, hi = 1;
        const dx = lon2 - lon1, dy = lat2 - lat1;
        const edges = [[-dx, lon1 - west - shift], [dx, east + shift - lon1],
          [-dy, lat1 - bounds[1]], [dy, bounds[3] - lat1]];
        let hit = true;
        for (const [p, q] of edges) {
          if (p === 0) { if (q < 0) { hit = false; break; } }
          else if (p < 0) lo = Math.max(lo, q / p);
          else hi = Math.min(hi, q / p);
          if (lo > hi) { hit = false; break; }
        }
        if (hit) return true;
      }
    }
    return false;
  }
  function chooseLevel(track, spacingKm) {
    const requested = Math.max(0, Math.min(.1, Number.isFinite(spacingKm) ? spacingKm : .1));
    return (track?.levels || []).filter(l => l.spacing_km >= 0 && l.spacing_km <= requested)
      .sort((a, b) => b.spacing_km - a.spacing_km)[0];
  }
  function selectChunks(level, {bounds, start = -Infinity, end = Infinity, legs}, maxRows) {
    const selected = [], allowed = Array.isArray(legs) ? new Set(legs) : null;
    let rows = 0, limited = false;
    for (const chunk of level.chunks || []) {
      if (chunk.end < start || chunk.start > end || !intersects(chunk.bounds, bounds) ||
          (allowed && chunk.leg != null && !allowed.has(chunk.leg))) continue;
      const count = Math.max(0, Number(chunk.n) || 0);
      // Bound transfer/parsing before fetching, including possible gap separators.
      if (rows + count + selected.length > maxRows) { limited = true; continue; }
      selected.push(chunk); rows += count;
    }
    return {chunks: selected, limited};
  }
  function assemble(parts, options, maxRows) {
    const {bounds, start = -Infinity, end = Infinity, legs} = options;
    const allowed = Array.isArray(legs) ? new Set(legs) : null;
    const names = [...new Set(parts.flatMap(p => Object.keys(p.data.vars || {})))];
    const result = {vars: Object.fromEntries(names.map(k => [k, []])), limits: {}, n: 0, shown: 0};
    fields.forEach(k => { result[k] = []; });
    let previous = null, limited = false;
    const seen = new Set();
    const append = (data, i, gap) => {
      fields.forEach(k => result[k].push(gap ? (k === 't' ? data.t[i] : null) : (data[k]?.[i] ?? null)));
      names.forEach(k => result.vars[k].push(gap ? null : (data.vars?.[k]?.[i] ?? null)));
    };
    parts.sort((a, b) => a.chunk.start - b.chunk.start);
    for (const {chunk, data} of parts) {
      let contiguous = false;
      const eligible = data.t.map((t, i) => t >= start && t <= end &&
        (!allowed || allowed.has(data.leg?.[i] ?? chunk.leg)) &&
        Number.isFinite(data.lat?.[i]) && Number.isFinite(data.lon?.[i]));
      const visible = eligible.map((ok, i) => ok && intersects(
        [data.lon[i], data.lat[i], data.lon[i], data.lat[i]], bounds));
      for (let i = 1; i < data.t.length; i++) {
        if (eligible[i - 1] && eligible[i] && data.leg?.[i - 1] === data.leg?.[i] &&
            !data.gap_before?.[i] && segmentIntersects(data.lon[i - 1], data.lat[i - 1], data.lon[i], data.lat[i], bounds))
          visible[i - 1] = visible[i] = true;
      }
      for (let i = 0; i < data.t.length; i++) {
        const t = data.t[i], leg = data.leg?.[i] ?? chunk.leg;
        const key = `${leg}:${t}`;
        const valid = visible[i];
        if (!valid) { contiguous = false; continue; }
        if (seen.has(key)) {
          contiguous = previous?.key === key && previous.segment === chunk.segment;
          continue;
        }
        const gap = previous && (!contiguous || previous.leg !== leg || previous.segment !== chunk.segment || data.gap_before?.[i]);
        if (result.t.length + 1 + (gap ? 1 : 0) > maxRows) { limited = true; break; }
        if (gap) append(data, i, true);
        append(data, i, false);
        if (previous && !gap) {
          const last = result.lon.length - 1, before = result.lon[last - 1];
          result.lon[last] = before + ((result.lon[last] - before + 180) % 360 + 360) % 360 - 180;
        }
        seen.add(key); result.shown++;
        previous = {key, leg, segment: chunk.segment}; contiguous = true;
      }
    }
    result.n = result.t.length;
    return {...result, limited};
  }
  function createLoader({fetchImpl = (...args) => fetch(...args), maxCacheRows = 100000,
      maxCacheBytes = 64 * 1024 * 1024, maxRows = 50000, concurrency = 4, timeoutMs = 30000} = {}) {
    let active = null, serial = 0, cachedRows = 0, cachedBytes = 0;
    const cache = new Map();
    function cancel() { serial++; active?.abort(); active = null; }
    function clear() { cancel(); cache.clear(); cachedRows = cachedBytes = 0; }
    async function load(options) {
      cancel();
      const ticket = serial, controller = new AbortController(); active = controller;
      const check = () => { if (ticket !== serial || controller.signal.aborted) throw abortError(); };
      const level = chooseLevel(options.track, options.spacingKm);
      if (!level) throw new Error('No track resolution available at or finer than the requested spacing');
      const selected = selectChunks(level, options, maxRows), parts = [];
      let next = 0;
      async function worker() {
        while (next < selected.chunks.length) {
          check(); const chunk = selected.chunks[next++];
          let item = cache.get(chunk.file);
          if (item) { cache.delete(chunk.file); cache.set(chunk.file, item); }
          else {
            const response = await fetchImpl(chunk.file, {signal: controller.signal});
            check(); if (!response.ok) throw new Error(`${response.status} ${chunk.file}`);
            const data = await response.json(); check();
            if (!Array.isArray(data.t) || data.t.length > Math.max(maxRows, Number(chunk.n) || 0))
              throw new Error('Invalid or oversized track chunk');
            // Numeric JS arrays plus conservative per-array/object overhead.
            const bytes = data.t.length * (fields.length + Object.keys(data.vars || {}).length) * 16 + 4096;
            item = {data, rows: data.t.length, bytes};
            if (item.rows <= maxCacheRows && bytes <= maxCacheBytes) {
              cache.set(chunk.file, item); cachedRows += item.rows; cachedBytes += bytes;
              while (cachedRows > maxCacheRows || cachedBytes > maxCacheBytes) {
                const key = cache.keys().next().value, old = cache.get(key);
                cache.delete(key); cachedRows -= old.rows; cachedBytes -= old.bytes;
              }
            }
          }
          parts.push({chunk, data: item.data});
        }
      }
      let timer;
      const deadline = new Promise((_, reject) => {
        timer = setTimeout(() => { controller.abort(); reject(new Error('Track request timed out')); }, timeoutMs);
      });
      try {
        await Promise.race([deadline, Promise.all(Array.from({length: Math.max(1, Math.min(4, concurrency, selected.chunks.length))}, worker))]);
        check(); const result = assemble(parts, options, maxRows);
        return {...result, spacing_km: level.spacing_km, limited: selected.limited || result.limited};
      } catch (error) { controller.abort(); throw error; }
      finally { clearTimeout(timer); if (ticket === serial) active = null; }
    }
    return {load, cancel, clear};
  }
  const api = {createLoader, longitudeRanges, intersects, segmentIntersects, chooseLevel, selectChunks, assemble};
  if (typeof window !== 'undefined') window.UWTrack = api;
  if (typeof module !== 'undefined' && module.exports) module.exports = api;
})();
