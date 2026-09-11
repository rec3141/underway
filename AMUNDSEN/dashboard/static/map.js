/* The map: a MapLibre map in #map (static/maplibre-gl.js, served locally so it
 * works offline), drawn from the layers the page's modules describe in the
 * shape of Plotly's scattermap traces: lat/lon arrays (null between runs),
 * mode ("lines", "markers", "text" joined by "+"), marker {size, color,
 * opacity, symbol, angle, colorscale, cmin, cmax, reversescale}, line {width,
 * color}, opacity, text, hovertext, hovertemplate, textposition, textfont,
 * customdata, hoverinfo and name.
 *
 * Every trace becomes features of four layers (lines, circles, sprite icons,
 * text labels) whatever the number of traces, so a redraw is one setData per
 * layer rather than a MapLibre layer per trace. A feature carries its trace's
 * place in the list as a sort key, so later traces draw on top and win the
 * pointer. Two groups of layers keep what moves on its own: "base" (the
 * record, the layers) and "live" (the ship and the focus mark, on top), so
 * the live poller moves the ship with a setData on a one-point source.
 *
 * Hover and click pick the topmost feature within a few pixels of the pointer
 * and report it the way Plotly reported a point: {lat, lon, text, hovertext,
 * customdata, pointIndex, pointNumber, data: {name}}. hoverinfo "skip" or
 * "none" makes a trace silent (no hover box) but still clickable.
 */
(function () {
  "use strict";
  const UW = (window.UW = window.UW || {});
  const KINDS = ["lines", "circles", "icons", "labels"];
  const GROUPS = ["base", "live"];
  const FONT = ["Open Sans Regular"];                 // the one glyph stack served under static/geo/glyphs
  const DEFAULT_COLOUR = "#1f77b4";                   // Plotly's first colour, for a trace that names none
  const NULL_COLOUR = "#7d8895";                      // a point whose value is missing on a coloured track
  const PICK_PX = 7;                                  // how near the pointer a point must be to be hovered or clicked

  // ---------------------------------------------------------------- colour maps
  // [position, colour] stops shared by the map, the charts and the colour bars.
  // Viridis is Plotly's own; Magma is matplotlib's; Phase is a cyclic map for
  // headings and directions (its ends meet).
  const CMAPS = {
    Viridis: [[0, "#440154"], [0.0627, "#48186a"], [0.1255, "#472d7b"], [0.1882, "#424086"], [0.251, "#3b528b"], [0.3137, "#33638d"],
      [0.3765, "#2c728e"], [0.4392, "#26828e"], [0.502, "#21918c"], [0.5647, "#1fa088"], [0.6275, "#28ae80"], [0.6902, "#3fbc73"],
      [0.7529, "#5ec962"], [0.8157, "#84d44b"], [0.8784, "#addc30"], [0.9412, "#d8e219"], [1, "#fde725"]],
    Magma: [[0, "#000004"], [0.125, "#1c1044"], [0.25, "#4f127b"], [0.375, "#812581"], [0.5, "#b5367a"], [0.625, "#e55064"],
      [0.75, "#fb8761"], [0.875, "#fec287"], [1, "#fcfdbf"]],
    Phase: [[0, "#a8780d"], [0.125, "#d3593a"], [0.25, "#e03b7a"], [0.375, "#b43fc4"], [0.5, "#6c5ce8"], [0.625, "#2c7ecb"],
      [0.75, "#1795a0"], [0.875, "#4f9f55"], [1, "#a8780d"]],
  };
  const cmap = (name) => (Array.isArray(name) ? name : CMAPS[name] || CMAPS.Viridis);
  const reversed = (stops) => stops.map(([t, c]) => [1 - t, c]).reverse();
  const rgbOf = (hex) => { const h = hex.replace("#", ""); return [0, 2, 4].map((k) => parseInt(h.slice(k, k + 2), 16)); };
  function colourAt(stops, t) {
    if (!isFinite(t)) return NULL_COLOUR;
    t = Math.max(0, Math.min(1, t));
    let k = 1;
    while (k < stops.length - 1 && stops[k][0] < t) k++;
    const [t0, c0] = stops[k - 1], [t1, c1] = stops[k];
    const f = t1 > t0 ? (t - t0) / (t1 - t0) : 0, a = rgbOf(c0), b = rgbOf(c1);
    return `rgb(${a.map((x, j) => Math.round(x + (b[j] - x) * f)).join(",")})`;
  }
  UW.cmap = (name, reverse = false) => (reverse ? reversed(cmap(name)) : cmap(name));

  // ---------------------------------------------------------------- traces -> features
  const at = (v, i) => (Array.isArray(v) ? v[i] : v);
  const has = (mode, part) => String(mode || "markers").split("+").includes(part);
  // where Plotly put a label for each textposition, as a MapLibre anchor
  const ANCHOR = { "top right": "bottom-left", "top left": "bottom-right", "top center": "bottom", "bottom right": "top-left",
    "bottom left": "top-right", "bottom center": "top", "middle right": "left", "middle left": "right" };
  const silent = (tr) => tr.hoverinfo === "skip" || tr.hoverinfo === "none";

  // the hover text of a trace's point, as Plotly would have shown it
  function hoverOf(tr, i) {
    if (silent(tr)) return "";
    const ht = at(tr.hovertext, i), tx = at(tr.text, i);
    if (tr.hovertemplate) {
      return String(tr.hovertemplate).replace(/<extra>.*?<\/extra>/g, "")
        .replace(/%\{hovertext\}/g, ht ?? "").replace(/%\{text\}/g, tx ?? "");
    }
    return String(ht ?? tx ?? "");
  }

  function featuresOf(traces) {
    const out = { lines: [], circles: [], icons: [], labels: [] };
    traces.forEach((tr, t) => {
      const lat = tr.lat || [], lon = tr.lon || [], n = lat.length, z = t;
      const op = tr.opacity ?? 1, h = silent(tr) ? 0 : 1;
      const ok = (i) => lat[i] != null && lon[i] != null && isFinite(lat[i]) && isFinite(lon[i]);
      if (has(tr.mode, "lines")) {
        const ln = tr.line || {};
        let run = [], first = 0;
        const flush = () => {
          if (run.length > 1) out.lines.push({ type: "Feature", geometry: { type: "LineString", coordinates: run },
            properties: { t, i0: first, z, c: ln.color || DEFAULT_COLOUR, w: ln.width ?? 2, o: op, h } });
          run = [];
        };
        for (let i = 0; i < n; i++) {
          if (!ok(i)) { flush(); continue; }
          if (!run.length) first = i;
          run.push([+lon[i], +lat[i]]);
        }
        flush();
      }
      if (has(tr.mode, "markers")) {
        const m = tr.marker || {};
        const numeric = Array.isArray(m.color) && m.color.some((c) => typeof c === "number");
        let stops = null, lo = 0, hi = 1;
        if (numeric) {
          stops = cmap(m.colorscale); if (m.reversescale) stops = reversed(stops);
          let vlo = Infinity, vhi = -Infinity;
          for (const c of m.color) if (typeof c === "number" && isFinite(c)) { if (c < vlo) vlo = c; if (c > vhi) vhi = c; }
          lo = m.cmin ?? vlo; hi = m.cmax ?? vhi;
        }
        for (let i = 0; i < n; i++) {
          if (!ok(i)) continue;
          const g = { type: "Point", coordinates: [+lon[i], +lat[i]] };
          const sym = at(m.symbol, i), size = at(m.size, i) ?? 6, o = (at(m.opacity, i) ?? 1) * op;
          if (sym && sym !== "circle") {
            // a sprite icon is scaled as Plotly scaled it: marker.size / 10 of its sprite image
            out.icons.push({ type: "Feature", geometry: g, properties: { t, i, z, ic: `${sym}-15`, s: size / 10, rot: at(m.angle, i) ?? 0, o, h } });
          } else {
            let c = at(m.color, i);
            if (numeric) c = typeof c === "number" ? colourAt(stops, (c - lo) / ((hi - lo) || 1)) : NULL_COLOUR;
            out.circles.push({ type: "Feature", geometry: g, properties: { t, i, z, c: c || DEFAULT_COLOUR, r: size / 2, o, h } });
          }
        }
      }
      if (has(tr.mode, "text")) {
        const tf = tr.textfont || {}, an = ANCHOR[tr.textposition] || "center";
        for (let i = 0; i < n; i++) {
          const s = at(tr.text, i);
          if (!ok(i) || s == null || s === "") continue;
          out.labels.push({ type: "Feature", geometry: { type: "Point", coordinates: [+lon[i], +lat[i]] },
            properties: { t, i, z, lb: String(s), ts: tf.size ?? 12, tc: tf.color || "#ffffff", an, h } });
        }
      }
    });
    return out;
  }

  // ---------------------------------------------------------------- layers
  const lid = (g, k) => `u-${g}-${k}`;
  const OFFSET = ["match", ["get", "an"], "bottom-left", ["literal", [0.45, -0.25]], "bottom-right", ["literal", [-0.45, -0.25]],
    "bottom", ["literal", [0, -0.6]], "top-left", ["literal", [0.45, 0.25]], "top-right", ["literal", [-0.45, 0.25]],
    "top", ["literal", [0, 0.6]], "left", ["literal", [0.6, 0]], "right", ["literal", [-0.6, 0]], ["literal", [0, 0]]];
  function layerDefs(g) {
    return [
      { id: lid(g, "lines"), type: "line", source: lid(g, "lines"),
        layout: { "line-join": "round", "line-cap": "round", "line-sort-key": ["get", "z"] },
        paint: { "line-color": ["to-color", ["get", "c"]], "line-width": ["get", "w"], "line-opacity": ["get", "o"] } },
      { id: lid(g, "circles"), type: "circle", source: lid(g, "circles"),
        layout: { "circle-sort-key": ["get", "z"] },
        paint: { "circle-color": ["to-color", ["get", "c"]], "circle-radius": ["get", "r"], "circle-opacity": ["get", "o"] } },
      // markers never give way: every icon draws, in trace order
      { id: lid(g, "icons"), type: "symbol", source: lid(g, "icons"),
        layout: { "icon-image": ["get", "ic"], "icon-size": ["get", "s"], "icon-rotate": ["get", "rot"], "icon-rotation-alignment": "map",
          "icon-allow-overlap": true, "icon-ignore-placement": true, "symbol-sort-key": ["get", "z"] },
        paint: { "icon-opacity": ["get", "o"] } },
      // labels do give way, to each other: the later trace's label is placed first
      { id: lid(g, "labels"), type: "symbol", source: lid(g, "labels"),
        layout: { "text-field": ["get", "lb"], "text-font": FONT, "text-size": ["get", "ts"], "text-anchor": ["get", "an"], "text-offset": OFFSET,
          "symbol-sort-key": ["-", 0, ["get", "z"]] },
        paint: { "text-color": ["to-color", ["get", "tc"]] } },
    ];
  }
  const OURS = new Set(GROUPS.flatMap((g) => KINDS.map((k) => lid(g, k))));

  // ---------------------------------------------------------------- the view
  class MapView {
    constructor(el, handlers = {}) {
      this.el = el;
      this.on = handlers;                             // onClick(point), onEmptyClick(), onMove(view), onZoom(zoom)
      this.traces = { base: [], live: [] };
      this.fc = { base: featuresOf([]), live: featuresOf([]) };
      this.map = null;
      this.styleId = null;
      this.tip = document.createElement("div");
      this.tip.className = "maptip"; this.tip.hidden = true;
    }

    // draw: the basemap style (reloaded only when its id changes), the view,
    // and the traces of either group (a group left out keeps what it has)
    draw({ style, view, base, live }) {
      if (!this.map) this.create(style, view);
      else if (style && style.id !== this.styleId) this.setStyle(style);
      if (base) this.setTraces("base", base);
      if (live) this.setTraces("live", live);
      if (view && this.map) this.setView(view);
      return this.ready();
    }

    create(style, view) {
      this.styleId = style.id;
      this.map = new maplibregl.Map({
        container: this.el, style, center: view ? [view.center.lon, view.center.lat] : [-90, 70], zoom: view ? view.zoom : 3,
        attributionControl: false, dragRotate: false, pitchWithRotate: false, touchPitch: false, maxPitch: 0,
        fadeDuration: 0, renderWorldCopies: true,
      });
      this.map.touchZoomRotate.disableRotation();
      this.map.keyboard.disableRotation?.();
      this.el.appendChild(this.tip);
      this._styled = false;
      this.map.on("style.load", () => { this._styled = true; this.ensureOverlay(); });
      this.map.on("styledata", () => this.ensureOverlay());
      this.map.on("error", (e) => console.warn("map:", e?.error?.message || e));
      this.map.on("mousemove", (e) => {
        this._at = e.point;
        if (!this._raf) this._raf = requestAnimationFrame(() => { this._raf = 0; this.hover(this._at); });
      });
      this.map.getCanvas().addEventListener("mouseleave", () => { this.tip.hidden = true; });
      this.map.on("movestart", () => { this.tip.hidden = true; });
      this.map.on("click", (e) => {
        const hit = this.pick(e.point, true);
        if (hit) this.on.onClick?.(this.pointOf(hit)); else this.on.onEmptyClick?.();
      });
      this.map.on("moveend", () => this.on.onMove?.(this.getView()));
      this.map.on("zoomend", () => this.on.onZoom?.(this.map.getZoom()));
    }

    // a new basemap: MapLibre diffs it against the old one, and our layers
    // ride along on top (transformStyle) so they are never dropped and re-added
    setStyle(style) {
      this.styleId = style.id;
      this.map.setStyle(style, { diff: true, transformStyle: (prev, next) => {
        if (!prev) return next;
        const sources = { ...next.sources }, layers = [...next.layers];
        for (const l of prev.layers || []) if (OURS.has(l.id)) { layers.push(l); sources[l.source] = prev.sources[l.source]; }
        return { ...next, sources, layers };
      } });
    }

    // our sources and layers, above the basemap: added when missing (the
    // first style, or a style MapLibre loaded afresh), filled with what we hold
    ensureOverlay() {
      const m = this.map;
      if (!m || !m.style || !m.style._loaded) return;
      for (const g of GROUPS) for (const def of layerDefs(g)) {
        if (!m.getSource(def.source)) m.addSource(def.source, { type: "geojson", data: this.fc[g][def.id.split("-")[2]], tolerance: 0.2 });
        if (!m.getLayer(def.id)) m.addLayer(def);
      }
    }

    setTraces(group, traces) {
      this.traces[group] = traces;
      this.fc[group] = Object.fromEntries(Object.entries(featuresOf(traces)).map(([k, fs]) => [k, { type: "FeatureCollection", features: fs }]));
      if (!this.map) return;
      for (const k of KINDS) this.map.getSource(lid(group, k))?.setData(this.fc[group][k]);
    }

    clear() { for (const g of GROUPS) this.setTraces(g, []); }

    ready() {
      if (!this.map || this._styled) return Promise.resolve();
      return new Promise((res) => { const t = setTimeout(res, 8000); this.map.once("style.load", () => { clearTimeout(t); res(); }); });
    }

    getView() {
      if (!this.map) return null;
      const c = this.map.getCenter();
      return { center: { lat: c.lat, lon: c.lng }, zoom: this.map.getZoom() };
    }

    setView(view) {
      const cur = this.getView();
      if (cur && Math.abs(cur.zoom - view.zoom) < 1e-3 && Math.abs(cur.center.lat - view.center.lat) < 1e-6 && Math.abs(cur.center.lon - view.center.lon) < 1e-6) return;
      this.map.jumpTo({ center: [view.center.lon, view.center.lat], zoom: view.zoom });
    }

    resize() { this.map?.resize(); }

    // the topmost feature near the pointer: points before lines, live before
    // base, later traces before earlier; a hover takes only the ones with text
    pick(pt, click = false) {
      const m = this.map;
      if (!m || !m.style?._loaded) return null;
      const layers = [...OURS].filter((id) => m.getLayer(id));
      const box = [[pt.x - PICK_PX, pt.y - PICK_PX], [pt.x + PICK_PX, pt.y + PICK_PX]];
      let best = null;
      for (const f of m.queryRenderedFeatures(box, { layers })) {
        const [, g, kind] = f.layer.id.split("-"), p = f.properties;
        if (!click && !p.h) continue;
        const rank = (kind === "lines" ? 0 : 4e6) + (p.h ? 2e6 : 0) + (g === "live" ? 1e6 : 0) + (p.z ?? 0);
        if (!best || rank > best.rank) best = { rank, g, kind, p };
      }
      if (!best) return null;
      const tr = this.traces[best.g][best.p.t];
      if (!tr) return null;
      const i = best.kind === "lines" ? this.nearestVertex(tr, best.p.i0, pt) : best.p.i;
      return { tr, i };
    }

    // on a line, the vertex of its run nearest the pointer, as Plotly hovered it
    nearestVertex(tr, i0, pt) {
      let bi = i0, bd = Infinity;
      for (let i = i0; i < tr.lat.length && tr.lat[i] != null && tr.lon[i] != null; i++) {
        const q = this.map.project([+tr.lon[i], +tr.lat[i]]), d = (q.x - pt.x) ** 2 + (q.y - pt.y) ** 2;
        if (d < bd) { bd = d; bi = i; }
      }
      return bi;
    }

    pointOf({ tr, i }) {
      return { lat: tr.lat[i], lon: tr.lon[i], text: at(tr.text, i), hovertext: at(tr.hovertext, i), customdata: at(tr.customdata, i),
               pointIndex: i, pointNumber: i, data: { name: tr.name } };
    }

    hover(pt) {
      const hit = this.pick(pt);
      // the hand over anything with a hover box, or a silent point that still takes a click
      const silentTarget = !hit && this.pick(pt, true);
      this.map.getCanvas().style.cursor = hit || (silentTarget && at(silentTarget.tr.customdata, silentTarget.i) != null) ? "pointer" : "";
      const html = hit ? hoverOf(hit.tr, hit.i) : "";
      if (!html) { this.tip.hidden = true; return; }
      this.tip.innerHTML = html;
      this.tip.hidden = false;
      const w = this.el.clientWidth, tw = this.tip.offsetWidth, th = this.tip.offsetHeight;
      const x = pt.x + 14 + tw > w ? pt.x - 14 - tw : pt.x + 14;
      this.tip.style.left = `${Math.max(2, x)}px`;
      this.tip.style.top = `${Math.max(2, Math.min(pt.y - th / 2, this.el.clientHeight - th - 2))}px`;
    }
  }

  UW.MapView = MapView;
  UW.mapFeaturesOf = featuresOf;                      // for tests: what the map would draw from a list of traces
})();
