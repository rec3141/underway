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
  // An arrow icon "uw-arrow|<length px>|<ink>|<halo>" pointing north, its
  // tail on a dot at the image centre so icon-rotate turns it about the
  // point it belongs to. Drawn at twice the size for pixelRatio 2; the head
  // keeps one size whatever the length, so only the shaft says how fast.
  function arrowImage(id) {
    const [, len, ink, halo] = id.split("|"), k = 2, L = +len * k;
    const head = 7 * k, half = 4 * k, pad = 3 * k, w = 2 * (half + pad), h = 2 * (L + pad);
    const cv = document.createElement("canvas"); cv.width = w; cv.height = h;
    const g = cv.getContext("2d"), cx = w / 2, cy = h / 2, tip = cy - L;
    const shape = () => {
      g.beginPath(); g.moveTo(cx, cy); g.lineTo(cx, tip + head * 0.8); g.stroke();
      g.beginPath(); g.moveTo(cx, tip); g.lineTo(cx - half, tip + head); g.lineTo(cx + half, tip + head); g.closePath(); g.fill(); g.stroke();
      g.beginPath(); g.arc(cx, cy, 1.8 * k, 0, 2 * Math.PI); g.fill(); g.stroke();
    };
    g.lineCap = "round"; g.lineJoin = "round";
    g.strokeStyle = g.fillStyle = halo; g.lineWidth = 4 * k; shape();
    g.strokeStyle = g.fillStyle = ink; g.lineWidth = 1.5 * k; shape();
    return g.getImageData(0, 0, w, h);
  }
  const NULL_COLOUR = "#7d8895";                      // a point whose value is missing on a coloured track
  const PICK_PX = 7;                                  // how near the pointer a point must be to be hovered or clicked
  const PRESS_MS = 550;                               // how long a press is held to stand for a double click
  const PRESS_PX = 10;                                // how far it may drift and still count as held still
  const FIT_PAD = 28;                                 // pixels kept clear round a box the view is fitted to
  const CLEAR_POINTS = 240;                           // a line is thinned to about this many before it is tested

  const inBox = (p, b) => !!p && p.x >= b.x0 && p.x <= b.x1 && p.y >= b.y0 && p.y <= b.y1;

  // a long route is tested at a fraction of its points: the box is far bigger
  // than the gap that leaves, and placing it runs on every frame of a pan
  const thin = (line) => {
    if (line.length <= CLEAR_POINTS) return line;
    const every = Math.ceil(line.length / CLEAR_POINTS);
    return line.filter((_, i) => i % every === 0 || i === line.length - 1);
  };

  // where a box of tw by th goes beside a point in a w by h map: to the
  // point's right, else its left, else below or above. Of those the one that
  // covers least of what has to stay in view, and any that covers nothing
  // ends the search. `keep` is lines already in map pixels.
  function tipSpot(pt, w, h, tw, th, keep = []) {
    const beside = Math.max(2, Math.min(pt.y - th / 2, h - th - 2));
    const over = Math.max(2, Math.min(pt.x - tw / 2, w - tw - 2));
    const spots = [{ x: pt.x + 14, y: beside }, { x: pt.x - 14 - tw, y: beside },
      { x: over, y: pt.y + 14 }, { x: over, y: pt.y - 14 - th }];
    let best = null;
    for (const spot of spots) {
      const off = spot.x < 2 || spot.x + tw > w - 2 || spot.y < 2 || spot.y + th > h - 2;
      const box = { x0: spot.x - 6, y0: spot.y - 6, x1: spot.x + tw + 6, y1: spot.y + th + 6 };
      let hits = 0;
      for (const line of keep) {
        if (!line.length) continue;
        if (line.length < 2) hits += inBox(line[0], box) ? 1 : 0;
        else for (let i = 1; i < line.length; i++) if (meets(line[i - 1], line[i], box)) hits++;
      }
      const score = (off ? 1e4 : 0) + hits;
      if (!best || score < best.score) best = { ...spot, score };
      if (!score) break;
    }
    return { x: Math.max(2, Math.min(best.x, Math.max(2, w - tw - 2))),
             y: Math.max(2, Math.min(best.y, Math.max(2, h - th - 2))) };
  }

  // whether a segment meets an upright box, by Liang and Barsky's clip: the
  // stretch of the segment still inside the box after each edge cuts it
  function meets(a, b, box) {
    const dx = b.x - a.x, dy = b.y - a.y;
    let t0 = 0, t1 = 1;
    for (const [p, q] of [[-dx, a.x - box.x0], [dx, box.x1 - a.x], [-dy, a.y - box.y0], [dy, box.y1 - a.y]]) {
      if (p === 0) { if (q < 0) return false; continue; }
      const t = q / p;
      if (p < 0) { if (t > t1) return false; if (t > t0) t0 = t; }
      else { if (t < t0) return false; if (t < t1) t1 = t; }
    }
    return true;
  }
  const FIT_MAX = 14;                                 // a fit never zooms closer than this

  // ---------------------------------------------------------------- colour maps
  // [position, colour] stops shared by the map, the charts and the colour bars.
  // Viridis is Plotly's own; Magma is matplotlib's; Phase is a cyclic map for
  // headings and directions (its ends meet); RdBu is ColorBrewer's diverging
  // map, blue low through white to red high, for signed values such as currents.
  const CMAPS = {
    Viridis: [[0, "#440154"], [0.0627, "#48186a"], [0.1255, "#472d7b"], [0.1882, "#424086"], [0.251, "#3b528b"], [0.3137, "#33638d"],
      [0.3765, "#2c728e"], [0.4392, "#26828e"], [0.502, "#21918c"], [0.5647, "#1fa088"], [0.6275, "#28ae80"], [0.6902, "#3fbc73"],
      [0.7529, "#5ec962"], [0.8157, "#84d44b"], [0.8784, "#addc30"], [0.9412, "#d8e219"], [1, "#fde725"]],
    Magma: [[0, "#000004"], [0.125, "#1c1044"], [0.25, "#4f127b"], [0.375, "#812581"], [0.5, "#b5367a"], [0.625, "#e55064"],
      [0.75, "#fb8761"], [0.875, "#fec287"], [1, "#fcfdbf"]],
    Phase: [[0, "#a8780d"], [0.125, "#d3593a"], [0.25, "#e03b7a"], [0.375, "#b43fc4"], [0.5, "#6c5ce8"], [0.625, "#2c7ecb"],
      [0.75, "#1795a0"], [0.875, "#4f9f55"], [1, "#a8780d"]],
    RdBu: [[0, "#053061"], [0.1, "#2166ac"], [0.2, "#4393c3"], [0.3, "#92c5de"], [0.4, "#d1e5f0"], [0.5, "#f7f7f7"],
      [0.6, "#fddbc7"], [0.7, "#f4a582"], [0.8, "#d6604d"], [0.9, "#b2182b"], [1, "#67001f"]],
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
  UW.colourAt = colourAt;
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
          if (sym === "arrow") {
            // an arrow marker.size pixels long, drawn to order (arrowImage), in marker.color with a marker.line.color halo
            out.icons.push({ type: "Feature", geometry: g, properties: { t, i, z, ic: `uw-arrow|${Math.max(4, Math.round(size))}|${at(m.color, i) || DEFAULT_COLOUR}|${m.line?.color || "rgba(0,0,0,0)"}`, s: 1, rot: at(m.angle, i) ?? 0, o, h } });
          } else if (sym && sym !== "circle") {
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

  let imageProtocolId = 0;
  async function textureImage(url, limit, signal) {
    const response = await fetch(url, { signal });
    if (!response.ok) throw new Error(`Map image request failed: ${response.status}`);
    const bitmap = await createImageBitmap(await response.blob());
    try {
      signal.throwIfAborted();
      const scale = Math.min(1, limit / Math.max(bitmap.width, bitmap.height));
      if (scale === 1) return { data: bitmap };
      const data = await createImageBitmap(bitmap, {
        resizeWidth: Math.max(1, Math.floor(bitmap.width * scale)),
        resizeHeight: Math.max(1, Math.floor(bitmap.height * scale)),
        resizeQuality: "high",
      });
      bitmap.close();
      if (signal.aborted) { data.close(); signal.throwIfAborted(); }
      return { data };
    } catch (error) { bitmap.close(); throw error; }
  }

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

    // draw: the basemap style (reloaded only when its id changes), the view
    // ({center, zoom}, or {bounds: [[w, s], [e, n]]} to fit a box: on the
    // globe the zoom that fits a box is the map's to work out, not ours),
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
      this.imageProtocol = `uw-image-${++imageProtocolId}`;
      maplibregl.addProtocol(this.imageProtocol, async (request, controller) => {
        const canvas = this.map?.getCanvas();
        const gl = canvas?.getContext("webgl2") || canvas?.getContext("webgl");
        const limit = gl ? gl.getParameter(gl.MAX_TEXTURE_SIZE) : 2048;
        return textureImage(decodeURIComponent(request.url.split("://")[1]), limit, controller.signal);
      });
      const camera = view?.bounds ? { bounds: view.bounds, fitBoundsOptions: { padding: FIT_PAD, maxZoom: FIT_MAX } }
        : { center: view ? [view.center.lon, view.center.lat] : [-90, 70], zoom: view ? view.zoom : 3 };
      this.map = new maplibregl.Map({
        container: this.el, style: this.imageStyle(style), ...camera,
        attributionControl: false, dragRotate: false, pitchWithRotate: false, touchPitch: false, maxPitch: 0,
        // app.js schedules container resizes after ResizeObserver delivery.
        // Avoid a second synchronous resize from MapLibre during the same cycle.
        trackResize: false, fadeDuration: 0, renderWorldCopies: true,
      });
      this.map.touchZoomRotate.disableRotation();
      this.map.keyboard.disableRotation?.();
      this.el.appendChild(this.tip);
      this._styled = false;
      this.map.on("styleimagemissing", (e) => { if (e.id.startsWith("uw-arrow|") && !this.map.hasImage(e.id)) this.map.addImage(e.id, arrowImage(e.id), { pixelRatio: 2 }); });
      this.map.on("style.load", () => { this._styled = true; this.ensureOverlay(); });
      this.map.on("styledata", () => this.ensureOverlay());
      this.map.on("remove", () => maplibregl.removeProtocol(this.imageProtocol));
      this.map.on("error", (e) => console.warn("map:", e?.error?.message || e));
      this.map.on("mousemove", (e) => {
        this._at = e.point;
        if (!this._raf) this._raf = requestAnimationFrame(() => { this._raf = 0; this.hover(this._at); });
      });
      this.map.getCanvas().addEventListener("mouseleave", () => { if (!this.pinned) this.tip.hidden = true; });
      this.map.on("movestart", () => { if (!this.pinned) this.tip.hidden = true; });
      this.map.on("move", () => { if (this.pinned) this.placeTip(this.pinned.html, this.map.project([this.pinned.lon, this.pinned.lat]), this.pinned.clear); });
      this.map.on("click", (e) => {
        if (this._eatClick) { this._eatClick = false; return; }   // the click that ends a long press
        const hit = this.pick(e.point, true);
        if (hit) this.on.onClick?.(this.pointOf(hit)); else this.on.onEmptyClick?.(e);
      });
      // a waypoint is asked for by a double click or by a press held still,
      // so an ordinary click stays free for reading a point and for panning
      this.map.doubleClickZoom.disable();
      this.map.on("dblclick", (e) => this.waypointAt(e));
      const canvas = this.map.getCanvas();
      const press = { timer: 0, at: null };
      const drop = () => { clearTimeout(press.timer); press.timer = 0; press.at = null; };
      canvas.addEventListener("pointerdown", (e) => {
        drop();
        if (e.button > 0) return;
        press.at = { x: e.clientX, y: e.clientY };
        press.timer = setTimeout(() => {
          const box = canvas.getBoundingClientRect(), point = { x: press.at.x - box.left, y: press.at.y - box.top };
          drop();
          this._eatClick = true;
          this.waypointAt({ point, lngLat: this.map.unproject([point.x, point.y]) });
        }, PRESS_MS);
      });
      canvas.addEventListener("pointermove", (e) => {
        if (press.at && Math.hypot(e.clientX - press.at.x, e.clientY - press.at.y) > PRESS_PX) drop();
      });
      for (const kind of ["pointerup", "pointercancel", "pointerleave", "wheel"]) canvas.addEventListener(kind, drop);
      // on a phone a held press raises the browser's own menu: not over the gesture
      canvas.addEventListener("contextmenu", (e) => { if (press.timer || this._eatClick) e.preventDefault(); });
      this.map.on("moveend", () => this.on.onMove?.(this.getView()));
      this.map.on("zoomend", () => this.on.onZoom?.(this.map.getZoom()));
    }

    // where a waypoint was asked for: open map only, since a point under the
    // pointer is already the mark a click would set
    waypointAt(e) {
      if (!e?.lngLat || this.pick(e.point, true)) return;
      const at = e.lngLat.wrap ? e.lngLat.wrap() : e.lngLat;
      this.on.onWaypoint?.(at.lat, at.lng);
    }

    // a new basemap: MapLibre diffs it against the old one, and our layers
    // ride along on top (transformStyle) so they are never dropped and re-added
    setStyle(style) {
      this.styleId = style.id;
      this.map.setStyle(this.imageStyle(style), { diff: true, transformStyle: (prev, next) => {
        if (!prev) return next;
        const sources = { ...next.sources }, layers = [...next.layers];
        for (const l of prev.layers || []) if (OURS.has(l.id)) { layers.push(l); sources[l.source] = prev.sources[l.source]; }
        return { ...next, sources, layers };
      } });
    }

    imageUrl(url) {
      return typeof createImageBitmap === "function" && this.imageProtocol
        ? `${this.imageProtocol}://${encodeURIComponent(url)}` : url;
    }

    // Map images can exceed a mobile GPU's texture size. Keep their bounds and
    // source resolution, fitting only the decoded display texture.
    imageStyle(style) {
      const sources = Object.fromEntries(Object.entries(style.sources).map(([id, source]) => [id,
        source.type === "image" && source.url && typeof createImageBitmap === "function"
          ? { ...source, url: this.imageUrl(source.url) } : source]));
      return { ...style, sources };
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
      if (view.bounds) { this.map.fitBounds(view.bounds, { padding: FIT_PAD, maxZoom: FIT_MAX, animate: false }); return; }
      const cur = this.getView();
      if (cur && Math.abs(cur.zoom - view.zoom) < 1e-3 && Math.abs(cur.center.lat - view.center.lat) < 1e-6 && Math.abs(cur.center.lon - view.center.lon) < 1e-6) return;
      this.map.jumpTo({ center: [view.center.lon, view.center.lat], zoom: view.zoom });
    }

    // the zoom a view lands at: its own, or the one that fits its box (null before the map exists)
    zoomFor(view) {
      if (view?.zoom != null) return view.zoom;
      if (!view?.bounds || !this.map) return null;
      try { return this.map.cameraForBounds(view.bounds, { padding: FIT_PAD, maxZoom: FIT_MAX })?.zoom ?? null; } catch { return null; }
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
      if (this.pinned) return;                        // a pinned box is not the pointer's to change
      const html = hit ? hoverOf(hit.tr, hit.i) : "";
      if (!html) { this.tip.hidden = true; return; }
      this.placeTip(html, pt);
    }

    // a box pinned at a point of the map: it stays (following the map as it
    // pans) and its text can be selected and copied, until it is unpinned;
    // `key` names what is shown, for a caller to refresh it while it is up
    // `clear` is lines the box should keep off, each a list of lat/lon pairs,
    // such as the route drawn to the point it belongs to; a lone point is a
    // line of one
    pin(lat, lon, content, key = null, clear = []) {
      if (!this.map || lat == null || lon == null) return;
      this.pinned = { lat: +lat, lon: +lon, html: content, key, clear };
      this.tip.classList.add("pinned");
      this.placeTip(content, this.map.project([+lon, +lat]), clear);
    }
    unpin() {
      if (!this.pinned) return;
      this.pinned = null;
      this.tip.classList.remove("pinned");
      this.tip.hidden = true;
    }
    pinnedIs(key) { return !!this.pinned && this.pinned.key === key; }
    pinnedBox() { return this.pinned && this.tip.firstElementChild; }

    placeTip(content, pt, clear = []) {
      if (content instanceof Node) { if (this.tip.firstChild !== content) this.tip.replaceChildren(content); }
      else this.tip.innerHTML = content;
      this.tip.hidden = false;
      // beside the point, never over it, and clear of the lines drawn to it:
      // whole lines, not just their corners, since a route crosses a box
      // between two points far outside it
      const w = this.el.clientWidth, h = this.el.clientHeight, tw = this.tip.offsetWidth, th = this.tip.offsetHeight;
      const keep = clear.map((line) => thin(line).map((p) => this.map.project([+p[1], +p[0]])));
      const spot = tipSpot(pt, w, h, tw, th, keep);
      this.tip.style.left = `${spot.x}px`;
      this.tip.style.top = `${spot.y}px`;
    }
  }

  UW.MapView = MapView;
  UW.mapFeaturesOf = featuresOf;                      // for tests: what the map would draw from a list of traces
  UW.mapTipSpot = tipSpot;                            // for tests: where a pinned box goes beside its point
})();
