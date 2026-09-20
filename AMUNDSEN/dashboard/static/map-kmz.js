/* KMZ export of the whole loaded map: every layer the page holds, including
 * what sits outside the viewport, as vector placemarks rather than a picture.
 *
 * The source is UW.mapView: its Plotly-shaped traces ({base, live}) become one
 * KML Folder per trace, lines become LineStrings (one per unbroken run of
 * lat/lon, a null breaking the run) and markers become Points. Ice-chart
 * polygons are read straight off the MapLibre source when one is loaded.
 *
 * Colours follow map.js: a per-point numeric marker.color is mapped through
 * UW.cmap/UW.colourAt, then quantised into QUANT bins so a coloured track
 * shares a handful of <Style> elements instead of carrying one per point.
 *
 * The KMZ is a zip of doc.kml plus the white dot the point styles tint, so the
 * file needs no network when Google Earth opens it.
 */
(function () {
  "use strict";
  const QUANT = 24;                                   // colour bins a per-point ramp is reduced to
  const DEFAULT_COLOUR = "#1f77b4";                   // as map.js draws a trace that names no colour
  const NULL_COLOUR = "#7d8895";                      // a point whose value is missing on a coloured track
  const DOT = "files/dot.png";                        // the icon every Point style tints
  const DOT_PNG =
    "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAAtklEQVR42u2X3QkEIQyELckSLMVS0klKsSRLmPNgFxbZE13cZB5u" +
    "YF5U9PM/CQDComNzbpaL81G+2leYbZiatblirHq0S7sAvrMqeKYysyqjSsEeyRMAxV7pCsDuwYcQby379Hb0B85C8RdAMQIodwAJ" +
    "tko9gBoDaA9QjQHqFSDCR/EEyE4A+QQQJwD5A9CcAfdb4P4OULyE7n+B+29IEQ+4R0QUMSFFVEyRF1BkRhS5IU12/Jo/LV60mqdU" +
    "j5EAAAAASUVORK5CYII=";

  const at = (v, i) => (Array.isArray(v) ? v[i] : v);
  const has = (mode, part) => String(mode || "markers").split("+").includes(part);
  const silent = (tr) => tr.hoverinfo === "skip" || tr.hoverinfo === "none";
  const num = (v) => (typeof v === "number" ? v : Number(v));
  const ok = (lat, lon) => lat != null && lon != null && isFinite(lat) && isFinite(lon);
  const clamp = (v, lo, hi) => Math.max(lo, Math.min(hi, v));

  // ---------------------------------------------------------------- text
  const ENTITIES = { amp: "&", lt: "<", gt: ">", quot: '"', apos: "'", nbsp: " ", deg: "°", middot: "·",
    times: "×", minus: "−", ndash: "–", mdash: "—", hellip: "…", plusmn: "±", micro: "µ" };
  function entity(_, body) {
    if (body[0] === "#") { const c = body[1] === "x" || body[1] === "X" ? parseInt(body.slice(2), 16) : parseInt(body.slice(1), 10);
      return isFinite(c) && c > 0 ? String.fromCodePoint(c) : ""; }
    return ENTITIES[body.toLowerCase()] ?? `&${body};`;
  }
  // a hover box's HTML as the plain text a KML <description> carries
  function plainText(html) {
    return String(html ?? "")
      .replace(/<\s*br\s*\/?\s*>/gi, "\n")
      .replace(/<\s*\/\s*(p|div|tr|li|h[1-6]|table)\s*>/gi, "\n")
      .replace(/<[^>]*>/g, "")
      .replace(/&(#x?[0-9a-f]+|[a-z]+);/gi, entity)
      .replace(/[ \t]+/g, " ").replace(/ ?\n ?/g, "\n").replace(/\n{3,}/g, "\n\n").trim();
  }
  const xml = (s) => String(s ?? "").replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
  const tag = (name, body) => (body === "" || body == null ? "" : `<${name}>${body}</${name}>`);

  // ---------------------------------------------------------------- colours
  const NAMED = { white: [255, 255, 255], black: [0, 0, 0], red: [255, 0, 0], green: [0, 128, 0], blue: [0, 0, 255],
    yellow: [255, 255, 0], orange: [255, 165, 0], grey: [128, 128, 128], gray: [128, 128, 128], transparent: [0, 0, 0, 0] };
  // a CSS colour as [r, g, b, a], or null when it is not one we can read
  function rgba(css) {
    const s = String(css ?? "").trim().toLowerCase();
    if (!s) return null;
    if (s[0] === "#") {
      const h = s.slice(1);
      const wide = h.length === 6 || h.length === 8, short = h.length === 3 || h.length === 4;
      if (!wide && !short) return null;
      const step = wide ? 2 : 1;
      const parts = [];
      for (let i = 0; i < h.length; i += step) {
        const v = parseInt(wide ? h.slice(i, i + 2) : h[i] + h[i], 16);
        if (!isFinite(v)) return null;
        parts.push(v);
      }
      return [parts[0], parts[1], parts[2], parts.length > 3 ? parts[3] / 255 : 1];
    }
    const fn = s.match(/^rgba?\(([^)]*)\)$/);
    if (fn) {
      const p = fn[1].split(/[,\s/]+/).filter(Boolean).map(Number);
      if (p.length < 3 || p.some((v) => !isFinite(v))) return null;
      return [p[0], p[1], p[2], p.length > 3 ? p[3] : 1];
    }
    const n = NAMED[s];
    return n ? [n[0], n[1], n[2], n.length > 3 ? n[3] : 1] : null;
  }
  const hex2 = (v) => clamp(Math.round(v), 0, 255).toString(16).padStart(2, "0");
  // KML wants aabbggrr: alpha first, then blue, green, red
  function kmlColour(css, opacity = 1) {
    const c = rgba(css) || rgba(DEFAULT_COLOUR);
    const a = clamp((c[3] ?? 1) * (isFinite(opacity) ? opacity : 1), 0, 1);
    return hex2(a * 255) + hex2(c[2]) + hex2(c[1]) + hex2(c[0]);
  }

  // the colour map.js paints each marker with, quantised: a numeric colour
  // array is binned so one track needs at most QUANT styles
  function markerColours(tr) {
    const m = tr.marker || {};
    const numeric = Array.isArray(m.color) && m.color.some((c) => typeof c === "number");
    if (!numeric) return (i) => at(m.color, i) || DEFAULT_COLOUR;
    const UW = window.UW || {};
    const stops = UW.cmap ? UW.cmap(m.colorscale, !!m.reversescale) : [[0, DEFAULT_COLOUR], [1, DEFAULT_COLOUR]];
    let vlo = Infinity, vhi = -Infinity;
    for (const c of m.color) if (typeof c === "number" && isFinite(c)) { if (c < vlo) vlo = c; if (c > vhi) vhi = c; }
    const lo = m.cmin ?? vlo, hi = m.cmax ?? vhi, span = (hi - lo) || 1;
    const binned = new Map();
    return (i) => {
      const v = at(m.color, i);
      if (typeof v !== "number" || !isFinite(v)) return NULL_COLOUR;
      const bin = clamp(Math.floor(((v - lo) / span) * QUANT), 0, QUANT - 1);
      if (!binned.has(bin)) binned.set(bin, UW.colourAt ? UW.colourAt(stops, (bin + 0.5) / QUANT) : DEFAULT_COLOUR);
      return binned.get(bin);
    };
  }

  // ---------------------------------------------------------------- styles
  // one <Style> per distinct body, referenced by the placemarks that share it
  function styleTable() {
    const ids = new Map();
    return {
      id(body) {
        let id = ids.get(body);
        if (!id) { id = `s${ids.size}`; ids.set(body, id); }
        return id;
      },
      xml: () => [...ids].map(([body, id]) => `<Style id="${id}">${body}</Style>`).join(""),
      get size() { return ids.size; },
    };
  }
  const lineStyle = (colour, width, opacity) =>
    `<LineStyle><color>${kmlColour(colour, opacity)}</color><width>${round(Math.max(1, width ?? 2), 1)}</width></LineStyle>`;
  const pointStyle = (colour, size, opacity, labelled) =>
    `<IconStyle><color>${kmlColour(colour, opacity)}</color><scale>${round(Math.max(0.2, (size ?? 6) / 10), 2)}</scale>` +
    `<Icon><href>${DOT}</href></Icon></IconStyle><LabelStyle><scale>${labelled ? 0.8 : 0}</scale></LabelStyle>`;
  const polyStyle = (colour, opacity) =>
    `<PolyStyle><color>${kmlColour(colour, opacity)}</color></PolyStyle>` +
    `<LineStyle><color>${kmlColour("#374151", 0.65)}</color><width>1</width></LineStyle>`;

  const round = (v, dp) => +(+v).toFixed(dp);
  const coord = (lon, lat) => `${round(+lon, 6)},${round(+lat, 6)},0`;

  // ---------------------------------------------------------------- traces -> placemarks
  function traceXml(tr, label, styles, box) {
    const lat = tr.lat || [], lon = tr.lon || [], n = lat.length;
    const op = tr.opacity ?? 1, marks = [];
    for (let i = 0; i < n; i++) if (ok(lat[i], lon[i])) grow(box, +lat[i], +lon[i]);
    if (has(tr.mode, "lines")) {
      const ln = tr.line || {}, style = styles.id(lineStyle(ln.color || DEFAULT_COLOUR, ln.width, op));
      const runs = [];
      let run = [];
      for (let i = 0; i < n; i++) {
        if (!ok(lat[i], lon[i])) { if (run.length > 1) runs.push(run); run = []; continue; }
        run.push(coord(lon[i], lat[i]));
      }
      if (run.length > 1) runs.push(run);
      runs.forEach((points, k) => marks.push(
        `<Placemark><name>${xml(runs.length > 1 ? `${label} ${k + 1}` : label)}</name><styleUrl>#${style}</styleUrl>` +
        `<LineString><tessellate>1</tessellate><altitudeMode>clampToGround</altitudeMode>` +
        `<coordinates>${points.join(" ")}</coordinates></LineString></Placemark>`));
    }
    if (has(tr.mode, "markers")) {
      const m = tr.marker || {}, colourOf = markerColours(tr), labelled = has(tr.mode, "text");
      for (let i = 0; i < n; i++) {
        if (!ok(lat[i], lon[i])) continue;
        const text = at(tr.text, i), hover = silent(tr) ? null : at(tr.hovertext, i);
        const style = styles.id(pointStyle(colourOf(i), at(m.size, i), (at(m.opacity, i) ?? 1) * op, labelled && text != null && text !== ""));
        const name = labelled && text != null && text !== "" ? String(text) : `${label} ${i + 1}`;
        const description = plainText(hover ?? text ?? "");
        marks.push(`<Placemark><name>${xml(name)}</name>${tag("description", xml(description))}` +
          `<styleUrl>#${style}</styleUrl><Point><coordinates>${coord(lon[i], lat[i])}</coordinates></Point></Placemark>`);
      }
    } else if (has(tr.mode, "text")) {
      // a text-only trace still carries its labels, as unmarked points
      const style = styles.id(pointStyle(tr.textfont?.color || DEFAULT_COLOUR, 1, op, true));
      for (let i = 0; i < n; i++) {
        const text = at(tr.text, i);
        if (!ok(lat[i], lon[i]) || text == null || text === "") continue;
        marks.push(`<Placemark><name>${xml(text)}</name><styleUrl>#${style}</styleUrl>` +
          `<Point><coordinates>${coord(lon[i], lat[i])}</coordinates></Point></Placemark>`);
      }
    }
    return marks;
  }

  function polygonXml(feature, label, styles, box, fill, opacity) {
    const g = feature.geometry, rings = g.type === "Polygon" ? [g.coordinates] : g.coordinates;
    const props = feature.properties || {};
    const body = Object.entries(props)
      .filter(([, v]) => v != null && v !== "" && typeof v !== "object")
      .map(([k, v]) => `${k}: ${v}`).join("\n");
    const parts = [];
    for (const poly of rings) {
      const ring = (points, kind) => {
        const cs = points.filter((p) => ok(p[1], p[0]));
        cs.forEach((p) => grow(box, +p[1], +p[0]));
        return cs.length > 2 ? `<${kind}><LinearRing><coordinates>${cs.map((p) => coord(p[0], p[1])).join(" ")}</coordinates></LinearRing></${kind}>` : "";
      };
      const outer = ring(poly[0] || [], "outerBoundaryIs");
      if (!outer) continue;
      parts.push(`<Polygon><tessellate>1</tessellate><altitudeMode>clampToGround</altitudeMode>${outer}` +
        poly.slice(1).map((r) => ring(r, "innerBoundaryIs")).join("") + `</Polygon>`);
    }
    if (!parts.length) return [];
    const style = styles.id(polyStyle(fill, opacity));
    const geometry = parts.length > 1 ? `<MultiGeometry>${parts.join("")}</MultiGeometry>` : parts[0];
    return [`<Placemark><name>${xml(label)}</name>${tag("description", xml(body))}<styleUrl>#${style}</styleUrl>${geometry}</Placemark>`];
  }

  // ---------------------------------------------------------------- the document
  const newBox = () => ({ w: Infinity, s: Infinity, e: -Infinity, n: -Infinity });
  function grow(box, lat, lon) {
    if (lat < box.s) box.s = lat;
    if (lat > box.n) box.n = lat;
    if (lon < box.w) box.w = lon;
    if (lon > box.e) box.e = lon;
  }
  // where Google Earth opens: over the middle of everything, far enough out to hold it
  function lookAt(box) {
    if (!isFinite(box.s) || !isFinite(box.w)) return "";
    const lat = (box.s + box.n) / 2, lon = (box.w + box.e) / 2;
    const across = Math.max((box.n - box.s) * 111320, (box.e - box.w) * 111320 * Math.cos((lat * Math.PI) / 180));
    const range = Math.max(2000, across * 1.4 + 1000);
    return `<LookAt><longitude>${round(lon, 6)}</longitude><latitude>${round(lat, 6)}</latitude>` +
      `<altitude>0</altitude><heading>0</heading><tilt>0</tilt><range>${Math.round(range)}</range></LookAt>`;
  }

  /* groups: {base: [trace], live: [trace]} in drawing order.
   * meta: {title, generated, polygons: {name, features, colour(feature), opacity}}. */
  // The page draws a trace per artifact, so the layers a reader knows are put
  // back together here: one folder per layer, named as the map names it.
  const LAYERS = [[/^hist/i, "History"], [/^nat/i, "Nature"], [/^plan/i, "Cruise plan"], [/^cam/i, "Camera"],
    [/^ice/i, "Ice"], [/^track|^no nearby photo/i, "Track"], [/^station/i, "Stations"], [/^waypoint/i, "Waypoints"],
    [/^event/i, "Event log"], [/^photo/i, "Photos"], [/^place|^communit/i, "Places"], [/^searoute/i, "Sea route"],
    [/^focus/i, "Marked point"], [/^latest|^ship/i, "Ship"]];
  const layerOf = (name) => (LAYERS.find(([pattern]) => pattern.test(name)) || [, name])[1];

  function kml(groups, meta = {}) {
    const styles = styleTable(), box = newBox(), layers = new Map();
    let index = 0;
    for (const traces of Object.values(groups || {})) {
      for (const tr of traces || []) {
        index++;
        if (!tr) continue;
        const label = String(tr.name || `Layer ${index}`);
        const marks = traceXml(tr, label, styles, box);
        if (!marks.length) continue;
        const layer = layerOf(label);
        if (!layers.has(layer)) layers.set(layer, []);
        layers.get(layer).push(...marks);
      }
    }
    const folders = [...layers].map(([layer, marks]) => `<Folder><name>${xml(layer)}</name>${marks.join("")}</Folder>`);
    const poly = meta.polygons;
    if (poly && poly.features?.length) {
      const marks = [];
      poly.features.forEach((f, i) => marks.push(...polygonXml(f, `${poly.name || "Polygons"} ${i + 1}`, styles, box,
        poly.colour ? poly.colour(f) : NULL_COLOUR, poly.opacity ?? 0.6)));
      if (marks.length) folders.push(`<Folder><name>${xml(poly.name || "Polygons")}</name>${marks.join("")}</Folder>`);
    }
    const generated = meta.generated instanceof Date ? meta.generated : new Date(meta.generated || Date.now());
    const stamp = isFinite(generated.getTime()) ? generated.toISOString().replace(/\.\d+Z$/, "Z") : "";
    return `<?xml version="1.0" encoding="UTF-8"?>\n` +
      `<kml xmlns="http://www.opengis.net/kml/2.2"><Document>` +
      `<name>${xml(meta.title || "Map")}</name>` +
      `<description>${xml(`Exported from the CCGS Amundsen underway dashboard${stamp ? ` at ${stamp}` : ""}.`)}</description>` +
      `<open>1</open>${lookAt(box)}${styles.xml()}${folders.join("")}</Document></kml>`;
  }

  // ---------------------------------------------------------------- zip
  const CRC = (() => {
    const table = new Uint32Array(256);
    for (let n = 0; n < 256; n++) { let c = n; for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1; table[n] = c >>> 0; }
    return table;
  })();
  function crc32(bytes) {
    let c = 0xffffffff;
    for (let i = 0; i < bytes.length; i++) c = CRC[(c ^ bytes[i]) & 255] ^ (c >>> 8);
    return (c ^ 0xffffffff) >>> 0;
  }
  async function deflate(bytes) {
    if (typeof CompressionStream !== "function") return null;
    try {
      const stream = new Blob([bytes]).stream().pipeThrough(new CompressionStream("deflate-raw"));
      return new Uint8Array(await new Response(stream).arrayBuffer());
    } catch { return null; }
  }
  function bytesOf(content) {
    if (typeof content === "string") return new TextEncoder().encode(content);
    return content instanceof Uint8Array ? content : new Uint8Array(content);
  }
  // base64 without atob, so the module works wherever it is evaluated
  const B64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  function unbase64(s) {
    const clean = s.replace(/[^A-Za-z0-9+/]/g, "");
    const out = new Uint8Array((clean.length * 3) >> 2);
    let acc = 0, bits = 0, k = 0;
    for (const ch of clean) {
      acc = (acc << 6) | B64.indexOf(ch); bits += 6;
      if (bits >= 8) { bits -= 8; out[k++] = (acc >> bits) & 255; }
    }
    return out.subarray(0, k);
  }

  /* files: {name: string | Uint8Array} -> the bytes of a zip Google Earth reads.
   * Entries are deflate-raw where the platform compresses, stored otherwise. */
  async function zip(files, when = new Date()) {
    const encoder = new TextEncoder();
    const entries = [];
    for (const [name, content] of Object.entries(files)) {
      const data = bytesOf(content), packed = await deflate(data);
      const body = packed && packed.length < data.length ? packed : data;
      entries.push({ name: encoder.encode(name), data, body, method: body === data ? 0 : 8, crc: crc32(data) });
    }
    const date = isFinite(when?.getTime?.()) ? when : new Date();
    const dosTime = (date.getHours() << 11) | (date.getMinutes() << 5) | (date.getSeconds() >> 1);
    const dosDate = ((Math.max(1980, date.getFullYear()) - 1980) << 9) | ((date.getMonth() + 1) << 5) | date.getDate();
    let size = 22;
    for (const e of entries) size += 30 + e.name.length + e.body.length + 46 + e.name.length;
    const out = new Uint8Array(size), view = new DataView(out.buffer);
    let at = 0;
    const u32 = (v) => { view.setUint32(at, v, true); at += 4; };
    const u16 = (v) => { view.setUint16(at, v, true); at += 2; };
    const put = (b) => { out.set(b, at); at += b.length; };
    for (const e of entries) {
      e.offset = at;
      u32(0x04034b50); u16(20); u16(0x0800); u16(e.method); u16(dosTime); u16(dosDate);
      u32(e.crc); u32(e.body.length); u32(e.data.length); u16(e.name.length); u16(0);
      put(e.name); put(e.body);
    }
    const dirAt = at;
    for (const e of entries) {
      u32(0x02014b50); u16(20); u16(20); u16(0x0800); u16(e.method); u16(dosTime); u16(dosDate);
      u32(e.crc); u32(e.body.length); u32(e.data.length); u16(e.name.length); u16(0); u16(0); u16(0); u16(0); u32(0); u32(e.offset);
      put(e.name);
    }
    const dirBytes = at - dirAt;                        // measured before the end record moves `at` on
    u32(0x06054b50); u16(0); u16(0); u16(entries.length); u16(entries.length); u32(dirBytes); u32(dirAt); u16(0);
    return out;
  }

  // ---------------------------------------------------------------- ice polygons
  // the narrow slice of MapLibre's expression language the ice fill uses, so
  // the KMZ takes its polygon colours from the live style rather than a copy
  function paint(expr, props) {
    if (!Array.isArray(expr)) return expr;
    const [op, ...rest] = expr;
    if (op === "literal") return rest[0];
    if (op === "get") return props?.[rest[0]] ?? null;
    if (op === "==") return paint(rest[0], props) === paint(rest[1], props);
    if (op === "case") {
      for (let i = 0; i + 1 < rest.length; i += 2) if (paint(rest[i], props)) return paint(rest[i + 1], props);
      return paint(rest[rest.length - 1], props);
    }
    if (op === "step") {
      const v = paint(rest[0], props);
      let out = paint(rest[1], props);
      for (let i = 2; i + 1 < rest.length; i += 2) if (typeof v === "number" && v >= rest[i]) out = paint(rest[i + 1], props);
      return out;
    }
    return null;
  }
  // the ice chart's polygons, when a polygon chart (not a raster) is on the map
  function icePolygons(mapView) {
    const map = mapView?.map;
    if (!map?.getSource) return null;
    let data, fill, opacity;
    try {
      const source = map.getSource("cis-ice-chart");
      data = source?._data ?? source?.serialize?.().data;
      if (map.getLayer("cis-ice-fill")) {
        fill = map.getPaintProperty("cis-ice-fill", "fill-color");
        opacity = map.getPaintProperty("cis-ice-fill", "fill-opacity");
      }
    } catch { return null; }
    if (!data || data.type !== "FeatureCollection" || !Array.isArray(data.features)) return null;
    const features = data.features.filter((f) => f?.geometry?.type === "Polygon" || f?.geometry?.type === "MultiPolygon");
    if (!features.length) return null;
    return { name: "Ice chart", features, opacity: isFinite(opacity) ? opacity : 0.6,
      colour: (f) => (typeof fill === "string" ? fill : paint(fill, f.properties || {})) || NULL_COLOUR };
  }

  // ---------------------------------------------------------------- download
  const pad = (v) => String(v).padStart(2, "0");
  function stampOf(date) {
    return `${date.getUTCFullYear()}${pad(date.getUTCMonth() + 1)}${pad(date.getUTCDate())}-${pad(date.getUTCHours())}${pad(date.getUTCMinutes())}`;
  }
  async function download(mapView, meta = {}) {
    const generated = meta.generated instanceof Date ? meta.generated : new Date();
    const groups = mapView?.traces || {};
    const bytes = await zip({
      "doc.kml": kml(groups, { title: meta.title || window.document.title, generated, polygons: icePolygons(mapView) }),
      [DOT]: unbase64(DOT_PNG),
    }, generated);
    const url = window.URL.createObjectURL(new window.Blob([bytes], { type: "application/vnd.google-earth.kmz" }));
    const link = window.document.createElement("a");
    link.href = url;
    link.download = `amundsen-map-${stampOf(generated)}.kmz`;
    window.document.body.append(link);
    link.click();
    link.remove();
    window.setTimeout(() => window.URL.revokeObjectURL(url), 10000);
    return bytes;
  }

  window.UWMapKMZ = { kml, zip, download, icePolygons, kmlColour, plainText, crc32, QUANT };
})();
