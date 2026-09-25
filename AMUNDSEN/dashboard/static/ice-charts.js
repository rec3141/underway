/* Dated CIS vector and raster charts load from the local cache, independently of ship observations. */
(() => {
  'use strict';
  // Display-only translations: source metadata, numeric codes and measurements are unchanged.
  const messageKeys = {
    "Unknown": "iceChart.unknown",
    "Latest on/before map end": "iceChart.latest",
    "Chart request timed out": "iceChart.timeout",
    "Cached chart unavailable": "iceChart.unavailable",
    "No ice charts cached for this dashboard.": "iceChart.noCharts",
    "valid after map end": "iceChart.validAfter",
    "same date as map end": "iceChart.sameDate",
    "Daily raster analysis for the ship area; use the original chart for egg codes.": "iceChart.rasterHelp",
    "Click a polygon for its egg code. Regional analysis; conditions can change between charts.": "iceChart.polygonHelp",
    "CIS ice egg": "iceChart.title",
    "Close": "iceChart.close",
    "Egg code: total concentration; partial concentrations; stages of development; ice forms": "iceChart.eggLabel",
    "Ice type": "iceChart.iceType",
    "Concentration": "iceChart.concentration",
    "Stage of development": "iceChart.stage",
    "Form / floe size": "iceChart.form",
    "Not reported": "iceChart.notReported",
    "Unknown / not reported": "iceChart.unknownReported",
    "The oval shows the three main ice types in decoded egg codes; missing values are shown as —. This regional chart is separate from camera observations.": "iceChart.note",
    "Source SIGRID codes": "iceChart.sourceCodes",
    "Original chart data": "iceChart.original",
    "Egg-code guide": "iceChart.guide",
    "Open Government Licence": "iceChart.licence",
    "No cached chart on or before {date}. Choose an available date.": "iceChart.noBefore",
    "{count} days after map end": "iceChart.daysAfter",
    "{count} days before map end": "iceChart.daysBefore",
    "1 day after map end": "iceChart.dayAfter",
    "1 day before map end": "iceChart.dayBefore",
    "Loading image…": "iceChart.loadingImage",
    "Loading polygons…": "iceChart.loadingPolygons",
    "Total concentration: {value}": "iceChart.total",
    " (SIGRID code {code})": "iceChart.sigrid",
    "Trace ice thicker than type A: {value} (<1/10).": "iceChart.trace",
    "Eastern Arctic": "iceChart.region.eastern",
    "Western Arctic": "iceChart.region.western",
    "Hudson Bay": "iceChart.region.hudson",
    "East Coast": "iceChart.region.eastCoast",
    "Great Lakes": "iceChart.region.greatLakes",
    "Eureka (daily raster)": "iceChart.region.eureka"
  };
  const t = (source, values = {}) => {
    const key = Object.prototype.hasOwnProperty.call(messageKeys, source) ? messageKeys[source] : null;
    const translated = key && window.UWI18n?.t(key, values);
    return translated && translated !== key ? translated : source.replace(/\{(\w+)\}/g, (match, name) => values[name] ?? match);
  };
  const UW = (window.UW = window.UW || {});
  const SOURCE = 'cis-ice-chart', FILL = 'cis-ice-fill', OUTLINE = 'cis-ice-outline', RASTER = 'cis-ice-raster';
  const DAY = 86400000;
  const colours = ['#b9e5fa', '#8edb91', '#fff176', '#ffb74d', '#ef5350'];
  const colour = value => value == null ? '#9aa5b1' : colours[value < 1 ? 0 : value < 4 ? 1 : value < 7 ? 2 : value < 9 ? 3 : 4];
  const safeLink = value => { try { const u = new URL(value); return u.protocol === 'https:' ? u.href : null; } catch { return null; } };
  const defaultChart = (charts, reference) => charts.filter(c => Date.parse(c.valid_time || c.date) <= Date.parse(reference))
    .sort((a, b) => Date.parse(b.valid_time || b.date) - Date.parse(a.valid_time || a.date))[0] || null;
  const validLabel = c => `${c.date}${c.valid_time ? ' · ' + c.valid_time.slice(11, 16) : ''} UTC`;
  const code = value => value == null || value === '' || value === '-9' ? '—' : String(value);
  const el = (tag, text, className) => { const node = document.createElement(tag); if (text != null) node.textContent = text; if (className) node.className = className; return node; };
  const $ = id => document.getElementById(id);
  // mode: off, the daily raster for the ship's area, or every region's weekly chart at once
  let mode = 'off', shown = 'off', enabled = false, initialized = false, charts = [], view = null, chart = null, collection = null;
  let day = '', generation = 0, controller = null, loading = false, failure = '', ensureBusy = false, detail = null, detailProperties = null;
  let signature = '', reference = '';
  const cache = new Map();
  const modes = () => ['daily', 'weekly'].filter(m => charts.some(c => (c.kind === 'raster') === (m === 'daily')));

  function initialize() {
    if (initialized || !$('icechart-toggle')) return;
    initialized = true;
    mode = UW.store?.get('icecharts.mode', UW.store?.get('icecharts.enabled', false) ? 'auto' : 'off') || 'off';
    // one button steps through daily (when there is one), weekly, then off
    $('icechart-toggle').onclick = () => {
      const order = modes(), at = order.indexOf(shown);
      mode = at + 1 >= order.length ? 'off' : order[at + 1];
      UW.store?.set('icecharts.mode', mode);
      closeDetails(); select();
    };
    $('icechart-retry').onclick = () => load(true);
    renderLegend();
  }

  function renderLegend() {
    const legend = $('icechart-legend');
    legend.replaceChildren();
    for (const [label, value] of [['<1/10', 0], ['1–3/10', 2], ['4–6/10', 5], ['7–8/10', 8], ['9–10/10', 10], [t("Unknown"), null]]) {
      const item = el('span', label), swatch = el('i'); swatch.style.backgroundColor = colour(value); item.prepend(swatch); legend.append(item);
    }
  }

  function refresh(manifest, mapView, referenceTime) {
    initialize(); if (!initialized) return;
    if (view !== mapView) {
      view = mapView;
      view.map.on('style.load', ensureLayers);
      view.map.on('styledata', ensureLayers);
    }
    const next = JSON.stringify(manifest?.charts || []);
    const changed = signature !== next || reference !== referenceTime;
    signature = next; reference = referenceTime; day = referenceTime.slice(0, 10);
    charts = (manifest?.charts || []).filter(c => c.id && c.url && /^\d{4}-\d{2}-\d{2}$/.test(c.date));
    if (changed || !chart) select();
    else ensureLayers();
  }

  // the daily raster valid by the map end, or the weekly charts as one: each
  // region's latest by the map end (the regions do not overlap)
  function current() {
    if (shown === 'daily') return defaultChart(charts.filter(c => c.kind === 'raster'), reference);
    const weekly = charts.filter(c => c.kind !== 'raster');
    const parts = [...new Set(weekly.map(c => c.region))].sort().map(r => defaultChart(weekly.filter(c => c.region === r), reference)).filter(Boolean);
    return parts.length ? {kind: 'weekly', parts, id: parts.map(c => c.id).join('+'), url: parts.map(c => c.url).join('|')} : null;
  }

  function select() {
    // the choice is kept while the charts for it are missing; another kind stands in
    const order = modes();
    shown = mode === 'off' ? 'off' : order.includes(mode) ? mode : order[0] || 'off';
    enabled = mode !== 'off';
    const previous = chart;
    chart = enabled ? current() : null;
    $('icechart-toggle').classList.toggle('on', enabled);
    $('icechart-toggle').setAttribute('aria-pressed', String(enabled));
    $('icechart-controls').hidden = !enabled;
    if (!enabled || previous?.url !== chart?.url || previous?.id !== chart?.id) {
      ++generation; controller?.abort(); loading = false; collection = null; failure = ''; removeLayers(); closeDetails();
    }
    if (enabled && chart && !collection && !loading) load(); else { status(); ensureLayers(); }
  }

  async function fetchChart(requested, signal, retry) {
    let data = !retry && cache.get(requested.url);
    if (data) return data;
    const response = await fetch(requested.url, {signal, cache: retry ? 'reload' : 'default'});
    if (!response.ok) throw Error(`HTTP ${response.status}`);
    if (requested.kind === 'raster') {
      if (!Array.isArray(requested.coordinates) || requested.coordinates.length !== 4 || requested.coordinates.some(p => !Array.isArray(p) || p.length !== 2)) throw Error('Invalid raster coordinates');
      const blob = await response.blob();
      if (!blob.type.startsWith('image/')) throw Error('Invalid raster chart');
      data = {kind: 'raster', url: URL.createObjectURL(blob)};
    } else {
      data = await response.json();
      if (data.type !== 'FeatureCollection' || !Array.isArray(data.features)) throw Error('Invalid polygon chart');
      if (data.features.some(f => !['Polygon', 'MultiPolygon'].includes(f.geometry?.type))) throw Error('Invalid chart geometry');
    }
    cache.set(requested.url, data);
    if (cache.size > 8) {
      const oldest = cache.keys().next().value, discarded = cache.get(oldest);
      if (discarded?.kind === 'raster') URL.revokeObjectURL(discarded.url);
      cache.delete(oldest);
    }
    return data;
  }

  async function load(retry = false) {
    if (!enabled || !chart) return;
    const requested = chart, token = ++generation;
    controller?.abort(); controller = new AbortController();
    const requestController = controller;
    loading = true; failure = ''; collection = null; removeLayers(); status();
    const timeout = setTimeout(() => requestController.abort(), 20000);
    try {
      let data;
      if (requested.kind === 'weekly') {
        // one collection; each polygon keeps the index of the chart it came from, for its details
        const parts = await Promise.all(requested.parts.map(c => fetchChart(c, requestController.signal, retry)));
        data = {type: 'FeatureCollection', features: parts.flatMap((p, i) => p.features.map(f => ({...f, properties: {...f.properties, chart_part: i}})))};
      } else data = await fetchChart(requested, requestController.signal, retry);
      if (token !== generation) return;
      collection = data; loading = false; ensureLayers();
    } catch (error) {
      if (token !== generation) return;
      loading = false; failure = error.name === 'AbortError' ? 'Chart request timed out' : 'Cached chart unavailable';
    } finally { clearTimeout(timeout); if (token === generation) status(); }
  }

  // the charts shown (region and valid date), or why there are none, go in the button's tooltip
  function status() {
    const hint = window.UWI18n?.t('mapControls.iceChartsHint') || "Canadian Ice Service daily and regional ice charts";
    const shown = chart?.kind === 'weekly' ? chart.parts : chart ? [chart] : [];
    let text = '';
    if (!charts.length) text = t("No ice charts cached for this dashboard.");
    else if (enabled && !chart) text = t("Cached chart unavailable");
    else if (enabled) text = shown.map(c => `${t(c.region)} · ${validLabel(c)}`).join('\n') +
      (loading ? '\n' + t(chart.kind === 'raster' ? 'Loading image…' : 'Loading polygons…') : failure ? '\n' + t(failure) : '');
    $('icechart-toggle').title = text ? `${text}\n${hint}` : hint;
    $('icechart-retry').hidden = !failure;
    $('icechart-legend').hidden = !collection || chart?.kind === 'raster';
    $('icechart-controls').hidden = !enabled || ($('icechart-retry').hidden && $('icechart-legend').hidden);
  }

  function removeLayers() {
    const m = view?.map; if (!m?.style?._loaded) return;
    for (const id of [OUTLINE, FILL, RASTER]) if (m.getLayer(id)) m.removeLayer(id);
    if (m.getSource(SOURCE)) m.removeSource(SOURCE);
  }

  function ensureLayers() {
    const m = view?.map;
    if (ensureBusy || !enabled || !collection || !m?.style?._loaded) return;
    ensureBusy = true;
    try {
      const before = m.getLayer('coast') ? 'coast' : m.getLayer('u-base-lines') ? 'u-base-lines' : undefined;
      if (chart.kind === 'raster') {
        if (!m.getSource(SOURCE)) m.addSource(SOURCE, {type: 'image', url: view.imageUrl(collection.url), coordinates: chart.coordinates});
        if (!m.getLayer(RASTER)) m.addLayer({id: RASTER, type: 'raster', source: SOURCE,
          paint: {'raster-opacity': 1, 'raster-fade-duration': 0, 'raster-resampling': 'linear'}}, before);
      } else {
        if (!m.getSource(SOURCE)) m.addSource(SOURCE, {type: 'geojson', data: collection, tolerance: .1, attribution: (chart.parts?.[0] || chart).attribution || 'Canadian Ice Service / ECCC'});
        if (!m.getLayer(FILL)) m.addLayer({id: FILL, type: 'fill', source: SOURCE, paint: {
          'fill-opacity': 1,
          'fill-color': ['case', ['==', ['get', 'concentration'], null], '#9aa5b1', ['step', ['get', 'concentration'], colours[0], 1, colours[1], 4, colours[2], 7, colours[3], 9, colours[4]]]
        }}, before);
        if (!m.getLayer(OUTLINE)) m.addLayer({id: OUTLINE, type: 'line', source: SOURCE, paint: {'line-color': '#374151', 'line-width': .8, 'line-opacity': .65}}, before);
      }
    } finally { ensureBusy = false; }
  }

  function closeDetails() { if (detail?.open) detail.close(); }
  function click(event) {
    const m = view?.map;
    if (!event?.point || !enabled || !collection || chart?.kind === 'raster' || !m?.getLayer(FILL)) return false;
    const feature = m.queryRenderedFeatures(event.point, {layers: [FILL]})[0];
    if (!feature) return false;
    detailProperties = feature.properties;
    renderDetail();
    return true;
  }

  function renderDetail() {
    if (!detailProperties || !chart) return;
    const source = chart.parts?.[detailProperties.chart_part] || chart;
    if (!detail) {
      detail = el('dialog', null, 'icechart-detail'); detail.setAttribute('aria-labelledby', 'icechart-detail-title'); document.body.append(detail);
    }
    const p = detailProperties, head = el('div', null, 'icechart-detail-head'), title = el('h3', t("CIS ice egg")); title.id = 'icechart-detail-title';
    const close = el('button', t("Close")); close.onclick = () => detail.close(); head.append(title, close);
    const meta = el('p', `${t(source.region)} · ${validLabel(source)}`);
    const egg = el('div', null, 'icechart-egg'); egg.setAttribute('aria-label', t("Egg code: total concentration; partial concentrations; stages of development; ice forms"));
    egg.append(el('div', p.egg_ct || '—', 'icechart-egg-total'));
    for (const row of [['ca', 'cb', 'cc'], ['sa', 'sb', 'sc'], ['fa', 'fb', 'fc']]) {
      const line = el('div', null, 'icechart-egg-row');
      for (const key of row) line.append(el('span', p['egg_' + key] || '—'));
      egg.append(line);
    }
    const concentration = el('p', t('Total concentration: {value}', {value: p.concentration_label || t('Unknown')}) + (p.CT ? t(' (SIGRID code {code})', {code: p.CT}) : ''));
    const table = el('table'), header = el('tr');
    for (const label of [t("Ice type"), t("Concentration"), t("Stage of development"), t("Form / floe size")]) header.append(el('th', label));
    const thead = el('thead'); thead.append(header); table.append(thead);
    const tbody = el('tbody');
    for (const suffix of ['a', 'b', 'c']) {
      const up = suffix.toUpperCase(), tr = el('tr');
      for (const value of [up, p['concentration_' + suffix + '_label'] || t("Not reported"), p['stage_' + suffix] || t("Unknown / not reported"), p['form_' + suffix] || t("Unknown / not reported")]) tr.append(el('td', value));
      tbody.append(tr);
    }
    table.append(tbody);
    const note = el('p', t("The oval shows the three main ice types in decoded egg codes; missing values are shown as —. This regional chart is separate from camera observations."));
    const raw = el('details'), summary = el('summary', t("Source SIGRID codes")); raw.append(summary, el('pre', ['CT', 'CA', 'CB', 'CC', 'SA', 'SB', 'SC', 'FA', 'FB', 'FC', 'CN', 'CD', 'CF'].map(k => k + ': ' + code(p[k])).join('  ')));
    const supplemental = el('div');
    if (p.trace_thicker_ice) supplemental.append(el('p', t('Trace ice thicker than type A: {value} (<1/10).', {value: p.trace_thicker_ice})));
    const footer = el('p', source.attribution || 'Canadian Ice Service / ECCC');
    const original = safeLink(source.source_url);
    if (original) { const link = el('a', t("Original chart data")); link.href = original; link.target = '_blank'; link.rel = 'noopener'; footer.append(document.createTextNode(' · '), link); }
    const guide = el('a', t("Egg-code guide")); guide.href = 'https://www.canada.ca/en/environment-climate-change/services/ice-forecasts-observations/publications/interpreting-charts/chapter-1.html'; guide.target = '_blank'; guide.rel = 'noopener'; footer.append(document.createTextNode(' · '), guide);
    const licence = safeLink(source.licence_url);
    if (licence) { const link = el('a', t("Open Government Licence")); link.href = licence; link.target = '_blank'; link.rel = 'noopener'; footer.append(document.createTextNode(' · '), link); }
    detail.replaceChildren(head, meta, egg, concentration, table, supplemental, note, raw, footer); if (!detail.open) detail.showModal();
    return true;
  }

  window.addEventListener?.('uw:localechange', () => {
    if (!initialized) return;
    // Re-label in place; do not call select/load or touch map state.
    renderLegend(); status();
    if (detail?.open) {
      const expanded = !!detail.querySelector('details')?.open;
      renderDetail();
      const raw = detail.querySelector('details');
      if (raw) raw.open = expanded;
    }
  });

  UW.iceCharts = {refresh, click};
  UW.iceChartHelpers = {colour, defaultChart, code, safeLink};
})();
