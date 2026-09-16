/* Live sampling settings and plots share the server's persisted sampling record. */
(() => {
  'use strict';
  const $ = id => document.getElementById(id);
  const state = { data: null, legs: [], configured: false, dirty: false, busy: false, polling: false, tsv: null, fileLoading: false, lastSuccess: 0, revision: 0, manifest: null, liveOrder: ['temperature', '__chart__', 'salinity', 'fluorescence', 'oxygen', 'transmission'], liveVars: ['temperature', 'salinity'], flowVars: null, flowSpan: null, flowMethod: null, flowTargetCount: null, flowSpacing: null, flowHistory: null, flowReplay: null, flowReplayConfig: null, flowRequest: 0, flowLoading: false, depthScale: 'linear' };
  try { const saved = JSON.parse(localStorage.getItem('uw:livesample-view')); if (saved) for (const key of ['liveOrder', 'liveVars', 'flowVars', 'flowSpan', 'flowMethod', 'flowTargetCount', 'flowSpacing', 'depthScale']) if (saved[key] !== undefined) state[key] = saved[key]; } catch (_) {}
  const methods = {
    coverage: 'Fill gaps in the sampled water properties, while spreading the proposed targets across distinct water types.',
    rarity: 'Prioritize water properties that are uncommon in the selected reference casts.',
    gradient: 'Prioritize sharp changes in water properties along the downcast.',
    surprise: 'Prioritize unexpected water properties using the existing surprise scoring approach.',
    hybrid: 'Balance 60% coverage, 25% rarity, and 15% vertical gradient in the sampling priority.'
  };
  const names = { temperature: 'Temperature (°C)', salinity: 'Practical salinity', theta: 'Potential temperature (°C)', pressure: 'Pressure (dbar)' };
  const featureNames = { Temperature: 'Temperature (°C)', Salinity: 'Practical salinity', Fluorescence: 'Fluorescence (µg/L)', Oxygen: 'Oxygen (µmol/L)', Transmission: 'Transmission (%)' };
  const preparing = data => /^(Updating sampling model|Preparing selected casts)/.test(data.status || '');
  const skip = new Set(['pressure', 'depth', 'depth_m', 'prdm', 'prm', 'pr', 'p', 'depsm', 'scan', 'time_s', 'lat', 'lon', 'bottles', 'bottom_depth_m']);
  const num = (value, decimals = 1) => Number.isFinite(value) ? value.toFixed(decimals) : '—';
  const esc = value => String(value ?? '').replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  const describe = value => typeof value === 'string' ? value : JSON.stringify(value);
  const colors = () => {
    const css = getComputedStyle(document.documentElement);
    const get = key => css.getPropertyValue(key).trim();
    return { bg: get('--plot-bg'), fg: get('--plot-fg'), grid: get('--plot-grid'), accent: get('--accent'), amber: get('--accent-2'), marker: get('--marker'), muted: get('--muted'), palette: get('--palette').split(/\s+/) };
  };
  function layout(extra = {}) {
    const c = colors();
    return { paper_bgcolor: c.bg, plot_bgcolor: c.bg, font: { color: c.fg, size: 12 }, margin: { l: 62, r: 24, t: 25, b: 58 }, hovermode: 'closest', legend: { orientation: 'h', y: 1.06 }, ...extra };
  }
  const plotConfig = { responsive: true, displaylogo: false, scrollZoom: false, modeBarButtonsToRemove: ['lasso2d', 'select2d'] };
  function emptyPlot(id, message) {
    $(`${id}-plot`).hidden = true;
    $(`${id}-empty`).hidden = false;
    $(`${id}-empty`).textContent = message;
  }
  function plot(id, traces, spec, options = {}) {
    if (!window.Plotly) { emptyPlot(id, 'Plotly is unavailable. Check that the dashboard’s local plotting assets are installed.'); return; }
    $(`${id}-empty`).hidden = true;
    $(`${id}-plot`).hidden = false;
    Plotly.react($(`${id}-plot`), traces, spec, { ...plotConfig, ...options });
  }
  async function request(method = 'GET', body) {
    const controller = new AbortController();
    const timer = setTimeout(() => controller.abort(), 15000);
    try {
      const response = await fetch(`api/livesample${method === 'GET' ? `?t=${Date.now()}` : ''}`, { method, signal: controller.signal, cache: 'no-store', ...(body ? { headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) } : {}) });
      const data = await response.json();
      if (!response.ok) throw new Error(data.error || `Request failed (${response.status})`);
      return data;
    } finally { clearTimeout(timer); }
  }
  function error(message) { $('request-error').hidden = !message; $('request-error').textContent = message; }
  function markDirty() { state.dirty = true; $('save-status').textContent = 'Unapplied changes'; }
  function selectedLegs() { return [...$('leglist').querySelectorAll('input:checked')].map(input => input.value); }
  function legSummary() {
    const selected = selectedLegs();
    $('legsummary').textContent = !selected.length ? 'No legs selected' : selected.length === state.legs.length ? `All ${selected.length} legs` : `${selected.length} of ${state.legs.length} legs`;
  }
  function showLegs(selected) {
    $('leglist').replaceChildren(...state.legs.map(leg => {
      const li = document.createElement('li'), label = document.createElement('label'), input = document.createElement('input'), text = document.createElement('span');
      input.type = 'checkbox'; input.value = leg.id; input.checked = selected.includes(leg.id);
      text.textContent = leg.label || leg.id; text.className = 'name';
      input.addEventListener('change', () => { legSummary(); markDirty(); });
      label.append(input, text); li.append(label); return li;
    }));
    legSummary();
  }
  function configure(config) {
    if (!config || (state.dirty && state.configured)) return;
    if (!state.configured) state.dirty = false;
    const selected = config.legs || [];
    const known = new Set(state.legs.map(leg => leg.id));
    for (const id of selected) if (!known.has(id)) state.legs.push({ id, label: id });
    showLegs(selected);
    $('algorithm').value = config.algorithm || 'coverage';
    $('target-count').value = config.count ?? 6;
    $('min-spacing').value = config.min_spacing ?? 10;
    $('telegram').checked = Boolean(config.telegram);
    $('method-description').textContent = methods[$('algorithm').value] || '';
    if (state.flowMethod === null) state.flowMethod = config.algorithm || 'coverage';
    if (state.flowTargetCount === null) state.flowTargetCount = config.count ?? 6;
    if (state.flowSpacing === null) state.flowSpacing = 15;
    $('flow-method').value = state.flowMethod;
    $('flow-target-count').value = state.flowTargetCount;
    $('flow-spacing').value = state.flowSpacing;
    settingControls().forEach(control => { control.disabled = state.busy; });
    $('save-status').textContent = 'Settings saved on this server';
    state.configured = true;
  }
  function settingControls() { return [...$('settings-form').querySelectorAll('input, select, button'), ...['algorithm', 'target-count', 'min-spacing', 'apply-chart-settings'].map($)]; }
  function saveView() { try { localStorage.setItem('uw:livesample-view', JSON.stringify({ liveOrder: state.liveOrder, liveVars: state.liveVars, flowVars: state.flowVars, flowSpan: state.flowSpan, flowMethod: state.flowMethod, flowTargetCount: state.flowTargetCount, flowSpacing: state.flowSpacing, depthScale: state.depthScale })); } catch (_) {} }
  function resetPlot(id) {
    const el = $(`${id}-plot`);
    if (el.hidden || !window.Plotly) return;
    const reset = {};
    for (const key of Object.keys(el.layout || {})) if (/^[xy]axis\d*$/.test(key)) reset[`${key}.autorange`] = key === 'yaxis' && id === 'live' ? 'reversed' : true;
    Plotly.relayout(el, reset);
  }
  function drawLiveControls(live, cast) {
    const columns = (cast?.columns || live?.columns || []).filter(key => !skip.has(key.toLowerCase()) && key !== cast?.pressure_col);
    const defaults = ['temperature', 'salinity', 'fluorescence', 'oxygen', 'transmission'];
    const keys = [...new Set([...defaults, ...columns])];
    state.liveOrder = [...state.liveOrder.filter(key => key === '__chart__' || keys.includes(key)), ...keys.filter(key => !state.liveOrder.includes(key))];
    if (!state.liveOrder.includes('__chart__')) state.liveOrder.splice(1, 0, '__chart__');
    const enabled = key => (cast?.cols?.[key] || []).some(Number.isFinite);
    const redraw = () => { saveView(); renderLive(state.data || {}, state.data?.recommendations || []); };
    const move = (from, to) => { if (to < 0 || to >= state.liveOrder.length || from === to) return; const [key] = state.liveOrder.splice(from, 1); state.liveOrder.splice(to, 0, key); redraw(); };
    const signature = JSON.stringify([state.liveOrder, state.liveVars, keys.map(enabled)]);
    if ($('live-parameters').dataset.signature === signature) return;
    $('live-parameters').dataset.signature = signature;
    $('live-parameters').replaceChildren(...state.liveOrder.map((key, i) => {
      const row = document.createElement('div'); row.className = `parameter ${key === '__chart__' ? 'chart-divider' : ''}`; row.draggable = true; row.dataset.key = key;
      const chip = document.createElement('button'); chip.type = 'button'; chip.className = key === '__chart__' ? 'chip chart-chip' : `chip singlevar ${state.liveVars.includes(key) ? 'on' : ''}`;
      chip.textContent = key === '__chart__' ? 'Chart' : (names[key] || key.replaceAll('_', ' '));
      if (key !== '__chart__') {
        chip.disabled = !enabled(key); chip.classList.toggle('unavailable', !enabled(key)); chip.setAttribute('aria-pressed', String(state.liveVars.includes(key)));
        chip.title = enabled(key) ? `${state.liveVars.includes(key) ? 'Hide' : 'Show'} ${key} axis` : 'No observations available for this parameter';
        const tag = document.createElement('small'); tag.textContent = enabled(key) ? state.liveVars.includes(key) ? 'On' : 'Off' : 'No data'; chip.append(tag);
        chip.addEventListener('click', () => { state.liveVars = state.liveVars.includes(key) ? state.liveVars.filter(v => v !== key) : [...state.liveVars, key]; redraw(); });
      } else chip.title = 'Drag Chart to divide upper and lower parameter axes';
      row.append(chip);
      for (const [delta, text] of [[-1, '▲'], [1, '▼']]) {
        const button = document.createElement('button'); button.type = 'button'; button.className = 'nudge'; button.textContent = text; button.disabled = delta < 0 ? i === 0 : i === state.liveOrder.length - 1; button.setAttribute('aria-label', `Move ${key === '__chart__' ? 'Chart' : key} ${delta < 0 ? 'up' : 'down'}`); button.addEventListener('click', () => move(i, i + delta)); row.append(button);
      }
      row.addEventListener('dragstart', event => event.dataTransfer.setData('text/plain', String(i)));
      row.addEventListener('dragover', event => event.preventDefault());
      row.addEventListener('drop', event => { event.preventDefault(); const from = Number(event.dataTransfer.getData('text/plain')); if (Number.isInteger(from)) move(from, i); });
      return row;
    }));
  }
  function depthFrom(pressure, latitude = 70) {
    const x = Math.sin(latitude * Math.PI / 180) ** 2;
    const g = 9.780318 * (1 + (5.2788e-3 + 2.36e-5 * x) * x) + 1.092e-6 * pressure;
    return (((-1.82e-15 * pressure + 2.279e-10) * pressure - 2.2512e-5) * pressure + 9.72659) * pressure / g;
  }
  const depthPosition = depth => depth == null ? null : state.depthScale === 'sqrt' ? Math.sqrt(Math.max(0, depth)) : depth;
  function renderLive(data, recommendations) {
    const live = data.live, rawCast = live?.current || live?.last;
    drawLiveControls(live, rawCast);
    const segment = data.cast_segment || {}, start = segment.soak_detected ? segment.start_index : 0;
    const cast = rawCast && start ? { ...rawCast, t: (rawCast.t || []).slice(start), n: Math.max(0, (rawCast.n || rawCast.t?.length || 0) - start), cols: Object.fromEntries(Object.entries(rawCast.cols || {}).map(([key, values]) => [key, values.slice(start)])) } : rawCast;
    $('live-depth-scale').setAttribute('aria-pressed', String(state.depthScale === 'sqrt'));
    $('live-depth-scale').textContent = state.depthScale === 'sqrt' ? '⇅ Depth: compressed' : '⇅ Depth: linear';
    if (!live) { $('live-status').textContent = 'Live feed unavailable'; emptyPlot('live', 'The service has not returned a live CTD feed.'); return; }
    const age = live.last_packet_age_s;
    const fresh = live.tcp_state === 'connected' && Number.isFinite(age) && age < 10;
    const source = live.active || live.tcp;
    const feed = !live.tcp ? 'OFF · no Seasave source configured' : fresh ? `LIVE · ${source}` : Number.isFinite(age) ? `STALE · last scan ${num(age, 0)} s ago · ${source}` : `Waiting · ${live.tcp_state || 'connecting'} · ${source}`;
    const direction = { down: '↓ downcast', up: '↑ upcast', hold: 'holding' }[rawCast?.direction] || '';
    $('live-status').textContent = [feed, rawCast ? live.current ? 'in the water' : 'last completed cast' : '', segment.soak_pending ? 'soak cycle' : segment.soak_detected ? 'soak excluded' : '', direction, cast ? `${(cast.n || 0).toLocaleString()} cast scans` : ''].filter(Boolean).join(' · ');
    if (segment.soak_pending) { emptyPlot('live', 'Soak cycle excluded. Waiting for the surface return and real downcast.'); return; }
    if (!cast?.cols) { emptyPlot('live', live.no_pressure ? 'The Seasave feed needs package pressure or depth to plot a cast.' : 'The live profile appears when the CTD enters the water. Configure Seasave on the main Casts page.'); return; }
    const latitude = state.manifest?.latest?.lat ?? 70;
    const vertical = (cast.cols[cast.pressure_col || live.pressure_col] || []).map(p => p == null ? null : cast.depth_like ? p : depthFrom(p, latitude));
    const chosen = state.liveOrder.filter(key => state.liveVars.includes(key) && cast.cols[key]?.some(Number.isFinite));
    if (!vertical.length || !chosen.length) { emptyPlot('live', 'Select an available parameter to plot the live cast.'); return; }
    let bottom = 0;
    for (let i = 0; i < vertical.length; i++) if (Number.isFinite(vertical[i]) && vertical[i] > (vertical[bottom] ?? -Infinity)) bottom = i;
    const c = colors(), upper = state.liveOrder.slice(0, state.liveOrder.indexOf('__chart__'));
    const nt = chosen.filter(key => upper.includes(key)).length, nb = chosen.length - nt;
    const extra = Math.max(0, nt - 1) + Math.max(0, nb - 1), height = 590 + extra * 48;
    $('live-plot').style.height = `${height}px`;
    const step = 48 / (height - 104), y0 = step * Math.max(0, nb - 1), y1 = 1 - step * Math.max(0, nt - 1);
    const spec = layout({ height, showlegend: false, margin: { l: 60, r: 18, t: 52, b: 52 }, uirevision: `cast:${cast.started}:${chosen.join()}:${upper.join()}:${state.depthScale}`, dragmode: 'pan', yaxis: { title: { text: 'Depth (m)' }, domain: [y0, y1], autorange: 'reversed', gridcolor: c.grid, zeroline: false } });
    if (state.depthScale === 'sqrt') {
      const maximum = Math.max(1, ...vertical.filter(Number.isFinite)) * 1.04;
      const ticks = [0,5,10,20,30,50,75,100,150,200,300,400,500,750,1000,1500,2000,3000,4000,5000].filter(d => d <= maximum);
      spec.yaxis.tickvals = ticks.map(depthPosition); spec.yaxis.ticktext = ticks.map(String);
    }
    const traces = [];
    chosen.forEach((key, i) => {
      const values = cast.cols[key], color = c.palette[i % c.palette.length], axis = i ? `x${i + 1}` : 'x', axisKey = i ? `xaxis${i + 1}` : 'xaxis';
      const below = !upper.includes(key), index = below ? i - nt : nt - 1 - i;
      const field = (live.columns || []).indexOf(key), title = names[key] || live.fields?.[field] || key.replaceAll('_', ' ');
      spec[axisKey] = { title: { text: title, font: { color, size: 12 }, standoff: 2 }, tickfont: { color, size: 11 }, ticks: 'outside', ticklen: 3, tickcolor: color, showline: true, linecolor: color, linewidth: 1.5, showgrid: i === 0, gridcolor: c.grid, zeroline: false, side: below ? 'bottom' : 'top', ...(i ? { overlaying: 'x' } : {}), anchor: index === 0 ? 'y' : 'free', ...(index ? { position: below ? y0 - step * index : y1 + step * index } : {}) };
      const segment = (from, to, up) => traces.push({ x: values.slice(from, to), y: vertical.slice(from, to).map(depthPosition), customdata: vertical.slice(from, to), xaxis: axis, type: 'scatter', mode: 'lines', name: title, line: { color, width: up ? 1.2 : 1.8, dash: up ? 'dot' : 'solid' }, connectgaps: false, hovertemplate: `${esc(title)}: %{x:.3f}<br>Depth: %{customdata:.1f} m<extra>${up ? 'Upcast' : 'Downcast'}</extra>` });
      segment(0, bottom + 1, false);
      if (bottom < vertical.length - 1) segment(bottom, vertical.length, true);
    });
    const last = vertical.length - 1;
    if (Number.isFinite(vertical[last])) traces.push({ x: [cast.cols[chosen[0]][last]], y: [depthPosition(vertical[last])], type: 'scatter', mode: 'markers', name: live.current ? 'Latest scan' : 'Last scan', marker: { color: c.amber, size: 10, symbol: 'diamond' }, hoverinfo: 'skip' });
    spec.shapes = recommendations.map(r => ({ ...r, at: r.depth_m ?? (cast.depth_like ? r.position : Number.isFinite(r.pressure) ? depthFrom(r.pressure, latitude) : null) })).filter(r => Number.isFinite(r.at)).map(r => ({ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: depthPosition(r.at), y1: depthPosition(r.at), line: { color: c.amber, width: 1.5, dash: 'dot' } }));
    plot('live', traces, spec, { scrollZoom: true });
  }
  function renderHistogram(id, distribution, targets) {
    const baseline = (distribution?.baseline_values || []).filter(Number.isFinite);
    const scored = (targets || []).filter(target => Number.isFinite(target.z));
    if (!baseline.length || !scored.length) { emptyPlot(id, distribution?.warning || 'Waiting for target novelty scores and archived bottles.'); return; }
    const c = colors();
    const annotations = scored.map((target, index) => ({
      x: target.z, y: 1 - (index % 3) * .16, xref: 'x', yref: 'paper', showarrow: true,
      arrowhead: 0, ax: 0, ay: -22, arrowcolor: c.amber, bgcolor: c.bg, bordercolor: c.amber,
      borderpad: 3, font: { size: 10, color: c.fg },
      text: `<b>#${target.rank ?? index + 1}</b> · score ${num(target.score, 2)} · z ${num(target.z, 2)}`
    }));
    const spec = layout({ height: 310, margin: { l: 62, r: 24, t: 52, b: 60 }, showlegend: false,
      dragmode: 'pan', bargap: .04, xaxis: { title: { text: 'Novelty z-score vs previous bottles' }, gridcolor: c.grid, zerolinecolor: c.grid },
      yaxis: { title: { text: 'Previous bottles' }, gridcolor: c.grid, rangemode: 'tozero' }, annotations,
      shapes: scored.map(target => ({ type: 'line', x0: target.z, x1: target.z, yref: 'paper', y0: 0, y1: 1, line: { color: c.amber, width: 1.5, dash: 'dot' } }))
    });
    plot(id, [{ type: 'histogram', x: baseline, nbinsx: Math.max(10, Math.min(40, Math.round(Math.sqrt(baseline.length) * 2))), marker: { color: c.accent, opacity: .72 }, hovertemplate: 'Novelty z: %{x:.2f}<br>Previous bottles: %{y}<extra></extra>' }], spec, { scrollZoom: true });
  }
  function flowVariables() { return (state.manifest?.variables || []).filter(v => v.tsg || /^Surprise/.test(v.name)); }
  function flowConfig() { return { legs: state.data?.config?.legs || [], window: state.flowSpan, algorithm: state.flowMethod, count: state.flowTargetCount, spacing: state.flowSpacing }; }
  async function loadFlow(force = false) {
    if (!state.manifest?.windows?.length) { emptyPlot('flow', 'No published flow-through windows are available.'); return; }
    if (state.flowLoading && !force) return;
    state.flowLoading = true;
    const windowSpec = state.manifest.windows.find(w => w.label === state.flowSpan) || state.manifest.windows.find(w => w.label === state.manifest.default_window) || state.manifest.windows[0];
    state.flowSpan = windowSpec.label;
    const requestConfig = JSON.stringify(flowConfig());
    const requestId = ++state.flowRequest;
    $('flow-data-status').textContent = 'Loading selected time span…';
    const controller = new AbortController(), timeout = setTimeout(() => controller.abort(), 15000);
    try {
      const responses = await Promise.allSettled([
        fetch(`${windowSpec.file}?t=${Date.now()}`, { signal: controller.signal, cache: 'no-store' }).then(async r => { if (!r.ok) throw new Error(`History unavailable (${r.status})`); return r.json(); }),
        fetch(`api/livesample/flow?window=${encodeURIComponent(windowSpec.label)}&algorithm=${encodeURIComponent(state.flowMethod)}&count=${encodeURIComponent(state.flowTargetCount)}&spacing=${encodeURIComponent(state.flowSpacing)}`, { signal: controller.signal, cache: 'no-store' }).then(async r => { if (!r.ok) throw new Error(`Historical picks unavailable (${r.status})`); return r.json(); })
      ]);
      if (requestId !== state.flowRequest) return;
      if (responses[0].status === 'rejected') throw responses[0].reason;
      if (requestConfig !== JSON.stringify(flowConfig())) { state.flowLoading = false; loadFlow(true); return; }
      state.flowHistory = responses[0].value;
      state.flowReplay = responses[1].status === 'fulfilled' ? responses[1].value : { targets: [], error: responses[1].reason.message };
      state.flowReplayConfig = requestConfig;
      renderFlow();
    } catch (e) { if (requestId === state.flowRequest) { $('flow-data-status').textContent = `Flow history unavailable: ${e.message}`; emptyPlot('flow', 'Could not load this time window. Retrying automatically.'); emptyPlot('flow-hist', 'Could not load target scores for this time window.'); } }
    finally { clearTimeout(timeout); if (requestId === state.flowRequest) state.flowLoading = false; }
  }
  function renderFlow() {
    const history = state.flowHistory;
    if (!history?.t?.length) { emptyPlot('flow', 'No flow-through observations in this time window.'); emptyPlot('flow-hist', 'No target scores in this time window.'); return; }
    const selectedLegs = new Set(state.data?.config?.legs || []), manifestLegs = state.manifest.legs || [];
    const indices = history.t.map((time, i) => ({ time, i })).filter(({ i }) => !history.leg || !selectedLegs.size || selectedLegs.has(manifestLegs.find((leg, index) => (leg.index ?? index) === history.leg[i])?.id || history.leg[i]));
    const vars = flowVariables(), c = colors();
    const available = key => indices.some(({ i }) => Number.isFinite(history.vars?.[key]?.[i]));
    if (state.flowVars === null) state.flowVars = vars.filter(v => /^(SST|Salinity)/.test(v.name) && available(v.name)).map(v => v.name);
    $('flow-chips').replaceChildren(...vars.map(v => {
      const chip = document.createElement('button'); chip.type = 'button'; chip.className = `chip ${state.flowVars.includes(v.name) ? 'on' : ''}`; chip.disabled = !available(v.name); chip.classList.toggle('unavailable', !available(v.name)); chip.setAttribute('aria-pressed', String(state.flowVars.includes(v.name))); chip.dataset.key = v.name;
      const arrow = document.createElement('span'); arrow.className = 'chart-state'; arrow.textContent = state.flowVars.includes(v.name) ? '▲' : '▼';
      const label = document.createElement('span'); label.className = 'cname'; label.textContent = v.name;
      const value = document.createElement('b'); value.textContent = num(indices.map(({ i }) => history.vars?.[v.name]?.[i]).reverse().find(Number.isFinite), 2);
      chip.append(arrow, label, value); chip.title = chip.disabled ? 'No observations in this window' : `Toggle ${v.name}`;
      chip.addEventListener('click', () => { state.flowVars = state.flowVars.includes(v.name) ? state.flowVars.filter(key => key !== v.name) : [...state.flowVars, v.name]; saveView(); renderFlow(); });
      return chip;
    }));
    const chosen = vars.filter(v => state.flowVars.includes(v.name) && available(v.name));
    const time = indices.map(({ time }) => new Date(time).toISOString());
    if (!time.length || !chosen.length) { emptyPlot('flow', time.length ? 'Choose a Lab parameter to display its time series.' : 'No observations from the selected reference legs in this window.'); renderHistogram('flow-hist', state.flowReplay?.distribution, state.flowReplay?.targets || []); return; }
    const height = Math.max(370, chosen.length * 165 + 65), gap = Math.min(.035, .15 / chosen.length);
    $('flow-plot').style.height = `${height}px`;
    const spec = layout({ height, margin: { l: 115, r: 25, t: 20, b: 55 }, dragmode: 'pan', showlegend: false, uirevision: `flow:${state.flowSpan}:${chosen.map(v => v.name).join()}:${[...selectedLegs].join()}`, xaxis: { type: 'date', title: { text: 'Time (UTC)' }, gridcolor: c.grid, zeroline: false } });
    const traces = chosen.map((v, i) => {
      const color = c.palette[i % c.palette.length], axis = i ? `y${i + 1}` : 'y', key = i ? `yaxis${i + 1}` : 'yaxis';
      const top = 1 - i / chosen.length, bottom = 1 - (i + 1) / chosen.length + (i < chosen.length - 1 ? gap : 0);
      spec[key] = { title: { text: v.name.replace(' (', '<br>('), font: { color, size: 11 }, standoff: 5 }, tickfont: { color, size: 10 }, domain: [bottom, top], showline: true, linecolor: color, showgrid: true, gridcolor: c.grid, zeroline: false, anchor: 'x' };
      return { type: 'scatter', mode: 'lines', name: v.name, x: time, y: indices.map(({ i }) => history.vars[v.name][i]), yaxis: axis, line: { color, width: 1.6 }, connectgaps: false, hovertemplate: `${esc(v.name)}: %{y:.3f}<br>%{x|%Y-%m-%d %H:%M:%S} UTC<extra></extra>` };
    });
    const first = Date.parse(time[0]), last = Date.parse(time.at(-1));
    const replayCurrent = state.flowReplayConfig === JSON.stringify(flowConfig()) && state.flowReplay?.window === state.flowSpan;
    const targets = (replayCurrent ? state.flowReplay?.targets || [] : []).filter(t => Date.parse(t.time) >= first && Date.parse(t.time) <= last);
    spec.shapes = targets.map(t => ({ type: 'line', x0: t.time, x1: t.time, yref: 'paper', y0: 0, y1: 1, line: { color: c.amber, width: 1.5, dash: 'dot' } }));
    const requested = state.flowReplay?.requested_count ?? state.flowTargetCount;
    const warning = (state.flowReplay?.warnings || []).join(' · ');
    $('flow-data-status').textContent = `${time.length.toLocaleString()} observations · ${targets.length} of ${requested} ${state.flowReplay?.algorithm || state.flowMethod} historical method picks${replayCurrent ? '' : ' · refreshing picks'}${warning ? ` · ${warning}` : ''}${state.flowReplay?.error ? ` · ${state.flowReplay.error}` : ''}`;
    plot('flow', traces, spec, { scrollZoom: true });
    renderHistogram('flow-hist', replayCurrent ? state.flowReplay?.distribution : null, targets);
  }
  function renderTargets(data) {
    const recs = data.recommendations || [];
    $('target-summary').textContent = recs.length ? `${recs.length} targets, shown in collection order from deepest to shallowest. ${data.phase === 'up' ? 'Remaining targets from the observed downcast plan.' : 'Priorities update as the downcast develops.'}` : data.phase ? `No targets · ${data.phase}` : 'Waiting for enough live observations and a reference model.';
    $('recommendations').replaceChildren(...[...recs].sort((a, b) => b.pressure - a.pressure).map((r, i) => {
      const li = document.createElement('li');
      const depth = Number.isFinite(r.depth_m) ? `${num(r.depth_m)} m` : `${num(r.pressure)} dbar`;
      li.innerHTML = `<span class="sample-rank">${String(i + 1).padStart(2, '0')}</span><span class="sample-pressure">${esc(depth)}${Number.isFinite(r.depth_m) ? `<small>${num(r.pressure)} dbar</small>` : ''}</span><span class="sample-reason">${esc(r.reason || 'Priority water type')}<span class="sample-score">score ${num(r.score, 3)}</span></span>`;
      return li;
    }));
    const flow = data.flow || {};
    $('flow-status').textContent = [typeof flow.recommendation === 'string' ? flow.recommendation : flow.recommendation === true ? 'Sample recommended now' : flow.status || 'Waiting for underway data', flow.reason].filter(Boolean).join(' · ');
    $('flow-status').closest('.sample-flow').classList.toggle('active', Boolean(flow.recommendation));
    $('flow-values').textContent = [Number.isFinite(flow.temperature) ? `${num(flow.temperature, 3)} °C` : '', Number.isFinite(flow.salinity) ? `salinity ${num(flow.salinity, 3)}` : '', Number.isFinite(flow.score) ? `score ${num(flow.score, 3)}` : '', flow.time ? `observed ${flow.time}` : ''].filter(Boolean).join(' · ');
    renderLive(data, recs);
    renderHistogram('ctd-hist', data.distributions?.ctd, recs);
  }
  function embeddingVariables(data) {
    const vars = [{ key: 'leg', name: 'Leg' }, { key: 'pressure', name: 'Pressure', unit: 'dbar' }, ...(data.variables || [])];
    const unique = [...new Map(vars.map(v => [v.key, v])).values()];
    const signature = JSON.stringify(unique), current = $('embedding-color').value;
    if ($('embedding-color').dataset.signature !== signature) {
      $('embedding-color').replaceChildren(...unique.map(v => { const option = document.createElement('option'); option.value = v.key; option.textContent = `${v.name || v.key}${v.unit ? ` (${v.unit})` : ''}`; return option; }));
      $('embedding-color').value = unique.some(v => v.key === current) ? current : 'leg';
      $('embedding-color').dataset.signature = signature;
    }
    return unique;
  }
  function renderEmbedding(name, data) {
    const variables = embeddingVariables(data);
    if (preparing(data) || data.error) { emptyPlot(name, data.error || 'Computing the selected reference model…'); return; }
    const embedding = data.embeddings?.[name];
    const showBottles = $('show-bottles').checked;
    const points = (embedding?.points || []).filter(p => !p.collected || p.reference || p.sampled || showBottles);
    if (!points.length) { emptyPlot(name, embedding?.error || 'No reference embedding is available yet.'); return; }
    const c = colors(), key = $('embedding-color').value, variable = variables.find(v => v.key === key), continuous = key !== 'leg';
    const propertyValue = p => key === 'pressure' ? p.pressure : p.properties?.[key];
    const labels = points.map(p => {
      const temperature = p.Temperature ?? p.temperature, salinity = p.Salinity ?? p.salinity, value = propertyValue(p);
      return `${p.source === 'flow' ? 'Flow-through sample' : `Leg ${esc(p.leg)} · cast ${esc(p.cast)}`}${Number.isFinite(p.pressure) ? `<br>${num(p.pressure)} dbar` : ''}${Number.isFinite(temperature) ? `<br>Temperature: ${num(temperature, 3)} °C` : ''}${Number.isFinite(salinity) ? `<br>Practical salinity: ${num(salinity, 3)}` : ''}${continuous ? `<br>${esc(variable?.name || key)}: ${Number.isFinite(value) ? `${num(value, 3)} ${esc(variable?.unit || '')}` : 'not measured'}` : ''}${p.collected ? `<br>Collected bottle${(p.bottles || []).length === 1 ? '' : 's'}: ${esc((p.bottles || []).join(', '))}` : ''}${p.sampled ? '<br>Uploaded sample inventory' : ''}`;
    });
    const indexed = points.map((p, index) => ({ ...p, index })), traces = [];
    const scatter = (selected, title, marker) => ({ x: selected.map(p => p.x), y: selected.map(p => p.y), text: selected.map(p => labels[p.index]), type: 'scattergl', mode: 'markers', name: title, marker, hovertemplate: '%{text}<extra></extra>' });
    if (continuous) {
      const measured = indexed.filter(p => Number.isFinite(propertyValue(p))), missing = indexed.filter(p => !Number.isFinite(propertyValue(p)));
      if (missing.length) traces.push(scatter(missing, 'Not measured', { size: 5, opacity: .35, color: c.muted }));
      if (measured.length) traces.push(scatter(measured, esc(variable?.name || key), { size: 5, opacity: .7, color: measured.map(propertyValue), colorscale: 'Viridis', colorbar: { title: { text: esc(variable?.unit || variable?.name || key) }, orientation: 'h', x: .5, xanchor: 'center', y: -.08, len: .85, thickness: 10 } }));
    } else {
      [...new Set(points.map(p => p.leg))].forEach((leg, i) => traces.push(scatter(indexed.filter(p => p.leg === leg), esc(state.legs.find(l => l.id === leg)?.label || leg || 'Flow-through'), { size: 5, opacity: .6, color: c.palette[i % c.palette.length] })));
    }
    if (showBottles) {
      const bottles = indexed.filter(p => p.collected);
      if (bottles.length) traces.push(scatter(bottles, 'Collected bottles', { size: 9, symbol: 'circle-open', color: c.marker, line: { width: 2 } }));
    }
    const sampled = indexed.filter(p => p.sampled);
    if (sampled.length) traces.push(scatter(sampled, 'Uploaded sample inventory', { size: 11, symbol: 'diamond-open', color: c.amber, line: { width: 2 } }));
    const axisName = name === 'tsne' ? 't-SNE' : 'UMAP';
    plot(name, traces, layout({ margin: { l: 45, r: 45, t: 18, b: 72 }, dragmode: 'pan', legend: { orientation: 'h', y: continuous ? -.27 : -.13 }, uirevision: `${name}:${JSON.stringify(data.config?.legs)}:${embedding.points.length}`, xaxis: { title: { text: `${axisName} 1` }, zeroline: false, showgrid: false, showticklabels: false, constrain: 'range' }, yaxis: { title: { text: `${axisName} 2` }, zeroline: false, showgrid: false, showticklabels: false, scaleanchor: 'x', scaleratio: 1, constrain: 'range' } }), { scrollZoom: true });
  }
  function render(data) {
    state.data = data;
    configure(data.config);
    $('service-state').textContent = data.status || 'Connected';
    $('service-state').className = `sample-badge ${data.error ? 'bad' : data.status === 'ready' ? 'good' : ''}`;
    const features = data.features || [];
    $('feature-list').textContent = [features.length ? `Embedding properties: ${features.map(f => typeof f === 'string' ? featureNames[f] || f : [f.label || f.name, f.unit].filter(Boolean).join(' ')).join(', ')}.` : '', data.scoring_features?.length ? `Live scoring properties: ${data.scoring_features.join(', ')}.` : ''].filter(Boolean).join(' ');
    const warnings = [...(data.warnings || []), ...(data.error ? [data.error] : [])];
    $('model-warnings').hidden = !warnings.length;
    $('model-warnings').textContent = warnings.map(describe).join('\n');
    const unmatched = data.unmatched || [];
    $('unmatched-details').hidden = !unmatched.length;
    $('unmatched-summary').textContent = `${unmatched.length} unmatched sample rows`;
    $('unmatched-list').replaceChildren(...unmatched.map(row => { const li = document.createElement('li'); li.textContent = describe(row); return li; }));
    const telegram = data.telegram || {};
    $('telegram-status').textContent = telegram.error || telegram.status || (data.config?.telegram ? 'Notifications enabled in saved settings.' : 'Notifications off.');
    renderTargets(data);
    renderEmbedding('tsne', data);
    renderEmbedding('umap', data);
    renderFlow();
  }
  async function poll() {
    if (state.polling || state.busy) return;
    state.polling = true;
    const revision = state.revision;
    try {
      const data = await request();
      if (!state.busy && revision === state.revision) { render(data); state.lastSuccess = Date.now(); if (state.manifest) loadFlow(); $('updated').textContent = `Updated ${new Date().toLocaleTimeString()} · every 10 s`; error(''); }
    } catch (e) {
      if (revision !== state.revision) return;
      error(e.name === 'AbortError' ? 'Sampling service timed out. Retrying automatically.' : `Sampling service unavailable: ${e.message}. Retrying automatically.`);
      $('service-state').textContent = 'Disconnected'; $('service-state').className = 'sample-badge bad';
      $('updated').textContent = state.lastSuccess ? `Last successful update ${new Date(state.lastSuccess).toLocaleTimeString()}` : 'No successful update yet';
      $('target-summary').textContent = 'Service disconnected. Any targets shown below are from the last successful update.';
      $('live-status').textContent = 'DISCONNECTED · displayed profile is from the last successful update';
      $('flow-status').textContent = 'Service disconnected · flow-through recommendation is stale';
    } finally { state.polling = false; }
  }
  $('settings-form').addEventListener('submit', async event => {
    event.preventDefault();
    if (!state.configured || state.busy || state.fileLoading) return;
    const legs = selectedLegs();
    if (!legs.length) { error('Select at least one reference leg before applying settings.'); return; }
    state.busy = true; state.revision++; settingControls().forEach(control => { control.disabled = true; }); $('save-status').textContent = 'Applying…';
    const body = { legs, algorithm: $('algorithm').value, count: Number($('target-count').value), min_spacing: Number($('min-spacing').value), telegram: $('telegram').checked };
    if (state.tsv !== null) body.tsv = state.tsv;
    try {
      const data = await request('POST', body);
      state.dirty = false; state.tsv = null; $('samples-file').value = ''; $('file-status').textContent = body.tsv !== undefined ? 'Sample list saved. Upload another TSV to replace it.' : 'Saved samples retained. Upload a TSV to replace them.';
      error('');
      if (data.config) render(data);
      $('save-status').textContent = 'Settings applied · model updates in the background';
    } catch (e) { error(`Could not apply settings: ${e.name === 'AbortError' ? 'request timed out; check the saved state before retrying' : e.message}`); $('save-status').textContent = 'Settings not confirmed'; }
    finally { state.busy = false; settingControls().forEach(control => { control.disabled = false; }); poll(); }
  });
  $('samples-file').addEventListener('change', async () => {
    const file = $('samples-file').files[0]; state.tsv = null;
    if (!file) { $('file-status').textContent = 'No new file selected; saved samples will be kept.'; return; }
    if (file.size > 2_000_000) { error('Please use a TSV smaller than 2 MB.'); $('samples-file').value = ''; return; }
    state.fileLoading = true; $('apply-settings').disabled = true;
    try { state.tsv = await file.text(); $('file-status').textContent = `${file.name} ready to replace saved samples when applied.`; markDirty(); error(''); }
    catch (e) { error(`Could not read the selected file: ${e.message}`); $('samples-file').value = ''; }
    finally { state.fileLoading = false; $('apply-settings').disabled = !state.configured || state.busy; }
  });
  ['algorithm', 'target-count', 'min-spacing', 'telegram'].forEach(id => $(id).addEventListener('change', () => { markDirty(); $('method-description').textContent = methods[$('algorithm').value] || ''; }));
  ['legall', 'legnone'].forEach(id => $(id).addEventListener('click', () => { $('leglist').querySelectorAll('input').forEach(input => { input.checked = id === 'legall'; }); legSummary(); markDirty(); }));
  $('embedding-color').addEventListener('change', () => { if (state.data) { renderEmbedding('tsne', state.data); renderEmbedding('umap', state.data); } });
  $('show-bottles').addEventListener('change', () => { if (state.data) { renderEmbedding('tsne', state.data); renderEmbedding('umap', state.data); } });
  $('live-depth-scale').addEventListener('click', () => { state.depthScale = state.depthScale === 'sqrt' ? 'linear' : 'sqrt'; saveView(); if (state.data) renderLive(state.data, state.data.recommendations || []); });
  $('live-reset').addEventListener('click', () => resetPlot('live'));
  $('flow-reset').addEventListener('click', () => resetPlot('flow'));
  $('embedding-reset').addEventListener('click', () => { resetPlot('tsne'); resetPlot('umap'); });
  $('flow-span').addEventListener('change', () => { state.flowSpan = $('flow-span').value; saveView(); loadFlow(true); });
  $('apply-flow-settings').addEventListener('click', () => {
    const count = Number($('flow-target-count').value), spacing = Number($('flow-spacing').value);
    if (!Number.isInteger(count) || count < 1 || count > 24 || !Number.isFinite(spacing) || spacing < 1 || spacing > 1440) { error('Flow targets must be 1–24 and spacing must be 1–1440 minutes.'); return; }
    state.flowMethod = $('flow-method').value; state.flowTargetCount = count; state.flowSpacing = spacing;
    saveView(); error(''); loadFlow(true);
  });
  document.addEventListener('click', event => { if (!$('legmenu').contains(event.target)) $('legmenu').open = false; });
  $('download-template').addEventListener('click', async () => {
    const legs = selectedLegs();
    if (!legs.length) { error('Select at least one leg to download its archived bottle template.'); return; }
    const button = $('download-template'); button.disabled = true;
    try {
      const params = new URLSearchParams(); legs.forEach(leg => params.append('leg', leg));
      const response = await fetch(`api/livesample/template?${params}`, { cache: 'no-store' });
      if (!response.ok) { const detail = await response.json(); throw new Error(detail.error || `Template unavailable (${response.status})`); }
      const text = await response.text();
      const url = URL.createObjectURL(new Blob([text], { type: 'text/tab-separated-values;charset=utf-8' }));
      const a = document.createElement('a'); a.href = url; a.download = 'selected-leg-bottles.tsv'; a.click(); setTimeout(() => URL.revokeObjectURL(url), 1000);
      error('');
    } catch (e) { error(`Could not download bottle template: ${e.message}`); }
    finally { button.disabled = false; }
  });
  async function start() {
    try {
      const controller = new AbortController(), timer = setTimeout(() => controller.abort(), 10000);
      try {
        const response = await fetch('data/manifest.json', { cache: 'no-store', signal: controller.signal });
        if (!response.ok) throw new Error(`HTTP ${response.status}`);
        const manifest = await response.json(); state.manifest = manifest; state.legs = manifest.legs || [];
        $('flow-span').replaceChildren(...(manifest.windows || []).map(w => { const option = document.createElement('option'); option.value = w.label; option.textContent = w.label === 'leg' ? 'This leg' : w.label; return option; }));
        if (!(manifest.windows || []).some(w => w.label === state.flowSpan)) state.flowSpan = manifest.default_window || manifest.windows?.[0]?.label;
        $('flow-span').value = state.flowSpan || '';
      } finally { clearTimeout(timer); }
    } catch (e) { error(`Leg manifest unavailable (${e.message}). Saved reference legs remain available.`); }
    await poll();
    setInterval(() => { if (!document.hidden) poll(); }, 10000);
    document.addEventListener('visibilitychange', () => { if (!document.hidden) poll(); });
  }
  settingControls().forEach(control => { control.disabled = true; });
  start();
})();
