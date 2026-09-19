/* Text-only UI translations. Catalog selection happens at build time. */
(() => {
  'use strict';
  const catalog = window.UW_UI_CATALOG || {sourceLocale:'en', locales:{en:{label:'English', messages:{}}}};
  const has = (o, k) => Object.prototype.hasOwnProperty.call(o, k);
  // Source-addressed authored UI messages use versioned catalog candidates too.
  // Never run this over article text, user messages, dataset keys or API values.
  const sourceKeys = new Map(Object.entries(catalog.locales[catalog.sourceLocale]?.messages || {})
    .filter(([key, value]) => key.startsWith('pages.') && typeof value === 'string')
    .map(([key, value]) => [value, key]));
  function text(source, values = {}) {
    return sourceKeys.has(source) ? t(sourceKeys.get(source), values) : String(source).replace(/\{([a-zA-Z][a-zA-Z0-9_]*)\}/g, (token, name) => has(values, name) ? String(values[name]) : token);
  }
  function html(source, values = {}) {
    // Escape translator text first; values are the original template's trusted
    // markup or already-escaped data, never translator-provided HTML.
    const escaped = text(source).replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
    return escaped.replace(/\{([a-zA-Z][a-zA-Z0-9_]*)\}/g, (token, name) => has(values, name) ? String(values[name]) : token);
  }
  // Measurement names remain immutable lookup keys; only their display changes.
  const variableKeys = {
    "SST (°C)": "variable.sst",
    "Salinity (PSU)": "variable.salinity",
    "Excess heat (°C)": "variable.excessHeat",
    "TSG line warming (°C)": "variable.tsgWarming",
    "TSG flow (V)": "variable.tsgFlow",
    "Fluorescence (µg/L)": "variable.fluorescence",
    "Oxygen (mL/L)": "variable.oxygen",
    "Short-wave radiation (W/m²)": "variable.solar",
    "Bottom depth (m)": "variable.bottomDepth",
    "Rosette depth (m)": "variable.rosetteDepth",
    "Rosette rate (m/s)": "variable.rosetteRate",
    "Cable length (m)": "variable.cableLength",
    "Cable rate (m/s)": "variable.cableRate",
    "Air temperature (°C)": "variable.airTemperature",
    "Relative humidity (%)": "variable.humidity",
    "Atmospheric pressure (hPa)": "variable.pressure",
    "True wind direction (°)": "variable.windDirection",
    "Relative wind speed (kn)": "variable.windSpeed",
    "Heading (°)": "variable.heading",
    "Ship speed (kn)": "variable.shipSpeed",
    "Sea state · 4σ heave (m)": "variable.seaState",
    "Roll & pitch RMS (°)": "variable.rollPitch",
    "Time elapsed (h)": "variable.elapsed",
    "Distance travelled (km)": "variable.distance",
    "Surprise (−log10 p)": "variable.surprise",
    "Surprise · 15 min": "variable.surprise0",
    "Surprise · 1 h": "variable.surprise1",
    "Surprise · 3 h": "variable.surprise2",
    "Surprise · 12 h": "variable.surprise3",
    "Surprise · 48 h": "variable.surprise4"
  };
  function variable(name) {
    if (!has(variableKeys, name)) return name;
    const result = t(variableKeys[name]);
    return result === variableKeys[name] ? name : result;
  }
  const supported = value => typeof value === 'string' && has(catalog.locales, value);
  let stored;
  try { stored = JSON.parse(localStorage.getItem('uw:locale')); } catch (_) {}
  const requested = new URLSearchParams(location.search).get('lang');
  let locale = supported(requested) ? requested : supported(stored) ? stored : catalog.sourceLocale;
  function t(key, values = {}) {
    const selected = catalog.locales[locale]?.messages || {}, fallback = catalog.locales[catalog.sourceLocale]?.messages || {};
    let message = has(selected, key) ? selected[key] : has(fallback, key) ? fallback[key] : undefined;
    if (message == null) return key;
    if (typeof message === 'object') {
      const category = new Intl.PluralRules(has(selected, key) ? locale : catalog.sourceLocale).select(Number(values.count));
      message = message[category] ?? message.other;
    }
    return message.replace(/\{([a-zA-Z][a-zA-Z0-9_]*)\}/g, (token, name) => has(values, name) ? String(values[name]) : token);
  }
  // Never inject translated HTML; attributes are explicitly allowlisted.
  function apply(root = document) {
    const bindings = [['data-i18n', null], ['data-i18n-title','title'], ['data-i18n-placeholder','placeholder'], ['data-i18n-aria-label','aria-label']];
    for (const [marker, attr] of bindings) {
      const nodes = [...root.querySelectorAll(`[${marker}]`)];
      if (root.nodeType === 1 && root.hasAttribute(marker)) nodes.unshift(root);
      for (const node of nodes) {
        const value = t(node.getAttribute(marker));
        if (attr) node.setAttribute(attr, value); else node.textContent = value;
        if (!attr || /^(BUTTON|INPUT|SELECT|TEXTAREA)$/.test(node.tagName)) {
          node.lang = has(catalog.locales[locale]?.messages || {}, node.getAttribute(marker)) ? locale : catalog.sourceLocale;
        }
      }
    }
    for (const picker of root.querySelectorAll('[data-locale-picker]')) picker.value = locale;
    for (const node of root.querySelectorAll('[data-i18n-locale]')) node.lang = locale;
  }
  function setLocale(value) {
    if (!supported(value)) return false;
    locale = value;
    try { localStorage.setItem('uw:locale', JSON.stringify(locale)); } catch (_) {}
    try { const url = new URL(location.href); url.searchParams.set('lang', locale); history.replaceState(null, '', url); } catch (_) {}
    document.documentElement.lang = locale;
    apply();
    window.dispatchEvent(new CustomEvent('uw:localechange', {detail:{locale}}));
    return true;
  }
  async function preserve(root, render) {
    // Repaint labels without losing a draft, selected files, focus or scroll.
    if (!root) return render();
    const selector = node => node.id ? '#' + CSS.escape(node.id) : node.name ? `${node.tagName.toLowerCase()}[name="${CSS.escape(node.name)}"]${/^(checkbox|radio)$/.test(node.type) ? `[value="${CSS.escape(node.value)}"]` : ''}` : null;
    const fields = [...root.querySelectorAll('input,textarea,select')].map(node => ({
      node, selector:selector(node), value:node.value, checked:node.checked,
      start:node.selectionStart, end:node.selectionEnd, focus:node === document.activeElement
    })).filter(x => x.selector);
    const details = [...root.querySelectorAll('details')].map((node,index) => ({id:node.id,index,open:node.open}));
    const scroll = [root,...root.querySelectorAll('[id]')].filter(node => node.scrollTop || node.scrollLeft)
      .map(node => ({node,id:node.id,top:node.scrollTop,left:node.scrollLeft}));
    await render();
    for (const state of fields) {
      let node = root.querySelector(state.selector); if (!node) continue;
      if (node.type === 'file') { if (node !== state.node) { node.replaceWith(state.node); node = state.node; } }
      else { node.value = state.value; node.checked = state.checked; }
      if (state.focus) { node.focus({preventScroll:true}); if (state.start != null) node.setSelectionRange?.(state.start,state.end); }
    }
    for (const state of details) {
      const node = state.id ? root.querySelector('#'+CSS.escape(state.id)) : root.querySelectorAll('details')[state.index];
      if (node) node.open = state.open;
    }
    for (const state of scroll) { const node = state.node.isConnected ? state.node : state.id ? root.querySelector('#'+CSS.escape(state.id)) : null; if (node) { node.scrollTop=state.top;node.scrollLeft=state.left; } }
  }
  function init() {
    for (const picker of document.querySelectorAll('[data-locale-picker]')) {
      picker.replaceChildren();
      for (const [code, data] of Object.entries(catalog.locales)) {
        const option = document.createElement('option'); option.value = code; option.textContent = data.label;
        picker.append(option);
      }
      picker.addEventListener('change', () => setLocale(picker.value));
    }
    document.documentElement.lang = locale;
    apply();
  }
  window.UWI18n = Object.freeze({t, text, html, preserve, variable, apply, setLocale, get locale() { return locale; }});
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init, {once:true}); else init();
})();
