/* Text-only UI translations. Catalog selection happens at build time. */
(() => {
  'use strict';
  const catalog = window.UW_UI_CATALOG || {sourceLocale:'en', locales:{en:{label:'English', messages:{}}}};
  const has = (o, k) => Object.prototype.hasOwnProperty.call(o, k);
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
  window.UWI18n = Object.freeze({t, apply, setLocale, get locale() { return locale; }});
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init, {once:true}); else init();
})();
