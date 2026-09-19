/* Cached Québec French translations; unknown text remains in its source language. */
(() => {
  'use strict';
  const base = new URL('.', document.currentScript.src);
  const key = 'underway:language';
  const normalize = text => String(text).replace(/\s+/g, ' ').trim();
  const dictionaries = new Map();
  let dictionary = Object.create(null), language = 'en', fetching = null, catalogVersion = '';
  const texts = new WeakMap(), attributes = new WeakMap();
  const skip = 'script,style,code,pre,textarea,canvas,option:not([value]),[contenteditable]:not([contenteditable="false"]),[translate="no"],.notranslate,[data-i18n-ignore],[data-language-select],.language-picker,.msg,.chat-message,.chat-msg,[data-name]';
  const attrs = ['title', 'aria-label', 'placeholder', 'alt'];
  try { if (localStorage.getItem(key) === 'fr-CA') language = 'fr-CA'; } catch (_) {}
  function t(source) {
    if (language !== 'fr-CA' || typeof source !== 'string') return source;
    const normalized = normalize(source);
    let translated = dictionary[normalized];
    if (typeof translated !== 'string') {
      const count = normalized.match(/^(\d+) (casts?|stations?|bottles?|photos?)( selected)?$/i);
      if (!count) return source;
      const plural = Number(count[1]) !== 1;
      const noun = count[2].toLowerCase().replace(/s$/, '');
      const nouns = {cast: 'profil', station: 'station', bottle: 'bouteille', photo: 'photo'};
      const feminine = noun !== 'cast';
      translated = `${count[1]} ${nouns[noun]}${plural ? 's' : ''}${count[3] ? ` sélectionné${feminine ? 'e' : ''}${plural ? 's' : ''}` : ''}`;
    }
    return source.replace(/\S(?:[\s\S]*\S)?/, () => translated);
  }
  function translateText(node) {
    if (!node.parentElement || node.parentElement.closest(skip)) return;
    let record = texts.get(node);
    if (!record || node.nodeValue !== record.rendered) record = {source: node.nodeValue};
    record.rendered = t(record.source);
    if (node.nodeValue !== record.rendered) node.nodeValue = record.rendered;
    texts.set(node, record);
  }
  function translateElement(el) {
    if (el.closest(skip)) return;
    let records = attributes.get(el);
    if (!records) { records = {}; attributes.set(el, records); }
    for (const name of attrs) {
      if (!el.hasAttribute(name)) { delete records[name]; continue; }
      const value = el.getAttribute(name);
      let record = records[name];
      if (!record || value !== record.rendered) record = {source: value};
      record.rendered = t(record.source);
      if (value !== record.rendered) el.setAttribute(name, record.rendered);
      records[name] = record;
    }
  }
  const observer = new MutationObserver(records => {
    const roots = new Set();
    for (const record of records) {
      if (record.type === 'childList') for (const node of record.addedNodes) roots.add(node);
      else roots.add(record.target);
    }
    for (const root of roots) if (root.isConnected) apply(root);
  });
  function observe() {
    observer.observe(document.documentElement, {subtree: true, childList: true, characterData: true, attributes: true, attributeFilter: attrs});
  }
  function apply(root = document) {
    // Flush pending app writes before suppressing the observer for our own writes.
    const pending = observer.takeRecords();
    observer.disconnect();
    const visit = node => {
      if (node.nodeType === Node.TEXT_NODE) { translateText(node); return; }
      if (![Node.ELEMENT_NODE, Node.DOCUMENT_NODE, Node.DOCUMENT_FRAGMENT_NODE].includes(node.nodeType)) return;
      if (node.nodeType === Node.ELEMENT_NODE) {
        if (node.closest(skip)) return;
        translateElement(node);
      }
      for (const child of node.childNodes) visit(child);
    };
    visit(root);
    for (const record of pending) {
      if (record.type === 'childList') {
        for (const node of record.addedNodes) if (node.isConnected) visit(node);
      } else if (record.target.isConnected) visit(record.target);
    }
    observe();
  }
  async function refresh() {
    if (fetching) return fetching;
    fetching = (async () => {
      await Promise.all(['fr-CA.generated.json', 'fr-CA.json'].map(async name => {
        try {
          const response = await fetch(new URL('i18n/' + name, base), {cache: 'no-cache', signal: AbortSignal.timeout(15000)});
          if (!response.ok) return;
          const value = await response.json();
          if (!value || Array.isArray(value) || typeof value !== 'object') return;
          dictionaries.set(name, Object.fromEntries(Object.entries(value).filter(([, v]) => typeof v === 'string').map(([k, v]) => [normalize(k), v])));
        } catch (_) { /* Keep the last usable catalog when the ship connection is offline. */ }
      }));
      dictionary = Object.assign(Object.create(null), dictionaries.get('fr-CA.generated.json'), dictionaries.get('fr-CA.json'));
      const version = JSON.stringify(dictionary);
      const changed = version !== catalogVersion;
      catalogVersion = version;
      apply();
      if (changed && language === 'fr-CA') window.dispatchEvent(new CustomEvent('underway:languagechange', {detail: {language, catalogUpdated: true}}));
    })().finally(() => { fetching = null; });
    return fetching;
  }
  function setLanguage(value) {
    language = value === 'fr-CA' ? 'fr-CA' : 'en';
    try { localStorage.setItem(key, language); } catch (_) {}
    document.documentElement.lang = language;
    document.querySelectorAll('[data-language-select]').forEach(select => { select.value = language; });
    apply();
    window.dispatchEvent(new CustomEvent('underway:languagechange', {detail: {language}}));
    if (language === 'fr-CA') refresh();
  }
  window.UnderwayI18n = {t, apply, setLanguage, get language() { return language; }};
  document.addEventListener('change', event => {
    if (event.target.matches('[data-language-select]')) setLanguage(event.target.value);
  });
  window.addEventListener('storage', event => { if (event.key === key) setLanguage(event.newValue); });
  function start() { setLanguage(language); }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start, {once: true});
  else start();
  setInterval(() => { if (language === 'fr-CA' && !document.hidden) refresh(); }, 60000);
})();
