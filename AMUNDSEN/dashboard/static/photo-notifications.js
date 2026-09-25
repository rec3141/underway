/* Per-browser unread counts for photographs in the ship's gallery. */
(() => {
  'use strict';
  const {store} = window.UW;
  const key = 'photos.seenIds';
  const saved = store.get(key, null);
  let seen = Array.isArray(saved) ? new Set(saved) : null;
  let photos = [];
  const badge = document.getElementById('photos-unread');
  function render() {
    if (!badge) return;
    const count = seen ? photos.filter(id => !seen.has(id)).length : 0;
    badge.hidden = window.UW.public || count === 0;
    badge.textContent = count > 99 ? '99+' : String(count);
    const label = window.UWI18n.t('photos.unread', {count});
    badge.setAttribute('aria-label', label);
    badge.title = label;
  }
  function remember(ids) {
    const stored = store.get(key, []);
    seen = new Set([...(seen || []), ...(Array.isArray(stored) ? stored : []), ...ids]);
    store.set(key, [...seen]);
    render();
  }
  function update(entries) {
    if (window.UW.public) return;
    photos = [...new Set(entries.filter(entry => entry.artifact_file && entry.id != null).map(entry => String(entry.id)))];
    // The first successful load establishes a baseline for future arrivals.
    if (seen === null) remember(photos);
    else render();
  }
  function markVisible() {
    if (document.hidden || window.UW.public || seen === null) return;
    const pane = document.getElementById('pane-wiki');
    if (!pane || pane.hidden || !pane.querySelector('#jtabs [data-t="gallery"].on')) return;
    // A background refresh must not acknowledge images absent from the rendered gallery.
    remember([...pane.querySelectorAll('[data-photo-id]')].map(card => card.dataset.photoId));
  }
  window.addEventListener('storage', event => {
    if (event.key !== 'uw:' + key && event.key !== null) return;
    const value = store.get(key, null);
    seen = Array.isArray(value) ? new Set(value) : null;
    render();
  });
  window.addEventListener('uw:localechange', render);
  document.addEventListener('visibilitychange', markVisible);
  window.UWPhotoNotifications = {update, markVisible};
})();
