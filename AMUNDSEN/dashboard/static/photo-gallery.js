/* Shared photo detail navigation and fullscreen slideshows. */
(() => {
  'use strict';
  const ui = (source, values) => window.UWI18n.text(source, values);
  const uh = (source, values = {}) => window.UWI18n.html(source, values);

  const details = new WeakMap();
  const esc = value => String(value ?? '').replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
  const step = (items, id, delta) => items[(Math.max(0, items.findIndex(item => item.id === id)) + delta + items.length) % items.length];
  function fitPhoto(figure) {
    const image = figure.querySelector('.photo-open');
    if (!image || !image.offsetParent) return;
    image.style.height = `${Math.max(120, innerHeight - Math.max(0, image.getBoundingClientRect().top) - 48)}px`;
  }
  addEventListener('resize', () => document.querySelectorAll('.photo-detail').forEach(fitPhoto));
  addEventListener('keydown', event => {
    if (event.defaultPrevented || event.altKey || event.ctrlKey || event.metaKey || event.target.closest?.('input,textarea,select,[contenteditable=true]')) return;
    if (document.querySelector('dialog[open]')) return;
    const figure = Array.from(document.querySelectorAll('.photo-detail')).find(el => el.offsetParent && details.has(el));
    if (!figure || !['ArrowLeft', 'ArrowRight'].includes(event.key)) return;
    const {items, id, onSelect} = details.get(figure);
    event.preventDefault();
    const item = step(items, id, event.key === 'ArrowLeft' ? -1 : 1);
    if (item) onSelect(item.id);
  });
  function slideshow({items, id, onSelect}) {
    const dialog = document.createElement('dialog');
    dialog.className = 'photo-slideshow'; dialog.setAttribute('aria-label', ui("Photo slideshow"));
    dialog.innerHTML = `<button type="button" class="photo-close" aria-label="${uh("Close slideshow")}">×</button><button type="button" class="photo-prev" aria-label="${uh("Previous photo")}">‹</button><img alt=""><button type="button" class="photo-next" aria-label="${uh("Next photo")}">›</button><div class="photo-caption" aria-live="polite"></div>`;
    let current = id;
    const relabel = () => {
      dialog.setAttribute('aria-label', ui('Photo slideshow'));
      for (const [selector, label] of [['.photo-close','Close slideshow'],['.photo-prev','Previous photo'],['.photo-next','Next photo']]) dialog.querySelector(selector).setAttribute('aria-label', ui(label));
    };
    window.addEventListener('uw:localechange', relabel);
    const show = delta => {
      const item = step(items, current, delta);
      if (!item) return;
      current = item.id;
      const img = dialog.querySelector('img'); img.src = item.src; img.alt = item.alt || item.title || ui("Photograph");
      dialog.querySelector('.photo-caption').textContent = `${items.findIndex(item => item.id === current) + 1} / ${items.length}${item.title ? ' · ' + item.title : ''}${item.credit ? ' · ' + item.credit : ''}`;
      for (const button of dialog.querySelectorAll('.photo-prev,.photo-next')) button.disabled = items.length < 2;
    };
    const close = () => {
      if (document.fullscreenElement === dialog) document.exitFullscreen().catch(() => {});
      dialog.close();
    };
    dialog.querySelector('.photo-close').onclick = close;
    dialog.querySelector('.photo-prev').onclick = () => show(-1);
    dialog.querySelector('.photo-next').onclick = () => show(1);
    dialog.onkeydown = event => {
      if (!['ArrowLeft','ArrowRight','Escape'].includes(event.key)) return;
      event.preventDefault(); event.stopPropagation();
      if (event.key === 'Escape') close(); else show(event.key === 'ArrowLeft' ? -1 : 1);
    };
    dialog.oncancel = event => { event.preventDefault(); close(); };
    dialog.onclose = () => { window.removeEventListener('uw:localechange', relabel); dialog.remove(); if (current !== id) onSelect(current); };
    document.body.append(dialog); show(0); dialog.showModal();
    dialog.requestFullscreen?.().catch(() => {});
  }
  function wireDetail(figure, options) {
    const {items, id, onSelect, onGallery} = options;
    const item = items.find(item => item.id === id);
    if (!item) return;
    details.set(figure, options);
    figure.classList.add('photo-detail'); figure.dataset.photo = id;
    figure.innerHTML = `<nav class="photo-navigation" aria-label="${uh("Gallery navigation")}"><button type="button" class="photo-gallery">${uh("← Gallery")}</button><span>${items.indexOf(item) + 1} / ${items.length}</span><button type="button" class="photo-prev" aria-label="${uh("Previous photo")}" ${items.length < 2 ? 'disabled' : ''}>‹</button><button type="button" class="photo-next" aria-label="${uh("Next photo")}" ${items.length < 2 ? 'disabled' : ''}>›</button></nav><button type="button" class="photo-open" aria-label="${uh("Open fullscreen slideshow")}"><img src="${esc(item.src)}" alt="${esc(item.alt || item.title || ui("Photograph"))}"></button><figcaption>${uh("{v11}click photo for fullscreen", {v11: (item.credit ? esc(item.credit) + ' · ' : '')})}</figcaption>`;
    figure.querySelector('.photo-gallery').onclick = onGallery;
    for (const [cls, delta] of [['.photo-prev',-1],['.photo-next',1]]) figure.querySelector(cls).onclick = () => { const next = step(items, id, delta); if (next) onSelect(next.id); };
    figure.querySelector('.photo-open').onclick = () => slideshow(options);
    fitPhoto(figure); requestAnimationFrame(() => fitPhoto(figure));
  }
  window.UWPhotoGallery = {wireDetail};
  window.addEventListener('uw:localechange', () => {
    for (const figure of document.querySelectorAll('.photo-detail')) {
      const options = details.get(figure); if (options) wireDetail(figure, options);
    }
  });
})();
