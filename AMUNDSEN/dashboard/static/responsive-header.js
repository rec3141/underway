/* Rearrange the existing controls; keep their IDs, listeners and saved state. */
(() => {
  const tabs = document.querySelector('#tabs'), top = document.querySelector('.top');
  const originals = [...tabs.querySelectorAll('button, a.games-link')];
  const appearance = document.querySelector('.themepick'), footer = document.querySelector('#mobile-appearance');
  const appearanceHome = appearance.parentElement;
  const alert = document.querySelector('#alert'), alertHome = document.createComment('schedule home');
  alert.after(alertHome);
  const phone = matchMedia('(max-width:640px)'), wide = matchMedia('(min-width:1000px)');
  let groups = [];
  const mapIcons = {none:'□', half:'◧', full:'■'};
  const mapLabels = {none:'No map', half:'Half map', full:'Full map'};
  function syncMapMenu() {
    const mode = window.UW?.mapMode?.() || 'half';
    const summary = tabs.querySelector('.mobile-map > summary');
    if (summary) { summary.textContent = `${mapIcons[mode]} Map`; summary.setAttribute('aria-label', `Map: ${mapLabels[mode]}`); }
    for (const b of tabs.querySelectorAll('[data-map-mode]')) b.setAttribute('aria-pressed', String(b.dataset.mapMode === mode));
  }
  const close = () => groups.forEach(d => { d.open = false; });
  function layout() {
    if (phone.matches && !groups.length) {
      tabs.setAttribute('role','navigation'); tabs.setAttribute('aria-label','Main navigation');
      for (const [label, names] of [['Map',['map']],['Science',['calendar','stations','underway','casts']],['Extras',['photos','wiki','chat','games','sources']]]) {
        const d = document.createElement('details'); d.className = 'mobile-nav';
        const s = document.createElement('summary'); s.textContent = '☰ ' + label;
        const panel = document.createElement('div'); panel.className = 'mobile-nav-items';
        d.append(s,panel); tabs.append(d); groups.push(d);
        for (const name of names) { const b=originals.find(b=>(b.dataset.tab || b.dataset.nav)===name); b.removeAttribute('role'); panel.append(b); }
        if (label==='Map') {
          d.classList.add('mobile-map');
          originals.find(b=>b.dataset.tab==='map').style.display='none';
          for (const mode of ['none','half','full']) {
            const b = document.createElement('button'); b.type='button'; b.dataset.mapMode=mode;
            b.textContent=`${mapIcons[mode]} ${mapLabels[mode]}`;
            b.onclick=()=>{ window.UW?.setMapMode?.(mode); syncMapMenu(); }; panel.append(b);
          }
        }
        if (label==='Extras') originals.find(b=>b.dataset.tab==='sources').textContent='Sources';
        d.addEventListener('toggle',()=>{ if(d.open) groups.filter(g=>g!==d).forEach(g=>{g.open=false;}); });
      }
    } else if (!phone.matches && groups.length) {
      for (const b of originals) { if(b.dataset.tab && b.dataset.tab!=='map') b.setAttribute('role','tab'); tabs.append(b); }
      originals.find(b=>b.dataset.tab==='map').style.display='';
      originals.find(b=>b.dataset.tab==='sources').textContent='?';
      for (const d of groups) d.remove(); groups=[];
      tabs.setAttribute('role','tablist'); tabs.removeAttribute('aria-label');
    }
    syncMapMenu();
    footer.hidden=!phone.matches;
    (phone.matches?footer:appearanceHome).append(appearance);
    if (wide.matches) top.append(alert); else alertHome.before(alert);
  }
  tabs.addEventListener('click',e=>{if(e.target.closest('button, a'))close();});
  document.addEventListener('click',e=>{if(!tabs.contains(e.target))close();});
  document.addEventListener('keydown',e=>{if(e.key==='Escape'){const d=groups.find(g=>g.open);close();d?.querySelector('summary').focus();}});
  new MutationObserver(syncMapMenu).observe(document.querySelector('main'), {attributes:true, attributeFilter:['data-mapmode']});
  phone.addEventListener('change',layout); wide.addEventListener('change',layout); layout();
})();
