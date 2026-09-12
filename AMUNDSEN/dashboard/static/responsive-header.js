/* Rearrange the existing controls; keep their IDs, listeners and saved state. */
(() => {
  const tabs = document.querySelector('#tabs'), top = document.querySelector('.top');
  const originals = [...tabs.querySelectorAll('button')];
  const appearance = document.querySelector('.themepick'), footer = document.querySelector('#mobile-appearance');
  const appearanceHome = appearance.parentElement;
  const alert = document.querySelector('#alert'), alertHome = document.createComment('schedule home');
  alert.after(alertHome);
  const phone = matchMedia('(max-width:640px)'), wide = matchMedia('(min-width:1000px)');
  let groups = [];
  const close = () => groups.forEach(d => { d.open = false; });
  function layout() {
    if (phone.matches && !groups.length) {
      tabs.setAttribute('role','navigation'); tabs.setAttribute('aria-label','Main navigation');
      for (const [label, names] of [['Map',['map']],['Science',['calendar','stations','underway','casts']],['Extras',['photos','wiki','chat','sources']]]) {
        const d = document.createElement('details'); d.className = 'mobile-nav';
        const s = document.createElement('summary'); s.textContent = '☰ ' + label;
        const panel = document.createElement('div'); panel.className = 'mobile-nav-items';
        d.append(s,panel); tabs.append(d); groups.push(d);
        for (const name of names) { const b=originals.find(b=>b.dataset.tab===name); b.removeAttribute('role'); panel.append(b); }
        if (label==='Map') for (const [mode,text] of [['full','Full map'],['none','Hide map']]) {
          const b = document.createElement('button'); b.type='button'; b.textContent=text;
          b.onclick=()=>window.UW?.setMapMode?.(mode); panel.append(b);
        }
        d.addEventListener('toggle',()=>{ if(d.open) groups.filter(g=>g!==d).forEach(g=>{g.open=false;}); });
      }
    } else if (!phone.matches && groups.length) {
      for (const b of originals) { if(b.dataset.tab!=='map') b.setAttribute('role','tab'); tabs.append(b); }
      for (const d of groups) d.remove(); groups=[];
      tabs.setAttribute('role','tablist'); tabs.removeAttribute('aria-label');
    }
    footer.hidden=!phone.matches;
    (phone.matches?footer:appearanceHome).append(appearance);
    if (wide.matches) top.append(alert); else alertHome.before(alert);
  }
  tabs.addEventListener('click',e=>{if(e.target.closest('button'))close();});
  document.addEventListener('click',e=>{if(!tabs.contains(e.target))close();});
  document.addEventListener('keydown',e=>{if(e.key==='Escape'){const d=groups.find(g=>g.open);close();d?.querySelector('summary').focus();}});
  phone.addEventListener('change',layout); wide.addEventListener('change',layout); layout();
})();
