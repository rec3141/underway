const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const script = fs.readFileSync(path.join(__dirname, '../dashboard/static/photo-notifications.js'), 'utf8');
const photo = id => ({id, artifact_file:`${id}.jpg`});
function setup(saved = null, publicSite = false) {
  let value = saved;
  const events = {}, badge = {hidden:true, setAttribute(key,value){this[key]=value}}, cards = [];
  const pane = {hidden:true, gallery:true, querySelector(){return this.gallery}, querySelectorAll(){return cards.map(id => ({dataset:{photoId:id}}))}};
  const document = {hidden:false, getElementById:id => id==='photos-unread'?badge:pane, addEventListener:(type,fn)=>events[type]=fn};
  const window = {UW:{public:publicSite,store:{get:(key,fallback)=>value??fallback,set:(key,next)=>value=Array.from(next)}},
    UWI18n:{t:(key,{count})=>`${count} new photo${count===1?'':'s'}`},addEventListener:(type,fn)=>events[type]=fn};
  vm.runInNewContext(script,{window,document});
  return {api:window.UWPhotoNotifications,badge,pane,cards,document,events,window,get saved(){return value},set saved(next){value=next}};
}
test('first successful load is a baseline; only unique photographs count as new',()=>{
  const h=setup();h.api.update([photo('a'),{id:'note'}]);assert(h.badge.hidden);
  h.api.update([photo('a'),photo('b'),photo('b'),{id:'note2'}]);assert.equal(h.badge.textContent,'1');assert.equal(h.badge['aria-label'],'1 new photo');assert(!h.badge.hidden);
  h.api.update([{...photo('a'),detail:'edited'},photo('b')]);assert.equal(h.badge.textContent,'1');
});
test('unread photos survive reloads and a gallery visit acknowledges only rendered images',()=>{
  const h=setup(['a']);h.api.update([photo('a'),photo('b'),photo('c')]);h.pane.hidden=false;h.cards.push('a','b');h.api.markVisible();
  assert.equal(h.badge.textContent,'1');assert.deepEqual(h.saved,['a','b']);
  const reload=setup(h.saved);reload.api.update([photo('a'),photo('b'),photo('c')]);assert.equal(reload.badge.textContent,'1');
  reload.pane.hidden=false;reload.cards.push('a','b','c');reload.api.markVisible();assert(reload.badge.hidden);
});
test('hidden panes, background tabs, and the submit form do not mark photos seen',()=>{
  const h=setup([]);h.api.update([photo('a')]);h.cards.push('a');h.api.markVisible();assert.equal(h.badge.textContent,'1');
  h.pane.hidden=false;h.document.hidden=true;h.api.markVisible();assert.equal(h.badge.textContent,'1');
  h.document.hidden=false;h.pane.gallery=false;h.api.markVisible();assert.equal(h.badge.textContent,'1');
  h.pane.gallery=true;h.events.visibilitychange();assert(h.badge.hidden);
});
test('rendering before a successful response does not create a baseline',()=>{
  const h=setup();h.pane.hidden=false;h.api.markVisible();assert.equal(h.saved,null);h.api.update([photo('a')]);assert(h.badge.hidden);
});
test('counts cap at 99+ while accessible labels retain the exact count',()=>{
  const h=setup([]);h.api.update(Array.from({length:102},(_,i)=>photo(String(i))));assert.equal(h.badge.textContent,'99+');assert.equal(h.badge['aria-label'],'102 new photos');
  h.window.UWI18n.t=(key,{count})=>`${count} nouvelles photos`;h.events['uw:localechange']();assert.equal(h.badge.title,'102 nouvelles photos');
});
test('seen state synchronizes across tabs and merges concurrent acknowledgements',()=>{
  const h=setup(['a']);h.api.update([photo('a'),photo('b'),photo('c')]);h.saved=['a','b'];h.events.storage({key:'uw:photos.seenIds'});assert.equal(h.badge.textContent,'1');
  h.pane.hidden=false;h.cards.push('c');h.api.markVisible();assert.deepEqual(h.saved,['a','b','c']);assert(h.badge.hidden);
});
test('public site neither counts nor records ship photographs',()=>{
  const h=setup(null,true);h.api.update([photo('a')]);h.pane.hidden=false;h.cards.push('a');h.api.markVisible();assert(h.badge.hidden);assert.equal(h.saved,null);
});
