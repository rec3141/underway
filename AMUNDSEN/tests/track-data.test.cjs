const {test} = require('node:test');
const assert = require('node:assert/strict');
const {createLoader, intersects, chooseLevel, assemble} = require('../dashboard/static/track-data.js');
const data = (t, lat = t.map(() => 70)) => ({t, lat, lon:t.map(() => 10), leg:t.map(() => 1), vars:{temperature:t}});
const chunk = (file, start, end, n = 2, segment = 'a') => ({file, start, end, n, segment, leg:1, bounds:[9,69,11,71]});
const opts = chunks => ({track:{levels:[{spacing_km:.1,chunks}]},bounds:[0,60,20,80],start:0,end:100,spacingKm:.1,legs:[1]});
test('wrapped viewports and repeated worlds intersect correctly', () => {
  assert.ok(intersects([170,0,-170,80], [-179,10,-175,20]));
  assert.ok(intersects([530,0,550,80], [175,10,179,20]));
  assert.ok(!intersects([170,0,-170,80], [-10,10,10,20]));
  assert.ok(intersects([-200,0,200,80], [0,10,10,20]));
});
test('resolution never exceeds 1km and native selects only native', () => {
  const track = {levels:[{spacing_km:1},{spacing_km:.1},{spacing_km:.025},{spacing_km:0}]};
  assert.equal(chooseLevel(track,10).spacing_km,1);
  assert.equal(chooseLevel(track,.04).spacing_km,.025);
  assert.equal(chooseLevel(track,0).spacing_km,0);
});
test('overlapping seams deduplicate, removed fixes and separate segments break lines', () => {
  const parts = [{chunk:chunk('a',1,3,3),data:data([1,2,3])},
    {chunk:chunk('b',3,5,3),data:data([3,4,5],[70,null,70])},
    {chunk:chunk('c',6,7,2,'b'),data:data([6,7])}];
  const out = assemble(parts,opts([]),50);
  assert.deepEqual(out.lat,[70,70,70,null,70,null,70,70]);
  assert.deepEqual(out.vars.temperature,[1,2,3,null,5,null,6,7]);
  assert.equal(out.shown,6);
});
test('cache survives manifest generations; request budget applies before fetching', async () => {
  const requested = [];
  const loader = createLoader({maxRows:5,fetchImpl:async url => {
    requested.push(url); return {ok:true,json:async()=>data(url==='a'?[1,2]:[3,4])};
  }});
  const options = opts([chunk('a',1,2),chunk('b',3,4),chunk('c',5,6)]);
  const first = await loader.load(options);
  assert.equal(first.limited,true);
  assert.equal(first.n,5);
  await loader.load({...options,generation:'new'});
  assert.deepEqual(requested,['a','b']);
});
test('superseded responses cannot publish even when fetch ignores abort', async () => {
  let release;
  const loader = createLoader({fetchImpl:async url => {
    if(url==='old') await new Promise(resolve=>{release=resolve;});
    return {ok:true,json:async()=>data([1,2])};
  }});
  const old = loader.load(opts([chunk('old',1,2)]));
  const rejected = assert.rejects(old,{name:'AbortError'});
  const fresh = await loader.load(opts([chunk('new',1,2)]));
  release(); await rejected;
  assert.equal(fresh.shown,2);
});
test('selected legs filter descriptors without fetching and bounded cache evicts', async () => {
  const requested=[];
  const loader=createLoader({maxCacheRows:2,concurrency:1,fetchImpl:async url=>{
    requested.push(url); return {ok:true,json:async()=>data([1,2])};
  }});
  await loader.load(opts([{...chunk('other',1,2),leg:2}]));
  await loader.load(opts([chunk('a',1,2)]));
  await loader.load(opts([chunk('b',1,2)]));
  await loader.load(opts([chunk('a',1,2)]));
  assert.deepEqual(requested,['a','b','a']);
});
test('tiny viewport retains crossing-segment endpoints and unwraps date line', () => {
  const crossing = {...data([1,2]),lon:[9,11]};
  const result=assemble([{chunk:chunk('a',1,2),data:crossing}],{...opts([]),bounds:[9.99,69.99,10.01,70.01]},50);
  assert.deepEqual(result.lon,[9,11]);
  const wrapped=assemble([{chunk:chunk('a',1,2),data:{...crossing,lon:[179,-179]}}],
    {...opts([]),bounds:[179.9,69.99,-179.9,70.01]},50);
  assert.deepEqual(wrapped.lon,[179,181]);
});
test('total timeout rejects unresponsive fetch and permits retry', async () => {
  let signal;
  const loader=createLoader({timeoutMs:10,fetchImpl:async (url,options)=>{
    if(url==='hang') {signal=options.signal; return new Promise(()=>{});}
    return {ok:true,json:async()=>data([1,2])};
  }});
  await assert.rejects(loader.load(opts([chunk('hang',1,2)])),error=>error.name!=='AbortError'&&/timed out/.test(error.message));
  assert.equal(signal.aborted,true);
  assert.equal((await loader.load(opts([chunk('good',1,2)]))).shown,2);
});

test('explicitly empty leg selection never loads observations', async () => {
  const loader = createLoader({fetchImpl:async()=>{throw Error('must not fetch');}});
  const result = await loader.load({...opts([chunk('a',1,2)]),legs:[]});
  assert.equal(result.shown,0);
});
