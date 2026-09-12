const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const source = fs.readFileSync(path.join(__dirname, '../dashboard/static/data.js'), 'utf8');
function api(overrides = {}) {
  const context = { window: {}, AbortController, setTimeout, clearTimeout, ...overrides };
  vm.runInNewContext(source, context);
  return context.window.UWData;
}
const deferred = () => { let resolve, reject; const promise = new Promise((a,b) => { resolve=a; reject=b; }); return {promise,resolve,reject}; };

test('custom ranges survive new data and replacement plot elements, follow variables and respect reset', async () => {
  const {EventEmitter} = require('node:events');
  const plot = () => new EventEmitter();
  const draw = api().plotState({react:async(gd,data,layout)=>{gd._fullLayout=layout;return gd;}});
  const layout = {xaxis:{title:{text:'Temperature'},range:[0,10]},yaxis:{title:{text:'Depth'},range:[100,0]}};
  const first=await draw(plot(),[],layout,{},'cast1');
  first._fullLayout.xaxis.range=[2,4];first._fullLayout.yaxis.range=[80,20];
  first.emit('plotly_relayout',{'xaxis.range':[2,4],'yaxis.range':[80,20]});
  const changed={...layout,xaxis:{title:{text:'Salinity'},range:[30,35]},xaxis2:{title:{text:'Temperature'},range:[-10,10]}};
  const next=await draw(plot(),[],changed,{},'cast1');
  assert.deepEqual(Array.from(next._fullLayout.xaxis2.range),[2,4]);
  assert.deepEqual(Array.from(next._fullLayout.xaxis.range),[30,35]);
  assert.deepEqual(Array.from(next._fullLayout.yaxis.range),[80,20]);
  next.emit('plotly_relayout',{'xaxis2.autorange':true,'yaxis.autorange':'reversed'});
  const reset=await draw(plot(),[],changed,{},'cast1');
  assert.deepEqual(Array.from(reset._fullLayout.xaxis2.range),[-10,10]);
  assert.deepEqual(Array.from(reset._fullLayout.yaxis.range),[100,0]);
  const other=await draw(plot(),[],layout,{},'cast2');
  assert.deepEqual(Array.from(other._fullLayout.xaxis.range),[0,10]);
});

test('failed requests retry, successful requests share a generation cache', async () => {
  let attempts=0;
  const get = api().generationCache(async () => { if (++attempts === 1) throw Error('offline'); return 42; }, () => 'one');
  await assert.rejects(get('table','data/table.json'));
  assert.equal(await get('table','data/table.json'),42);
  assert.equal(await get('table','data/table.json'),42);
  assert.equal(attempts,2);
});

test('a revised cast is fetched again when the manifest changes', async () => {
  let generation='one', attempts=0;
  const get = api().generationCache(async url => { attempts++; return url; }, () => generation);
  assert.equal(await get('cast:1','data/cast.json'),'data/cast.json?v=one');
  generation='two';
  assert.equal(await get('cast:1','data/cast.json'),'data/cast.json?v=two');
  assert.equal(attempts,2);
});

test('old in-flight responses cannot overwrite newer data', async () => {
  let generation='one'; const old=deferred(), fresh=deferred();
  const get = api().generationCache(url => url.endsWith('one') ? old.promise : fresh.promise, () => generation);
  const first=get('calendar','data/calendar.json');
  generation='two';
  const next=get('calendar','data/calendar.json');
  fresh.resolve('new'); assert.equal(await next,'new');
  old.resolve('old'); await assert.rejects(first,/Data changed/);
  assert.equal(await get('calendar','data/calendar.json'),'new');
});

test('concurrent callers share a single request', async () => {
  const response=deferred(); let attempts=0;
  const get = api().generationCache(() => {attempts++; return response.promise;}, () => 'one');
  const a=get('index','data/index.json'), b=get('index','data/index.json');
  response.resolve('index');
  assert.deepEqual(await Promise.all([a,b]),['index','index']);
  assert.equal(attempts,1);
});

test('JSON parse and HTTP failures reject rather than caching empty data', async () => {
  await assert.rejects(api({fetch:async()=>({ok:false,status:503})}).fetchJSON('data.json'),/503/);
  await assert.rejects(api({fetch:async()=>({ok:true,json:async()=>{throw Error('partial JSON');}})}).fetchJSON('data.json'),/partial JSON/);
});

test('timeout also bounds reading the response body and is cleared', async () => {
  let expire, cleared=false, signal;
  const reading=deferred();
  const {fetchJSON}=api({
    setTimeout: fn => {expire=fn; return 1;}, clearTimeout:()=>{cleared=true;},
    fetch: async (url,opts) => { signal=opts.signal; return {ok:true,json:()=>new Promise((resolve,reject)=>{signal.addEventListener('abort',()=>reject(Error('timeout'))); reading.resolve();})}; },
  });
  const result=fetchJSON('data.json');
  await reading.promise; expire();
  await assert.rejects(result,/timeout/);
  assert.equal(signal.aborted,true); assert.equal(cleared,true);
});
