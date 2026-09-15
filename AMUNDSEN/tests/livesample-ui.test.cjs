/* Synthetic service/DOM integration check; run with node tests/livesample-ui.test.cjs.
 * An optional JSON snapshot argument validates a real /api/livesample response.
 * Loads the full production script and makes no network requests or Telegram sends. */
const fs = require('node:fs'), vm = require('node:vm'), assert = require('node:assert/strict'), path = require('node:path');
const root = path.join(__dirname, '../dashboard/static/');
const html = fs.readFileSync(root + 'livesample.html', 'utf8');
const ids = [...html.matchAll(/id="([^"]+)"/g)].map(m => m[1]);
assert.equal(new Set(ids).size, ids.length, 'HTML IDs unique');
class El {
  constructor(tag = '', id = '') { this.tag = tag; this.id = id; this.children = []; this.events = {}; this.dataset = {}; this.value = ''; this.classList = { toggle() {} }; this.files = []; this.style = {}; this.attributes = {}; }
  setAttribute(name, value) { this.attributes[name] = value; }
  addEventListener(name, fn) { this.events[name] = fn; }
  append(...nodes) { this.children.push(...nodes); }
  replaceChildren(...nodes) { this.children = nodes; }
  querySelectorAll(q) { const descendants = this.children.flatMap(n => [n, ...n.querySelectorAll('*')]); if (q === '*') return descendants; return descendants.filter(n => q === 'input:checked' ? n.tag === 'input' && n.checked : q.split(', ').includes(n.tag)); }
  get selectedOptions() { return this.children.filter(n => n.value === this.value); }
  closest() { return this; }
  contains() { return false; }
  click() {}
}
const elements = Object.fromEntries(ids.map(id => [id, new El('div', id)]));
for (const id of ['algorithm','flow-method','flow-span','embedding-color']) elements[id].tag = 'select';
for (const id of ['target-count','min-spacing','flow-target-count','flow-spacing','telegram','samples-file']) elements[id].tag = 'input';
elements['settings-form'].children = ['algorithm','target-count','min-spacing','telegram','samples-file','apply-settings','leglist'].map(id => elements[id]);
elements['embedding-color'].value = 'leg';
const plots = {}, posts = [], intervals = [], requested = [];
const stamp = Date.parse('2026-09-14T12:00:00Z');
const manifest = { default_window: '1h', windows: [{label:'1h',file:'data/w-1h.json'},{label:'3h',file:'data/w-3h.json'}], legs:[{id:'L1',index:0,label:'Leg one'},{id:'L2',index:1,label:'Leg two'}], variables:[{name:'SST (°C)',unit:'°C',tsg:true},{name:'Salinity (PSU)',unit:'PSU',tsg:true},{name:'Nitrates',unit:'µM',tsg:true}] };
const history = {t:[stamp,stamp+60000,stamp+120000,stamp+180000],leg:[0,0,0,1],vars:{'SST (°C)':[2,3,4,99],'Salinity (PSU)':[30,31,32,99],Nitrates:[null,null,null,9]}};
let payload = {config:{legs:['L1'],algorithm:'coverage',count:1,min_spacing:10,telegram:false}, status:'ready', features:['Temperature','Salinity'], scoring_features:['Temperature','Salinity'], reference_count:10, sample_count:2, recommendations:[{pressure:101,depth_m:100,position:100,rank:1,score:.92,z:1.4,reason:'Water missing from samples'}], distributions:{ctd:{baseline_values:[-1,-.3,0,.2,.8],values:[1.4]}}, flow:{status:'ready',recommendation:true,temperature:2,salinity:32,score:.7}, embeddings:{tsne:{points:[{x:1,y:2,cast:'001',leg:'L1',pressure:101,Temperature:2.125,Salinity:32.5,sampled:true}]},umap:{points:[{x:3,y:4,cast:'001',leg:'L1',pressure:101,sampled:false}]}},live:{tcp:'host:1',tcp_state:'connected',last_packet_age_s:1,columns:['depth','temperature'],fields:['Depth [m]','Temperature [deg C]'],current:{started:1,n:4,depth_like:true,direction:'up',columns:['depth','temperature'],pressure_col:'depth',cols:{depth:[5,100,200,150],temperature:[3,2,1,1.5]}}}};
payload.variables = [{key:'Temperature [°C]',name:'Temperature',unit:'°C'},{key:'Nitrates',name:'Nitrates',unit:'µM'}];
payload.embeddings.tsne.points[0].properties = {'Temperature [°C]':2.125,Nitrates:null};
payload.embeddings.tsne.points.push({x:5,y:6,leg:'L1',cast:'001',pressure:150,reference:true,properties:{'Temperature [°C]':1,Nitrates:3}});
payload.embeddings.tsne.points.push({x:7,y:8,leg:'L1',cast:'001',pressure:180,collected:true,bottles:[1,2],properties:{'Temperature [°C]':.5,Nitrates:4}});
payload.live.current.columns.push('salinity'); payload.live.current.cols.salinity=[30,31,32,31.5];
const fixture = process.argv[2] ? JSON.parse(fs.readFileSync(process.argv[2], 'utf8')) : null;
if (fixture) payload = fixture;
let fail = false, holdOneHour = false, releaseHistory;
const context = {console, AbortController, Blob, URL, URLSearchParams, Date, Set, Number, JSON, Promise, setTimeout, clearTimeout, setInterval:fn => intervals.push(fn), document:{getElementById:id=>elements[id],createElement:tag=>new El(tag), documentElement:{},addEventListener(){},hidden:false}, getComputedStyle:()=>({getPropertyValue:key=>key==='--palette'?'#fff #000':'#fff'}), Plotly:{react:(el,traces,layout,config)=>{ el.layout=layout; plots[el.id]={traces,layout,config}; },relayout:(el,reset)=>{el.reset=reset}}, fetch:async(url,options={})=>{
 requested.push(url);
 if (url==='data/manifest.json') return {ok:true,json:async()=>manifest};
 if (url.startsWith('api/livesample/template?')) return {ok:true,text:async()=> 'leg\tcast\tbottle\tsampled\nL1\tL1:CTD_001\t1\t\n'};
 if (holdOneHour && url.startsWith('data/w-1h')) return new Promise(resolve => { releaseHistory = () => resolve({ok:true,json:async()=>history}); });
 if (url.startsWith('data/w-')) return {ok:true,json:async()=>history};
 if (url.startsWith('api/livesample/flow?')) { const requestedCount = url.includes('count=2') ? 2 : 1; return {ok:true,json:async()=>({targets:Array.from({length:requestedCount},(_,i)=>({time:new Date(stamp+(i+1)*60000).toISOString(),reason:'Historical replay',rank:i+1,score:1-i*.1,z:1.2+i*.4})),distribution:{baseline_values:[-1,-.2,0,.4,.9],values:Array.from({length:requestedCount},(_,i)=>1.2+i*.4)},requested_count:requestedCount,algorithm:url.includes('algorithm=rarity')?'rarity':'coverage',window:url.includes('3h')?'3h':'1h'})}; }
 if (options.method==='POST') { const body=JSON.parse(options.body);posts.push(body);payload={...payload,config:body};return {ok:true,json:async()=>payload}; }
 return {ok:!fail,status:503,json:async()=>fail?{error:'Service down'}:payload};
}};
context.window = context;
vm.runInNewContext(fs.readFileSync(root+'livesample.js','utf8'), context);
const tick=()=>new Promise(resolve=>setTimeout(resolve,20));
(async()=>{
 await tick();
 if (fixture) {
  assert.equal(elements['service-state'].textContent, fixture.status || 'Connected');
 assert.equal(elements['recommendations'].children.length, (fixture.recommendations || []).length);
 assert.match(elements['live-status'].textContent, /host:1/);
  for (const name of ['tsne','umap']) if (fixture.embeddings?.[name]?.points?.length && !fixture.error && !/^(Updating sampling model|Preparing selected casts)/.test(fixture.status || '')) assert.ok(plots[`${name}-plot`]?.traces?.length, `${name} renders real data`);
  assert.equal(elements['request-error'].hidden, true);
  console.log(`PASS: real API snapshot rendered (${fixture.reference_count || 0} reference observations; ${(fixture.recommendations || []).length} targets).`);
  return;
 }
 assert.equal(elements['service-state'].textContent,'ready');
 assert.equal(elements['legsummary'].textContent,'1 of 2 legs');
 await elements['download-template'].events.click();
 assert.ok(requested.some(url=>url==='api/livesample/template?leg=L1'),'bottle template uses selected picker legs');
 assert.equal(plots['live-plot'].layout.yaxis.title.text,'Depth (m)');
 assert.equal(plots['live-plot'].layout.shapes.length,payload.config.count,'CTD chart shows the complete requested target budget');
 assert.equal(plots['live-plot'].layout.shapes[0].y0,100,'targets use depth axis rather than pressure');
 assert.deepEqual(plots['ctd-hist-plot'].traces[0].x,payload.distributions.ctd.baseline_values,'CTD histogram is based on previous bottles');
 assert.equal(plots['ctd-hist-plot'].layout.shapes.length,1);
 assert.match(plots['ctd-hist-plot'].layout.annotations[0].text,/#1.*score.*z/);
 assert.equal(plots['live-plot'].traces[1].line.dash,'dot');
 elements['live-depth-scale'].events.click();
 assert.equal(plots['live-plot'].layout.shapes[0].y0,10,'sampling lines share compressed depth transform');
 assert.equal(plots['live-plot'].traces[0].y[1],10);
 assert.equal(plots['live-plot'].traces[0].customdata[1],100,'hover depth remains metres');
 assert.ok(plots['live-plot'].layout.yaxis.ticktext.includes('100'));
 elements['live-depth-scale'].events.click();
 payload.live.current.depth_like=false;payload.live.current.pressure_col='pressure';payload.live.current.cols.pressure=[5,100,200,150];
 await intervals[0]();await tick();
 assert.ok(plots['live-plot'].traces[0].y[1]>98 && plots['live-plot'].traces[0].y[1]<100,'pressure feed converts to metres');
 payload.live.current.depth_like=true;payload.live.current.pressure_col='depth';
 payload.cast_segment={soak_detected:true,start_index:2,start_time:payload.live.current.started+2};await intervals[0]();await tick();
 assert.ok(plots['live-plot'].traces.flatMap(trace=>trace.customdata || []).every(depth=>depth>=150),'live chart hides scans from the soak cycle');
 payload.cast_segment={soak_detected:false,start_index:0,start_time:null};await intervals[0]();await tick();
 assert.equal(plots['live-plot'].layout.xaxis.side,'top');
 assert.equal(plots['live-plot'].layout.xaxis2.side,'bottom');
 const unavailable = elements['live-parameters'].children.find(row=>row.dataset.key==='oxygen').children[0];
 assert.equal(unavailable.disabled,true,'unavailable live parameter is grey and disabled');
 elements['live-parameters'].children.find(row=>row.dataset.key==='__chart__').children[1].events.click();
 assert.equal(plots['live-plot'].layout.xaxis.side,'bottom','moving chart divider changes axis placement');
 assert.ok(plots['live-plot'].layout.height>590,'extra stacked axis expands canvas');
 elements['live-reset'].events.click();assert.ok(elements['live-plot'].reset);
 assert.equal(plots['flow-plot'].traces.length,2);
 assert.equal(plots['flow-plot'].layout.shapes.length,1);
 assert.equal(plots['flow-hist-plot'].layout.shapes.length,1);
 assert.match(elements['flow-data-status'].textContent,/historical method picks/);
 assert.equal(elements['flow-chips'].children.find(chip=>chip.dataset.key==='Nitrates').disabled,true,'excluded-leg values do not enable chips');
 assert.equal(elements['flow-chips'].children.find(chip=>chip.dataset.key==='SST (°C)').children[2].textContent,'4.00','chip latest value belongs to selected leg');
 elements['flow-span'].value='3h';elements['flow-span'].events.change();await tick();
 assert.ok(requested.some(url=>url.startsWith('data/w-3h.json')));
 assert.ok(requested.some(url=>url.startsWith('api/livesample/flow?window=3h&algorithm=coverage&count=1&spacing=15')));
 elements['flow-method'].value='rarity';elements['flow-target-count'].value='2';elements['flow-spacing'].value='30';elements['apply-flow-settings'].events.click();await tick();
 assert.ok(requested.some(url=>url.includes('algorithm=rarity&count=2&spacing=30')));
 assert.equal(plots['flow-plot'].layout.shapes.length,2,'flow chart recalculates the requested target count');
 assert.equal(plots['flow-hist-plot'].layout.shapes.length,2,'flow histogram marks every recalculated target');
 holdOneHour=true;elements['flow-span'].value='1h';elements['flow-span'].events.change();await tick();
 elements['flow-span'].value='3h';elements['flow-span'].events.change();await tick();
 releaseHistory();await tick();
 assert.match(plots['flow-plot'].layout.uirevision,/flow:3h:/,'late old window response cannot replace selected span');
 holdOneHour=false;
 assert.equal(plots['tsne-plot'].traces.length,2,'sampled overlays render');
 assert.match(plots['tsne-plot'].traces[0].text[0], /Temperature: 2.125 °C/);
 assert.match(plots['tsne-plot'].traces[0].text[0], /Practical salinity: 32.500/);
 assert.equal(plots['tsne-plot'].config.scrollZoom,true);
 assert.equal(plots['tsne-plot'].layout.dragmode,'pan');
 const margins=plots['tsne-plot'].layout.margin;assert.equal(margins.l+margins.r,margins.t+margins.b,'square container retains square plotting area');
 assert.equal(plots['tsne-plot'].traces[0].x.length,2,'bottle-only reference points hidden by default');
 elements['show-bottles'].checked=true;elements['show-bottles'].events.change();
 assert.ok(plots['tsne-plot'].traces.some(trace=>trace.name==='Collected bottles'));
 assert.equal(plots['tsne-plot'].traces[0].x.length,3);
 elements['embedding-color'].value='Nitrates';elements['embedding-color'].events.change();
 assert.ok(plots['tsne-plot'].traces.some(trace=>trace.name==='Not measured'),'missing colour property stays visible in grey');
 assert.ok(plots['tsne-plot'].traces.some(trace=>trace.name==='Nitrates'));
 assert.ok(!ids.includes('reference-count'),'stats bar removed');
 assert.ok(html.indexOf('id="model-warnings"')>html.indexOf('id="water-title"'),'warnings below charts');
 assert.match(html, /id="min-spacing" type="number" min="2" max="500"/);
 elements.algorithm.value='rarity';elements.algorithm.events.change();
 await intervals[0]();await tick();
 assert.equal(elements.algorithm.value,'rarity','poll preserves unapplied edits');
 elements['samples-file'].files=[{name:'samples.tsv',size:20,text:async()=> 'cast\tbottle\n001\t1\n'}];
 await elements['samples-file'].events.change();
 elements.telegram.checked=true;
 await elements['settings-form'].events.submit({preventDefault(){}});await tick();
 assert.equal(posts.length,1);assert.equal(posts[0].telegram,true);assert.equal(posts[0].tsv,'cast\tbottle\n001\t1\n');
 assert.equal(posts[0].algorithm,'rarity');
 await elements['settings-form'].events.submit({preventDefault(){}});await tick();
 assert.equal(posts.length,2);assert.ok(!('tsv' in posts[1]),'settings-only save retains samples');
 payload = {...payload,status:'Updating sampling model'};await intervals[0]();await tick();
 assert.equal(elements['tsne-plot'].hidden,true,'old model is hidden while reference settings change');
 assert.match(elements['tsne-empty'].textContent,/Computing/);
 fail=true;await intervals[0]();await tick();
 assert.equal(elements['service-state'].textContent,'Disconnected');
 assert.match(elements['target-summary'].textContent,/last successful update/);
 assert.equal(elements['request-error'].hidden,false);
 console.log('PASS: cast parameter reorder, stacked axes, pressure/depth compression, target transforms, flow lab filtering/window races/replay markers, square embeddings/property colors/bottle overlays, settings and TSV persistence, Telegram control, and disconnected state.');
})().catch(e=>{console.error(e);process.exitCode=1;});
