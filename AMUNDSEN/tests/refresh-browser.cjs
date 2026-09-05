/* Integration check with real templates/scripts and an isolated synthetic HTTP
 * source. Usage: PYTHON=python3 node tests/refresh-browser.cjs /path/to/chromium
 * Needs Node 22+, Chromium and Jinja2; never contacts the deployed dashboard. */
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const http = require('node:http');
const {spawn, spawnSync} = require('node:child_process');
const root = path.resolve(__dirname, '..');
const chrome = process.argv[2];
if (!chrome) throw Error('Pass the Chromium executable path');
const wait = ms => new Promise(r => setTimeout(r,ms));
let generation=1;
const failures=new Set(['/data/manifest.json','/data/w-1h.json']), requests=[];
const hold=new Set(), held=[];
const leg='2026_LEG_03', t=Date.parse('2026-09-04T12:00:00Z');
const stamp = () => `2026-09-04T12:00:0${generation}Z`;
function manifest() {
  return {
    generated_utc:stamp(),default_window:'1h',local_tz:'UTC',title:'Refresh test',version:'test',
    windows:['1h','3h'].map(label=>({label,hours:label==='1h'?1:3,step_s:10,file:`data/w-${label}.json`})),
    legs:[{id:leg,index:0,label:'2026 Leg 3',year:2026,number:3,first_date:'20260904',last_date:'20260904',files:1}],live:leg,
    variables:[{name:'SST (°C)',unit:'°C',resolved:true,derived:false,coverage:{[leg]:true},source:'TSG'}],
    surprise:{scales:[],note:''},stations:[],columns_seen:[],files:{total:1,latest:'ACSD_20260904.csv'},
    data_range:{start:new Date(t-10000).toISOString(),end:new Date(t+10000).toISOString()},
    latest:{lat:76,lon:-78},casts:{index:'data/casts/index.json'},calendar:{file:'data/calendar.json'},
    aggregates:{'1h':{file:'data/agg-1h.json'},'1d':{file:'data/agg-1d.json'}},intranet:[],
  };
}
function dataset(p) {
  if(p==='/data/manifest.json') return manifest();
  if(p.startsWith('/data/w-')) return {label:p.includes('3h')?'3h':'1h',step_s:10,n:2,t:[t,t+10000],lat:[76,76.001],lon:[-78,-78.001],dist_km:[0,1],leg:[0,0],vars:{'SST (°C)':[generation,generation]},limits:{'SST (°C)':[0,10]},start:new Date(t).toISOString(),end:new Date(t+10000).toISOString()};
  if(p==='/data/calendar.json') return {events:[{leg,time_utc:new Date(t).toISOString(),event:`event-${generation}`,activity:'CTD',station:'Test',lat:76,lon:-78}],schedule:{rows:[]}};
  if(p.startsWith('/data/agg-')) return {variables:['SST (°C)'],rows:[{t,leg:0,lat:76,lon:-78,'SST (°C)':[generation,generation,generation,2]}]};
  const cast={id:`${leg}:CTD_001`,leg,kind:'CTD',cast:'001',station:'Test',time:new Date(t).toISOString(),lat:76,lon:-78,p:[1,2],units:{Temperature:'°C'},vars:{Temperature:[generation,generation]}};
  if(p==='/data/casts/index.json') return {variables:['Temperature'],casts:[{...cast,vars:['Temperature'],file:'data/casts/cast.json'}]};
  if(p==='/data/casts/cast.json') return cast;
  if(p==='/api/chat') return {messages:[],online:[],crew:[],typing:[]};
  if(p==='/api/live') return {port:0};
}
const site={title:'Refresh test',version:'test',local_tz:'UTC',default_window:'1h',geo_layers:[],intranet:[],links:[],asset_version:'test',plotly_version:'test'};
const rendered=spawnSync(process.env.PYTHON||'python3',['-c',
  'import sys,json; from jinja2 import Environment,FileSystemLoader; d=json.load(sys.stdin); print(Environment(loader=FileSystemLoader(sys.argv[1]),autoescape=True).get_template("index.html.j2").render(**d))',
  path.join(root,'dashboard/templates')],{input:JSON.stringify({site,m:manifest()}),encoding:'utf8'});
if(rendered.status!==0) throw Error(rendered.stderr);
const server=http.createServer((req,res)=>{
  const p=new URL(req.url,'http://localhost').pathname; requests.push(req.url);
  if(failures.has(p)){res.writeHead(503);res.end('temporary failure');return;}
  if(p==='/'){res.setHeader('Content-Type','text/html');res.end(rendered.stdout);return;}
  if(p.startsWith('/static/')){
    const file=path.join(root,'dashboard',p.slice(1));
    if(fs.existsSync(file)){res.setHeader('Content-Type',p.endsWith('.js')?'application/javascript':'text/css');res.end(fs.readFileSync(file));return;}
  }
  const value=dataset(p);
  if(hold.has(p)){held.push({res,value});return;}
  if(value){res.setHeader('Content-Type','application/json');res.end(JSON.stringify(value));}
  else {res.writeHead(404);res.end();}
});
let child,ws;
const profile=fs.mkdtempSync(path.join(os.tmpdir(),'underway-browser-'));
const watchdog=setTimeout(()=>{child?.kill();server.closeAllConnections();server.close();process.exitCode=1;},90000);
(async()=>{
  try {
    await new Promise(r=>server.listen(0,'127.0.0.1',r));
    let stderr='';
    child=spawn(chrome,['--no-sandbox','--headless','--enable-unsafe-swiftshader','--disable-dev-shm-usage','--remote-debugging-port=0',`--user-data-dir=${profile}`,'about:blank']);
    child.stderr.on('data',d=>{stderr+=d;});
    for(let i=0;i<100&&!stderr.includes('DevTools listening');i++) await wait(100);
    const endpoint=stderr.match(/DevTools listening on (ws:\/\/\S+)/)?.[1];
    if(!endpoint) throw Error(stderr);
    const pages=await(await fetch(`http://${new URL(endpoint).host}/json/list`)).json();
    ws=new WebSocket(pages[0].webSocketDebuggerUrl);
    await new Promise(r=>ws.addEventListener('open',r,{once:true}));
    let id=0; const pending=new Map();
    ws.addEventListener('message',e=>{const m=JSON.parse(e.data);if(m.id){pending.get(m.id)?.(m);pending.delete(m.id);}});
    const call=(method,params={})=>new Promise(r=>{const n=++id;pending.set(n,r);ws.send(JSON.stringify({id:n,method,params}));});
    const evaluate=async expression=>{
      const r=await call('Runtime.evaluate',{expression,returnByValue:true,awaitPromise:true});
      if(r.result?.exceptionDetails) throw Error(r.result.exceptionDetails.exception?.description||expression);
      return r.result?.result?.value;
    };
    const until=async expression=>{for(let i=0;i<150;i++){if(await evaluate(expression))return;await wait(100);}throw Error(`Timed out: ${expression}`);};
    const poll=async()=>{await evaluate('window.__poll()');await wait(250);};
    await call('Page.enable');
    await call('Page.addScriptToEvaluateOnNewDocument',{source:`
      window.__errors=[];
      addEventListener('error',e=>window.__errors.push(e.message));
      addEventListener('unhandledrejection',e=>window.__errors.push(String(e.reason)));
      const realInterval=window.setInterval;
      window.setInterval=(fn,ms,...args)=>{if(ms===30000)window.__poll=fn;return realInterval(fn,ms,...args);};
    `});
    await call('Emulation.setDeviceMetricsOverride',{width:390,height:844,deviceScaleFactor:1,mobile:true});
    await call('Page.navigate',{url:`http://127.0.0.1:${server.address().port}/`});
    await until('window.UW && document.querySelector("#connection").textContent.includes("Underway")');
    assert.equal(await evaluate('!!window.UW.state.raw'),false);
    failures.delete('/data/manifest.json'); await poll();
    assert.equal(await evaluate('!!window.UW.state.raw'),false);
    failures.clear(); await evaluate('window.dispatchEvent(new Event("online"))');
    await until('window.UW.state.raw?.vars["SST (°C)"][0]===1');
    await evaluate('window.__mapErrors=[]; document.querySelector("#map")._fullLayout?.map?._subplot?.map?.on("error",e=>window.__mapErrors.push(String(e.error)))');
    console.log('PASS initial load retries without reload');

    await evaluate('window.UW.showTab("calendar")');
    await until('document.querySelector("#calendar").textContent.includes("event-1")');
    generation=2;failures.add('/data/w-1h.json');await poll();
    assert.equal(await evaluate('window.UW.M.generated_utc'),'2026-09-04T12:00:01Z');
    assert.equal(await evaluate('window.UW.state.raw.vars["SST (°C)"][0]'),1);
    failures.clear();await poll();
    await until('document.querySelector("#calendar").textContent.includes("event-2")');
    assert.equal(await evaluate('window.UW.M.generated_utc'),'2026-09-04T12:00:02Z');
    console.log('PASS failed refresh retains matching metadata/data; open Agenda updates on retry');

    generation=3;failures.add('/data/calendar.json');await poll();
    assert.equal(await evaluate('document.querySelector("#calendar").textContent.includes("event-2")'),true);
    assert.equal(await evaluate('document.querySelector("#connection").textContent.includes("Agenda")'),true);
    failures.clear();await poll();
    await until('document.querySelector("#calendar").textContent.includes("event-3")');
    console.log('PASS Agenda failure preserves previous content and retries the same generation');

    await evaluate('window.UW.showTab("table")');
    failures.add('/data/agg-1h.json');
    await evaluate('document.querySelector("#aggrule [data-r=\\"1h\\"]").click()');
    await until('document.querySelector("#connection").textContent.includes("Table")');
    failures.clear();await poll();
    await until('document.querySelector("#aggtable").textContent.includes("3.00")');
    generation=4;await poll();
    await until('document.querySelector("#aggtable").textContent.includes("4.00")');
    console.log('PASS failed aggregate is retried; open Table updates');

    await evaluate('window.UW.showTab("casts")');
    await until('document.querySelector("#castlist input")');
    await evaluate('document.querySelector("#castlist input").click()');
    await until('document.querySelector("#castplots .js-plotly-plot")?.data?.[0]?.x?.[0]===4');
    generation=5;await poll();
    await until('document.querySelector("#castplots .js-plotly-plot")?.data?.[0]?.x?.[0]===5');
    assert.ok(requests.some(u=>u==='/data/casts/cast.json?v=2026-09-04T12%3A00%3A05Z'));
    console.log('PASS revised cast is downloaded and plotted without reopening the tab');
    generation=6;failures.add('/data/casts/cast.json');await poll();
    assert.equal(await evaluate('document.querySelector("#castplots .js-plotly-plot").data[0].x[0]'),5);
    assert.equal(await evaluate('document.querySelector("#connection").textContent.includes("Casts")'),true);
    failures.clear();await poll();
    await until('document.querySelector("#castplots .js-plotly-plot")?.data?.[0]?.x?.[0]===6');
    console.log('PASS failed cast refresh retains the old plot and retries');

    hold.add('/data/w-3h.json');
    await evaluate('document.querySelector("#span").value="1";document.querySelector("#span").dispatchEvent(new Event("change"))');
    for(let i=0;i<100&&!held.length;i++) await wait(20);
    assert.equal(held.length,1);
    await evaluate('document.querySelector("#span").value="0";document.querySelector("#span").dispatchEvent(new Event("change"))');
    await wait(250);
    hold.clear();for(const {res,value} of held.splice(0)){res.setHeader('Content-Type','application/json');res.end(JSON.stringify(value));}
    await wait(250);
    assert.equal(await evaluate('window.UW.state.raw.label'),'1h');
    assert.equal(await evaluate('window.UW.state.win'),'1h');
    console.log('PASS delayed window response does not overwrite a newer span selection');
    const before=requests.filter(u=>u.startsWith('/data/casts/cast.json')).length;
    await poll();
    assert.equal(requests.filter(u=>u.startsWith('/data/casts/cast.json')).length,before);
    const errors=await evaluate('window.__errors');
    if(errors.length) console.error(stderr.slice(0,4000),await evaluate('window.__mapErrors'));
    assert.deepEqual(errors,[]);
    console.log('PASS unchanged-generation polling reuses data; no uncaught browser errors');
  } finally {
    ws?.close();child?.kill();server.closeAllConnections();server.close();clearTimeout(watchdog);
    console.log(`Temporary browser profile: ${profile}`);
  }
})().catch(e=>{console.error(e);process.exitCode=1;});
