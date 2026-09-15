/* Adaptive track integration using real templates/scripts and isolated HTTP.
 * Usage: PYTHON=python3 node tests/track-browser.cjs /path/to/chromium
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
const pumpEvent = {id:'pump|test',leg,time_utc:new Date(t).toISOString(),end_utc:new Date(t+60000).toISOString(),activity:'TSG pump',event:'Pump off / low intake flow',comment:'Intake flow below 0.5 V'};
function manifest() {
  return {
    generated_utc:stamp(),default_window:'1h',local_tz:'UTC',title:'Refresh test',version:'test',
    ...(process.env.UPLOAD_UI||process.env.WIKI_UI?{history:{stamp:'test'}}:{}),
    track:{levels:[1,.1,.025,.005,0].map((spacing_km,i)=>({spacing_km,chunks:[{file:`data/track/level-${i}.json`,n:2,start:t,end:t+10000,leg:0,segment:0,bounds:[-78.01,75.99,-77.99,76.01]}]}))},
    windows:['1h','3h'].map(label=>({label,hours:label==='1h'?1:3,step_s:10,file:`data/w-${label}.json`,fine_file:`data/w-${label}-fine.json`})),
    legs:[{id:leg,index:0,label:'2026 Leg 3',year:2026,number:3,first_date:'20260904',last_date:'20260904',files:1}],live:leg,
    variables:[{name:'SST (°C)',unit:'°C',resolved:true,derived:false,tsg:true,coverage:{[leg]:true},source:'TSG'},...(process.env.DEPTH_UI?['Bottom depth (m)','Rosette depth (m)'].map(name=>({name,unit:'m',resolved:true,reverse:true,coverage:{[leg]:true},source:'Winches'})):[])],
    surprise:{scales:[],note:''},stations:process.env.STATION_SPAN_UI?[{kind:'CTD',leg,cast:'001',station:'Earlier',time:new Date(t-86400000).toISOString(),lat:76.01,lon:-78.01},{kind:'CTD',leg,cast:'002',station:'Current',time:new Date(t).toISOString(),lat:76,lon:-78}]:[],columns_seen:[],files:{total:1,latest:'ACSD_20260904.csv'},
    data_range:{start:new Date(t-10000).toISOString(),end:new Date(t+10000).toISOString()},
    latest:{lat:76,lon:-78},casts:{index:'data/casts/index.json'},calendar:{file:'data/calendar.json'},
    aggregates:{'1h':{file:'data/agg-1h.json'},'1d':{file:'data/agg-1d.json'}},intranet:[],
  };
}
function dataset(p) {
  if(p==='/data/manifest.json') return manifest();
  if(p.startsWith('/data/w-') || p.startsWith('/data/track/')) return {label:p.includes('3h')?'3h':'1h',step_s:10,n:2,t:[p.includes('3h')?t-3600000:t,t+10000],lat:[76,76.001],lon:[-78,-78.001],dist_km:[0,1],leg:[0,0],pump_low:process.env.TSG_UI?[false,false]:[false,true],vars:{'SST (°C)':process.env.TSG_UI?[1.234,null]:[generation,generation],...(process.env.DEPTH_UI?{'Bottom depth (m)':[25,100],'Rosette depth (m)':[25,100]}:{})},limits:{'SST (°C)':[0,10]},start:new Date(t).toISOString(),end:new Date(t+10000).toISOString()};
  if(p==='/data/calendar.json') return {events:[{...pumpEvent,id:'old-event',time_utc:new Date(t-86400000).toISOString(),event:'outside-span'},pumpEvent,{leg,time_utc:new Date(t).toISOString(),event:`event-${generation}`,activity:'CTD',station:'Test',lat:76,lon:-78}],pump_events:[pumpEvent],schedule:{rows:[]}};
  if(p.startsWith('/data/agg-')) return {variables:['SST (°C)'],rows:[{t,leg:0,lat:76,lon:-78,'SST (°C)':[generation,generation,generation,2]}]};
  const cast={id:`${leg}:CTD_001`,leg,kind:'CTD',cast:'001',station:'Test',time:new Date(t).toISOString(),lat:76,lon:-78,p:[1,2],units:{Temperature:'°C'},vars:{Temperature:[generation,generation]}};
  if(p==='/data/casts/index.json') return {variables:Object.keys(cast.vars),casts:[{...cast,vars:Object.keys(cast.vars),file:'data/casts/cast.json'}]};
  if(p==='/data/casts/cast.json') return cast;
  if(p==='/api/chat') return {messages:[],online:[],crew:[],typing:[]};
  if(p==='/api/live') return {};
}
const site={title:'Refresh test',version:'test',local_tz:process.env.TIMEZONE_UI?'America/Toronto':'UTC',default_window:'1h',geo_layers:[],intranet:[],links:[],asset_version:'test',plotly_version:'test'};
const rendered=spawnSync(process.env.PYTHON||'python3',['-c',
  'import sys,json; from jinja2 import Environment,FileSystemLoader; d=json.load(sys.stdin); print(Environment(loader=FileSystemLoader(sys.argv[1]),autoescape=True).get_template("index.html.j2").render(**d))',
  path.join(root,'dashboard/templates')],{input:JSON.stringify({site,m:manifest()}),encoding:'utf8'});
if(rendered.status!==0) throw Error(rendered.stderr);
const server=http.createServer((req,res)=>{
  const p=new URL(req.url,'http://localhost').pathname.replace(/^\/underway\//,'/'); requests.push(req.url);
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
    ws=new WebSocket(pages.find(p=>p.type==='page').webSocketDebuggerUrl);
    await new Promise(r=>ws.addEventListener('open',r,{once:true}));
    let id=0; const pending=new Map();
    ws.addEventListener('message',e=>{const m=JSON.parse(e.data);if(m.id){pending.get(m.id)?.(m);pending.delete(m.id);}});
    const call=(method,params={})=>new Promise(r=>{const n=++id;pending.set(n,r);ws.send(JSON.stringify({id:n,method,params}));});
    const evaluate=async expression=>{
      const r=await call('Runtime.evaluate',{expression,returnByValue:true,awaitPromise:true});
      if(r.result?.exceptionDetails) throw Error(r.result.exceptionDetails.exception?.description||expression);
      return r.result?.result?.value;
    };
    const until=async expression=>{for(let i=0;i<150;i++){if(await evaluate(expression))return;await wait(100);}throw Error(`Timed out: ${expression}; ${JSON.stringify(await evaluate("({errors:window.__errors,url:location.href,calendar:document.querySelector('#calendar')?.textContent?.slice(0,500)})"))}`);};
    const poll=async()=>{await evaluate('window.__poll()');await wait(250);};
    await call('Page.enable');
    if(process.env.TIMEZONE_UI) await call('Emulation.setTimezoneOverride',{timezoneId:process.env.TIMEZONE_UI});
    await call('Page.addScriptToEvaluateOnNewDocument',{source:`
      window.__errors=[];
      localStorage.setItem("uw:trackKm", "0");
      ${process.env.SUMMARY_UI?'':"localStorage.setItem('uw:panel','{}');"}
      addEventListener('error',e=>window.__errors.push(e.message));
      addEventListener('unhandledrejection',e=>window.__errors.push(String(e.reason)));
      const realInterval=window.setInterval;
      window.setInterval=(fn,ms,...args)=>{if(ms===30000)window.__poll=fn;return realInterval(fn,ms,...args);};
      const realTimeout=window.setTimeout;
      window.setTimeout=(fn,ms,...args)=>{if(ms===4000||ms===20000)window.__chatPoll=fn;return realTimeout(fn,ms,...args);};
    `});
    const initialWidth=Number(process.env.UI_WIDTH)||(process.env.HEADER_UI||process.env.WIKI_UI?1400:390);
    await call('Emulation.setDeviceMetricsOverride',{width:initialWidth,height:844,deviceScaleFactor:1,mobile:initialWidth<=640});
    await call('Page.navigate',{url:`http://127.0.0.1:${server.address().port}/${process.env.UI_PREFIX?'underway/':''}`});
    await until('window.UW && document.querySelector("#connection").textContent.includes("Underway")');
    assert.equal(await evaluate('!!window.UW.state.raw'),false);
    failures.delete('/data/manifest.json'); await poll();
    assert.equal(await evaluate('!!window.UW.state.raw'),false);
    failures.clear(); await evaluate('window.dispatchEvent(new Event("online"))');
    await until(`window.UW.state.raw?.vars["SST (°C)"][0]===${process.env.TSG_UI?'1.234':'1'}`);
    await evaluate('window.__mapErrors=[]; window.UW.mapView?.map?.on("error",e=>window.__mapErrors.push(String(e.error)))');
    console.log('PASS initial load retries without reload');
    await until('window.UW.mapView?.map && document.querySelector("#trackstatus")?.textContent.includes("visible track points")');
    if(process.env.STATION_SPAN_UI){
      assert.deepEqual(await evaluate('UW.state.stationList.map(s=>s.station)'),['Earlier','Current']);
      await evaluate('UW.setSpan("3h")');
      await until('UW.state.raw?.label==="3h"');
      assert.deepEqual(await evaluate('UW.state.stationList.map(s=>s.station)'),['Earlier','Current']);
      assert.deepEqual(await evaluate('window.__errors'),[]);
      console.log('PASS map stations stay visible when the track span changes');return;
    }
    if(process.env.TSG_UI){
      await until('UW.mapView.traces.base.some(t=>t.name==="track readings")');
      assert.deepEqual(await evaluate('UW.mapView.traces.base.find(t=>t.name==="track readings").lat'),[76,null]);
      assert.equal(await evaluate('UW.mapView.fc.base.circles.features.filter(f=>UW.mapView.traces.base[f.properties.t]?.name==="track readings").length'),1);
      assert.equal(await evaluate('!!document.querySelector("#map .map-legend")'),true);
      await evaluate('UW.state.track=false;UW.renderMap()');
      assert.equal(await evaluate('document.querySelector("#map .map-legend")'),null);
      assert.equal(await evaluate('UW.mapLegend'),null);
      assert.deepEqual(await evaluate('window.__errors'),[]);
      console.log('PASS missing TSG readings have no map markers and disabled track has no legend');return;
    }
    assert.ok(requests.some(p=>p.includes('/data/track/')));
    assert.ok(!requests.some(p=>p.includes('-fine.json')));
    assert.equal(await evaluate('!!document.querySelector("#trackstep, #trackstepsel, .maptrack")'),false);
    console.log('PASS initial map loads viewport track separately from coarse chart data');
    await evaluate('UW.mapView.map.jumpTo({center:[-78,76],zoom:3})');
    await until('document.querySelector("#trackstatus")?.textContent.includes("1 km detail")');
    assert.ok(requests.some(p=>p.includes('level-0.json')));
    await evaluate('UW.mapView.map.jumpTo({center:[-78,76],zoom:8})');
    await until('document.querySelector("#trackstatus")?.textContent.includes("100 m detail")');
    await evaluate('UW.mapView.map.jumpTo({center:[-78,76],zoom:12})');
    await until('document.querySelector("#trackstatus")?.textContent.includes("5 m detail")');
    assert.ok(requests.some(p=>p.includes('level-3.json')));
    const view=await evaluate('({zoom:UW.mapView.map.getZoom(),center:UW.mapView.map.getCenter().toArray()})');
    assert.equal(view.zoom,12); assert.deepEqual(view.center,[-78,76]);
    console.log('PASS automatic zoom changes resolution and preserves map view');
    hold.add('/data/track/level-4.json');
    await evaluate('UW.mapView.map.jumpTo({center:[-78,76],zoom:16})');
    for(let i=0;i<100&&!held.length;i++) await wait(50);
    assert.ok(held.length,'native track request held');
    await evaluate('UW.setSpan("3h")');
    await until('UW.state.win==="3h" && UW.state.raw.label==="3h"');
    hold.clear(); for(const h of held.splice(0)){h.value.lat=[0,0];h.res.setHeader('Content-Type','application/json');h.res.end(JSON.stringify(h.value));}
    await until('document.querySelector("#trackstatus")?.textContent.includes("visible track points")');
    assert.ok((await evaluate('document.querySelector("#trackstatus").textContent')).includes('2 visible track points'));
    assert.ok(!requests.some(p=>p.includes('-fine.json')));
    assert.deepEqual(await evaluate('window.__errors'),[]);
    console.log('PASS span changes supersede pending detail; no fine download or uncaught browser errors');
    await evaluate('UW.showTab("calendar")');
    await wait(500);
    await evaluate('document.querySelector("#calview [data-v=timeline]").click()');
    await until('!!document.querySelector("#cal-plot")?._fullLayout?.xaxis');
    const checkAxis=async()=>{
      const info=await evaluate('(()=>{const gd=document.querySelector("#cal-plot"),s=UW.spanFilter();return {range:gd.layout.xaxis.range.map(x=>+new Date(x)),expected:[s.start,s.end].map(x=>+new Date(UW.plotDate(UW.shipAxis(x))))}})()');
      assert.deepEqual(info.range,info.expected);
    };
    await checkAxis();
    assert.equal(await evaluate('document.querySelector("#cal-plot").data.some(trace=>trace.text?.some(text=>text.includes("outside-span")))'),true);
    await evaluate('Plotly.relayout(document.querySelector("#cal-plot"), {"xaxis.range":["2020-01-01","2020-01-02"]})');
    await evaluate('UW.setSpan("1h")');
    await until('UW.state.raw.label==="1h"');
    await wait(300);
    await checkAxis();
    console.log('PASS event axis matches span and resets remembered zoom on span change');
  } finally {
    ws?.close();child?.kill();server.closeAllConnections();server.close();clearTimeout(watchdog);
    console.log(`Temporary browser profile: ${profile}`);
  }
})().catch(e=>{console.error(e);process.exitCode=1;});
