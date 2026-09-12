/* Integration check with real templates/scripts and an isolated synthetic HTTP
 * source. Usage: PYTHON=python3 node tests/refresh-browser.cjs /path/to/chromium
 * PROFILE_UI=check runs the isolated mobile refresh profile and regression assertions.
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
let rejectFeedback=true; const feedbackRows=[];
let rejectLiveConfig=true;
const uploadBatches=[], uploadFiles=[]; let rejectUpload=true;
const liveCast=(pressure_col, pressure)=>({started:1,n:2,n_raw:2,max_p:pressure,depth_like:true,t:[1,2],columns:['scan',pressure_col,'temperature'],pressure_col,cols:{scan:[1,2],[pressure_col]:[pressure,pressure],temperature:[3,4]}});
const liveData={port:5555,columns:['scan','depth','temperature'],pressure_col:'depth',current:liveCast('depth',25),last:liveCast('depth_m',10)};
const failures=new Set(['/data/manifest.json','/data/w-1h.json']), requests=[];
const chatMessages=Array.from({length:45},(_,i)=>({id:i+1,t:1700000000+i,name:'Sailor',text:`Message ${i+1}: reading a long conversation.`,emoji:''}));
const hold=new Set(), held=[];
const leg='2026_LEG_03', t=Date.parse('2026-09-04T12:00:00Z');
const stamp = () => `2026-09-04T12:00:0${generation}Z`;
const pumpEvent = {id:'pump|test',leg,time_utc:new Date(t).toISOString(),end_utc:new Date(t+60000).toISOString(),activity:'TSG pump',event:'Pump off / low intake flow',comment:'Intake flow below 0.5 V'};
function manifest() {
  return {
    generated_utc:stamp(),default_window:'1h',local_tz:'UTC',title:'Refresh test',version:'test',
    windows:['1h','3h'].map(label=>({label,hours:label==='1h'?1:3,step_s:10,file:`data/w-${label}.json`})),
    legs:[{id:leg,index:0,label:'2026 Leg 3',year:2026,number:3,first_date:'20260904',last_date:'20260904',files:1}],live:leg,
    variables:[{name:'SST (°C)',unit:'°C',resolved:true,derived:false,tsg:true,coverage:{[leg]:true},source:'TSG'},...(process.env.DEPTH_UI?['Bottom depth (m)','Rosette depth (m)'].map(name=>({name,unit:'m',resolved:true,reverse:true,coverage:{[leg]:true},source:'Winches'})):[])],
    surprise:{scales:[],note:''},stations:[],columns_seen:[],files:{total:1,latest:'ACSD_20260904.csv'},
    data_range:{start:new Date(t-10000).toISOString(),end:new Date(t+10000).toISOString()},
    latest:{lat:76,lon:-78},casts:{index:'data/casts/index.json'},calendar:{file:'data/calendar.json'},
    aggregates:{'1h':{file:'data/agg-1h.json'},'1d':{file:'data/agg-1d.json'}},intranet:[],
  };
}
function dataset(p) {
  if((process.env.NATURE_UI||process.env.WIKI_UI||process.env.UPLOAD_UI) && p.startsWith('/data/history/')) {
    if(p.endsWith('/index.json')) return {topics:[],pages:[]};
    if(p.endsWith('/subjects.json')) return {subjects:[{name:'Seal',domain:'biology',kind:'taxon'},{name:'Rock',domain:'geology',kind:'mineral'}]};
    if(p.endsWith('/observations.json')) return {observations:[{id:'seal',subject:'Seal',date:'2020-01-01',lat:76,lon:-78},{id:'rock',subject:'Rock',date:'2020-01-01',lat:77,lon:-79}]};
    return {};
  }
  if(p==='/api/nature/journal') return {observations:[]};
  if(p==='/data/manifest.json') return manifest();
  if(p.startsWith('/data/w-')) return {label:p.includes('3h')?'3h':'1h',step_s:10,n:2,t:[t,t+10000],lat:[76,76.001],lon:[-78,-78.001],dist_km:[0,1],leg:[0,0],pump_low:[false,true],vars:{'SST (°C)':[generation,generation],...(process.env.DEPTH_UI?{'Bottom depth (m)':[25,100],'Rosette depth (m)':[25,100]}:{})},limits:{'SST (°C)':[0,10]},start:new Date(t).toISOString(),end:new Date(t+10000).toISOString()};
  if(p==='/data/calendar.json') return {events:[pumpEvent,{leg,time_utc:new Date(t).toISOString(),event:`event-${generation}`,activity:'CTD',station:'Test',lat:76,lon:-78}],pump_events:[pumpEvent],schedule:{rows:[]}};
  if(p.startsWith('/data/agg-')) return {variables:['SST (°C)'],rows:[{t,leg:0,lat:76,lon:-78,'SST (°C)':[generation,generation,generation,2]}]};
  const cast={id:`${leg}:CTD_001`,leg,kind:'CTD',cast:'001',station:'Test',time:new Date(t).toISOString(),lat:76,lon:-78,p:[1,2],units:{Temperature:'°C'},vars:{Temperature:[generation,generation]}};
  if(process.env.SINGLE_UI) {
    cast.vars={Temperature:[2,3],Salinity:[30,31],'Sigma-t':[23,24],Fluorescence:[0.1,0.2],CDOM:[1,2],Oxygen:[280,290]};
    cast.units={Temperature:'°C',Salinity:'PSU','Sigma-t':'kg/m³',Fluorescence:'mg/m³',CDOM:'ppb',Oxygen:'µmol/kg'};
    cast.log_url='data/casts/RosetteSheet_001.xlsx';
  }
  if(process.env.TRANSECT_UI) {
    const casts=Array.from({length:30},(_,i)=>({...cast,id:`${leg}:CTD_${String(i+1).padStart(3,'0')}`,cast:String(i+1),station:`Station ${i+1}`,lat:76+i/100,lon:-78-i/100,file:`data/casts/cast-${i}.json`}));
    if(p==='/data/casts/index.json') return {variables:['Temperature'],casts:casts.map(c=>({...c,vars:['Temperature']}))};
    const match=p.match(/^\/data\/casts\/cast-(\d+)\.json$/); if(match) return casts[+match[1]];
  }
  if(p==='/data/casts/index.json') return {variables:Object.keys(cast.vars),casts:[{...cast,vars:Object.keys(cast.vars),file:'data/casts/cast.json'}]};
  if(p==='/data/casts/cast.json') return cast;
  if(p==='/api/chat') return {messages:[],online:[],crew:[],typing:[]};
  if(p==='/api/live') return liveData;
}
const site={title:'Refresh test',version:'test',local_tz:process.env.TIMEZONE_UI?'America/Toronto':'UTC',default_window:'1h',geo_layers:[],intranet:[],links:[],asset_version:'test',plotly_version:'test'};
const rendered=spawnSync(process.env.PYTHON||'python3',['-c',
  'import sys,json; from jinja2 import Environment,FileSystemLoader; d=json.load(sys.stdin); print(Environment(loader=FileSystemLoader(sys.argv[1]),autoescape=True).get_template("index.html.j2").render(**d))',
  path.join(root,'dashboard/templates')],{input:JSON.stringify({site,m:manifest()}),encoding:'utf8'});
if(rendered.status!==0) throw Error(rendered.stderr);
const server=http.createServer((req,res)=>{
  const p=new URL(req.url,'http://localhost').pathname.replace(/^\/underway\//,'/'); requests.push(req.url);
  if(process.env.UPLOAD_UI && p.startsWith('/api/nature/')) {
    res.setHeader('Content-Type','application/json');
    const q=new URL(req.url,'http://localhost').searchParams;
    if(p==='/api/nature/share') {const folder=q.get('path')??'Pictures';res.end(JSON.stringify({path:folder,folders:folder==='Pictures'?[{name:'Destination'}]:[],files:folder.endsWith('/batch')?[{name:'001-a.jpg'},{name:'002-b.jpg'}]:[]}));return;}
    if(p==='/api/nature/import') {res.end(JSON.stringify({jobs:[],watches:[],licences:{attribution:'Attribution'}}));return;}
    if(p==='/api/nature/upload'||p==='/api/nature/upload/file') {
      const chunks=[];req.on('data',c=>chunks.push(c));req.on('end',()=>{
        assert.equal(req.headers['x-photo-upload'],'1');
        if(p.endsWith('/file')) {const i=Number(q.get('index'));uploadFiles.push(i);if(i===1&&rejectUpload){rejectUpload=false;res.writeHead(503);res.end(JSON.stringify({error:'Test interrupted upload'}));}else res.end(JSON.stringify({ok:true}));}
        else {const spec=JSON.parse(Buffer.concat(chunks));uploadBatches.push(spec);res.end(JSON.stringify({id:'test-batch',path:spec.parent+'/batch'}));}
      });return;
    }
  }
  if(p==='/api/chat' && process.env.TRANSECT_UI) {
    const since=Number(new URL(req.url,'http://localhost').searchParams.get('since')||0);
    res.setHeader('Content-Type','application/json');res.end(JSON.stringify({messages:chatMessages.filter(m=>m.id>since),online:[],crew:[],typing:[]}));return;
  }
  if(p==='/api/feedback' && req.method==='POST') {
    let body=''; req.on('data', chunk=>body+=chunk);
    req.on('end',()=>{
      res.setHeader('Content-Type','application/json');
      if(rejectFeedback) { res.writeHead(503); res.end(JSON.stringify({error:'Temporary failure'})); }
      else { const row=JSON.parse(body); feedbackRows.push(row); res.end(JSON.stringify({ok:true,id:row.id})); }
    }); return;
  }
  if(p==='/api/live' && req.method==='POST'){
    req.resume();res.setHeader('Content-Type','application/json');
    res.writeHead(rejectLiveConfig?400:200);
    res.end(JSON.stringify(rejectLiveConfig?{error:'Port already in use'}:liveData));return;
  }
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
    const until=async expression=>{for(let i=0;i<150;i++){if(await evaluate(expression))return;await wait(100);}throw Error(`Timed out: ${expression}; ${JSON.stringify(await evaluate("({errors:window.__errors,url:location.href})"))}`);};
    const poll=async()=>{await evaluate('window.__poll()');await wait(250);};
    await call('Page.enable');
    if(process.env.TIMEZONE_UI) await call('Emulation.setTimezoneOverride',{timezoneId:process.env.TIMEZONE_UI});
    await call('Page.addScriptToEvaluateOnNewDocument',{source:`
      window.__errors=[];
      ${process.env.SUMMARY_UI?'':"localStorage.setItem('uw:panel','{}');"}
      addEventListener('error',e=>window.__errors.push(e.message));
      addEventListener('unhandledrejection',e=>window.__errors.push(String(e.reason)));
      const realInterval=window.setInterval;
      window.setInterval=(fn,ms,...args)=>{if(ms===30000)window.__poll=fn;return realInterval(fn,ms,...args);};
      const realTimeout=window.setTimeout;
      window.setTimeout=(fn,ms,...args)=>{if(ms===4000||ms===20000)window.__chatPoll=fn;return realTimeout(fn,ms,...args);};
    `});
    await call('Emulation.setDeviceMetricsOverride',{width:Number(process.env.UI_WIDTH)||390,height:844,deviceScaleFactor:1,mobile:!process.env.UI_WIDTH});
    await call('Page.navigate',{url:`http://127.0.0.1:${server.address().port}/${process.env.UI_PREFIX?'underway/':''}`});
    await until('window.UW && document.querySelector("#connection").textContent.includes("Underway")');
    assert.equal(await evaluate('!!window.UW.state.raw'),false);
    failures.delete('/data/manifest.json'); await poll();
    assert.equal(await evaluate('!!window.UW.state.raw'),false);
    failures.clear(); await evaluate('window.dispatchEvent(new Event("online"))');
    await until('window.UW.state.raw?.vars["SST (°C)"][0]===1');
    await evaluate('window.__mapErrors=[]; window.UW.mapView?.map?.on("error",e=>window.__mapErrors.push(String(e.error)))');
    console.log('PASS initial load retries without reload');
    if(process.env.DEPTH_UI) {
      assert.deepEqual(await evaluate('[...document.querySelectorAll("#maplayers .tools button")].map(b=>b.id)'),['mapdetailsopen','mapexport','mapreset','mapnone','mapfull']);
      assert.equal(await evaluate('getComputedStyle(document.querySelector("#maplayers .tools")).justifyContent'),'flex-end');
      assert.equal(await evaluate('document.querySelector("#trackstep").closest("#controls-underway")!==null'),true);
      assert.equal(await evaluate('(()=>{const tools=document.querySelector("#maplayers .tools").getBoundingClientRect(),colour=document.querySelector(".mapcolour").getBoundingClientRect();return tools.bottom<=colour.top+1})()'),true);
      if(process.env.UI_WIDTH)assert.equal(await evaluate('(()=>{const a=getComputedStyle(document.querySelector(".group.span")),b=getComputedStyle(document.querySelector(".group.maptrack"));return ["backgroundColor","padding","borderRadius","gap"].every(k=>a[k]===b[k])&&getComputedStyle(document.querySelector("#span")).width===getComputedStyle(document.querySelector("#trackstep")).width&&getComputedStyle(document.querySelector("#spanlabel")).fontFamily===getComputedStyle(document.querySelector("#tracksteplabel")).fontFamily})()'),true);
      if(process.env.UI_WIDTH)assert.equal(await evaluate('(()=>{const span=document.querySelector("#span"),track=document.querySelector("#trackstep"),size=()=>[span.getBoundingClientRect().width,track.getBoundingClientRect().width,span.getBoundingClientRect().height,track.getBoundingClientRect().height,document.querySelector(".maptrack").getBoundingClientRect().width];const before=size();for(let i=0;i<=7;i++){track.value=i;track.dispatchEvent(new Event("input"));if(JSON.stringify(size())!==JSON.stringify(before))return false;}return before[0]===before[1]&&before[2]===before[3]})()'),true);
      assert.equal(await evaluate('getComputedStyle(document.querySelector("#trackstepsel")).display!=="none"'),!process.env.UI_WIDTH);
      await evaluate('const select=document.querySelector("#trackstepsel");select.value="2";select.dispatchEvent(new Event("change"))');
      assert.equal(await evaluate('document.querySelector("#trackstep").value'), '2');
      assert.equal(await evaluate('document.querySelector("#mapdetails").open'),false);
      await evaluate('document.querySelector("#mapdetailsopen").click()');
      assert.equal(await evaluate('document.querySelector("#mapdetails").open'),true);
      assert.equal(await evaluate('getComputedStyle(document.querySelector("#mapfoot")).fontSize===getComputedStyle(document.querySelector("#mapattrib")).fontSize'),true);
      await evaluate('document.querySelector("#mapdetailsclose").click()');
      for(const name of ['Bottom depth (m)','Rosette depth (m)']) {
        await evaluate(`UW.showTab('underway');UW.selectColour(${JSON.stringify(name)});document.querySelector('[data-name="${name}"]').scrollIntoView();`);
        await until(`!!document.querySelector('[data-name="${name}"] .plot')?._fullLayout`);
        const inspect=()=>evaluate(`(()=>{const p=document.querySelector('[data-name="${name}"] .plot');return {range:p._fullLayout.yaxis.range,y:p.data[0].y,raw:p.data[0].customdata,reverse:p.data[0].marker.reversescale}})()`);
        const normal=await inspect();assert.equal(normal.range[1],0);assert(normal.range[0]>100);assert.equal(normal.reverse,true);
        await evaluate(`document.querySelector('[data-name="${name}"] .depthscale').click()`);
        await until(`document.querySelector('[data-name="${name}"] .plot').data[0].y[1]===10`);
        const compressed=await inspect();assert.deepEqual(compressed.y,[5,10]);assert.deepEqual(compressed.raw,[25,100]);assert.equal(compressed.range[1],0);
        await evaluate(`document.querySelector('[data-name="${name}"] .reset').click()`);
        await wait(100);assert.equal((await inspect()).range[1],0);
      }
      await evaluate(`window.__download=[];HTMLAnchorElement.prototype.click=function(){window.__download.push({url:this.href,name:this.download})};document.querySelector('[data-name="Rosette depth (m)"] .plot-export').click()`);
      await until('!!document.querySelector(".export-canvas")?.data && !document.querySelector("[data-format=png]").disabled');
      assert.deepEqual(await evaluate('[...document.querySelectorAll(\'[data-name="Rosette depth (m)"] .tools button\')].filter(b=>b.matches(".plot-export,.reset,.min,.wide")).map(b=>b.textContent)'),['⇩','⟲','—','⤢']);
      await evaluate('document.querySelector(".plot-export-dialog").style.width="800px";document.querySelector(".plot-export-dialog").style.height="700px";const s=document.querySelector(".plot-export-dialog input[type=range]");s.value="3";s.dispatchEvent(new Event("input"))');
      await wait(200);
      assert.equal(await evaluate('(()=>{const c=document.querySelector(".plot-export-controls").getBoundingClientRect(),p=document.querySelector(".export-canvas").getBoundingClientRect();return c.right<=p.left && getComputedStyle(document.querySelector(".plot-export-dialog")).opacity==="1"})()'),true);
      await evaluate('for(const [name,value] of Object.entries({width:640,height:820})){const input=document.querySelector(`.plot-export-dialog [name=${name}]`);input.value=value;input.dispatchEvent(new Event("change"))}');
      await wait(200);
      const size=await evaluate('({w:document.querySelector(".export-canvas").clientWidth,h:document.querySelector(".export-canvas").clientHeight})');
      assert.deepEqual(size,{w:640,h:820});
      await evaluate('document.querySelector("[data-format=png]").click()');
      await until('window.__download.length===1');
      assert.match(await evaluate('window.__download[0].name'),/\.png$/);
      assert.deepEqual(await evaluate('new Promise((resolve,reject)=>{const im=new Image;im.onload=()=>resolve([im.width,im.height]);im.onerror=reject;im.src=window.__download[0].url})'),[size.w*3,size.h*3]);
      await evaluate('document.querySelector("[data-format=svg]").click()');
      await until('window.__download.length===2');
      assert.match(await evaluate('window.__download[1].name'),/\.svg$/);
      assert.match(await evaluate('window.__download[1].url'),/^data:image\/svg\+xml/);
      await evaluate('document.querySelector(".export-close").click()');
      assert.equal(await evaluate('document.querySelector(".plot-export-dialog").open'),false);
      await evaluate(`UW.mapView.map.setStyle({version:8,sources:{test:{type:'geojson',data:{type:'Feature',geometry:{type:'LineString',coordinates:[[-79,75],[-77,77]]}}}},layers:[{id:'background',type:'background',paint:{'background-color':'#123456'}},{id:'test-line',type:'line',source:'test',paint:{'line-color':'#ff0000','line-width':8}}]});`);
      await wait(600);
      const original=await evaluate('({center:UW.mapView.map.getCenter(),zoom:UW.mapView.map.getZoom(),width:UW.mapView.map.getCanvas().width})');
      await evaluate('document.querySelector("#mapexport").click()');
      await until('!!document.querySelector(".export-canvas canvas") && !document.querySelector("[data-format=png]").disabled');
      assert.match(await evaluate('document.querySelector(".export-canvas .map-legend").getAttribute("aria-label")'),/Rosette depth/);
      await evaluate('for(const [name,value] of Object.entries({width:500,height:600})){const input=document.querySelector(`.plot-export-dialog [name=${name}]`);input.value=value;input.dispatchEvent(new Event("change"))}');
      await wait(200);
      assert.equal(await evaluate('document.querySelector(".export-canvas .map-legend").dataset.edge'),'top');
      assert.equal(await evaluate('document.querySelector(".export-canvas .map-legend").getContext("2d").getImageData(10,10,1,1).data[3]'),209);
      await evaluate('window.__legendText=[];const fill=CanvasRenderingContext2D.prototype.fillText;CanvasRenderingContext2D.prototype.fillText=function(text,...args){window.__legendText.push(text);return fill.call(this,text,...args)}');
      await evaluate('document.querySelector("[data-format=png]").click()');
      await until('window.__download.length===3');
      assert.match((await evaluate('window.__legendText')).join(' '),/Rosette depth/);
      assert.deepEqual(await evaluate('new Promise((resolve,reject)=>{const im=new Image;im.onload=()=>{const c=document.createElement("canvas");c.width=im.width;c.height=im.height;const ctx=c.getContext("2d");ctx.drawImage(im,0,0);resolve([im.width,im.height,...ctx.getImageData(1,1,1,1).data])};im.onerror=reject;im.src=window.__download[2].url})'),[1500,1800,18,52,86,255]);
      await evaluate('document.querySelector("[data-format=svg]").click()');
      await until('window.__download.length===4');
      assert.match(await evaluate('decodeURIComponent(window.__download[3].url)'),/<image/);
      await evaluate('for(const [name,value] of Object.entries({width:700,height:400})){const input=document.querySelector(`.plot-export-dialog [name=${name}]`);input.value=value;input.dispatchEvent(new Event("change"))}');
      await wait(200);
      assert.equal(await evaluate('document.querySelector(".export-canvas .map-legend").dataset.edge'),'left');
      await evaluate('document.querySelector(".export-close").click()');
      assert.deepEqual(await evaluate('({center:UW.mapView.map.getCenter(),zoom:UW.mapView.map.getZoom(),width:UW.mapView.map.getCanvas().width})'),original);
      assert.deepEqual(await evaluate('window.__errors'),[]);
      console.log('PASS depth axes, sidebar/exact-size graph export and isolated PNG/SVG map export');
      return;
    }
    if(process.env.SINGLE_UI) {
      await evaluate('UW.showTab("casts")');
      await until('document.querySelector("#casttable tr[data-id]")');
      await evaluate('document.querySelector("#casttable tr[data-id]").click();document.querySelector("#castmode [data-m=single]").click()');
      await until('document.querySelector("#single-plot")?.data');
      assert.equal(await evaluate('document.querySelectorAll(".singlevar[aria-pressed=false]").length'),4);
      assert.equal(await evaluate('document.querySelector(".livebar a").getAttribute("href")'),'data/casts/RosetteSheet_001.xlsx');
      assert.equal(await evaluate('document.querySelector("#casttable a").getAttribute("href")'),'data/casts/RosetteSheet_001.xlsx');
      assert.equal(await evaluate('new Set([...document.querySelectorAll("#casttable th,#casttable td,#casttable .kind")].map(e=>getComputedStyle(e).fontFamily)).size'),1);
      const geometry=()=>evaluate('(()=>{const b=document.querySelector("#singlebody").getBoundingClientRect(),p=document.querySelector(".single-parameters").getBoundingClientRect(),s=document.querySelector(".single-layout").getBoundingClientRect(),c=document.querySelector("#singlebody .castplot").getBoundingClientRect();return {beside:b.left>=p.right,aligned:Math.abs(b.top-p.top)<2,full:Math.abs(c.width-b.width)<2,span:Math.abs(s.width-document.querySelector("#castplots").clientWidth)<2}})()');
      assert.deepEqual(await geometry(),{beside:true,aligned:true,full:true,span:true});
      await evaluate('for(const b of [...document.querySelectorAll(".singlevar:not(.on)")]){document.querySelector(`.singlevar[data-v="${b.dataset.v}"]`).click()}');
      await until('document.querySelector("#single-plot")?.data?.length===6');
      await evaluate(`for(let i=0;i<2;i++)document.querySelector('.chart-divider .nudge[data-d="1"]').click()`);
      await until('document.querySelector("#single-plot")?._fullLayout?.xaxis?.anchor==="free"');
      assert.equal(await evaluate('document.querySelector("#single-plot")._fullLayout.xaxis.anchor'),'free');
      assert.equal(await evaluate('(()=>{const l=document.querySelector("#single-plot")._fullLayout;return l.xaxis.position>l.xaxis2.position&&l.xaxis2.position>l.yaxis.domain[1]&&l.xaxis5.position<l.yaxis.domain[0]&&l.xaxis6.position<l.xaxis5.position})()'),true);
      const before=await evaluate('document.querySelector(".singlevar").dataset.v');
      await evaluate('Plotly.relayout(document.querySelector("#single-plot"),{"xaxis.range":[2.2,2.8],"xaxis.autorange":false,"yaxis.range":[1.5,1.1],"yaxis.autorange":false})');
      generation=2;await poll();
      await until('!!document.querySelector("#single-plot")?._fullLayout');
      assert.deepEqual(await evaluate('document.querySelector("#single-plot")._fullLayout.xaxis.range'),[2.2,2.8]);
      assert.deepEqual(await evaluate('document.querySelector("#single-plot")._fullLayout.yaxis.range'),[1.5,1.1]);
      await evaluate('document.querySelector(".parameter .nudge[data-d=\\"1\\"]").click()');
      await until(`document.querySelector('#single-plot')?.data?.[1]?.name===${JSON.stringify(before)}`);
      assert.deepEqual(await evaluate('document.querySelector("#single-plot")._fullLayout.xaxis2.range'),[2.2,2.8]);
      await evaluate(`document.querySelector('.singlevar[data-v="${before}"]').click()`);
      await until('document.querySelector("#single-plot")?.data?.length===5');
      assert.equal(await evaluate(`document.querySelector('#single-plot').data.some(t=>t.name===${JSON.stringify(before)})`),false);
      await evaluate('for(const b of [...document.querySelectorAll(".singlevar.on")])document.querySelector(`.singlevar[data-v="${b.dataset.v}"]`).click()');
      assert.equal(await evaluate('document.querySelector("#singlebody").textContent.includes("Select at least one parameter")'),true);
      await evaluate(`document.querySelector('.singlevar[data-v="${before}"]').click()`);
      await until('document.querySelector("#single-plot")?.data?.length===1');
      await evaluate(`while(!document.querySelector('.chart-divider .nudge[data-d="1"]').disabled)document.querySelector('.chart-divider .nudge[data-d="1"]').click()`);
      await until('document.querySelector("#single-plot")?._fullLayout?.xaxis.side==="top"');
      await evaluate(`while(!document.querySelector('.chart-divider .nudge[data-d="-1"]').disabled)document.querySelector('.chart-divider .nudge[data-d="-1"]').click()`);
      await until('document.querySelector("#single-plot")?._fullLayout?.xaxis.side==="bottom"');
      await evaluate('document.documentElement.classList.add("bigtype");window.dispatchEvent(new Event("resize"))');
      assert.deepEqual(await geometry(),{beside:true,aligned:true,full:true,span:true});
      if(process.env.UI_SCREENSHOT) {const shot=await call('Page.captureScreenshot',{format:'png',captureBeyondViewport:true});fs.writeFileSync(process.env.UI_SCREENSHOT,Buffer.from(shot.result.data,'base64'));}
      assert.deepEqual(await evaluate('window.__errors'),[]);
      console.log('PASS Single plot beside parameters, full width, six ordered axes and large text');
      console.log('PASS movable Chart divider and custom axis ranges survive refresh and parameter reorder');
      await evaluate('document.querySelector("#castmode [data-m=profiles]").click()');
      await until('!!document.querySelector("#cp-Temperature")?._fullLayout');
      await evaluate('Plotly.relayout(document.querySelector("#cp-Temperature"),{"xaxis.range":[2.2,2.8],"xaxis.autorange":false,"yaxis.range":[1.5,1.1],"yaxis.autorange":false})');
      generation=3;await poll();
      await until('!!document.querySelector("#cp-Temperature")?._fullLayout');
      assert.deepEqual(await evaluate('document.querySelector("#cp-Temperature")._fullLayout.xaxis.range'),[2.2,2.8]);
      assert.deepEqual(await evaluate('document.querySelector("#cp-Temperature")._fullLayout.yaxis.range'),[1.5,1.1]);
      await evaluate('document.querySelector("#cp-Temperature").closest(".castplot").querySelector(".reset").click()');
      generation=4;await poll();
      await until('!!document.querySelector("#cp-Temperature")?._fullLayout');
      assert.notDeepEqual(await evaluate('document.querySelector("#cp-Temperature")._fullLayout.xaxis.range'),[2.2,2.8]);
      await evaluate('document.querySelector("#castmode [data-m=single]").click();document.querySelector("#castkind [data-k=live]").click()');
      await until('!!document.querySelector("#live-plot")?._fullLayout');
      await evaluate('Plotly.relayout(document.querySelector("#live-plot"),{"xaxis.range":[3.2,3.8],"xaxis.autorange":false,"yaxis.range":[30,20],"yaxis.autorange":false})');
      await wait(2500);
      assert.deepEqual(await evaluate('document.querySelector("#live-plot")._fullLayout.xaxis.range'),[3.2,3.8]);
      assert.deepEqual(await evaluate('document.querySelector("#live-plot")._fullLayout.yaxis.range'),[30,20]);
      assert.deepEqual(await evaluate('window.__errors'),[]);
      console.log('PASS Multi refresh/reset and Live polling retain custom axes');
      return;
    }
    if(process.env.TIMEZONE_UI) {
      await evaluate('UW.showTab("underway")');
      await until('document.querySelector("#panels .plot")?.calcdata?.[0]?.length');
      const axis=await evaluate('(()=>{const p=document.querySelector("#panels .plot");return {x:p.calcdata[0][0].x,title:p._fullLayout.xaxis.title.text,hover:p._fullLayout.xaxis.hoverformat}})()');
      console.log('TIMEZONE', process.env.TIMEZONE_UI, axis);
      assert.equal(axis.x,Date.parse('2026-09-04T08:00:00Z'));
      assert.match(axis.title,/EDT/);
      assert.equal(axis.hover.includes('Z'),false);
      await evaluate('Plotly.relayout(document.querySelector("#panels .plot"),{"yaxis.range":[0.2,0.8],"yaxis.autorange":false,"xaxis.range":["2026-09-04T08:00:01","2026-09-04T08:00:05"],"xaxis.autorange":false})');
      generation=2;await poll();
      assert.deepEqual(await evaluate('document.querySelector("#panels .plot")._fullLayout.yaxis.range'),[0.2,0.8]);
      assert.deepEqual(await evaluate('document.querySelector("#panels .plot")._fullLayout.xaxis.range'),['2026-09-04T08:00:01','2026-09-04T08:00:05']);
      assert.equal(await evaluate('document.querySelector(".dockgroup .gn").textContent.includes("ago")'),true);
      const ages=await evaluate(`(async()=>{const old=Date.now;try{const ages=[];for(const now of [${t+310000},${t+370000}]){Date.now=()=>now;await window.__poll();ages.push(document.querySelector('.dockgroup .gn').textContent)}return ages}finally{Date.now=old}})()`);
      assert.deepEqual(ages,['5 min ago','6 min ago']);
      assert.equal(await evaluate('UW.shipAxis(Date.parse("2026-01-04T12:00:00Z"))'),Date.parse('2026-01-04T07:00:00Z'));
      console.log('PASS ship-time plot coordinates independent of browser timezone, including winter offset');
      return;
    }
    if(process.env.TRANSECT_UI) {
      await evaluate('UW.showTab("casts")');
      await until('document.querySelectorAll("#casttable tr[data-id]").length===30');
      assert.deepEqual(await evaluate('[...document.querySelectorAll("#castkind button")].map(b=>b.textContent)'),['All','Live','Rosette','TM','MVP','TRS']);
      assert.equal(await evaluate('document.querySelector("#castlist").getBoundingClientRect().height <= innerHeight/3+1'),true);
      assert.equal(await evaluate('document.querySelector("#castlist").scrollHeight > document.querySelector("#castlist").clientHeight'),true);
      await evaluate(`document.querySelector('#casttable tr[data-id="${leg}:CTD_001"]').click(); document.querySelector('#casttable tr[data-id="${leg}:CTD_002"]').click(); document.querySelector('#castmode [data-m=section]').click()`);
      await until('document.querySelector("#cs-plot")?.data && !document.querySelector("#savetransect").disabled');
      assert.equal(await evaluate('document.querySelector("#cs-plot")._fullData[0].colorbar.tickangle'),180);
      assert.equal(await evaluate('document.querySelector("#cs-plot").layout.yaxis.title.text'),'depth (m)');
      await evaluate('document.querySelector(".sectionwrap .dscale").click()');
      await until('document.querySelector("#cs-plot")?.layout.yaxis.ticktext');
      assert.equal(await evaluate('document.querySelector("#cs-plot").layout.yaxis.title.text'),'depth (m)');
      console.log('PASS section colorbar rotation and plain depth title in both depth modes');
      await evaluate('document.querySelector(".castlegend .reorder .nudge[data-d=\\"1\\"]").click()');
      await evaluate('window.prompt=()=>"Shelf <transect>"; document.querySelector("#savetransect").click()');
      await until('document.querySelector("#casttable tr.sel")?.textContent.includes("Shelf <transect>")');
      const saved=await evaluate('JSON.parse(localStorage.getItem("uw:casts.transects"))[0]');
      assert.deepEqual(saved.members,[`${leg}:CTD_002`,`${leg}:CTD_001`]);
      assert.deepEqual(await evaluate('UW.extraMapTraces().find(t=>t.name==="Shelf <transect>").lat'),[76.01,76]);
      assert.equal(await evaluate('UW.selectedCastKeys().size'),2);
      await evaluate('document.querySelector("#castclear").click()');
      assert.equal(await evaluate('UW.extraMapTraces().some(t=>t.name==="Shelf <transect>")'),false);
      await evaluate('document.querySelector("#casttable tr[data-id]").click()');
      await until('document.querySelector("#cs-plot")?.data');
      await call('Page.reload');
      await until('window.UW && window.UW.state.raw');
      await evaluate('UW.showTab("casts")');
      await until('document.querySelector("#casttable tr.sel") && document.querySelector("#cs-plot")?.data');
      assert.equal(await evaluate('document.querySelectorAll(".castlegend .lbl").length'),2);
      assert.deepEqual(await evaluate('UW.extraMapTraces().find(t=>t.name==="Shelf <transect>").lon'),[-78.01,-78]);
      await evaluate('window.confirm=()=>true;document.querySelector("[data-delete-transect]").click()');
      assert.deepEqual(await evaluate('JSON.parse(localStorage.getItem("uw:casts.transects"))'),[]);
      assert.equal(await evaluate('UW.extraMapTraces().some(t=>t.name==="Shelf <transect>")'),false);
      await evaluate('UW.showTab("stations");UW.showTab("wiki");document.querySelector("#wikiclose").click()');
      assert.equal(await evaluate('document.querySelector("#pane-stations").hidden'),false);
      assert.equal(await evaluate('document.querySelector("#stationtable th").dataset.k'),'station');
      assert.equal(await evaluate('document.querySelector("#stationtable").textContent.includes("(ship)")'),false);
      await evaluate('UW.chatToggle(true)');
      await until('document.querySelectorAll("#chatlog .msg").length===45');
      await evaluate('document.querySelector("#chatlog").scrollTop=80');
      chatMessages.push({id:46,t:1700000046,name:'Sailor',text:'A new message',emoji:''});
      await evaluate('window.__chatPoll()');
      await until('document.querySelectorAll("#chatlog .msg").length===46');
      await wait(200);
      assert.equal(await evaluate('Math.abs(document.querySelector("#chatlog").scrollTop-80)<2'),true);
      await evaluate('document.querySelector("#chatlog").scrollTop=document.querySelector("#chatlog").scrollHeight');
      chatMessages.push({id:47,t:1700000047,name:'Sailor',text:'Follow this message',emoji:''});
      await evaluate('window.__chatPoll()');
      await until('document.querySelectorAll("#chatlog .msg").length===47');
      assert.equal(await evaluate('(()=>{const e=document.querySelector("#chatlog");return e.scrollHeight-e.scrollTop-e.clientHeight<2})()'),true);
      assert.deepEqual(await evaluate('window.__errors'),[]);
      console.log('PASS transect save, ordered map line, deselection, reload, deletion and table height');
      console.log('PASS Wiki close, station column/timezone and chat scroll preservation/following');
      return;
    }
    if(process.env.SUMMARY_UI){
      assert.equal(await evaluate('!!document.querySelector("#tabs [data-tab=table]")'),false);
      assert.equal(await evaluate('!!document.querySelector("#pane-underway #aggtable")'),true);
      assert.equal(await evaluate('!!document.querySelector(".tabrow .themepick #feedback-open")'),true);
      assert.equal(await evaluate('!!document.querySelector(".themepick .lbl")'),false);
      await evaluate('UW.showTab("sources");document.documentElement.classList.add("bigtype")');
      assert.equal(await evaluate('document.querySelector("#pane-sources").hidden'),false);
      await until('document.querySelector(".mapcolour .cbar")?.getBoundingClientRect().width>0');
      assert.equal(await evaluate(`(()=>{const a=document.querySelector('.mapcolour .cbar').getBoundingClientRect(),b=document.querySelector('.maplayers .tools').getBoundingClientRect();return a.right<=b.left||b.right<=a.left||a.bottom<=b.top||b.bottom<=a.top})()`),true);
      await evaluate('document.documentElement.classList.remove("bigtype");UW.showTab("table")');
      assert.equal(await evaluate('document.querySelector("#pane-underway").hidden'),false);
      await until('document.querySelector("#aggtable tbody tr")');
      await evaluate('UW.showTab("underway")');
      assert.equal(await evaluate('document.querySelector("#g-Lab .chip").getAttribute("aria-pressed")'),'false');
      assert.equal(await evaluate('document.querySelector("#g-Lab .chart-state").textContent'),'▼');
      await evaluate('document.querySelector("#g-Lab .chip").click()');
      await until('document.querySelector("#panels .plot")?.data');
      assert.equal(await evaluate('document.querySelectorAll("#g-Lab .chip").length'),1);
      assert.equal(await evaluate('document.querySelector("#g-Lab .chip").getAttribute("aria-pressed")'),'true');
      await evaluate('document.querySelector("#g-Lab .chip").click()');
      assert.equal(await evaluate('document.querySelector("#g-Lab .chip").getAttribute("aria-pressed")'),'false');
      await evaluate('document.querySelector("#g-Lab .chip").click()');
      await until('document.querySelector("#panels .plot")?.data');
      await evaluate('UW.state.data.vars["TSG flow (V)"]=[2,0.1];UW.refreshExtraData()');
      assert.equal(await evaluate('!!document.querySelector(".dockgroup.pump-alarm")'),true);
      await evaluate('UW.state.data.vars["TSG flow (V)"]=[0.1,2];UW.refreshExtraData()');
      assert.equal(await evaluate('!!document.querySelector(".dockgroup.pump-alarm")'),false);
      assert.equal(await evaluate('getComputedStyle(document.querySelector("#g-Lab .chips")).overflowY'),'visible');
      await until('!!UW.mapView?.map');
      await evaluate('UW.mapView.map.jumpTo({zoom:3});document.querySelector("#panels .plot").emit("plotly_click",{points:[{pointIndex:1}]})');
      assert.equal(await evaluate('UW.state.view.zoom'),3);
      assert.equal(await evaluate('UW.state.view.center.lat'),76.001);
      await evaluate(`{const plot=document.querySelector('#panels .plot'),a=plot._fullLayout.xaxis,box=plot.querySelector('.svg-container').getBoundingClientRect();for(const type of ['pointerdown','pointerup'])plot.dispatchEvent(new PointerEvent(type,{bubbles:true,clientX:box.left+a._offset+1,clientY:box.top+50}))}`);
      assert.equal(await evaluate('UW.state.view.center.lat'),76);
      assert.equal(await evaluate('UW.state.view.zoom'),3);
      assert.equal(await evaluate('document.querySelector("#panels .plot").layout.xaxis.tickangle'),0);
      assert((await evaluate('document.querySelector("#g-Lab .gn").title')).includes('Latest displayed observation'));
      await evaluate(`UW.mapView.draw=async opts=>{window.satStyle=opts.style};
        const bounds=[[0,1],[1,1],[1,0],[0,0]],moved=[[1,1],[2,1],[2,0],[1,0]];
        UW.M.satellite={images:{s1:{url:'current.webp',scene:'2026-09-11T00:00:00Z',corners:bounds,label:'Radar'},s1near:{url:'current-near.webp',scene:'2026-09-11T00:00:00Z',corners:bounds}},archive:{s1:[{url:'wide-old.webp',scene:'2026-09-09T00:00:00Z',corners:bounds}],s1near:[{url:'near-one.webp',scene:'2026-09-10T00:00:00Z',corners:bounds},{url:'near-two.webp',scene:'2026-09-10T00:00:00Z',corners:moved}]}};
        UW.state.sat='';UW.selectColour('SST (°C)');document.querySelector('#satpill').click();document.querySelector('#satprev').click()`);
      assert.equal(await evaluate('UW.state.satAt'),'near-two.webp');
      assert.deepEqual(await evaluate('satStyle.sources.satnear.coordinates'),[[1,1],[2,1],[2,0],[1,0]]);
      await evaluate('document.querySelector("#satprev").click()');
      assert.equal(await evaluate('UW.state.satAt'),'near-one.webp');
      assert.deepEqual(await evaluate('satStyle.sources.satnear.coordinates'),[[0,1],[1,1],[1,0],[0,0]]);
      await evaluate('document.querySelector("#satprev").click()');
      assert.equal(await evaluate('!!satStyle.sources.satnear'),false);
      assert.deepEqual(await evaluate('window.__errors'),[]);
      console.log('PASS summary table toggles, pump alarm, no internal scrolling, chart click preserves map zoom');
      console.log('PASS satellite history selects separate moved near boxes with original bounds; older wide scenes do not borrow current detail');
      return;
    }
    if(process.env.WIKI_UI){
      await evaluate('UW.M.history={stamp:"test"};UW.showTab("wiki")');
      await until('document.querySelector("#histask").textContent==="Ask a Q"');
      assert.equal(await evaluate('document.querySelectorAll("#wikidomains .on").length'),0);
      assert.equal(await evaluate('UW.histShared.domainOn("history")&&UW.histShared.domainOn("nature")'),true);
      assert.equal(await evaluate('(()=>{const search=document.querySelector(".wiki-search-row").getBoundingClientRect(),domains=document.querySelector("#wikidomains").getBoundingClientRect();return search.top>=domains.bottom})()'),true);
      for(const width of [1400,900,640,390,320]){
        await call('Emulation.setDeviceMetricsOverride',{width,height:844,deviceScaleFactor:1,mobile:width<=640});
        await evaluate('UW.histShared.refresh()');await wait(100);
        const boxes=await evaluate('(()=>{const s=document.querySelector("#histsearch").getBoundingClientRect(),b=document.querySelector("#histkindsel").getBoundingClientRect(),r=document.querySelector(".wiki-search-row").getBoundingClientRect();return {search:s.width,browse:b.width,row:r.width,overlap:s.left<b.right&&b.left<s.right&&s.top<b.bottom&&b.top<s.bottom}})()');
        assert(boxes.search>=Math.min(160,boxes.row)-1,JSON.stringify({width,...boxes}));
        assert(boxes.browse<150,JSON.stringify({width,...boxes}));
        assert.equal(boxes.overlap,false);
      }
      await evaluate('document.querySelector(\'[data-domain="history"]\').click()');
      await until('document.querySelector("#histask").textContent==="Ask Ada"');
      assert.equal(await evaluate('UW.histShared.domainOn("nature")'),false);
      await evaluate('document.querySelector(\'[data-domain="history"]\').click()');
      await until('document.querySelector("#histask").textContent==="Ask a Q" && location.hash==="#wiki/"');
      assert.equal(await evaluate('document.querySelectorAll("#wikidomains .on").length'),0);
      assert.equal(await evaluate('UW.histShared.domainOn("history")&&UW.histShared.domainOn("nature")'),true);
      assert.deepEqual(await evaluate('window.__errors'),[]);
      console.log('PASS Wiki search row, unselected all-domain home and toggle-off reset');return;
    }
    if (process.env.UPLOAD_UI) {
      await evaluate(`(async()=>{UW.M.history={stamp:'test'};await UW.natureViews.ensure();UW.showTab('wiki');location.hash='#wiki/journal';})()`);
      await until('document.querySelector("#jtabs")');
      await evaluate('document.querySelector("#jtabs [data-t=submit]").click()');
      await until('document.querySelector("a[data-share=\\"Pictures/Destination\\"]")');
      await evaluate('document.querySelector("a[data-share=\\"Pictures/Destination\\"]").click()');
      await until('document.querySelector(".crumbs")?.textContent.includes("Destination") && document.querySelector("#phone-files")');
      await evaluate(`const f=document.querySelector('#natimportform input[name=name]');f.value='Test photographer';f.dispatchEvent(new Event('input',{bubbles:true}));const dt=new DataTransfer();dt.items.add(new File(['photo a'],'a.jpg',{type:'image/jpeg'}));dt.items.add(new File(['photo b'],'b.jpg',{type:'image/jpeg'}));const picker=document.querySelector('#phone-files');picker.files=dt.files;picker.dispatchEvent(new Event('change'));`);
      assert.equal(await evaluate('document.querySelector("#natimportform input[name=name]").value'),'Test photographer');
      await evaluate('document.querySelector("#phone-upload").click()');
      await until('document.querySelector("#phone-message")?.textContent.includes("Test interrupted upload")');
      assert.deepEqual(uploadFiles,[0,1]);
      assert.equal(uploadBatches.length,1);assert.equal(uploadBatches[0].parent,'Pictures/Destination');
      await evaluate('document.querySelector("#phone-upload").click()');
      await until('document.querySelector("#phone-message")?.textContent.includes("Saved 2 photos")');
      assert.deepEqual(uploadFiles,[0,1,1]);assert.equal(uploadBatches.length,1);
      if(process.env.UI_PREFIX) assert(requests.filter(p=>p.includes('/api/nature/')).every(p=>p.startsWith('/underway/')));
      assert.equal(await evaluate('document.querySelector("#natimportgo").disabled'),false);
      assert.equal(await evaluate('document.querySelector(".crumbs").textContent.includes("batch")'),true);
      assert.equal(await evaluate('document.documentElement.scrollWidth<=innerWidth'),true);
      assert.deepEqual(await evaluate('window.__errors'),[]);
      console.log('PASS phone picker, selected destination, retry only remaining files, credit retention, and import handoff');return;
    }
    if (process.env.NATURE_UI) {
      await evaluate(`(async()=>{ UW.M.history={stamp:'test'}; UW.state.nature=true; UW.state.photos=false;
        await UW.natureViews.ensure(); UW.renderMap(); })()`);
      await until('window.UW.mapView?.traces.base.find(t=>t.name==="nature")?.lat.length===2');
      const before=await evaluate('UW.extraMapTraces().find(t=>t.name==="nature").customdata');
      await evaluate(`window.__focusCalls=0; const focus=UW.focusMap; UW.focusMap=(...a)=>{window.__focusCalls++; return focus(...a);};
        UW.onNatureClick('seal',{lat:76,lon:-78,text:'Seal'});`);
      await until('document.querySelector("#pane-wiki").textContent.includes("Seal") && location.hash.includes("observation/seal")');
      await wait(300);
      assert.deepEqual(await evaluate('UW.extraMapTraces().find(t=>t.name==="nature").customdata'),before);
      assert.equal(await evaluate('window.__focusCalls'),0);
      await evaluate(`const box=document.querySelector('#mapnatlayers input[data-k="geology"]'); box.checked=false; box.dispatchEvent(new Event('change'));`);
      assert.deepEqual(await evaluate('UW.extraMapTraces().find(t=>t.name==="nature").customdata'),['nat:seal']);
      assert.deepEqual(await evaluate('window.__errors'),[]);
      console.log('PASS nature point opens sidebar without hiding other subjects or recentering; explicit domain filter still works');
      return;
    }
    if (process.env.FEEDBACK_UI) {
      await evaluate(`document.querySelector('#feedback-open').click();
        document.querySelector('#feedback-message').value='Map points disappear when clicked';
        document.querySelector('#feedback-form').requestSubmit();`);
      await until('document.querySelector("#feedback-status").textContent.includes("retry")');
      assert.equal(await evaluate('document.querySelector("#feedback-message").value'),'Map points disappear when clicked');
      rejectFeedback=false;
      await evaluate('document.querySelector("#feedback-form").requestSubmit()');
      await until('document.querySelector("#feedback-status").textContent.includes("saved")');
      assert.equal(feedbackRows.length,1);
      assert.equal(feedbackRows[0].context.tab,'underway');
      assert.equal(feedbackRows[0].context.viewport.width,390);
      assert.equal(feedbackRows[0].message,'Map points disappear when clicked');
      await evaluate('document.querySelector("#feedback-close").click(); document.querySelector("#feedback-open").click()');
      assert.equal(await evaluate('document.querySelector("#feedback-message").value'),'');
      assert.equal(await evaluate('document.activeElement.id'),'feedback-message');
      if (process.env.UI_SCREENSHOT) {
        const shot=await call('Page.captureScreenshot',{format:'png'});
        fs.writeFileSync(process.env.UI_SCREENSHOT,Buffer.from(shot.result.data,'base64'));
      }
      assert.deepEqual(await evaluate('window.__errors'),[]);
      console.log('PASS feedback opens, preserves failed submissions, saves current context, and resets after success');
      return;
    }
    // Profile repeated refreshes with a phone-sized viewport and a full dock.
    // Count work as well as time: timings vary by host, DOM churn does not.
    if (process.env.PROFILE_UI) {
      await evaluate('[...document.querySelectorAll(".panel")].find(e=>e.dataset.name==="SST (°C)").scrollIntoView()');
      await until('!![...document.querySelectorAll(".panel")].find(e=>e.dataset.name==="SST (°C)").querySelector(".plot").data');
      await wait(250);
      const metrics = await evaluate(`(() => {
        for (let i=0;i<24;i++) UW.registerPanel('Profile '+i, {resolved:true,group:'Other',render() {}});
        const group=document.querySelector('#g-Other');
        const size=()=>[group.offsetWidth,group.offsetHeight];
        const expandedSize=size();
        const original=[...document.querySelectorAll('#panels .panel')].find(e=>e.dataset.name==='Profile 0');
        document.querySelector('#g-Other .ghead').click();
        const minimizedSize=size();
        const observer = new MutationObserver(() => {});
        observer.observe(document.querySelector('#panels'), {childList:true});
        let resizes=0; const resize=Plotly.Plots.resize;
        Plotly.Plots.resize=(...args)=>{resizes++; return resize(...args);};
        const start=performance.now();
        for(let i=0;i<10;i++) UW.selectColour(UW.state.colour);
        const elapsed=performance.now()-start, mutations=observer.takeRecords().length;
        observer.disconnect(); Plotly.Plots.resize=resize;
        document.querySelector('#g-Other .ghead').click();
        return {expandedSize,minimizedSize,restoredSize:size(),groupDraggable:group.draggable,groupInGrid:group.parentElement.id==='panels',refreshes:10,minimized:24,elapsed_ms:Math.round(elapsed),panel_mutations:mutations,resizes,
          reused: original === [...document.querySelectorAll('#panels .panel')].find(e=>e.dataset.name==='Profile 0'),
          restored: [...document.querySelectorAll('#panels .panel')].filter(e=>e.dataset.name?.startsWith('Profile ')).length};
      })()`);
      console.log('UI PROFILE', JSON.stringify(metrics));
      assert.equal(metrics.restored,24);
      if (process.env.PROFILE_UI === 'check') {
        assert.equal(metrics.groupDraggable,true);
        assert.equal(metrics.groupInGrid,true);
        assert.deepEqual(metrics.expandedSize,metrics.minimizedSize);
        assert.deepEqual(metrics.expandedSize,metrics.restoredSize);
        assert.equal(metrics.reused,true);
        assert.equal(metrics.panel_mutations,0);
        assert.equal(metrics.resizes,0);
      }
      const reordered = await evaluate(`(() => {
        const group=document.querySelector('#g-Other'), target=document.querySelector('[data-name="SST (°C)"]');
        const before=[...group.parentElement.children].indexOf(target), dt=new DataTransfer();
        group.dispatchEvent(new DragEvent('dragstart',{bubbles:true,dataTransfer:dt}));
        target.dispatchEvent(new DragEvent('drop',{bubbles:true,dataTransfer:dt}));
        group.dispatchEvent(new DragEvent('dragend',{bubbles:true,dataTransfer:dt}));
        const moved=[...group.parentElement.children].indexOf(group)===before;
        group.querySelector('.ghead').click();
        const size=group.offsetHeight;
        group.querySelector('.chip').click();
        const restoredOne=UW.state.panel['Profile 0'] !== 'min';
        group.scrollIntoView();
        return {moved,restoredOne,stable:size===group.offsetHeight,scrolls:group.querySelector('.chips').scrollHeight>group.querySelector('.chips').clientHeight};
      })()`);
      assert.deepEqual(reordered,{moved:true,restoredOne:true,stable:true,scrolls:true});
      if (process.env.UI_SCREENSHOT) {
        await wait(300);
        const shot=await call('Page.captureScreenshot',{format:'png'});
        fs.writeFileSync(process.env.UI_SCREENSHOT,Buffer.from(shot.result.data,'base64'));
      }
      await wait(500);
      assert.deepEqual(await evaluate('window.__errors'),[]);
      return;
    }

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
    await evaluate(`document.querySelector('#aggrule [data-r="1h"]').click()`);
    await until('document.querySelector("#connection").textContent.includes("Table")');
    failures.clear();await poll();
    await until('document.querySelector("#aggtable").textContent.includes("3.00")');
    generation=4;await poll();
    await until('document.querySelector("#aggtable").textContent.includes("4.00")');
    console.log('PASS failed aggregate is retried; open Table updates');

    await evaluate('window.UW.showTab("casts")');
    await until('document.querySelector("#casttable tr[data-id]")');
    await evaluate('document.querySelector("#casttable tr[data-id]").click()');
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
    await evaluate('window.UW.showTab("casts"); document.querySelector("#castmode [data-m=live]").click()');
    await until('document.querySelector("#livecfg")');
    await evaluate('document.querySelector("#livecfg").click();document.querySelector("#livecfgform [name=port]").value="6000";document.querySelector("#livecfgform").requestSubmit()');
    await until('document.querySelector("#livecfgerror")?.textContent.includes("Port already in use")');
    assert.equal(await evaluate('document.querySelector("#livecfgform [name=port]").value'),'6000');
    rejectLiveConfig=false;
    await evaluate('document.querySelector("#livecfgform").requestSubmit()');
    await until('!document.querySelector("#livecfgform")');
    await evaluate('document.querySelector("[data-w=last]").click()');
    await until('document.querySelector("#livestatus").textContent.includes("max 10 m")');
    await until('document.querySelector("#livebody .js-plotly-plot")?.data?.[0]?.y?.[0]===10');
    console.log('PASS live settings retain edits on rejection; successful retry closes form; last cast uses its own pressure schema');
    const fits = async selector => {
      const outside = await evaluate(`Array.from(document.querySelectorAll(${JSON.stringify(selector)})).filter(e => {
        const r=e.getBoundingClientRect(); return r.width && (r.left < -1 || r.right > innerWidth+1);
      }).map(e => e.id || e.name || e.className || e.tagName)`);
      assert.deepEqual(outside, [], `Controls outside viewport: ${selector}`);
    };
    for (const width of [320,390,768]) {
      await call('Emulation.setDeviceMetricsOverride',{width,height:844,deviceScaleFactor:1,mobile:true});
      await evaluate('window.UW.showTab("calendar")');
      await until('document.querySelector(".agenda .ev")');
      await evaluate('document.querySelector(".agenda .ev .st").textContent="Ice station — multidisciplinary sampling"');
      await fits('#tabs button, .agenda .ev > *, #sources');
      await evaluate('document.querySelector("#legmenu").open=true');
      await fits('#legmenu .pop, #legmenu li label > *');
      await evaluate('document.querySelector("#legmenu").open=false;window.UW.showTab("casts")');
      await until('document.querySelector("#livecfg")');
      await evaluate('document.querySelector("#livecfg").click()');
      await fits('#livecfgform input, #livecfgform button');
      if (width <= 640) {
        assert.equal(await evaluate(`Array.from(document.querySelectorAll('#tabs button,.maptools button,#castmode button')).every(e=>e.getBoundingClientRect().height>=44)`),true);
      }
      await evaluate('document.querySelector("#livecfgclose").click()');
    }
    console.log('PASS mobile navigation, Agenda, leg menu and live settings fit 320/390/768px; phone touch targets are enlarged');
    await evaluate('window.UW.state.raw.pump_low=[false,true];window.UW.showTab("underway");window.UW.renderMap()');
    await until('document.querySelector(".panel .plot")?.data?.length===3');
    assert.equal(await evaluate('document.querySelector(".panel .plot").data[1].y[1]'),null);
    assert.equal(await evaluate('document.querySelector(".panel .plot").data[2].marker.color'),'#7d8895');
    assert.equal(await evaluate('document.querySelector(".panel .plot").data[2].y[1]'),6);
    await until('window.UW.mapView?.traces.base.some(t=>t.name==="pump off")');
    await evaluate('window.UW.showTab("calendar")');
    await until('document.querySelector("#calendar").textContent.includes("Pump off / low intake flow")');
    await evaluate('document.querySelector("#calview [data-v=month]").click()');
    await until('document.querySelector("#calspan")');
    await evaluate('document.querySelector("#calspan [data-s=month]").click()');
    await until('document.querySelector(".mev")?.textContent.includes("Pump off / low intake flow")');
    console.log('PASS pump episodes appear in Agenda/calendar; affected TSG panel and map samples are grey with values retained');
    const errors=await evaluate('window.__errors');
    if(errors.length) console.error(stderr.slice(0,4000),await evaluate('window.__mapErrors'));
    assert.deepEqual(errors,[]);
    console.log('PASS unchanged-generation polling reuses data; no uncaught browser errors');
  } finally {
    ws?.close();child?.kill();server.closeAllConnections();server.close();clearTimeout(watchdog);
    console.log(`Temporary browser profile: ${profile}`);
  }
})().catch(e=>{console.error(e);process.exitCode=1;});
