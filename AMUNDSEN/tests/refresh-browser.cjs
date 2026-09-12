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
    variables:[{name:'SST (°C)',unit:'°C',resolved:true,derived:false,tsg:true,coverage:{[leg]:true},source:'TSG'}],
    surprise:{scales:[],note:''},stations:[],columns_seen:[],files:{total:1,latest:'ACSD_20260904.csv'},
    data_range:{start:new Date(t-10000).toISOString(),end:new Date(t+10000).toISOString()},
    latest:{lat:76,lon:-78},casts:{index:'data/casts/index.json'},calendar:{file:'data/calendar.json'},
    aggregates:{'1h':{file:'data/agg-1h.json'},'1d':{file:'data/agg-1d.json'}},intranet:[],
  };
}
function dataset(p) {
  if(process.env.NATURE_UI && p.startsWith('/data/history/')) {
    if(p.endsWith('/index.json')) return {topics:[],pages:[]};
    if(p.endsWith('/subjects.json')) return {subjects:[{name:'Seal',domain:'biology',kind:'taxon'},{name:'Rock',domain:'geology',kind:'mineral'}]};
    if(p.endsWith('/observations.json')) return {observations:[{id:'seal',subject:'Seal',date:'2020-01-01',lat:76,lon:-78},{id:'rock',subject:'Rock',date:'2020-01-01',lat:77,lon:-79}]};
    return {};
  }
  if(p==='/api/nature/journal') return {observations:[]};
  if(p==='/data/manifest.json') return manifest();
  if(p.startsWith('/data/w-')) return {label:p.includes('3h')?'3h':'1h',step_s:10,n:2,t:[t,t+10000],lat:[76,76.001],lon:[-78,-78.001],dist_km:[0,1],leg:[0,0],pump_low:[false,true],vars:{'SST (°C)':[generation,generation]},limits:{'SST (°C)':[0,10]},start:new Date(t).toISOString(),end:new Date(t+10000).toISOString()};
  if(p==='/data/calendar.json') return {events:[pumpEvent,{leg,time_utc:new Date(t).toISOString(),event:`event-${generation}`,activity:'CTD',station:'Test',lat:76,lon:-78}],pump_events:[pumpEvent],schedule:{rows:[]}};
  if(p.startsWith('/data/agg-')) return {variables:['SST (°C)'],rows:[{t,leg:0,lat:76,lon:-78,'SST (°C)':[generation,generation,generation,2]}]};
  const cast={id:`${leg}:CTD_001`,leg,kind:'CTD',cast:'001',station:'Test',time:new Date(t).toISOString(),lat:76,lon:-78,p:[1,2],units:{Temperature:'°C'},vars:{Temperature:[generation,generation]}};
  if(process.env.TRANSECT_UI) {
    const casts=Array.from({length:30},(_,i)=>({...cast,id:`${leg}:CTD_${String(i+1).padStart(3,'0')}`,cast:String(i+1),station:`Station ${i+1}`,lat:76+i/100,lon:-78-i/100,file:`data/casts/cast-${i}.json`}));
    if(p==='/data/casts/index.json') return {variables:['Temperature'],casts:casts.map(c=>({...c,vars:['Temperature']}))};
    const match=p.match(/^\/data\/casts\/cast-(\d+)\.json$/); if(match) return casts[+match[1]];
  }
  if(p==='/data/casts/index.json') return {variables:['Temperature'],casts:[{...cast,vars:['Temperature'],file:'data/casts/cast.json'}]};
  if(p==='/data/casts/cast.json') return cast;
  if(p==='/api/chat') return {messages:[],online:[],crew:[],typing:[]};
  if(p==='/api/live') return liveData;
}
const site={title:'Refresh test',version:'test',local_tz:'UTC',default_window:'1h',geo_layers:[],intranet:[],links:[],asset_version:'test',plotly_version:'test'};
const rendered=spawnSync(process.env.PYTHON||'python3',['-c',
  'import sys,json; from jinja2 import Environment,FileSystemLoader; d=json.load(sys.stdin); print(Environment(loader=FileSystemLoader(sys.argv[1]),autoescape=True).get_template("index.html.j2").render(**d))',
  path.join(root,'dashboard/templates')],{input:JSON.stringify({site,m:manifest()}),encoding:'utf8'});
if(rendered.status!==0) throw Error(rendered.stderr);
const server=http.createServer((req,res)=>{
  const p=new URL(req.url,'http://localhost').pathname; requests.push(req.url);
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
    await call('Page.navigate',{url:`http://127.0.0.1:${server.address().port}/`});
    await until('window.UW && document.querySelector("#connection").textContent.includes("Underway")');
    assert.equal(await evaluate('!!window.UW.state.raw'),false);
    failures.delete('/data/manifest.json'); await poll();
    assert.equal(await evaluate('!!window.UW.state.raw'),false);
    failures.clear(); await evaluate('window.dispatchEvent(new Event("online"))');
    await until('window.UW.state.raw?.vars["SST (°C)"][0]===1');
    await evaluate('window.__mapErrors=[]; window.UW.mapView?.map?.on("error",e=>window.__mapErrors.push(String(e.error)))');
    console.log('PASS initial load retries without reload');
    if(process.env.TRANSECT_UI) {
      await evaluate('UW.showTab("casts")');
      await until('document.querySelectorAll("#casttable tr[data-id]").length===30');
      assert.deepEqual(await evaluate('[...document.querySelectorAll("#castkind button")].map(b=>b.textContent)'),['All','Live','Rosette','TM','MVP','TRS']);
      assert.equal(await evaluate('document.querySelector("#castlist").getBoundingClientRect().height <= innerHeight/3+1'),true);
      assert.equal(await evaluate('document.querySelector("#castlist").scrollHeight > document.querySelector("#castlist").clientHeight'),true);
      await evaluate(`document.querySelector('#casttable tr[data-id="${leg}:CTD_001"]').click(); document.querySelector('#casttable tr[data-id="${leg}:CTD_002"]').click(); document.querySelector('#castmode [data-m=section]').click()`);
      await until('document.querySelector("#cs-plot")?.data && !document.querySelector("#savetransect").disabled');
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
