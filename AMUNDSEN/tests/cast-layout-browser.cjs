/* Responsive CSS geometry smoke test; pass Chromium path as first argument. */
const assert=require('node:assert/strict'),fs=require('node:fs'),path=require('node:path'),os=require('node:os');
const {spawn}=require('node:child_process');
const root=path.resolve(__dirname,'..'),profile=fs.mkdtempSync(path.join(os.tmpdir(),'cast-layout-'));
const wait=ms=>new Promise(r=>setTimeout(r,ms));
const template=fs.readFileSync(path.join(root,'dashboard/templates/index.html.j2'),'utf8');
const controls=template.slice(template.indexOf('  <div class="controls"'),template.indexOf('</header>')).replace('<select id="spansel" title="span" aria-label="Time span"></select>','<select id="spansel" title="span" aria-label="Time span"><option>6h</option></select>');
const panel=(cls='')=>`<div class="panel card castplot ${cls}"><div class="head"><h3>Temperature · °C</h3></div><div class="plot" style="background:linear-gradient(135deg,var(--plot-bg),var(--card-2))"></div></div>`;
const html=`<style>${fs.readFileSync(path.join(root,'dashboard/static/style.css'),'utf8')}</style><header class="top"><div class="brand"><h1>Cast layout</h1></div>${controls}</header><div style="overflow:auto;padding:12px"><div id="castplots" class="castplots">${panel()}${panel()}<div class="single-layout"><div class="single-parameters"><button class="chip">Temperature</button><button class="chip">Salinity</button><div class="chip chart-chip">Chart</div><button class="chip">Oxygen</button></div><div id="singlebody">${panel('tall')}</div></div></div></div>`;
let child,ws; const timer=setTimeout(()=>{child?.kill();process.exitCode=1;},30000);
(async()=>{try{
 let stderr='';child=spawn(process.argv[2]||'/opt/google/chrome/chrome',['--headless','--no-sandbox','--disable-dev-shm-usage','--remote-debugging-port=0',`--user-data-dir=${profile}`,'about:blank']);child.stderr.on('data',d=>stderr+=d);
 for(let i=0;i<100&&!stderr.includes('DevTools listening');i++)await wait(50);
 const endpoint=stderr.match(/DevTools listening on (ws:\/\/\S+)/)?.[1];assert.ok(endpoint,stderr);
 const pages=await(await fetch(`http://${new URL(endpoint).host}/json/list`)).json();ws=new WebSocket(pages.find(p=>p.type==='page').webSocketDebuggerUrl);await new Promise(r=>ws.addEventListener('open',r,{once:true}));
 let id=0;const pending=new Map();ws.addEventListener('message',e=>{const m=JSON.parse(e.data);if(m.id){pending.get(m.id)(m);pending.delete(m.id);}});const call=(method,params={})=>new Promise(r=>{const n=++id;pending.set(n,r);ws.send(JSON.stringify({id:n,method,params}));});
 await call('Page.enable');
 for(const width of [320,390,768,1440]){
  await call('Emulation.setDeviceMetricsOverride',{width,height:900,deviceScaleFactor:1,mobile:false});
  await call('Page.setDocumentContent',{frameId:(await call('Page.getFrameTree')).result.frameTree.frame.id,html});await wait(70);
  const result=await call('Runtime.evaluate',{expression:`JSON.stringify((()=>{const box=s=>{const r=document.querySelector(s).getBoundingClientRect();return {x:r.x,y:r.y,w:r.width,h:r.height,right:r.right}};return {multi:box('.castplot:not(.tall) .plot'),single:box('#singlebody .plot'),hint:box('.pickhint'),legs:box('#legmenu'),span:box('.group.span'),body:document.body.scrollWidth}})())`,returnByValue:true});
  const m=JSON.parse(result.result.result.value);assert.ok(Math.abs(m.multi.h/m.multi.w-1.618)<.01,JSON.stringify(m));assert.ok(m.single.h>m.single.w,JSON.stringify(m));assert.ok(m.span.right<=width+1,JSON.stringify(m));assert.ok(m.body<=width,JSON.stringify(m));
  if(width<=640)assert.ok(Math.abs((m.hint.y+m.hint.h/2)-(m.legs.y+m.legs.h/2))<2,JSON.stringify(m));
  const shot=await call('Page.captureScreenshot',{format:'png'});fs.writeFileSync(`/tmp/cast-layout-${width}.png`,Buffer.from(shot.result.data,'base64'));console.log(`PASS ${width}px: Multi ${m.multi.w.toFixed(0)}×${m.multi.h.toFixed(0)}, Single ${m.single.w.toFixed(0)}×${m.single.h.toFixed(0)}, header fits`);
 }
}finally{clearTimeout(timer);ws?.close();child?.kill();}})().catch(e=>{console.error(e);process.exitCode=1;});
