/* Real MapLibre/WebGL check of the dated CIS overlay against local synthetic data.
 * Usage: node tests/ice-charts-browser.cjs /usr/bin/google-chrome */
const assert=require('node:assert/strict');
const fs=require('node:fs'),os=require('node:os'),path=require('node:path'),http=require('node:http');
const {spawn}=require('node:child_process');
const wait=ms=>new Promise(r=>setTimeout(r,ms));
const assets=path.join(__dirname,'../dashboard/static');
const fixture={type:'FeatureCollection',features:[{type:'Feature',properties:{CT:'92',CA:'92',SA:'86',FA:'04',egg_ct:'10',egg_ca:'10',egg_sa:'6',egg_fa:'4',concentration_a_label:'10/10',concentration:10,concentration_label:'10/10 (compact ice)',stage_a:'First-year ice',form_a:'Medium floe',polygon_type:'ice'},geometry:{type:'Polygon',coordinates:[[[-95,70],[-75,70],[-75,80],[-95,80],[-95,70]]]}}]};
const template=fs.readFileSync(path.join(__dirname,'../dashboard/templates/index.html.j2'),'utf8');
const controls=template.slice(template.indexOf('<div id="icechart-controls"'),template.indexOf('<div id="map">'));
const html=`<!doctype html><html data-theme="dark"><meta charset="utf-8"><link rel="stylesheet" href="/style.css"><link rel="stylesheet" href="/ice-charts.css">
<style>body{overflow:auto;padding:15px}#map{width:900px;height:600px;position:relative}button{padding:.5rem}</style>
<button id="icechart-toggle" aria-pressed="false">Ice charts</button>${controls}<div id="map"></div>
<script src="/maplibre-gl.js"></script><script src="/map.js"></script><script src="/ice-charts.js"></script><script>
window.errors=[];window.done=false;window.result={};addEventListener('error',e=>errors.push(e.message));
(async()=>{
 const wait=ms=>new Promise(r=>setTimeout(r,ms));
 const until=async(fn)=>{for(let i=0;i<120;i++){if(fn())return;await wait(50)}throw Error('Condition timed out')};
 const view=new UW.MapView(document.querySelector('#map'),{onEmptyClick:e=>UW.iceCharts.click(e),onClick:()=>result.trackClick=true});
 const style={version:8,id:'first',sources:{},layers:[{id:'background',type:'background',paint:{'background-color':'#19374b'}}]};
 await view.draw({style,view:{center:{lon:-85,lat:75},zoom:3},base:[],live:[]});
 view.map.on('error',e=>errors.push(e.error.message));
 const manifest={charts:[{id:'old',date:'2026-09-07',region:'Eastern Arctic',url:'/chart-old',source_url:'https://ice-glaces.ec.gc.ca/prods/sigrids/',attribution:'Canadian Ice Service / ECCC'},{id:'new',date:'2026-09-14',region:'Eastern Arctic',url:'/chart-new'}]};
 UW.iceCharts.refresh(manifest,view,'2026-09-10');
 document.querySelector('#icechart-toggle').click();
 await until(()=>view.map.getLayer('cis-ice-fill')&&view.map.isSourceLoaded('cis-ice-chart'));
 result.initial=document.querySelector('#icechart-status').textContent.includes('2026-09-07')?'old':'wrong';
 const pt=view.map.project([-85,75]);view.map.fire('click',{point:pt});
 result.clicked=!!document.querySelector('.icechart-detail[open]');
 document.querySelector('.icechart-detail button').click();
 const slider=document.querySelector('#icechart-opacity');slider.value='65';slider.dispatchEvent(new Event('input'));
 result.opacity=view.map.getPaintProperty('cis-ice-fill','fill-opacity');
 view.setStyle({...style,id:'second',layers:[{id:'background',type:'background',paint:{'background-color':'#244355'}}]});
 await until(()=>view.map.getLayer('cis-ice-fill')&&view.map.isSourceLoaded('cis-ice-chart'));
 result.layerRestored=true;
 view.setTraces('base',[{lat:[75],lon:[-85],mode:'markers',marker:{size:12,color:'#fff'},customdata:'station'}]);
 await until(()=>view.map.isSourceLoaded('u-base-circles'));await wait(100);
 view.map.fire('click',{point:pt});
 view.setTraces('base',[]);
 document.querySelector('#icechart-toggle').click();result.off=!view.map.getLayer('cis-ice-fill');
 UW.iceCharts.refresh({charts:[{...manifest.charts[0],id:'slow',url:'/chart-slow'}]},view,'2026-09-10');
 document.querySelector('#icechart-toggle').click();await wait(50);document.querySelector('#icechart-toggle').click();await wait(400);
 result.cancelled=!view.map.getLayer('cis-ice-fill');
 const failing={charts:[{id:'bad',date:'2026-09-07',region:'Eastern Arctic',url:'/chart-fail'}]};
 UW.iceCharts.refresh(failing,view,'2026-09-10');document.querySelector('#icechart-toggle').click();
 await until(()=>!document.querySelector('#icechart-retry').hidden);
 result.raceCleared=!view.map.getLayer('cis-ice-fill');
 const realFetch=window.fetch;window.fetch=(url,options)=>realFetch(url==='/chart-fail'?'/chart-old':url,options);
 document.querySelector('#icechart-retry').click();
 await until(()=>view.map.getLayer('cis-ice-fill')&&view.map.isSourceLoaded('cis-ice-chart'));result.retried=true;
 UW.iceCharts.refresh(manifest,view,'2025-09-10');result.futureHidden=!view.map.getLayer('cis-ice-fill')&&document.querySelector('#icechart-status').textContent.includes('No cached chart on or before');
 UW.iceCharts.refresh(null,view,'2026-09-10');result.empty=document.querySelector('#icechart-status').textContent.includes('No ice charts cached');
 UW.iceCharts.refresh({charts:[{...manifest.charts[0],region:'<img src=x onerror=alert(1)>'}]},view,'2026-09-10');
 await until(()=>view.map.getLayer('cis-ice-fill')&&view.map.isSourceLoaded('cis-ice-chart'));await wait(100);view.map.fire('click',{point:pt});
 result.escaped=document.querySelectorAll('.icechart-detail img').length===0&&document.querySelector('.icechart-detail').textContent.includes('<img');
 document.querySelector('.icechart-detail button').click();
 UW.iceCharts.refresh(manifest,view,'2026-09-10');
 await until(()=>view.map.getLayer('cis-ice-fill')&&view.map.isSourceLoaded('cis-ice-chart'));await wait(100);view.map.fire('click',{point:pt});
 result.egg=document.querySelector('.icechart-egg-total').textContent;
 window.done=true;
})().catch(e=>{errors.push(e.stack);window.done=true});
</script></html>`;
const server=http.createServer((req,res)=>{
 if(req.url==='/chart-slow'){setTimeout(()=>{res.setHeader('Content-Type','application/json');res.end(JSON.stringify(fixture))},250);return;}
 if(req.url.startsWith('/chart')){if(req.url==='/chart-fail'){res.writeHead(503);res.end();return;}res.setHeader('Content-Type','application/json');res.end(JSON.stringify(fixture));return;}
 if(req.url==='/'){res.setHeader('Content-Type','text/html');res.end(html);return;}
 if(['/map.js','/maplibre-gl.js','/ice-charts.js','/ice-charts.css','/style.css'].includes(req.url)){res.setHeader('Content-Type',req.url.endsWith('.css')?'text/css':'text/javascript');res.end(fs.readFileSync(path.join(assets,req.url.slice(1))));return;}
 res.writeHead(404);res.end();
});
const profile=fs.mkdtempSync(path.join(os.tmpdir(),'underway-sat-browser-'));
let child,ws;
const watchdog=setTimeout(()=>{child?.kill();server.closeAllConnections();server.close();process.exitCode=1;},35000);
(async()=>{try{
 await new Promise(r=>server.listen(0,'127.0.0.1',r));
 let stderr='';child=spawn(process.argv[2]||'/usr/bin/google-chrome',['--no-sandbox','--headless','--enable-unsafe-swiftshader','--disable-dev-shm-usage','--remote-debugging-port=0',
 '--use-angle=swiftshader', '--user-data-dir='+profile,'about:blank']);
 child.stderr.on('data',d=>stderr+=d);
 for(let i=0;i<100&&!stderr.includes('DevTools listening');i++)await wait(100);
 const endpoint=stderr.match(/DevTools listening on (ws:\/\/\S+)/)?.[1];if(!endpoint)throw Error(stderr);
 const pages=await(await fetch('http://'+new URL(endpoint).host+'/json/list')).json();
 ws=new WebSocket(pages.find(p=>p.type==='page').webSocketDebuggerUrl);await new Promise(r=>ws.addEventListener('open',r,{once:true}));
 let id=0;const pending=new Map();ws.addEventListener('message',e=>{const m=JSON.parse(e.data);if(m.id){pending.get(m.id)?.(m);pending.delete(m.id)}});
 const call=(method,params={})=>new Promise(r=>{const n=++id;pending.set(n,r);ws.send(JSON.stringify({id:n,method,params}))});
 const evaluate=async expression=>{const r=await call('Runtime.evaluate',{expression,returnByValue:true,awaitPromise:true});if(r.result?.exceptionDetails)throw Error(JSON.stringify(r.result.exceptionDetails));return r.result?.result?.value};
 await call('Page.navigate',{url:'http://127.0.0.1:'+server.address().port});
 for(let i=0;i<280;i++){if(await evaluate('window.done'))break;await wait(100)}
 assert.equal(await evaluate('window.done'),true,'Browser timed out');
 assert.deepEqual(await evaluate('window.errors'),[]);
 const result=await evaluate('window.result');
 assert.equal(result.initial,'old');assert.equal(result.clicked,true);assert.equal(result.layerRestored,true);
 assert.equal(result.off,true);assert.equal(result.cancelled,true);assert.equal(result.raceCleared,true);assert.equal(result.retried,true);
 assert.equal(result.futureHidden,true);assert.equal(result.empty,true);assert.equal(result.escaped,true);
 assert.equal(result.egg,'10');assert.equal(result.trackClick,true);assert.equal(result.opacity,.65);
 const screenshot=await call('Page.captureScreenshot',{format:'png'});
 fs.writeFileSync('/tmp/underway-cis-ice-browser.png',Buffer.from(screenshot.result.data,'base64'));
 console.log('PASS CIS chart date selection, actual MapLibre polygons, popup, style change, toggle, failure/retry and no-cache states',result);
}finally{ws?.close();child?.kill();server.closeAllConnections();server.close();clearTimeout(watchdog);}})().catch(e=>{console.error(e);process.exitCode=1});
