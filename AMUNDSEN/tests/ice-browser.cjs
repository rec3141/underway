/* Isolated camera-panel smoke test with real Plotly; no live services touched. */
const fs=require('fs'),http=require('http'),os=require('os'),path=require('path'),{spawn}=require('child_process'),assert=require('assert/strict');
const root=path.resolve(__dirname,'../dashboard/static'),profile=fs.mkdtempSync(path.join(os.tmpdir(),'ice-browser-'));
const html=`<!doctype html><link rel="stylesheet" href="/ice.css"><div id="panels" style="width:800px"></div><script src="/plotly.min.js"></script><script>
window.errors=[];onerror=(m)=>errors.push(m);onunhandledrejection=e=>errors.push(String(e.reason));
const interval=window.setInterval;window.setInterval=(fn,ms)=>{if(ms===60000)window.refreshIce=fn;return interval(fn,ms)};
const t=Date.now(),specs=new Map();window.specs=specs;window.UW={state:{data:{t:[t-120000,t],leg:[0,0],dist_km:[0,1]},xmode:'time'},M:{legs:[{id:'test',index:0}]},THEME:{},CFG:{displayModeBar:false},shipAxis:t=>new Date(t),fmtTs:t=>new Date(t).toISOString(),spanFilter:()=>({start:t-3600000,end:t+60000}),inFilter:()=>true,axisZoom:()=>{},linkX:()=>{},selectColour:()=>{},registerColour:s=>window.colour=s,fetchJSON:async()=>({photos:[{id:'a'.repeat(20),time:t-120000,leg:'test',status:'filtered',ice:0,types:[0,0,0,0,0,0]},{id:'b'.repeat(20),time:t-60000,leg:'test',status:'gemma',ice:60,types:[0,0,0,20,40,0]},{id:'c'.repeat(20),time:t,leg:'test',status:'pending',ice:null,types:null}]}),registerPanel:(name,s)=>{let el=document.createElement('section');el.innerHTML='<h3></h3><span class="now"></span><div class="plot" style="height:220px"></div>';document.querySelector('#panels').append(el);specs.set(name,{s,el})},refreshExtraData:()=>{for(const {s,el} of specs.values())s.render(el,el.querySelector('.plot'))}};
</script><script src="/ice.js"></script>`;
const server=http.createServer((req,res)=>{const p=new URL(req.url,'http://localhost').pathname;if(p==='/'){res.end(html);return}if(['/ice.js','/plotly.min.js','/ice.css','/style.css'].includes(p)){res.setHeader('Content-Type',p.endsWith('.css')?'text/css':'text/javascript');res.end(fs.readFileSync(path.join(root,p)));return}res.writeHead(404);res.end()});
let child,ws;
const wait=ms=>new Promise(r=>setTimeout(r,ms));
(async()=>{try{
 await new Promise(r=>server.listen(0,'127.0.0.1',r));let stderr='';child=spawn(process.argv[2]||'/usr/bin/google-chrome',['--headless','--no-sandbox','--disable-dev-shm-usage','--remote-debugging-port=0','--user-data-dir='+profile,'about:blank']);child.stderr.on('data',d=>stderr+=d);
 for(let i=0;i<100&&!stderr.includes('DevTools listening');i++)await wait(100);
 const endpoint=stderr.match(/DevTools listening on (ws:\/\/\S+)/)?.[1];assert(endpoint,stderr);
 const pages=await(await fetch('http://'+new URL(endpoint).host+'/json/list')).json();ws=new WebSocket(pages.find(p=>p.type==='page').webSocketDebuggerUrl);await new Promise(r=>ws.addEventListener('open',r,{once:true}));let id=0;const pending=new Map();ws.onmessage=e=>{const m=JSON.parse(e.data);if(m.id){pending.get(m.id)(m);pending.delete(m.id)}};
 const call=(method,params={})=>new Promise(r=>{const n=++id;pending.set(n,r);ws.send(JSON.stringify({id:n,method,params}))});const evaluate=async expression=>{const r=await call('Runtime.evaluate',{expression,returnByValue:true,awaitPromise:true});if(r.result.exceptionDetails)throw Error(JSON.stringify(r.result.exceptionDetails));return r.result.result.value};
 await call('Page.navigate',{url:'http://127.0.0.1:'+server.address().port});
 for(let i=0;i<100;i++){if(await evaluate('Boolean(window.specs&&[...specs.values()][0]?.el.querySelector(".plot").data)'))break;await wait(100)}
 assert.equal(await evaluate('specs.size'),5);assert.deepEqual(await evaluate('errors'),[]);
 assert.deepEqual(await evaluate('[...specs.values()].map(p=>p.s.group)'),Array(5).fill('Ice camera'));
 assert.deepEqual(await evaluate('[...specs.values()].map(p=>p.s.label)'),['Concentration','Ice composition','Ice types','ROI','Slices']);
 assert.equal(await evaluate('[...specs.values()][0].s.chip().text'),'60%');
 assert.equal(await evaluate('[...specs.values()][0].s.groupSummary()'),'1/3 pending');
 assert((await evaluate('[...specs.values()][3].s.chip().image')).includes('kind=roi'));
 assert.equal(await evaluate('[...document.querySelectorAll(".now")].some(e=>e.textContent.includes("pending"))'),false);
 await evaluate('UW.fz=n=>n*1.5;UW.tzAbbr=()=>"EDT";const first=[...specs.values()][0];first.el.querySelector(".plot").prepend(document.createTextNode("No camera products in this window"));UW.refreshExtraData()');
 assert.equal(await evaluate('[...specs.values()][0].el.querySelector(".plot").layout.xaxis.tickfont.size'),18);
 assert.equal(await evaluate('[...specs.values()][0].el.querySelector(".plot").layout.margin.b'),51);
 assert.equal(await evaluate('[...specs.values()][0].el.querySelector(".plot").textContent.includes("No camera products")'),false);
 assert.equal(await evaluate('[...specs.values()][0].el.querySelector(".plot").layout.xaxis.title.text'),'ship time (EDT)');
 assert.deepEqual(await evaluate('[...specs.values()][0].el.querySelector(".plot").data[0].y'),[0,60,null]);
 assert.deepEqual(await evaluate('[...specs.values()][0].el.querySelector(".plot").data[1].y'),[30,30,null]);
 assert.equal(await evaluate('[...specs.values()][0].el.querySelector(".plot").data[1].name'),'1 h centered mean');
 assert.equal(await evaluate('[...specs.values()][1].el.querySelector(".plot").data[1].name'),'thin ice floe');
 assert.deepEqual(await evaluate('[...specs.values()][1].el.querySelector(".plot").data[1].y'),[0,0,null]);
 await evaluate('document.querySelector(".ice-image-button").click()');assert.equal(await evaluate('document.querySelector("dialog").open'),true);
 await evaluate('document.querySelector("dialog button").click()');assert((await evaluate('document.querySelector("dialog p").textContent')).includes('filtered'));
 assert.deepEqual(await evaluate('colour.sizes(UW.state.data)'),[4,3]);
 await evaluate('UW.spanFilter=()=>({start:t-90000,end:t});UW.refreshExtraData()');
 assert.equal(await evaluate('+new Date([...specs.values()][0].el.querySelector(".plot").layout.xaxis.range[0])'),await evaluate('t-90000'));
 assert.equal(await evaluate('[...specs.values()][1].el.querySelector(".plot").layout.showlegend'),false);
 assert.equal(await evaluate('document.querySelectorAll(".ice-type-legend span").length'),6);
 await evaluate(`new Promise(resolve=>{const plot=document.querySelector('.ice-roi');plot.parentElement.classList.add('panel');plot.style.width='250px';const im=plot.querySelector('img');im.onload=resolve;im.src='data:image/svg+xml,'+encodeURIComponent('<svg xmlns="http://www.w3.org/2000/svg" width="1200" height="600"></svg>')})`);
 assert.equal(await evaluate(`(()=>{const b=document.querySelector('.ice-image-button'),im=b.querySelector('img');return Math.abs(im.getBoundingClientRect().height-b.clientHeight)<2&&im.getBoundingClientRect().width>b.clientWidth&&getComputedStyle(b).overflow==='hidden'})()`),true);
 assert.deepEqual(await evaluate('errors'),[]);console.log('PASS: five panels, zero/pending distinction, ROI modal navigation, map sizes, no JS errors');
 await evaluate(`(async()=>{const plot=document.createElement('div');plot.className='plot';plot.style.height='200px';document.querySelector('#panels').append(plot);await Plotly.newPlot(plot,[{x:[new Date(t-120000),new Date(t)],y:[1,2]}],{xaxis:{type:'date',range:[new Date(t-120000),new Date(t)]}});const axis=plot._fullLayout.xaxis,rect=plot.getBoundingClientRect();plot.dispatchEvent(new PointerEvent('pointermove',{bubbles:true,clientX:rect.left+axis._offset+axis._length/2,clientY:rect.top+80}));await new Promise(requestAnimationFrame)})()`);
 assert.equal(await evaluate(`document.querySelectorAll('.ice-time-pointer:not([hidden])').length>=4`),true);
 assert.equal(await evaluate(`(()=>{for(const plot of document.querySelectorAll('#panels .plot')){const line=plot.querySelector('.ice-time-pointer'),y=plot._fullLayout?.yaxis;if(!y||!line||line.hidden)continue;const svg=plot.querySelector('.svg-container').getBoundingClientRect(),rect=plot.getBoundingClientRect();if(Math.abs(parseFloat(line.style.top)-(svg.top-rect.top+y._offset))>1||Math.abs(parseFloat(line.style.height)-y._length)>1)return false}return true})()`),true);
 assert.deepEqual(await evaluate('errors'),[]);console.log('PASS shared cursor from a non-camera dataset reaches other charts and slices');
 await evaluate(`(async()=>{UW.fetchJSON=async()=>({photos:[
 {id:'d',leg:'test',time:t-3600000,ice:100,status:'gemma'},
 {id:'e',leg:'test',time:t-1800000,ice:0,status:'filtered'},
 {id:'f',leg:'test',time:t,ice:60,status:'gemma'},
 {id:'g',leg:'other',time:t,ice:100,status:'gemma'},
 {id:'h',leg:'test',time:t+1800000,ice:30,status:'gemma'},
 {id:'i',leg:'test',time:t+1800001,ice:100,status:'gemma'},
 {id:'j',leg:'test',time:t+2400000,ice:null,status:'pending'}]});await refreshIce()})()`);
 assert.equal(await evaluate(`(()=>{const trace=[...specs.values()][0].el.querySelector('.plot').data[1];return trace.y[trace.customdata.findIndex(p=>p.id==='f')]})()`),30);
 assert.equal(await evaluate(`(()=>{const trace=[...specs.values()][0].el.querySelector('.plot').data[1];return trace.y[trace.customdata.findIndex(p=>p.id==='g')]})()`),100);
 console.log('PASS centered hour includes endpoints and zeros, excludes outside samples and separates legs');
 await evaluate(`new Promise(resolve=>{const link=document.createElement('link');link.rel='stylesheet';link.href='/style.css';link.onload=resolve;document.head.append(link)})`);
 for(const theme of ['navigator','navigator-dark']){
   await evaluate(`document.documentElement.dataset.theme=${JSON.stringify(theme)};UW.spanFilter=()=>({start:t-3600000,end:t+3600000});UW.refreshExtraData()`);
   await evaluate(`new Promise(resolve=>{const button=document.querySelector('.ice-slice');button.style.width='4px';const im=button.querySelector('img');im.onload=resolve;im.src='data:image/svg+xml,'+encodeURIComponent('<svg xmlns="http://www.w3.org/2000/svg" width="4" height="600"><rect width="4" height="600" fill="red"/></svg>')})`);
   assert.equal(await evaluate(`getComputedStyle(document.querySelector('.ice-slice')).borderLeftWidth`),'0px');
   assert.equal(await evaluate(`document.querySelector('.ice-slice img').getBoundingClientRect().width`),4);
 }
 console.log('PASS navigator slice images retain their full 4px width');
}finally{ws?.close();child?.kill();server.closeAllConnections();server.close()}})().catch(e=>{console.error(e);process.exitCode=1});
