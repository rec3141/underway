/* Real MapLibre/WebGL integration with synthetic local satellite imagery.
 * Usage: node tests/map-satellite-browser.cjs /usr/bin/google-chrome */
const assert=require('node:assert/strict');
const fs=require('node:fs'),os=require('node:os'),path=require('node:path'),http=require('node:http');
const {spawn}=require('node:child_process');
const wait=ms=>new Promise(r=>setTimeout(r,ms));
const assets=path.join(__dirname,'../dashboard/static');
const html=`<!doctype html><style>#map{width:600px;height:400px}</style><div id="map"></div>
<script src="/maplibre-gl.js"></script><script src="/map.js"></script><script>
window.errors=[];window.done=false;
addEventListener('error',e=>errors.push(e.message));
(async()=>{
 const c=document.createElement('canvas');c.width=4800;c.height=3200;
 const ctx=c.getContext('2d');ctx.fillStyle='#dddddd';ctx.fillRect(0,0,c.width,c.height);
 ctx.fillStyle='#888888';ctx.fillRect(0,0,2400,1600);
 const url=URL.createObjectURL(await new Promise(r=>c.toBlob(r,'image/png')));
 const coords=[[-90,78],[-80,78],[-80,77],[-90,77]];
 const view=new UW.MapView(document.querySelector('#map'));
 const style={version:8,id:'test',sources:{sat:{type:'image',url,coordinates:coords}},layers:[{id:'sat',source:'sat',type:'raster'}]};
 view.create(style,{center:{lon:-85,lat:77.5},zoom:3});
 view.map.on('error',e=>errors.push(e.error.message));
 const gl=view.map.getCanvas().getContext('webgl2');
 const nativeLimit=gl.getParameter(gl.MAX_TEXTURE_SIZE),getParameter=gl.getParameter.bind(gl);
 // Exercise the constrained-device loader while keeping a real WebGL upload.
 gl.getParameter=p=>p===gl.MAX_TEXTURE_SIZE?4096:getParameter(p);
 await new Promise((resolve,reject)=>{view.map.once('idle',resolve);setTimeout(()=>reject(Error('Map did not become idle')),12000)});
 const first=view.map.getSource('sat');
 window.result={nativeLimit,width:first.image.width,height:first.image.height,coordinates:first.coordinates,loaded:first.loaded(),glError:gl.getError()};
 const next={...style,id:'archive',sources:{sat:{...style.sources.sat,url:url+'#archive'}}};
 view.setStyle(next);
 await new Promise((resolve,reject)=>{view.map.once('idle',resolve);setTimeout(()=>reject(Error('Archive did not become idle')),12000)});
 result.archiveWidth=view.map.getSource('sat').image.width;
 const removeProtocol=maplibregl.removeProtocol;
 maplibregl.removeProtocol=name=>{result.removed=name===view.imageProtocol;removeProtocol(name);};
 view.map.remove();URL.revokeObjectURL(url);window.done=true;
})().catch(e=>{errors.push(e.stack);window.done=true});
</script>`;
const server=http.createServer((req,res)=>{
 if(req.url==='/'){res.setHeader('Content-Type','text/html');res.end(html);return;}
 if(['/map.js','/maplibre-gl.js'].includes(req.url)){res.setHeader('Content-Type','text/javascript');res.end(fs.readFileSync(path.join(assets,req.url.slice(1))));return;}
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
 assert.equal(result.width,4096);assert.equal(result.height,2730);assert.equal(result.archiveWidth,4096);
 assert.deepEqual(result.coordinates,[[-90,78],[-80,78],[-80,77],[-90,77]]);
 assert.equal(result.loaded,true);assert.equal(result.glError,0);assert.equal(result.removed,true);
 console.log('PASS real MapLibre satellite source, constrained texture, archive swap, bounds and protocol cleanup',result);
}finally{ws?.close();child?.kill();server.closeAllConnections();server.close();clearTimeout(watchdog);}})().catch(e=>{console.error(e);process.exitCode=1});
