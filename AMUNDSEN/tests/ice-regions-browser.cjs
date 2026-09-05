const {spawn}=require('node:child_process'),assert=require('node:assert/strict'),{pathToFileURL}=require('node:url');
const wait=ms=>new Promise(r=>setTimeout(r,ms));let child,ws;
const watchdog=setTimeout(()=>{child?.kill();process.exit(1)},60000);
(async()=>{try{
 let stderr='';child=spawn(process.argv[2],['--no-sandbox','--headless','--disable-gpu','--remote-debugging-port=0','about:blank']);child.stderr.on('data',d=>stderr+=d);
 for(let i=0;i<100&&!stderr.includes('DevTools listening');i++)await wait(100);
 const endpoint=stderr.match(/DevTools listening on (ws:\/\/\S+)/)[1],pages=await(await fetch(`http://${new URL(endpoint).host}/json/list`)).json();
 ws=new WebSocket(pages[0].webSocketDebuggerUrl);await new Promise(r=>ws.addEventListener('open',r,{once:true}));
 let id=0;const pending=new Map();ws.addEventListener('message',e=>{const m=JSON.parse(e.data);if(m.id){pending.get(m.id)(m);pending.delete(m.id);}});
 const call=(method,params={})=>new Promise(r=>{const n=++id;pending.set(n,r);ws.send(JSON.stringify({id:n,method,params}));});
 const ev=async expression=>{const r=await call('Runtime.evaluate',{expression,returnByValue:true,awaitPromise:true});if(r.result.exceptionDetails)throw Error(JSON.stringify(r.result.exceptionDetails));return r.result.result.value;};
 await call('Page.navigate',{url:pathToFileURL(process.argv[3]).href});
 for(let i=0;i<100;i++){if(await ev('!!document.getElementById("title")?.textContent'))break;await wait(100);}
 assert(await ev('data.points.length>10000'));
 assert(await ev("Object.keys(data.projections).includes('umap')"));
 assert.equal(await ev('new Set(cutTree([[0,1,1,2],[2,3,5,3]],3,0)).size'),3);
 assert.equal(await ev('new Set(cutTree([[0,1,1,2],[2,3,5,3]],3,1)).size'),2);
 assert.equal(await ev('new Set(cutTree([[0,1,1,2],[2,3,5,3]],3,5)).size'),1);
 await ev("$('linkage').value='ward';recut();$('height').value=1000;recut()");assert.equal(await ev('clusters.size'),1);
 await ev("selection=new Set([data.points[0].id,data.points[1].id]);$('label').value='ice';applyLabel();selection=new Set([data.points[1].id]);$('label').value='fog';applyLabel()");
 assert.deepEqual(await ev('human[data.points[1].id]'),['fog','ice']);
 await ev("$('projection').value='umap';reposition()");assert.deepEqual(await ev('human[data.points[1].id]'),['fog','ice']);
 assert(await ev('inside(1,1,[[0,0],[2,0],[2,2],[0,2]])'));assert.equal(await ev('inside(3,1,[[0,0],[2,0],[2,2],[0,2]])'),false);
 await ev("canvas.setPointerCapture=()=>{};canvas.releasePointerCapture=()=>{};$('mode').value='pan';canvas.onpointerdown({clientX:200,clientY:300,pointerId:1});canvas.onpointermove({clientX:220,clientY:300});canvas.onpointerup({clientX:220,clientY:300,pointerId:1})");assert(await ev('panX>0'));
 await ev("window.exported=null;URL.createObjectURL=b=>{window.exported=b;return 'blob:test'};HTMLAnchorElement.prototype.click=()=>{};$('export').click()");const exported=JSON.parse(await ev('exported.text()'));assert(exported.labels.some(r=>r.labels.includes('fog')&&r.labels.includes('ice')));
 assert.equal(await ev("JSON.parse(localStorage.getItem(key))[data.points[1].id].length"),2);
 await ev("$('height').value=1000;recut();$('queue').click()");const queue=JSON.parse(await ev('exported.text()'));assert(queue.queue.length<=3&&queue.queue.length>0);
 console.log('PASS full-data projection switch, Ward cuts, pan, polygon hit test, overlapping labels, persistence, export and representative queue');
}finally{clearTimeout(watchdog);ws?.close();child?.kill();}})().catch(e=>{console.error(e);process.exitCode=1;});
