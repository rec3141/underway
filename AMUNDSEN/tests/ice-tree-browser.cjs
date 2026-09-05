const {spawn}=require('node:child_process'),assert=require('node:assert/strict'),{pathToFileURL}=require('node:url');
const wait=ms=>new Promise(r=>setTimeout(r,ms));let child,ws;
const watchdog=setTimeout(()=>{child?.kill();process.exit(1)},60000);
(async()=>{try{
 let stderr='';child=spawn(process.argv[2],['--no-sandbox','--headless','--disable-gpu','--remote-debugging-port=0','about:blank']);child.stderr.on('data',d=>stderr+=d);
 for(let i=0;i<100&&!stderr.includes('DevTools listening');i++)await wait(100);
 const endpoint=stderr.match(/DevTools listening on (ws:\/\/\S+)/)[1],pages=await(await fetch(`http://${new URL(endpoint).host}/json/list`)).json();ws=new WebSocket(pages[0].webSocketDebuggerUrl);await new Promise(r=>ws.addEventListener('open',r,{once:true}));
 let id=0;const pending=new Map();ws.addEventListener('message',e=>{const m=JSON.parse(e.data);if(m.id){pending.get(m.id)(m);pending.delete(m.id);}});const call=(method,params={})=>new Promise(r=>{const n=++id;pending.set(n,r);ws.send(JSON.stringify({id:n,method,params}));});
 const ev=async expression=>{const r=await call('Runtime.evaluate',{expression,returnByValue:true,awaitPromise:true});if(r.result.exceptionDetails)throw Error(JSON.stringify(r.result.exceptionDetails));return r.result.result.value;};
 await call('Page.navigate',{url:pathToFileURL(process.argv[3]).href});for(let i=0;i<100;i++){if(await ev('!!document.getElementById("status")?.textContent'))break;await wait(100);}
 assert.equal(await ev('queue.length'),16);assert.equal(await ev("$('sheet').querySelectorAll('img').length"),await ev('displayed.length'));
 const key=k=>ev(`document.body.dispatchEvent(new KeyboardEvent('keydown',{key:${JSON.stringify(k)},bubbles:true}))`);
 await key('ArrowRight');assert.equal(await ev('cursor'),1);await key('ArrowLeft');assert.equal(await ev('cursor'),0);
 await ev('window.original=queue[cursor];window.originalLeaves=displayed.length');await key('ArrowDown');assert.equal(await ev('parent[queue[cursor]]===original'),true);assert(await ev('displayed.length<originalLeaves'));
 await ev('window.labelledIds=displayed.map(i=>data.points[i].id)');await key('l');assert(await ev("$('labelbox').open"));await ev("$('name').value='test ice';$('form').dispatchEvent(new Event('submit',{cancelable:true}))");assert.equal(await ev('displayed.some(i=>labelledIds.includes(data.points[i].id))'),false);
 await key('ArrowUp');assert.equal(await ev('displayed.some(i=>labelledIds.includes(data.points[i].id))'),false);
 await ev("$('undo').click()");assert.equal(await ev('labelledIds.some(id=>labels[id]?.length)'),false);
 await key('l');const before=await ev('queue[cursor]');await ev("$('name').dispatchEvent(new KeyboardEvent('keydown',{key:'ArrowDown',bubbles:true}))");assert.equal(await ev('queue[cursor]'),before);await ev("$('cancel').click()");
 console.log('PASS initial k16, complete proofsheet, left/right, split/parent, L dialog, labeling exclusion, undo and typing guard');
}finally{clearTimeout(watchdog);ws?.close();child?.kill();}})().catch(e=>{console.error(e);process.exitCode=1;});
