const {test}=require('node:test');
const assert=require('node:assert/strict');
const fs=require('node:fs');
const path=require('node:path');
const vm=require('node:vm');
const script=fs.readFileSync(path.join(__dirname,'../tools/underway-local.html'),'utf8').match(/<script>([\s\S]*?)<\/script>/)[1];
for(const result of ['load','error','timeout'])test(`launcher: ${result}`,()=>{
  let probe,timer;const navigations=[];
  const context={Image:class{constructor(){probe=this}},setTimeout:fn=>{timer=fn;return 1},clearTimeout(){},document:{getElementById:()=>({textContent:''})},location:{replace:url=>navigations.push(url)}};
  vm.runInNewContext(script,context);
  assert.match(probe.src,/underway\.local\/static\/geo\/sprite\.png/);
  if(result==='timeout')timer();else probe['on'+result]();
  assert.deepEqual(navigations,[result==='load'?'http://underway.local/':'http://10.0.0.58/underway/']);
  probe.onload();timer();assert.equal(navigations.length,1);
});
