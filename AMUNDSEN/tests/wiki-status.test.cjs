const {test}=require('node:test'), assert=require('node:assert/strict'),fs=require('node:fs'),vm=require('node:vm'),path=require('node:path');
const staticDir=path.join(__dirname,'../dashboard/static');
const history=fs.readFileSync(path.join(staticDir,'history.js'),'utf8'),app=fs.readFileSync(path.join(staticDir,'app.js'),'utf8');
function between(source,start,end){return source.slice(source.indexOf(start),source.indexOf(end,source.indexOf(start)));}
test('artifact focus changes only focus/view, preserving layer and kind choices',()=>{
 const state={history:false,nature:false},types=new Set(),writes=[],focused=[];
 const context={UW:{state,focusMap:(...args)=>focused.push(args),mapMode:()=> 'half'},hist:{types},TYPES:{image:{}},store:{set:(...a)=>writes.push(a)}};
 vm.createContext(context);vm.runInContext(between(history,'  function focusPoint(', '  function focusArtifact(')+'\nfocusPoint(70,-80,"Image","image","history");',context);
 assert.deepEqual(state,{history:false,nature:false});assert.equal(types.size,0);assert.equal(writes.length,0);assert.deepEqual(focused,[[70,-80,'Image']]);
});
test('entering/leaving Wiki preserves explicit layers and later user changes',()=>{
 const state={history:false,nature:true,stations:true,events:false,track:true,sat:'sst',satAt:1};
 const context={UW:{state},document:{body:{classList:{toggle(){}}}},pill(){},renderChips(){}};
 vm.createContext(context);vm.runInContext(between(history,'  const SHIP_LAYERS =','  const prevTab =')+'\nhistoryMap(true);',context);
 assert.equal(state.history,false);assert.equal(state.nature,true);assert.equal(state.track,false);
 state.history=true;state.nature=false;vm.runInContext('historyMap(false)',context);
 assert.equal(state.history,true);assert.equal(state.nature,false);assert.equal(state.track,true);
});
function alerts(mode='open',now=null){
 const els=new Map();function el(id){if(!els.has(id))els.set(id,{hidden:false,innerHTML:'',dataset:{},classList:{toggle(){},remove(){}},querySelector(){return this.button ||= {};},querySelectorAll(){return[];},style:{}});return els.get(id);}
 const writes=[],renders=[],context={M:{calendar:{now}},$:el,inapp:{msgs:[]},alertTimer:null,schedMode:()=>mode,renderStatus(){renders.push(context.inapp.msgs.at(-1)?.text||'normal');},SITE:{local_tz:'UTC'},tms:Date.parse,fmtTs:x=>new Date(x).toISOString(),store:{set:(...a)=>writes.push(a)},esc:x=>String(x).replace(/</g,'&lt;'),setTimeout:()=>1,clearTimeout(){},Date};
 vm.createContext(context);vm.runInContext(between(app,'  function renderAlert()','  setInterval(() => { if (M?.calendar?.now)')+between(app,'  function renderInapp()','  async function pollInapp()'),context);
 return {context,el,writes,renders,render:()=>vm.runInContext('renderAlert();renderInapp()',context)};
}
test('inbox messages replace the subtitle without growing the schedule bar',()=>{
 const {context,el,renders,render}=alerts();context.inapp.msgs=[{t:'2026-09-13T12:00:00Z',text:'Schedule changed',receivedAt:Date.now()}];render();
 assert.equal(el('#alert').hidden,true);assert.equal(renders.at(-1),'Schedule changed');
});
test('hidden schedule remains hidden while a message replaces the subtitle',()=>{
 const {context,el,renders,render}=alerts('hidden',{in_progress:[]});context.inapp.msgs=[{t:'2026-09-13T12:00:00Z',text:'Updated',receivedAt:Date.now()}];render();
 assert.equal(el('#alert').hidden,true);assert.equal(renders.at(-1),'Updated');
});
test('template keeps status subtitle in the header',()=>{
 const template=fs.readFileSync(path.join(__dirname,'../dashboard/templates/index.html.j2'),'utf8');
 assert.match(template,/<div class="sub" id="status">loading…<\/div>/);
});
test('legacy auto-enabled layers reset once; new explicit choices survive reload',()=>{
 const source=between(history,'  if (store.get("wiki.map-layers.v", 0)','  // artifact kinds:');
 for(const legacy of [false,true]){
  const values=new Map(),state={history:legacy,nature:legacy};
  const context={UW:{state},store:{get:(k,d)=>values.get(k)??d,set:(k,v)=>values.set(k,v)}};
  vm.createContext(context);vm.runInContext(source,context);
  assert.deepEqual(state,{history:false,nature:false});
  state.history=true;values.set('history',true);vm.runInContext(source,context);
  assert.equal(state.history,true);assert.equal(values.get('history'),true);
 }
});
