const {test}=require('node:test');
const assert=require('node:assert/strict');
const fs=require('node:fs'),path=require('node:path'),vm=require('node:vm');
const source=fs.readFileSync(path.join(__dirname,'../dashboard/static/ice.js'),'utf8');
function setup(photos){
  const context=vm.createContext({photos,U:{cmap:()=>[],colourAt:(_,t)=>String(t)}});
  vm.runInContext(source.slice(source.indexOf('  function centeredMeans('),source.indexOf('  function recenterPhoto('))+
    source.slice(source.indexOf('  function meanTraces('),source.indexOf('  function concentrationTraces('))+'\nthis.api={centeredMeans,meanTraces};',context);
  return context.api;
}
test('centered hour includes both endpoints and zeros, excludes missing values and other legs',()=>{
  const rows=[
    {id:'outside',leg:'a',time:-1800001,ice:100},
    {id:'left',leg:'a',time:-1800000,ice:0},
    {id:'middle',leg:'a',time:0,ice:60},
    {id:'other',leg:'b',time:0,ice:100},
    {id:'missing',leg:'a',time:100,ice:null},
    {id:'right',leg:'a',time:1800000,ice:30},
    {id:'outside2',leg:'a',time:1800001,ice:100},
  ];
  const means=setup(rows).centeredMeans(rows);
  assert.equal(means.get('middle'),30);
  assert.equal(means.get('other'),100);
  assert.equal(means.get('missing'),null);
});
test('mean segments break at pending observations, leg changes and long gaps',()=>{
  const rows=[0,60000,120000,180000,240000,300000,1200000,1260000].map((time,i)=>({id:String(i),time,leg:i<4?'a':'b',ice:i===2?null:40}));
  const traces=setup(rows).meanTraces({x:rows.map(p=>p.time)},rows,rows.map(()=>1),rows.map(()=>true),{limits:[0,2],variable:{}});
  assert.equal(traces.length,1);
  assert.deepEqual(Array.from(traces[0].x),[0,60000,null,240000,300000,null,1200000,1260000,null]);
  assert.equal(traces[0].mode,'lines');
});
test('long records use bounded colour traces instead of one trace per segment',()=>{
  const rows=Array.from({length:10000},(_,i)=>({id:String(i),time:i*60000,leg:'a',ice:40}));
  const api=setup(rows),base={x:rows.map(p=>p.time)},valid=rows.map(()=>true);
  const numeric=api.meanTraces(base,rows,rows.map((_,i)=>i),valid,{limits:[0,9999],variable:{}});
  const rgb=api.meanTraces(base,rows,rows.map((_,i)=>`rgb(${i%256},${(i*17)%256},${(i*43)%256})`),valid,{variable:{rgb:true}});
  assert(numeric.length<=64);
  assert(rgb.length<=64);
  assert.equal(numeric.reduce((n,t)=>n+t.x.length/3,0),9999);
  assert.equal(rgb.reduce((n,t)=>n+t.x.length/3,0),9999);
});
