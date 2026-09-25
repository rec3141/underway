const {test}=require('node:test');
const assert=require('node:assert/strict');
const fs=require('node:fs'),path=require('node:path'),vm=require('node:vm');
const ctx={window:{}};
vm.runInNewContext(fs.readFileSync(path.join(__dirname,'../dashboard/static/ladcp.js'),'utf8'),ctx);
const {sampleAt,arrowPoints}=ctx.window.UWLadcp;
const profile={depth:[8,16,24,32,64,72],vars:{'Eastward current':[0,.3,null,.2,.1,.1],'Northward current':[.5,.4,.1,-.2,.1,.1],'Current error':[.01,.02,.03,.04,.05,.06]}};
test('depth selection keeps measured components, bin and uncertainty',()=>{
 const s=sampleAt(profile,17);assert.equal(s.depth,16);assert.equal(s.speed,.5);assert.equal(s.error,.02);assert.ok(Math.abs(s.direction-36.8699)<.001);
 assert.equal(sampleAt(profile,8).direction,0);
});
test('no extrapolation, missing-data bridging or null-to-zero current',()=>{
 for(const d of [0,7.9,72.1,48,24,NaN])assert.equal(sampleAt(profile,d),null);
 const calm=sampleAt({depth:[8],vars:{'Eastward current':[0],'Northward current':[0]}},8);
 assert.equal(calm.speed,0);assert.equal(calm.direction,null);assert.equal(calm.error,null);
});
test('arrows point toward flow in all cardinal directions and retain screen scale',()=>{
 for(const [u,v] of [[.5,0],[-.5,0],[0,.5],[0,-.5]]){
  const pts=arrowPoints(76,-85,u,v,7),start=pts[0],end=pts[1];
  if(u)assert.equal(Math.sign(end[0]-start[0]),Math.sign(u));
  if(v)assert.equal(Math.sign(end[1]-start[1]),Math.sign(v));
  const y=lat=>Math.log(Math.tan(Math.PI/4+lat*Math.PI/360))*180/Math.PI;
  const pixels=Math.hypot(end[0]-start[0],y(end[1])-y(start[1]))*512*2**7/360;
  assert.ok(Math.abs(pixels-36)<1e-8);
 }
 assert.equal(arrowPoints(76,-85,0,0,7).length,0);
});
