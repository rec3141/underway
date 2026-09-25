const {test}=require('node:test');
const assert=require('node:assert/strict');
const fs=require('node:fs'),path=require('node:path'),vm=require('node:vm');
const ctx={window:{}};
vm.runInNewContext(fs.readFileSync(path.join(__dirname,'../dashboard/static/ladcp.js'),'utf8'),ctx);
const {sampleAt}=ctx.window.UWLadcp;
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
test('arrow length follows speed on a fixed screen scale',()=>{
 const {arrowLength}=ctx.window.UWLadcp;
 assert.equal(arrowLength(.1),20);assert.equal(arrowLength(.25),50);
 assert.equal(arrowLength(0.001),6);assert.equal(arrowLength(2),120);
 assert.ok(arrowLength(.2)>arrowLength(.1));
});
test('the depth slider is quadratic: fine steps near the surface',()=>{
 const {depthFromSlider,sliderFromDepth}=ctx.window.UWLadcp;
 assert.equal(depthFromSlider(0,1000),0);assert.equal(depthFromSlider(1,1000),1000);assert.equal(depthFromSlider(.5,1000),250);
 assert.ok(depthFromSlider(.1,1000)<=10);
 for(const d of [0,7,50,333,1000])assert.equal(depthFromSlider(sliderFromDepth(d,1000),1000),d);
 assert.equal(sliderFromDepth(2000,1000),1);assert.equal(sliderFromDepth(5,0),0);
});
