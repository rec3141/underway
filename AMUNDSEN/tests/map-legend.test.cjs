const {test}=require('node:test');
const assert=require('node:assert/strict');
const fs=require('node:fs');
const path=require('node:path');
const vm=require('node:vm');
const context={window:{}};
vm.runInNewContext(fs.readFileSync(path.join(__dirname,'../dashboard/static/map-legend.js'),'utf8'),context);
const {formatRange,draw}=context.window.UWMapLegend;
test('legend values are unit-free and conservatively rounded without collapsing narrow ranges',()=>{
  for(const [input,expected] of [
    [[123.456,987.654],['120','990']], [[-.012345,.06789],['-0.012','0.068']],
    [[34.651,34.659],['34.65','34.66']], [[0,0],['0','0']], [[NaN,1],['','']],
  ])assert.deepEqual(Array.from(formatRange(input)),expected);
});
test('landscape legend stays left, centers its title and puts values beyond the bar ends',()=>{
  const text=[],rects=[],translations=[];
  const ctx={save(){},restore(){},translate:(...args)=>translations.push(args),measureText:s=>({width:s.length*6}),fillRect:(...args)=>rects.push(args),strokeRect(){},createLinearGradient:()=>({addColorStop(){}}),fillText:function(s,x,y){text.push({s,x,y,align:this.textAlign})}};
  draw(ctx,700,400,{name:'Depth (m)',showScale:true,low:'0',high:'990',stops:[[0,'black'],[1,'white']]});
  assert.deepEqual(translations,[[8,8]]);
  const title=text[0],high=text.find(x=>x.s==='990'),low=text.find(x=>x.s==='0'),bar=rects[1];
  assert.equal(title.align,'center');assert.equal(title.x,rects[0][2]/2);
  assert(high.y<bar[1]);assert(low.y>bar[1]+bar[3]);assert.equal(high.x,low.x);
});
