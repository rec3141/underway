const {test}=require('node:test');
const assert=require('node:assert/strict');
const fs=require('node:fs');
const path=require('node:path');
const vm=require('node:vm');
const context={window:{UW:{}},document:{createElement:()=>({classList:{add(){},remove(){},toggle(){}},style:{},appendChild(){}})}};
vm.runInNewContext(fs.readFileSync(path.join(__dirname,'../dashboard/static/map.js'),'utf8'),context);
const spot=context.window.UW.mapTipSpot;

const covers=(s,tw,th,line)=>line.some((p,i)=>i&&segmentMeets(line[i-1],p,{x0:s.x,y0:s.y,x1:s.x+tw,y1:s.y+th}));
function segmentMeets(a,b,box){
  const dx=b.x-a.x,dy=b.y-a.y;let t0=0,t1=1;
  for(const [p,q] of [[-dx,a.x-box.x0],[dx,box.x1-a.x],[-dy,a.y-box.y0],[dy,box.y1-a.y]]){
    if(p===0){if(q<0)return false;continue;}
    const t=q/p;
    if(p<0){if(t>t1)return false;if(t>t0)t0=t;}else{if(t<t0)return false;if(t<t1)t1=t;}
  }
  return true;
}

test('a box goes to the right of its point when nothing is in the way',()=>{
  const s=spot({x:300,y:300},900,700,220,130,[]);
  assert.equal(s.x,314);
});

test('a box keeps off a route that leaves its point with no vertex nearby',()=>{
  // the route runs east-north-east from the point to the ship, two vertices only
  const point={x:260,y:350},line=[{x:260,y:350},{x:800,y:230}];
  const tw=230,th=140;
  const s=spot(point,900,880,tw,th,[line]);
  assert.equal(covers(s,tw,th,line),false,'the box sits on the route');
});

test('a lone point, such as the ship, is kept off as well',()=>{
  const ship=[{x:420,y:300}];
  const s=spot({x:300,y:300},900,700,220,130,[ship]);
  const on=ship[0].x>=s.x&&ship[0].x<=s.x+220&&ship[0].y>=s.y&&ship[0].y<=s.y+130;
  assert.equal(on,false);
});

test('a box hemmed in still lands inside the map',()=>{
  const s=spot({x:10,y:10},300,200,280,180,[[{x:0,y:0},{x:300,y:200}]]);
  assert.ok(s.x>=2&&s.y>=2&&s.x+280<=300&&s.y+180<=200+2);
});

test('a long route is thinned but still measured end to end',()=>{
  const line=Array.from({length:4000},(_,i)=>({x:200+i*0.15,y:350}));
  const tw=230,th=140;
  const s=spot({x:200,y:350},1000,800,tw,th,[line]);
  assert.equal(covers(s,tw,th,line),false);
});
