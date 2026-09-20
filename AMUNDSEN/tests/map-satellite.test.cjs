const {test}=require('node:test');
const assert=require('node:assert/strict');
const fs=require('node:fs');
const path=require('node:path');
const vm=require('node:vm');

function setup(limit=4096) {
  const protocols=new Map(), decoded=[];
  const canvas={getContext:()=>({MAX_TEXTURE_SIZE:1,getParameter:()=>limit}),addEventListener(){}};
  const context={window:{}, document:{createElement:()=>({})},
    fetch:async(url,options)=>{context.request={url,options};return {ok:true,blob:async()=>({})};},
    createImageBitmap:async(input,options)=>{
      const image={width:options?.resizeWidth||4800,height:options?.resizeHeight||3200,closed:false,close(){this.closed=true;}};
      decoded.push(image);return image;
    },
    maplibregl:{addProtocol:(name,loader)=>protocols.set(name,loader),Map:class {
      constructor(options){this.options=options;this.touchZoomRotate={disableRotation(){}};this.keyboard={};this.doubleClickZoom={disable(){}};}
      getCanvas(){return canvas;}
      on(){}
      setStyle(style){this.options.style=style;}
    }},
  };
  vm.runInNewContext(fs.readFileSync(path.join(__dirname,'../dashboard/static/map.js'),'utf8'),context);
  const view=new context.window.UW.MapView({appendChild(){}});
  const corners=[[-90,78],[-80,78],[-80,77],[-90,77]];
  const style={id:'satellite',sources:{sat:{type:'image',url:'https://example.test/s1.webp?v=123',coordinates:corners},base:{type:'raster',tiles:['tiles/{z}/{x}/{y}.png']}},layers:[]};
  view.create(style);
  const load=(controller=new AbortController())=>protocols.get(view.imageProtocol)({url:view.map.options.style.sources.sat.url},controller);
  const loadUrl=(url,controller=new AbortController())=>protocols.get(view.imageProtocol)({url},controller);
  return {context,view,style,decoded,load,loadUrl};
}

test('oversized radar texture fits GPU with its aspect ratio and geographic corners intact',async()=>{
  const {context,view,style,decoded,load}=setup();
  const {data}=await load();
  assert.equal(data.width,4096);assert.equal(data.height,2730);
  assert.equal(decoded[0].closed,true);
  assert.equal(view.map.options.style.sources.sat.coordinates,style.sources.sat.coordinates);
  assert.equal(view.map.options.style.sources.base,style.sources.base);
  assert.equal(context.request.url,style.sources.sat.url);
  assert.equal(style.sources.sat.url,'https://example.test/s1.webp?v=123');
  const next={...style,id:'archive',sources:{...style.sources,sat:{...style.sources.sat,url:'https://example.test/archive/s1.webp'}}};
  view.setStyle(next);await load();
  assert.equal(context.request.url,next.sources.sat.url);
});

test('capable GPU retains the full-resolution radar image',async()=>{
  const {decoded,load}=setup(8192);
  const {data}=await load();
  assert.equal(data.width,4800);assert.equal(data.height,3200);
  assert.equal(decoded.length,1);assert.equal(data.closed,false);
});

test('dynamic image sources use the GPU fitting protocol',async()=>{
  const {context,view,loadUrl}=setup(2048);
  const original='blob:https://example.test/large-chart';
  const url=view.imageUrl(original);
  assert.match(url,/^uw-image-\d+:\/\//);
  const {data}=await loadUrl(url);
  assert.equal(context.request.url,original);
  assert.equal(data.width,2048);assert.equal(data.height,1365);
});

test('failed image request rejects instead of uploading a blank texture',async()=>{
  const {context,decoded,load}=setup();
  context.fetch=async()=>({ok:false,status:404});
  await assert.rejects(load(),/404/);assert.equal(decoded.length,0);
});

test('cancelled image load releases decoded bitmap',async()=>{
  const {decoded,load}=setup();
  const controller=new AbortController();controller.abort();
  await assert.rejects(load(controller),{name:'AbortError'});
  assert.equal(decoded[0].closed,true);
});
