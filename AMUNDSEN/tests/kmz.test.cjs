/* The KMZ export: the KML a set of traces becomes, and the zip it ships in. */
const {test}=require('node:test');
const assert=require('node:assert/strict');
const fs=require('node:fs');
const path=require('node:path');
const zlib=require('node:zlib');

const load=name=>fs.readFileSync(path.join(__dirname,'../dashboard/static',name),'utf8');
const window={UW:{},document:{title:'Amundsen'}};
new Function('window',load('map.js'))(window);          // the real UW.cmap/UW.colourAt
new Function('window',load('map-kmz.js'))(window);
const KMZ=window.UWMapKMZ;

const count=(s,tag)=>(s.match(new RegExp(`<${tag}[ >]`,'g'))||[]).length;
const meta={title:'Underway map',generated:new Date('2026-09-20T12:34:56Z')};

test('a null in a line trace starts a new LineString and the point is dropped',()=>{
  const doc=KMZ.kml({base:[{name:'Track',mode:'lines',line:{color:'#ff0000',width:3},
    lat:[70,70.5,null,71,71.5],lon:[-90,-90.5,null,-91,-91.5]}]},meta);
  assert.equal(count(doc,'LineString'),2);
  assert.match(doc,/<coordinates>-90,70,0 -90.5,70.5,0<\/coordinates>/);
  assert.match(doc,/<coordinates>-91,71,0 -91.5,71.5,0<\/coordinates>/);
  assert.equal(count(doc,'Folder'),1);
  assert.match(doc,/<name>Track 1<\/name>/);
  assert.match(doc,/<LineStyle><color>ff0000ff<\/color><width>3<\/width>/);
});

test('a trace with no name and a trace with no finite point',()=>{
  const doc=KMZ.kml({base:[{mode:'markers',lat:[70],lon:[-90]},{name:'Empty',mode:'markers',lat:[null],lon:[null]}]},meta);
  assert.match(doc,/<Folder><name>Layer 1<\/name>/);
  assert.doesNotMatch(doc,/Empty/);
});

test('numeric marker colours become a bounded set of quantised styles',()=>{
  const n=500;
  const trace={name:'SST',mode:'markers',marker:{size:6,colorscale:'Viridis',
    color:Array.from({length:n},(_,i)=>i/n)},lat:[],lon:[]};
  for(let i=0;i<n;i++){trace.lat.push(70+i/1000);trace.lon.push(-90+i/1000);}
  const doc=KMZ.kml({base:[trace]},meta);
  assert.equal(count(doc,'Point'),n);
  const styles=count(doc,'Style');
  assert(styles<=KMZ.QUANT,`${styles} styles`);
  assert(styles>=KMZ.QUANT-1,`${styles} styles`);
  // the colours are the map's own ramp, alpha first then BGR
  const first=window.UW.colourAt(window.UW.cmap('Viridis'),0.5/KMZ.QUANT);
  assert.match(doc,new RegExp(`<color>${KMZ.kmlColour(first)}</color>`));
});

test('a missing value on a coloured track keeps its own style',()=>{
  const doc=KMZ.kml({base:[{name:'Salinity',mode:'markers',marker:{color:[1,null,3],cmin:0,cmax:4},
    lat:[70,71,72],lon:[-90,-91,-92]}]},meta);
  assert.equal(count(doc,'Point'),3);
  assert.match(doc,new RegExp(`<color>${KMZ.kmlColour('#7d8895')}</color>`));
});

test('hover HTML comes out as plain text and the name follows the trace text',()=>{
  const doc=KMZ.kml({base:[{name:'Stations',mode:'markers+text',marker:{size:9,color:'rgb(0,128,255)'},
    text:['ST-12'],hovertext:['<b>ST&nbsp;12</b><br>Depth&nbsp;320&nbsp;m &amp; rising<br><i>cast 4</i>'],
    lat:[70],lon:[-90]}]},meta);
  assert.match(doc,/<name>ST-12<\/name>/);
  assert.match(doc,/<description>ST 12\nDepth 320 m &amp; rising\ncast 4<\/description>/);
  assert.equal(count(doc,'Point'),1);
  assert.match(doc,/<color>ffff8000<\/color>/);         // rgb(0,128,255) -> aabbggrr
});

test('a silent trace carries no description and a text trace still places its labels',()=>{
  const quiet=KMZ.kml({base:[{name:'Grid',mode:'markers',hoverinfo:'skip',hovertext:['hidden'],lat:[70],lon:[-90]}]},meta);
  assert.doesNotMatch(quiet,/hidden/);
  const labels=KMZ.kml({base:[{name:'Places',mode:'text',text:['Iqaluit'],lat:[63.75],lon:[-68.5]}]},meta);
  assert.match(labels,/<name>Iqaluit<\/name>/);
  assert.equal(count(labels,'Point'),1);
});

test('the live group and opacity ride along, and the document is framed on everything',()=>{
  const doc=KMZ.kml({base:[{name:'Track',mode:'lines',lat:[70,72],lon:[-90,-86]}],
    live:[{name:'Ship',mode:'markers',opacity:.5,marker:{size:12,color:'#fff'},lat:[71],lon:[-88]}]},meta);
  assert.equal(count(doc,'Folder'),2);
  assert.match(doc,/<color>80ffffff<\/color>/);
  assert.match(doc,/<latitude>71<\/latitude>/);
  assert.match(doc,/<longitude>-88<\/longitude>/);
  assert.match(doc,/Exported from the CCGS Amundsen underway dashboard at 2026-09-20T12:34:56Z/);
});

test('ice polygons are read off the MapLibre source and coloured by its paint expression',()=>{
  const data={type:'FeatureCollection',features:[
    {geometry:{type:'Polygon',coordinates:[[[-90,70],[-89,70],[-89,71],[-90,70]]]},properties:{concentration:9,egg:'9/9'}},
    {geometry:{type:'Point',coordinates:[-90,70]},properties:{}}]};
  const fill=['case',['==',['get','concentration'],null],'#9aa5b1',
    ['step',['get','concentration'],'#b9e5fa',1,'#8edb91',4,'#fff176',7,'#ffb74d',9,'#ef5350']];
  const mapView={map:{getSource:()=>({_data:data}),getLayer:()=>true,
    getPaintProperty:(_,p)=>p==='fill-color'?fill:0.6}};
  const found=KMZ.icePolygons(mapView);
  assert.equal(found.features.length,1);
  assert.equal(found.colour(found.features[0]),'#ef5350');
  const doc=KMZ.kml({},{...meta,polygons:found});
  assert.equal(count(doc,'Polygon'),1);
  assert.match(doc,/<Folder><name>Ice chart<\/name>/);
  assert.match(doc,/egg: 9\/9/);
  assert.match(doc,/<outerBoundaryIs><LinearRing><coordinates>-90,70,0 -89,70,0 -89,71,0 -90,70,0<\/coordinates>/);
});

// A zip read back the way Google Earth reads one: the central directory names
// the entries, and each local header's payload inflates to what went in.
function unzip(bytes){
  const view=new DataView(bytes.buffer,bytes.byteOffset,bytes.byteLength);
  let end=bytes.length-22;
  while(end>=0&&view.getUint32(end,true)!==0x06054b50)end--;
  assert(end>=0,'no end-of-central-directory record');
  const total=view.getUint16(end+10,true);
  let at=view.getUint32(end+16,true);
  const out={};
  for(let i=0;i<total;i++){
    assert.equal(view.getUint32(at,true),0x02014b50);
    const nameLen=view.getUint16(at+28,true),extraLen=view.getUint16(at+30,true),commentLen=view.getUint16(at+32,true);
    const name=Buffer.from(bytes.subarray(at+46,at+46+nameLen)).toString('utf8');
    const offset=view.getUint32(at+42,true);
    assert.equal(view.getUint32(offset,true),0x04034b50);
    const method=view.getUint16(offset+8,true),crc=view.getUint32(offset+14,true);
    const packed=view.getUint32(offset+18,true),plain=view.getUint32(offset+22,true);
    const start=offset+30+view.getUint16(offset+26,true)+view.getUint16(offset+28,true);
    const body=Buffer.from(bytes.subarray(start,start+packed));
    const data=method===8?zlib.inflateRawSync(body):body;
    assert.equal(data.length,plain);
    assert.equal(zlib.crc32?zlib.crc32(data):crc,crc);
    out[name]=data;
    at+=46+nameLen+extraLen+commentLen;
  }
  // what the end record claims about the directory has to be true of it, or
  // a reader that trusts those fields (Python's zipfile, Google Earth) refuses the file
  assert.equal(at,end,'the central directory must end where the end record begins');
  assert.equal(view.getUint32(end+12,true),at-view.getUint32(end+16,true),'the end record must measure the central directory');
  return out;
}

test('the zip round-trips through a reader that only knows the central directory',async()=>{
  const text='doc'.repeat(400);
  const binary=Uint8Array.from({length:256},(_,i)=>i);
  const bytes=await KMZ.zip({'doc.kml':text,'files/dot.png':binary},new Date('2026-09-20T12:34:56Z'));
  assert.equal(bytes.constructor.name,'Uint8Array');
  const files=unzip(bytes);
  assert.deepEqual(Object.keys(files),['doc.kml','files/dot.png']);
  assert.equal(files['doc.kml'].toString('utf8'),text);
  assert.deepEqual(Uint8Array.from(files['files/dot.png']),binary);
});

test('a download names the file for the hour it was made and holds a KML and its icon',async()=>{
  const saved=[];
  const anchor={click(){},remove(){},set download(v){saved.push(v);},get download(){return saved.at(-1);}};
  window.document={title:'Underway map',createElement:()=>anchor,body:{append(){}}};
  window.URL={createObjectURL:()=>'blob:x',revokeObjectURL(){}};
  window.Blob=class{constructor(parts){this.parts=parts;}};
  window.setTimeout=()=>0;
  const bytes=await KMZ.download({traces:{base:[{name:'Track',mode:'lines',lat:[70,71],lon:[-90,-91]}],live:[]}},
    {generated:new Date('2026-09-20T12:34:56Z')});
  assert.equal(saved.at(-1),'amundsen-map-20260920-1234.kmz');
  const files=unzip(bytes);
  assert.deepEqual(Object.keys(files),['doc.kml','files/dot.png']);
  assert.match(files['doc.kml'].toString('utf8'),/<LineString>/);
  assert.equal(files['files/dot.png'].subarray(0,8).toString('binary'),'\x89PNG\r\n\x1a\n');
});
