/* Released camera products only. Browser never starts models or walks the share. */
(() => {
  const U=window.UW;if(!U?.registerPanel)return;
  const types=['grease ice','nilas','thin ice floe','icy bits','brash ice','thick ice floe'],palette=['#7ee787','#57c9bd','#78baff','#e3b341','#ff9e72','#d2a8ff'];
  const typeOrder=[0,1,3,4,2,5]; // Display order only; preserve the API's class indices.
  const detailedSurface=[['whitecaps',['whitecap']],['small waves',['small waves','water (unspecified)']],['calm',['calm water']],['grease',['grease ice']],['nilas',['nilas']],['bits',['icy bits']],['brash',['brash ice']],['thin',['thin ice floe']],['thick',['thick ice floe']]];
  const compactSurface=[['water',['whitecap','small waves','calm water','water (unspecified)']],['new ice',['grease ice','nilas']],['broken ice',['icy bits','brash ice']],['ice floe',['thin ice floe','thick ice floe']]];
  const surfaceValue=(p,name)=>p.ice==null?null:p.surface?.[name]??(types.includes(name)?p.types?.[types.indexOf(name)]??null:name==='water (unspecified)'?(p.status==='filtered'?100:0):p.status==='filtered'?0:null);
  const groupedSurface=(p,keys)=>{const values=keys.map(k=>surfaceValue(p,k)).filter(v=>v!=null);return values.length?values.reduce((a,b)=>a+b,0):null;};
  function recenterPhoto(p){
    const d=U.state.data;if(!d?.lat)return;
    const leg=U.M.legs.find(l=>l.id===p.leg)?.index;
    let best=-1,delta=Infinity;
    for(let i=0;i<d.t.length;i++)if(d.leg[i]===leg&&d.lat[i]!=null&&d.lon[i]!=null&&Math.abs(d.t[i]-p.time)<delta){best=i;delta=Math.abs(d.t[i]-p.time)}
    if(best>=0&&delta<=Math.max(120000,(d.step_s||60)*1000))U.focusMap?.(d.lat[best],d.lon[best],U.fmtTs(p.time),true);
  }
  const colour='Camera · ice type',names=['Camera · concentration','Camera · ice composition','Camera · ice fingerprints','Camera · ROI','Camera · slices'];
  const concentrationColour='Camera · ice concentration',sliceColour='Camera · slice mean RGB';
  const panelColour=i=>i===0?concentrationColour:i>=3?sliceColour:colour;
  let photos=[],chosen=null,lastKey='',busy=false,matchedData=null,matched=[],error='';
  const image=(p,kind)=>`api/ice/image?id=${encodeURIComponent(p.id)}&kind=${kind}`;
  const visible=()=>photos.filter(p=>U.inFilter(p.leg,p.time,U.currentFilter()));
  const dominant=p=>p.types?.indexOf(Math.max(...p.types));
  const labels=['Concentration','Ice composition','Surface types','ROI','Slices'];
  const latest=()=>visible().filter(p=>p.ice!=null).at(-1);
  function chip(mode){const p=latest();if(p&&mode===2){const [name,keys]=compactSurface.reduce((a,b)=>(groupedSurface(p,b[1])??-1)>(groupedSurface(p,a[1])??-1)?b:a);return {text:`${name} · ${groupedSurface(p,keys)??0}%`,image:null}}return {text:p?(mode===1?`${p.ice===0?'water':types[dominant(p)]||'ice'} · ${p.ice}%`:`${p.ice}%`):'—',image:p&&mode>=3?image(p,'roi'):null}}
  let cursor=null,cursorMode=null,cursorFrame=0;
  function paintCursor(){cursorFrame=0;for(const plot of document.querySelectorAll('#panels .plot')){let line=plot.querySelector(':scope > .ice-time-pointer');if(!line){line=document.createElement('i');line.className='ice-time-pointer';line.setAttribute('aria-hidden','true');plot.append(line)}const axis=plot._fullLayout?.xaxis;let left,top,height;if(axis){const pixel=axis.d2p(cursor);const origin=plot.querySelector(".svg-container")?.getBoundingClientRect()||plot.getBoundingClientRect(),box=plot.getBoundingClientRect(),y=plot._fullLayout.yaxis;left=origin.left-box.left+axis._offset+pixel;top=origin.top-box.top+(y?._offset??plot._fullLayout._size.t);height=y?._length??plot._fullLayout._size.h;if(pixel<0||pixel>axis._length)left=null}else if(plot._iceRange){const [lo,hi]=plot._iceRange;const value=typeof cursor==='number'?cursor:Date.parse(cursor + 'Z');const f=(value-lo)/(hi-lo||1);if(f>=0&&f<=1){left=f*plot.clientWidth;top=0;height=plot.clientHeight*.9}}line.hidden=cursor==null||cursorMode!==U.state.xmode||left==null||!Number.isFinite(left);if(!line.hidden){plot.style.position='relative';line.style.left=left+'px';line.style.top=top+'px';line.style.height=height+'px'}}}
  function pointAt(x){cursor=x!=null&&U.state.xmode==='time'&&typeof x!=='string'?U.plotDate(+x):x;cursorMode=U.state.xmode;if(!cursorFrame)cursorFrame=requestAnimationFrame(paintCursor)}
  function selectedCursor(){const p=chosen&&xpoints([chosen])[0];pointAt(p?p.x:null)}
  const panelRoot=document.querySelector('#panels');
  panelRoot.addEventListener('pointermove',e=>{const plot=e.target.closest('.plot');if(!plot)return;const rect=plot.getBoundingClientRect(),axis=plot._fullLayout?.xaxis;if(axis){const origin=plot.querySelector('.svg-container')?.getBoundingClientRect()||rect;const px=e.clientX-origin.left-axis._offset;if(px>=0&&px<=axis._length)pointAt(axis.p2d(px))}else if(plot._iceRange){const [lo,hi]=plot._iceRange;pointAt(lo+(e.clientX-rect.left)/rect.width*(hi-lo))}});
  panelRoot.addEventListener('pointerleave',selectedCursor);
  function nearest(a,t){let lo=0,hi=a.length;while(lo<hi){const mid=(lo+hi)>>1;if(a[mid].time<t)lo=mid+1;else hi=mid;}return [a[lo-1],a[lo]].filter(Boolean).sort((x,y)=>Math.abs(x.time-t)-Math.abs(y.time-t))[0]}
  function matches(d){if(d===matchedData)return matched;matchedData=d;const legs=new Map();for(const p of photos){if(!legs.has(p.leg))legs.set(p.leg,[]);legs.get(p.leg).push(p)}matched=d.t.map((t,i)=>{const leg=U.M.legs.find(l=>l.index===d.leg[i])?.id,p=nearest(legs.get(leg)||[],t);return p&&Math.abs(p.time-t)<=120000?p:null});return matched}
  const popup=document.createElement('dialog');popup.className='ice-preview';popup.innerHTML='<div class="head"><button data-step="-1" aria-label="Previous image">←</button><button data-step="1" aria-label="Next image">→</button><button class="close">Close ✕</button></div><p></p><img alt="Ship camera context with ROI outline">';document.body.append(popup);popup.querySelector('.close').onclick=()=>popup.close();
  function choose(p){if(!p)return;recenterPhoto(p);chosen=p;popup.querySelector('p').textContent=`${U.fmtTs(p.time)} · ${p.status} · ${p.ice==null?'pending':p.ice+'% ice'}`;popup.querySelector('img').src=image(p,'source');U.refreshExtraData();selectedCursor();if(!popup.open)document.querySelector('.ice-roi')?.focus({preventScroll:true})}
  function step(n){const a=visible().filter(p=>p.status!=='pending');if(a.length)choose(a[Math.max(0,Math.min(a.length-1,a.findIndex(p=>p.id===chosen?.id)+n))])}
  popup.querySelectorAll('[data-step]').forEach(b=>b.onclick=()=>step(Number(b.dataset.step)));
  document.addEventListener('keydown',e=>{if(!chosen||(!popup.open&&!document.querySelector('.ice-roi:focus-within'))||/INPUT|TEXTAREA|SELECT/.test(e.target.tagName))return;if(e.key==='ArrowLeft'||e.key==='ArrowRight'){e.preventDefault();step(e.key==='ArrowLeft'?-1:1)}});
  U.registerColour({name:colour,resolved:true,unit:'ice type',rgb:true,sizes:d=>matches(d).map(p=>p?.ice==null?3:4+8*p.ice/100),onPoint:(d,i)=>choose(matches(d)[i]),values:d=>matches(d).map(p=>!p||p.ice==null?'#000000':p.ice===0?'#8b9bb0':palette[dominant(p)])});
  U.registerColour({name:concentrationColour,resolved:true,unit:'%',cmap:'Viridis',onPoint:(d,i)=>choose(matches(d)[i]),values:d=>matches(d).map(p=>p?.ice??null)});
  U.registerColour({name:sliceColour,resolved:true,unit:'RGB',rgb:true,onPoint:(d,i)=>choose(matches(d)[i]),values:d=>matches(d).map(p=>p?.rgb?'rgb('+p.rgb.join(',')+')':'#000000')});
  function xpoints(rows){
    const d=U.state.data;if(U.state.xmode==='time')return rows.map(p=>({p,x:U.shipAxis(p.time)}));
    const legs=new Map();
    for(let i=0;i<(d?.t.length||0);i++){
      if(d.dist_km[i]==null)continue;
      const leg=U.M.legs.find(l=>l.index===d.leg[i])?.id;
      if(!legs.has(leg))legs.set(leg,[]);
      legs.get(leg).push({time:d.t[i],x:d.dist_km[i]});
    }
    return rows.map(p=>{
      const nav=legs.get(p.leg)||[];let lo=0,hi=nav.length;
      while(lo<hi){const mid=(lo+hi)>>1;if(nav[mid].time<p.time)lo=mid+1;else hi=mid}
      const before=nav[lo-1],after=nav[lo];let x=null;
      // Camera times fall between the chart's navigation bins. Interpolate
      // distance within the same leg without extending its recorded track.
      if(before&&after)x=before.x+(after.x-before.x)*(p.time-before.time)/(after.time-before.time);
      else {const edge=after||before;if(edge&&Math.abs(edge.time-p.time)<=120000)x=edge.x}
      return {p,x};
    }).filter(r=>r.x!=null);
  }
  function concentrationTraces(base,meta){
    const d=U.state.data,c=U.colourData(),legs=new Map(),legIds=new Map(U.M.legs.map(l=>[l.id,l.index]));
    for(let i=0;i<(d?.t.length||0);i++){const leg=d.leg[i];if(!legs.has(leg))legs.set(leg,[]);legs.get(leg).push({time:d.t[i],i})}
    const indices=meta.map(p=>{const row=nearest(legs.get(legIds.get(p.leg))||[],p.time);return row&&Math.abs(row.time-p.time)<=Math.max(120000,(d?.step_s||60)*1000)?row.i:null});
    // Camera colour providers sample the actual image time, even in coarse windows.
    const sampled={t:meta.map(p=>p.time),leg:meta.map(p=>legIds.get(p.leg)),vars:{},limits:{[U.state.colour]:c.limits}};
    const values=c.custom?U.colourData(sampled).values:indices.map(i=>i==null?null:c.values[i]);
    const valid=values.map((v,i)=>(c.variable?.rgb?typeof v==='string':Number.isFinite(v))&&!(indices[i]!=null&&c.low?.[indices[i]]));
    return [
      {...base,mode:'markers',y:base.y.map((v,i)=>valid[i]?v:null),marker:{size:6,color:values.map((v,i)=>valid[i]?v:c.variable?.rgb?'#7d8895':0),colorscale:U.cmap(c.variable?.cmap),reversescale:!!c.variable?.reverse,cmin:c.variable?.rgb?undefined:c.limits?.[0],cmax:c.variable?.rgb?undefined:c.limits?.[1],showscale:false}},
      {...base,mode:'markers',y:base.y.map((v,i)=>valid[i]?null:v),marker:{size:6,color:'#7d8895'}}
    ];
  }
  // Slice buttons occupy the same plotting rectangle as the other charts.
  // Rebin on pan and resize so image requests stay bounded by screen width.
  function paintSlices(plot,points){
    const axis=plot._fullLayout?.xaxis,y=plot._fullLayout?.yaxis;if(!axis||!y)return;
    plot.style.position='relative';
    let layer=plot.querySelector(':scope > .ice-slices');
    if(!layer){layer=document.createElement('div');layer.className='ice-slices';plot.append(layer)}
    layer.style.cssText=`position:absolute;left:${axis._offset}px;top:${y._offset}px;width:${axis._length}px;height:${y._length}px;overflow:hidden;pointer-events:none`;
    const bins=Math.max(1,Math.floor(axis._length/4)),selected=new Map();
    for(const r of points){if(r.p.status==='pending')continue;const x=U.state.xmode==='time'?U.plotDate(+r.x):r.x,pixel=axis.d2p(x),bin=Math.floor(pixel/axis._length*bins);if(bin>=0&&bin<bins&&!selected.has(bin))selected.set(bin,r.p)}
    const signature=JSON.stringify([bins,[...selected].map(([bin,p])=>[bin,p.id])]);if(layer.dataset.signature===signature)return;layer.dataset.signature=signature;layer.replaceChildren();
    for(const [bin,p] of selected){const b=document.createElement('button');b.className='ice-slice';b.style.cssText=`position:absolute;left:${bin/bins*100}%;width:${100/bins}%;height:100%;padding:0;border:0;overflow:hidden;pointer-events:auto`;b.title=`${U.fmtTs(p.time)} · ${p.status}`;const im=document.createElement('img');im.loading='lazy';im.src=image(p,'slice');im.alt=b.title;im.style.cssText='width:100%;height:100%';b.append(im);b.onclick=()=>choose(p);layer.append(b)}
  }
  function render(mode,el,plot){
    const rows=visible();el.querySelector('h3').textContent=labels[mode];el.querySelector('.now').textContent=latest()?`${latest().ice}%`:'';
    for(const node of [...plot.childNodes])if(node.nodeType===Node.TEXT_NODE)node.remove();
    if(!rows.length){if(plot.data)Plotly.purge(plot);plot.replaceChildren();plot.textContent=error||'No camera products for the selected legs';return}
    if(mode===3){if(plot.data)Plotly.purge(plot);plot.replaceChildren();plot.classList.add('ice-roi');plot.tabIndex=0;const p=rows.find(p=>p.id===chosen?.id)||rows.filter(p=>p.status!=='pending').at(-1);if(!p){plot.textContent='Awaiting classification';return}const bar=document.createElement('div');for(const [s,n] of [['←',-1],['→',1]]){const b=document.createElement('button');b.textContent=s;b.onclick=()=>{chosen=p;step(n)};bar.append(b)}const note=document.createElement('span');note.textContent=` ${U.fmtTs(p.time)} · ${p.status} · ${p.ice}%`;bar.append(note);const b=document.createElement('button');b.className='ice-image-button';const im=document.createElement('img');im.src=image(p,'roi');im.alt='Selected camera ROI';b.append(im);b.onclick=()=>{choose(p);popup.showModal()};plot.append(bar,b);return}
    const points=xpoints(rows);if(!points.length){plot.textContent='No matching navigation distance';return}
    // Explicit nulls at long gaps: no interpolation over unavailable photos.
    const expanded=[];for(const r of points){const prev=expanded.at(-1);if(prev&&r.p.time-prev.p.time>600000)expanded.push({x:r.x,p:{time:r.p.time,ice:null,types:null,status:'gap'}});expanded.push(r)}
    const xx=expanded.map(r=>U.state.xmode==='time'?U.plotDate(+r.x):r.x),meta=expanded.map(r=>r.p),trace=(y,name,color)=>({type:'scatter',mode:'lines+markers',x:xx,y,name,connectgaps:false,customdata:meta,marker:{size:3,color},line:{color,width:2},hovertemplate:'%{y}%<br>%{customdata.status}<extra>'+name+'</extra>'});
    const surface=[...(el.classList.contains('wide')?detailedSurface:compactSurface)].reverse();
    let traces=mode===4?[{type:'scatter',mode:'markers',x:points.map(r=>U.state.xmode==='time'?U.plotDate(+r.x):r.x),y:points.map(()=>50),marker:{opacity:0},hoverinfo:'skip'}]:mode===0?concentrationTraces(trace(meta.map(p=>p.ice),'Total ice','#5cc8ff'),meta):mode===1?typeOrder.map(k=>({...trace(meta.map(p=>p.types?.[k]??null),types[k],palette[k]),type:'bar',width:U.state.xmode==='time'?120000:undefined})):[{type:'heatmap',x:xx,y:surface.map(([name])=>name),z:surface.map(([,keys])=>meta.map(p=>groupedSurface(p,keys))),zmin:0,zmax:100,colorscale:'Viridis',customdata:surface.map(()=>meta),hovertemplate:'%{y}: %{z}%<extra></extra>',hoverongaps:false,showscale:false}];
    const span=U.spanFilter(),range=U.state.xmode==='time'?[U.plotDate(+U.shipAxis(span.start)),U.plotDate(+U.shipAxis(span.end+60000))]:[points[0].x,points.at(-1).x];
    if(mode===1&&!el.querySelector('.ice-type-legend')){const legend=document.createElement('div');legend.className='ice-type-legend';typeOrder.forEach(i=>{const label=document.createElement('span');label.textContent=types[i];label.style.borderLeft='8px solid '+palette[i];legend.append(label)});plot.after(legend)}
    const fz=U.fz||((n)=>n);
U.reactPlot(plot,traces,{...U.THEME,margin:U.chartMargin(),barmode:'stack',showlegend:false,hovermode:'closest',hoverdistance:14,xaxis:{...U.THEME.xaxis,type:U.state.xmode==='time'?'date':'linear',range,autorange:false,title:{text:U.state.xmode==='time'?`ship time (${U.tzAbbr?.()||'local'})`:'distance along track (km)',font:{size:fz(12)},standoff:4},tickfont:{size:fz(12)},hoverformat:U.state.xmode==='time'?'%Y-%m-%d %H:%M:%S':'.1f',ticksuffix:U.state.xmode==='time'?'':' km',nticks:Math.max(2,Math.floor((plot.clientWidth||300)/fz(100))),tickangle:0,automargin:true},yaxis:{...U.THEME.yaxis,automargin:false,...(mode===4?{visible:false,fixedrange:true}:{}),...(mode===2?{autorange:'reversed',categoryorder:'array',categoryarray:surface.map(([name])=>name)}:{}),range:mode===2?undefined:[0,100],title:{text:mode===2?'':'%',font:{size:fz(12)},standoff:2},tickfont:{size:fz(12)}}},U.CFG,mode===2?surface.map(([name])=>name):'').then(()=>{U.axisZoom(plot);U.linkX(plot);plot.removeAllListeners?.('plotly_afterplot');plot.on('plotly_afterplot',()=>{if(mode===4)paintSlices(plot,points);pointAt(cursor)});if(mode===4)paintSlices(plot,points);selectedCursor();plot.removeAllListeners?.('plotly_click');plot.on('plotly_click',e=>{const p=e.points?.[0]?.customdata;if(p?.id){choose(p)}})});
  }
  names.forEach((name,i)=>U.registerPanel(name,{group:'Ice camera',updated:()=>visible().at(-1)?.time,label:labels[i],chip:()=>chip(i),groupSummary:()=>{const rows=visible();return `${rows.filter(p=>p.status==='pending').length}/${rows.length} pending`},unit:'%',resolved:true,log_ok:false,colours:[panelColour(i)],after:i?names[i-1]:'Surprise (−log10 p)',description:'Experimental camera estimates. Filtered seawater = 0%; pending is unknown. Click for ROI.',onTitle:()=>U.selectColour(panelColour(i)),render:(el,plot)=>render(i,el,plot)}));
  function cameraFilter(){
    return {...U.currentFilter(),start:Math.max(U.legsStart(),Date.parse(U.M.data_range.start))};
  }
  async function refresh(){if(busy||document.hidden||!document.querySelector('#panels')?.offsetParent)return;const f=cameraFilter();if(!Number.isFinite(f?.start)||!Number.isFinite(f?.end))return;const key=`${f.start}:${f.end}`;busy=true;try{const collected=new Map();for(let start=f.start;start<=f.end+60000;start+=31*86400000){const end=Math.min(f.end+60000,start+31*86400000),payload=await U.fetchJSON(`api/ice/track?start=${start/1000}&end=${end/1000}`);for(const p of payload.photos)collected.set(p.id,p);const current=cameraFilter();if(`${current.start}:${current.end}`!==key)return}photos=[...collected.values()].sort((a,b)=>a.time-b.time);error='';lastKey=key;matchedData=null;U.refreshExtraData()}catch(e){error='Camera unavailable; keeping previous results';console.warn(error,e)}finally{busy=false}}
  setInterval(()=>{const f=cameraFilter();if(`${f?.start}:${f?.end}`!==lastKey)refresh()},1500);setInterval(refresh,60000);refresh();
})();
