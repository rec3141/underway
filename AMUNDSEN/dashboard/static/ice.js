/* Released camera products only. Browser never starts models or walks the share. */
(() => {
  const U=window.UW;if(!U?.registerPanel)return;
  const types=['grease ice','nilas','thin ice floe','icy bits','brash ice','thick ice floe'],palette=['#7ee787','#57c9bd','#78baff','#e3b341','#ff9e72','#d2a8ff'];
  const typeOrder=[0,2,1,3,4,5]; // Display order only; preserve the API's class indices.
  function centeredMeans(rows){
    const groups=new Map(),means=new Map();
    for(const p of rows){if(!groups.has(p.leg))groups.set(p.leg,[]);groups.get(p.leg).push(p)}
    for(const group of groups.values()){
      let left=0,right=0,sum=0,count=0;
      for(const p of group){
        while(right<group.length&&group[right].time<=p.time+1800000){const v=group[right++].ice;if(v!=null){sum+=v;count++}}
        while(left<right&&group[left].time<p.time-1800000){const v=group[left++].ice;if(v!=null){sum-=v;count--}}
        means.set(p.id,p.ice==null||!count?null:sum/count);
      }
    }
    return means;
  }
  function recenterPhoto(p){
    const d=U.state.data;if(!d?.lat)return;
    const leg=U.M.legs.find(l=>l.id===p.leg)?.index;
    let best=-1,delta=Infinity;
    for(let i=0;i<d.t.length;i++)if(d.leg[i]===leg&&d.lat[i]!=null&&d.lon[i]!=null&&Math.abs(d.t[i]-p.time)<delta){best=i;delta=Math.abs(d.t[i]-p.time)}
    if(best>=0&&delta<=120000)U.focusMap?.(d.lat[best],d.lon[best],U.fmtTs(p.time),true);
  }
  const colour='Camera · ice type',names=['Camera · concentration','Camera · ice composition','Camera · ice fingerprints','Camera · ROI','Camera · slices'];
  let photos=[],chosen=null,lastKey='',busy=false,matchedData=null,matched=[],error='';
  const image=(p,kind)=>`/api/ice/image?id=${encodeURIComponent(p.id)}&kind=${kind}`;
  const visible=()=>photos.filter(p=>U.inFilter(p.leg,p.time,U.spanFilter()));
  const dominant=p=>p.types?.indexOf(Math.max(...p.types));
  const labels=['Concentration','Ice composition','Ice types','ROI','Slices'];
  const latest=()=>visible().filter(p=>p.ice!=null).at(-1);
  function chip(mode){const p=latest();return {text:p?(mode===1||mode===2?`${p.ice===0?'water':types[dominant(p)]||'ice'} · ${p.ice}%`:`${p.ice}%`):'—',image:p&&mode>=3?image(p,'roi'):null}}
  let cursor=null,cursorMode=null,cursorFrame=0;
  function paintCursor(){cursorFrame=0;for(const plot of document.querySelectorAll('#panels .plot')){let line=plot.querySelector(':scope > .ice-time-pointer');if(!line){line=document.createElement('i');line.className='ice-time-pointer';line.setAttribute('aria-hidden','true');plot.append(line)}const axis=plot._fullLayout?.xaxis;let left,top,height;if(axis){const pixel=axis.d2p(cursor);const origin=plot.querySelector(".svg-container")?.getBoundingClientRect()||plot.getBoundingClientRect(),box=plot.getBoundingClientRect(),y=plot._fullLayout.yaxis;left=origin.left-box.left+axis._offset+pixel;top=origin.top-box.top+(y?._offset??plot._fullLayout._size.t);height=y?._length??plot._fullLayout._size.h;if(pixel<0||pixel>axis._length)left=null}else if(plot._iceRange){const [lo,hi]=plot._iceRange;const value=typeof cursor==='number'?cursor:Date.parse(cursor);const f=(value-lo)/(hi-lo||1);if(f>=0&&f<=1){left=f*plot.clientWidth;top=0;height=plot.clientHeight*.9}}line.hidden=cursor==null||cursorMode!==U.state.xmode||left==null||!Number.isFinite(left);if(!line.hidden){plot.style.position='relative';line.style.left=left+'px';line.style.top=top+'px';line.style.height=height+'px'}}}
  function pointAt(x){cursor=x;cursorMode=U.state.xmode;if(!cursorFrame)cursorFrame=requestAnimationFrame(paintCursor)}
  function selectedCursor(){const p=chosen&&xpoints([chosen])[0];pointAt(p?p.x:null)}
  const panelRoot=document.querySelector('#panels');
  panelRoot.addEventListener('pointermove',e=>{const plot=e.target.closest('.plot');if(!plot)return;const rect=plot.getBoundingClientRect(),axis=plot._fullLayout?.xaxis;if(axis){const origin=plot.querySelector('.svg-container')?.getBoundingClientRect()||rect;const px=e.clientX-origin.left-axis._offset;if(px>=0&&px<=axis._length)pointAt(axis.p2d(px))}else if(plot._iceRange){const [lo,hi]=plot._iceRange;pointAt(lo+(e.clientX-rect.left)/rect.width*(hi-lo))}});
  panelRoot.addEventListener('pointerleave',selectedCursor);
  function nearest(a,t){let lo=0,hi=a.length;while(lo<hi){const mid=(lo+hi)>>1;if(a[mid].time<t)lo=mid+1;else hi=mid;}return [a[lo-1],a[lo]].filter(Boolean).sort((x,y)=>Math.abs(x.time-t)-Math.abs(y.time-t))[0]}
  function matches(d){if(d===matchedData)return matched;matchedData=d;const legs=new Map();for(const p of photos){if(!legs.has(p.leg))legs.set(p.leg,[]);legs.get(p.leg).push(p)}matched=d.t.map((t,i)=>{const leg=U.M.legs.find(l=>l.index===d.leg[i])?.id,p=nearest(legs.get(leg)||[],t);return p&&Math.abs(p.time-t)<=120000?p:null});return matched}
  const popup=document.createElement('dialog');popup.className='ice-preview';popup.innerHTML='<div class="head"><button data-step="-1" aria-label="Previous image">←</button><button data-step="1" aria-label="Next image">→</button><button class="close">Close ✕</button></div><p></p><img alt="Ship camera context with ROI outline">';document.body.append(popup);popup.querySelector('.close').onclick=()=>popup.close();
  function choose(p){if(!p)return;chosen=p;popup.querySelector('p').textContent=`${U.fmtTs(p.time)} · ${p.status} · ${p.ice==null?'pending':p.ice+'% ice'}`;popup.querySelector('img').src=image(p,'source');U.refreshExtraData();selectedCursor();if(!popup.open)document.querySelector('.ice-roi')?.focus({preventScroll:true})}
  function step(n){const a=visible().filter(p=>p.status!=='pending');if(a.length)choose(a[Math.max(0,Math.min(a.length-1,a.findIndex(p=>p.id===chosen?.id)+n))])}
  popup.querySelectorAll('[data-step]').forEach(b=>b.onclick=()=>step(Number(b.dataset.step)));
  document.addEventListener('keydown',e=>{if(!chosen||(!popup.open&&!document.querySelector('.ice-roi:focus-within'))||/INPUT|TEXTAREA|SELECT/.test(e.target.tagName))return;if(e.key==='ArrowLeft'||e.key==='ArrowRight'){e.preventDefault();step(e.key==='ArrowLeft'?-1:1)}});
  U.registerColour({name:colour,resolved:true,unit:'ice type',rgb:true,sizes:d=>matches(d).map(p=>p?.ice==null?3:4+8*p.ice/100),onPoint:(d,i)=>choose(matches(d)[i]),values:d=>matches(d).map(p=>!p||p.ice==null?'#000000':p.ice===0?'#8b9bb0':palette[dominant(p)])});
  function xpoints(rows){const d=U.state.data;if(U.state.xmode==='time')return rows.map(p=>({p,x:U.shipAxis(p.time)}));const legs=new Map();for(let i=0;i<(d?.t.length||0);i++){if(d.dist_km[i]==null)continue;const leg=U.M.legs.find(l=>l.index===d.leg[i])?.id;if(!legs.has(leg))legs.set(leg,[]);legs.get(leg).push({time:d.t[i],x:d.dist_km[i]})}return rows.map(p=>{const n=nearest(legs.get(p.leg)||[],p.time);return {p,x:n&&Math.abs(n.time-p.time)<=120000?n.x:null}}).filter(r=>r.x!=null)}
  function render(mode,el,plot){
    const rows=visible();el.querySelector('h3').textContent=labels[mode];el.querySelector('.now').textContent=latest()?`${latest().ice}%`:'';
    for(const node of [...plot.childNodes])if(node.nodeType===Node.TEXT_NODE)node.remove();
    if(!rows.length){if(plot.data)Plotly.purge(plot);plot.replaceChildren();plot.textContent=error||'No camera products in this window';return}
    if(mode===3){if(plot.data)Plotly.purge(plot);plot.replaceChildren();plot.classList.add('ice-roi');plot.tabIndex=0;const p=rows.find(p=>p.id===chosen?.id)||rows.filter(p=>p.status!=='pending').at(-1);if(!p){plot.textContent='Awaiting classification';return}const bar=document.createElement('div');for(const [s,n] of [['←',-1],['→',1]]){const b=document.createElement('button');b.textContent=s;b.onclick=()=>{chosen=p;step(n)};bar.append(b)}const note=document.createElement('span');note.textContent=` ${U.fmtTs(p.time)} · ${p.status} · ${p.ice}%`;bar.append(note);const b=document.createElement('button');b.className='ice-image-button';const im=document.createElement('img');im.src=image(p,'roi');im.alt='Selected camera ROI';b.append(im);b.onclick=()=>{choose(p);popup.showModal()};plot.append(bar,b);return}
    const points=xpoints(rows);if(!points.length){plot.textContent='No matching navigation distance';return}
    if(mode===4){if(plot.data)Plotly.purge(plot);plot.replaceChildren();plot.style.position='relative';const bounds=U.spanFilter(),timeMode=U.state.xmode==='time';const lo=timeMode?+U.shipAxis(bounds.start):Number(points[0].x),hi=timeMode?+U.shipAxis(bounds.end):Number(points.at(-1).x),bins=Math.max(1,Math.floor(plot.clientWidth/4)),selected=new Map();plot._iceRange=[lo,hi];for(const r of points){if(r.p.status==='pending')continue;const bin=Math.floor((Number(r.x)-lo)/(hi-lo||1)*bins);if(bin>=0&&bin<bins&&!selected.has(bin))selected.set(bin,r)}for(const [bin,{p}] of selected){const b=document.createElement('button');b.style.cssText=`position:absolute;left:${bin/bins*100}%;width:${100/bins}%;height:90%;padding:0;border:0;overflow:hidden`;b.title=`${U.fmtTs(p.time)} · ${p.status}`;const im=document.createElement('img');im.loading='lazy';im.src=image(p,'slice');im.alt=b.title;im.style.cssText='width:100%;height:100%';b.append(im);b.onclick=()=>choose(p);plot.append(b)}selectedCursor();return}
    // Explicit nulls at long gaps: no interpolation over unavailable photos.
    const expanded=[];for(const r of points){const prev=expanded.at(-1);if(prev&&r.p.time-prev.p.time>600000)expanded.push({x:r.x,p:{time:r.p.time,ice:null,types:null,status:'gap'}});expanded.push(r)}
    const xx=expanded.map(r=>r.x),meta=expanded.map(r=>r.p),trace=(y,name,color)=>({type:'scatter',mode:'lines+markers',x:xx,y,name,connectgaps:false,customdata:meta,marker:{size:3,color},line:{color,width:2},hovertemplate:'%{y}%<br>%{customdata.status}<extra>'+name+'</extra>'});
    let traces=mode===0?[trace(meta.map(p=>p.ice),'Total ice','#5cc8ff')]:mode===1?typeOrder.map(k=>({...trace(meta.map(p=>p.types?.[k]??null),types[k],palette[k]),type:'bar',width:U.state.xmode==='time'?120000:undefined})):[{type:'heatmap',x:xx,y:typeOrder.map(k=>types[k]),z:typeOrder.map(k=>meta.map(p=>p.types?.[k]??null)),zmin:0,zmax:100,colorscale:'Viridis',customdata:types.map(()=>meta),hoverongaps:false,showscale:false}];
    if(mode===0){const means=centeredMeans(photos);traces.push({...trace(meta.map(p=>means.get(p.id)??null),'1 h centered mean','#ffb454'),mode:'lines',line:{color:'#ffb454',width:3}})}
    const span=U.spanFilter(),range=U.state.xmode==='time'?[U.shipAxis(span.start),U.shipAxis(span.end+60000)]:[points[0].x,points.at(-1).x];
    if(mode===1&&!el.querySelector('.ice-type-legend')){const legend=document.createElement('div');legend.className='ice-type-legend';typeOrder.forEach(i=>{const label=document.createElement('span');label.textContent=types[i];label.style.borderLeft='8px solid '+palette[i];legend.append(label)});plot.after(legend)}
    const fz=U.fz||((n)=>n);
    Plotly.react(plot,traces,{...U.THEME,margin:{l:fz(mode===2?105:52),r:8,t:fz(6),b:fz(34)},barmode:'stack',showlegend:false,hovermode:'closest',hoverdistance:14,xaxis:{...U.THEME.xaxis,type:U.state.xmode==='time'?'date':'linear',range,autorange:false,title:{text:U.state.xmode==='time'?`ship time (${U.tzAbbr?.()||'local'})`:'distance along track (km)',font:{size:fz(12)},standoff:4},tickfont:{size:fz(12)},hoverformat:U.state.xmode==='time'?'%Y-%m-%d %H:%M:%SZ':'.1f',ticksuffix:U.state.xmode==='time'?'':' km',...(window.innerWidth<640?{nticks:4,tickangle:0}:{})},yaxis:{...U.THEME.yaxis,range:mode===2?undefined:[0,100],title:{text:mode===2?'':'%',font:{size:fz(12)},standoff:2},tickfont:{size:fz(12)}}},U.CFG).then(()=>{U.axisZoom(plot);U.linkX(plot);plot.removeAllListeners?.('plotly_afterplot');plot.on('plotly_afterplot',()=>pointAt(cursor));selectedCursor();plot.removeAllListeners?.('plotly_click');plot.on('plotly_click',e=>{const p=e.points?.[0]?.customdata;if(p?.id){recenterPhoto(p);choose(p)}})});
  }
  names.forEach((name,i)=>U.registerPanel(name,{group:'Ice camera',label:labels[i],chip:()=>chip(i),groupSummary:()=>{const rows=visible();return `${rows.filter(p=>p.status==='pending').length}/${rows.length} pending`},unit:'%',resolved:true,log_ok:false,colours:[colour],after:i?names[i-1]:'Surprise (−log10 p)',description:'Experimental camera estimates. Filtered seawater = 0%; pending is unknown. Click for ROI.',onTitle:()=>U.selectColour(colour),render:(el,plot)=>render(i,el,plot)}));
  async function refresh(){if(busy||document.hidden||!document.querySelector('#panels')?.offsetParent)return;const f=U.spanFilter();if(!Number.isFinite(f?.start)||!Number.isFinite(f?.end))return;const key=`${f.start}:${f.end}`;busy=true;try{const collected=new Map();for(let start=f.start-1800000;start<=f.end+1800000;start+=31*86400000){const end=Math.min(f.end+1800000,start+31*86400000),payload=await U.fetchJSON(`/api/ice/track?start=${start/1000}&end=${end/1000}`);for(const p of payload.photos)collected.set(p.id,p);const current=U.spanFilter();if(`${current.start}:${current.end}`!==key)return}photos=[...collected.values()].sort((a,b)=>a.time-b.time);error='';lastKey=key;matchedData=null;U.refreshExtraData()}catch(e){error='Camera unavailable; keeping previous results';console.warn(error,e)}finally{busy=false}}
  setInterval(()=>{const f=U.spanFilter();if(`${f?.start}:${f?.end}`!==lastKey)refresh()},1500);setInterval(refresh,60000);refresh();
})();
