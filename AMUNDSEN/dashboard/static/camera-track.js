/* Native underway card with fixed mean-RGB colours. */
(() => {
  const UW=window.UW;
  if(!UW?.registerPanel)return;
  UW.fetchJSON('data/camera-track.json').then(payload=>{
    const names=['Texture','Brightness','Colorfulness','Patchiness'],modes=names.map((name,i)=>`Camera · ${name} (PC${i+1})`),RGB='Camera · mean RGB',cycle=[...modes,RGB];
    const score=(p,k)=>p?.pcs?.[k]??(k===0?p?.pc1:null);
    const byLeg=new Map();
    function indexPhotos(){byLeg.clear();for(const p of payload.photos){if(!byLeg.has(p.leg))byLeg.set(p.leg,[]);byLeg.get(p.leg).push({...p,ms:Date.parse(p.time)});}
      for(const rows of byLeg.values())rows.sort((a,b)=>a.ms-b.ms);}
    indexPhotos();
    let lastData=null,matched=[];
    function matches(d){if(d===lastData)return matched;lastData=d;matched=d.t.map((t,i)=>{
      const leg=UW.M.legs.find(l=>l.index===d.leg[i])?.id,a=byLeg.get(leg)||[];let lo=0,hi=a.length;
      while(lo<hi){const mid=(lo+hi)>>1;if(a[mid].ms<t)lo=mid+1;else hi=mid;}
      const p=[a[lo-1],a[lo]].filter(Boolean).sort((x,y)=>Math.abs(x.ms-t)-Math.abs(y.ms-t))[0];
      return p&&Math.abs(p.ms-t)<=300000?p:null;
    });return matched;}
    const popup=document.createElement('div');popup.id='photopop';popup.className='campop';popup.hidden=true;popup.style.width='min(480px, 85%)';popup.setAttribute('role','dialog');popup.setAttribute('aria-label','Camera photo');
    const head=document.createElement('div');head.className='head';const heading=document.createElement('b'),close=document.createElement('button');close.textContent='✕';close.title='Close photo';close.setAttribute('aria-label','Close photo');const spacer=document.createElement('span');head.append(heading,spacer,close);
    const img=document.createElement('img');img.id='photoimage';img.style.cssText='display:block;width:100%;min-height:0;max-height:55vh;object-fit:contain;background:#000;border-radius:6px';
    const imageBox=document.createElement('div');imageBox.style.position='relative';
    const roi=document.createElementNS('http://www.w3.org/2000/svg','svg');roi.id='photo-roi';roi.setAttribute('viewBox','0 0 3648 2052');roi.setAttribute('preserveAspectRatio','xMidYMid meet');roi.setAttribute('aria-hidden','true');
    roi.style.cssText='position:absolute;inset:0;width:100%;height:100%;pointer-events:none;display:none';
    const u=Math.cos(-Math.PI/6),v=Math.sin(-Math.PI/6),ox=.45*3648-600*u+300*v,oy=.62*2052-600*v-300*u;
    const vertices=[[ox,oy],[ox+1200*u,oy+1200*v],[ox+1200*u-600*v,oy+1200*v+600*u],[ox-600*v,oy+600*u]];
    const outline=document.createElementNS('http://www.w3.org/2000/svg','polygon');outline.setAttribute('points',vertices.map(p=>p.join(',')).join(' '));outline.setAttribute('fill','none');outline.setAttribute('stroke','#ff9d00');outline.setAttribute('stroke-width','2');outline.setAttribute('vector-effect','non-scaling-stroke');roi.append(outline);imageBox.append(img,roi);
    const foot=document.createElement('div');foot.className='foot';const link=document.createElement('a');link.textContent='Open full-size photo';link.target='_blank';link.rel='noopener';const roiNote=document.createElement('span');roiNote.style.marginLeft='10px';foot.append(link,roiNote);popup.append(head,imageBox,foot);document.querySelector('#map').parentElement.append(popup);
    img.onload=()=>{const supported=img.naturalWidth===3648&&img.naturalHeight===2052;roi.style.display=supported?'block':'none';roiNote.textContent=supported?'Orange outline: analyzed ROI':'ROI unavailable for these image dimensions';};
    const closePhoto=()=>{popup.hidden=true;roi.style.display='none';img.removeAttribute('src');};close.onclick=closePhoto;
    document.addEventListener('keydown',e=>{if(e.key==='Escape'&&!popup.hidden){e.preventDefault();closePhoto();}});
    img.onerror=()=>{roi.style.display='none';roiNote.textContent='';heading.textContent='Photo unavailable — archive may be offline';};
    const openPhoto=p=>{if(!p)return;const url='photos/'+[p.leg,...p.file.split('/')].map(encodeURIComponent).join('/');
      document.querySelector('#camclose')?.click();roi.style.display='none';roiNote.textContent='';heading.textContent=p.time.replace('T',' ').replace('+00:00',' UTC');img.alt=p.file;img.src=url;link.href=url;popup.hidden=false;close.focus({preventScroll:true});};
    const onPoint=(d,i)=>openPhoto(matches(d)[i]);
    modes.forEach((name,k)=>UW.registerColour({name,resolved:true,unit:`PC${k+1}`,cmap:'Viridis',onPoint,values:d=>matches(d).map(p=>score(p,k)??null)}));
    UW.registerColour({name:RGB,resolved:true,unit:'RGB',rgb:true,onPoint,values:d=>matches(d).map(p=>p?'rgb('+p.rgb.join(',')+')':'#000000')});
    UW.registerPanel('Camera · feature PC1 · mean RGB',{
      unit:'PC1',resolved:true,log_ok:false,
      after:'Surprise (−log10 p)',
      layoutRevision:'camera-second-v1',
      colours:cycle,
      description:'Texture, Brightness, Colorfulness, Patchiness are provisional interpretations of PC1–4, not ice concentration.',
      onTitle(){UW.selectColour(cycle[(cycle.indexOf(UW.state.colour)+1)%cycle.length]);},
      render(el,plot){
        const selectedPC=modes.indexOf(UW.state.colour),k=Math.max(0,selectedPC),axisLabel=selectedPC>=0?`${names[k]} (PC${k+1})`:'Mean RGB (0–255)';
        const yValue=p=>selectedPC>=0?score(p,k):p.rgb.reduce((a,b)=>a+b,0)/3;
        el.querySelector('h3').textContent=selectedPC>=0?modes[k]:RGB;
        el.querySelector('h3').title='Click to cycle Texture → Brightness → Colorfulness → Patchiness → mean RGB. Names are provisional PC interpretations.';
        const filter=UW.spanFilter(),rows=payload.photos.filter(p=>UW.inFilter(p.leg,p.time,filter)),d=UW.state.data,byLeg=new Map();
        if(UW.state.xmode!=='time'&&d)for(let i=0;i<d.t.length;i++){
          if(d.dist_km[i]==null||d.leg[i]==null)continue;
          const leg=UW.M.legs.find(l=>l.index===d.leg[i])?.id;
          if(!byLeg.has(leg))byLeg.set(leg,[]);
          byLeg.get(leg).push([Number(d.t[i]),d.dist_km[i]]);
        }
        const distance=p=>{const a=byLeg.get(p.leg)||[],t=Date.parse(p.time);let lo=0,hi=a.length;
          while(lo<hi){const mid=(lo+hi)>>1;if(a[mid][0]<t)lo=mid+1;else hi=mid;}
          const best=[a[lo-1],a[lo]].filter(Boolean).sort((x,y)=>Math.abs(x[0]-t)-Math.abs(y[0]-t))[0];
          return best&&Math.abs(best[0]-t)<=300000?best[1]:null;};
        const points=rows.map(p=>({p,x:UW.state.xmode==='time'?p.time:distance(p)})).filter(r=>r.x!=null);
        el.querySelector('.now').textContent=points.length?yValue(points.at(-1).p)?.toFixed(2)+(selectedPC>=0?` PC${k+1}`:' RGB'):'';
        if(!points.length){if(plot.data)Plotly.purge(plot);plot.className='plot empty';plot.textContent=rows.length?'No matching underway distances in this span':'No camera data in this span for the selected legs';return;}
        if(plot.classList.contains('empty')){plot.className='plot';plot.textContent='';}
        const trace={type:'scatter',mode:'markers',x:points.map(r=>r.x),y:points.map(r=>yValue(r.p)),marker:{size:4,color:selectedPC>=0?points.map(r=>score(r.p,k)):points.map(r=>'rgb('+r.p.rgb.join(',')+')'),colorscale:'Viridis'},text:points.map(r=>r.p.file),hovertemplate:`%{x}<br>${axisLabel}: %{y:.3f}<br>%{text}<extra></extra>`};
        const xs=UW.state.xmode==='time'?d?.t:d?.dist_km;
        const finite=(xs||[]).filter(x=>x!=null);
        const sharedRange=finite.length?[finite[0],finite.at(-1)]:undefined;
        Plotly.react(plot,[trace],{...UW.THEME,margin:{l:52,r:8,t:6,b:34},showlegend:false,dragmode:'pan',xaxis:{...UW.THEME.xaxis,type:UW.state.xmode==='time'?'date':'linear',range:sharedRange,title:{text:UW.state.xmode==='time'?'UTC':'Distance travelled (km)'}},yaxis:{...UW.THEME.yaxis,title:{text:axisLabel}}},UW.CFG).then(()=>{UW.axisZoom(plot);UW.linkX(plot);plot.removeAllListeners?.('plotly_click');plot.on('plotly_click',ev=>{if(cycle.includes(UW.state.colour))openPhoto(points[ev.points?.[0]?.pointIndex]?.p);});});
      }
    });
    let refreshing=false;
    setInterval(async()=>{if(refreshing||document.hidden)return;refreshing=true;
      try{const next=await UW.fetchJSON('data/camera-track.json');
        if(!Array.isArray(next.photos)||!next.updated_utc||next.updated_utc===payload.updated_utc)return;
        payload=next;indexPhotos();lastData=null;UW.refreshExtraData?.();
      }catch(error){console.warn('Keeping last good camera data',error);}finally{refreshing=false;}
    },60000);
  }).catch(error=>console.warn('Camera track unavailable',error));
})();
