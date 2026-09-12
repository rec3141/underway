/* A separate, resizable copy of a graph; exporting never changes the live plot. */
(() => {
  'use strict';
  let dialog, preview, observer, frame, version = 0, filename, ready = false, mapPreview = null, dialogSize = '';
  const scaleInput = () => dialog.querySelector('input[type=range]');
  const dimensions = () => ({width:Math.max(1,preview.clientWidth),height:Math.max(1,preview.clientHeight)});
  function resolution() {
    const {width,height}=dimensions(), scale=Number(scaleInput().value);
    dialog.querySelector('output').textContent=`${scale}× · ${Math.round(width*scale)} × ${Math.round(height*scale)} px`;
    for(const [key,value] of Object.entries({width,height})) {
      const input=dialog.querySelector(`[name=${key}]`);
      if(document.activeElement!==input)input.value=value;
    }
  }
  function setup() {
    if (dialog) return;
    dialog=document.createElement('dialog');dialog.className='plot-export-dialog';
    dialog.setAttribute('aria-labelledby','plot-export-title');
    dialog.innerHTML=`<div class="plot-export-heading"><h3 id="plot-export-title"></h3><button type="button" class="export-close" aria-label="Close export preview">×</button></div>
      <div class="plot-export-body"><aside class="plot-export-controls">
      <p class="muted">Drag the panel’s bottom-right corner, or enter exact image dimensions. Large previews can be scrolled.</p>
      <label>Width (px)<input name="width" type="number" min="160" max="4096" step="1"></label>
      <label>Height (px)<input name="height" type="number" min="160" max="4096" step="1"></label>
      <label>PNG DPI scale <input type="range" min="1" max="4" step="0.5" value="2"></label><output></output>
      <p class="export-format-note muted"></p><button type="button" data-format="png">Export PNG</button><button type="button" data-format="svg">Export SVG</button><p class="export-status" role="status"></p>
      </aside><div class="export-viewport"><div class="export-canvas"></div></div></div>`;
    document.body.append(dialog);preview=dialog.querySelector('.export-canvas');
    dialog.querySelector('.export-close').onclick=()=>dialog.close();
    scaleInput().oninput=resolution;
    for(const input of dialog.querySelectorAll('input[type=number]'))input.onchange=()=>{
      if(!input.checkValidity()){input.reportValidity();resolution();return;}
      preview.style[input.name]=`${Number(input.value)}px`;
    };
    dialog.onclose=()=>{version++;ready=false;observer?.disconnect();cancelAnimationFrame(frame);mapPreview?.remove();mapPreview=null;if(preview.data)Plotly.purge(preview);preview.replaceChildren();};
    observer=new ResizeObserver(()=>{
      const size=`${dialog.clientWidth}:${dialog.clientHeight}`;
      if(dialogSize && size!==dialogSize){preview.style.width='';preview.style.height='';}
      dialogSize=size;
      cancelAnimationFrame(frame);
      frame=requestAnimationFrame(()=>{if(!dialog.open)return;resolution();if(ready){if(mapPreview)mapPreview.resize();else Plotly.relayout(preview,dimensions());}});
    });
    for(const button of dialog.querySelectorAll('[data-format]'))button.onclick=async()=>{
      if(!ready)return;
      const buttons=[...dialog.querySelectorAll('[data-format]')],status=dialog.querySelector('.export-status');
      buttons.forEach(b=>b.disabled=true);status.textContent='Preparing download…';
      try {
        const format=button.dataset.format,scale=format==='png'?Number(scaleInput().value):1;
        const url=mapPreview?await mapImage(format,scale):await Plotly.toImage(preview,{format,...dimensions(),scale});
        const link=document.createElement('a');link.href=url;link.download=`${filename}.${format}`;document.body.append(link);link.click();link.remove();
        status.textContent='Downloaded.';
      } catch {status.textContent='Export failed. Try a smaller size or resolution, and check that map tiles have loaded.';}
      finally {buttons.forEach(b=>b.disabled=false);}
    };
  }
  function mapIdle(map) {
    return new Promise((resolve,reject)=>{
      const finish=()=>{clearTimeout(timer);map.off('idle',finish);resolve();};
      const timer=setTimeout(()=>{map.off('idle',finish);reject(Error('Map tiles are still loading.'));},20000);
      map.once('idle',finish);map.triggerRepaint();
    });
  }
  async function mapImage(format,scale) {
    const map=mapPreview;
    try {
      map.setPixelRatio(scale);await mapIdle(map);
      const source=map.getCanvas(), canvas=document.createElement('canvas');
      const requested=dimensions();
      if(source.width!==Math.round(requested.width*scale)||source.height!==Math.round(requested.height*scale))throw Error('Requested size exceeds the map renderer limit.');
      canvas.width=source.width;canvas.height=source.height;
      const ctx=canvas.getContext('2d');ctx.drawImage(source,0,0);
      const credit=document.querySelector('#mapattrib')?.textContent?.trim();
      if(credit){ctx.font=`${11*scale}px sans-serif`;const lines=credit.match(/.{1,90}(?:\s|$)|.{1,90}/g)||[credit];ctx.fillStyle='#fff';ctx.fillRect(0,canvas.height-(lines.length*15+8)*scale,canvas.width,(lines.length*15+8)*scale);ctx.fillStyle='#222';lines.forEach((line,i)=>ctx.fillText(line,6*scale,canvas.height-(lines.length-1-i)*15*scale-6*scale,canvas.width-12*scale));}
      const png=canvas.toDataURL('image/png');
      if(format==='png')return png;
      return 'data:image/svg+xml;charset=utf-8,'+encodeURIComponent(`<svg xmlns="http://www.w3.org/2000/svg" width="${canvas.width}" height="${canvas.height}" viewBox="0 0 ${canvas.width} ${canvas.height}"><image width="100%" height="100%" href="${png}"/></svg>`);
    } finally {if(map===mapPreview)map.setPixelRatio(1);}
  }
  async function open(plot, sourceMap = null) {
    setup();const current=++version;ready=false;
    const title=sourceMap?'Map':plot.closest('.panel')?.querySelector('h3')?.textContent||'Graph';
    filename=title.replace(/[^\p{L}\p{N}._-]+/gu,'-').replace(/^-|-$/g,'')||'graph';
    dialog.querySelector('h3').textContent=`Export · ${title}`;
    dialog.querySelector('.export-status').textContent='';
    dialog.querySelectorAll('[data-format]').forEach(b=>b.disabled=true);
    dialog.querySelector('.export-format-note').textContent=sourceMap?'Map SVG contains a raster image; graph SVGs retain vector detail.':'SVG keeps its vector detail at any size.';
    preview.style.width='';preview.style.height='';dialogSize='';
    if(!dialog.open)dialog.showModal();observer.observe(dialog);observer.observe(preview);resolution();
    try {
      if(sourceMap){
        mapPreview=new maplibregl.Map({container:preview,style:structuredClone(sourceMap.getStyle()),center:sourceMap.getCenter(),zoom:sourceMap.getZoom(),bearing:sourceMap.getBearing(),pitch:sourceMap.getPitch(),interactive:false,attributionControl:false,pixelRatio:1,canvasContextAttributes:{preserveDrawingBuffer:true},fadeDuration:0});
        await mapIdle(mapPreview);
      }else{
      const layout=structuredClone(plot.layout),data=structuredClone(plot.data);
      for(const key of Object.keys(plot._fullLayout||{}))if(/^[xy]axis\d*$/.test(key)){
        const axis=plot._fullLayout[key];layout[key]={...layout[key],range:[...axis.range],autorange:false};
      }
      const background=getComputedStyle(dialog).getPropertyValue('--plot-bg').trim();
      await Plotly.newPlot(preview,data,{...layout,paper_bgcolor:background,plot_bgcolor:background,...dimensions(),autosize:false},{displayModeBar:false,staticPlot:true});
      }
      if(current!==version){if(!dialog.open)Plotly.purge(preview);return;}
      ready=true;dialog.querySelectorAll('[data-format]').forEach(b=>b.disabled=false);resolution();
    } catch {if(current===version)dialog.querySelector('.export-status').textContent='Could not prepare the preview. Check that map tiles have loaded, then try again.';}
  }
  window.UWPlotExport={openMap:map=>open(null,map),attach(plot){
    const tools=plot.closest('.panel')?.querySelector('.head .tools');if(!tools)return;
    let button=tools.querySelector('.plot-export');
    if(!button){button=document.createElement('button');button.type='button';button.className='plot-export';button.textContent='Export';button.title='Preview and export this graph';tools.prepend(button);}
    button.onclick=()=>open(plot);
  }};
})();
