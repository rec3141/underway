/* A separate, resizable copy of a graph; exporting never changes the live plot. */
(() => {
  'use strict';
  let dialog, preview, observer, frame, version = 0, filename, ready = false;
  const dimensions = () => ({width:Math.max(1,preview.clientWidth),height:Math.max(1,preview.clientHeight)});
  function resolution() {
    const {width,height}=dimensions(), scale=Number(dialog.querySelector('input').value);
    dialog.querySelector('output').textContent=`${scale}× · ${Math.round(width*scale)} × ${Math.round(height*scale)} px`;
  }
  function setup() {
    if (dialog) return;
    dialog=document.createElement('dialog');dialog.className='plot-export-dialog';
    dialog.setAttribute('aria-labelledby','plot-export-title');
    dialog.innerHTML=`<div class="plot-export-heading"><h3 id="plot-export-title"></h3><button type="button" class="export-close" aria-label="Close export preview">×</button></div>
      <p class="muted">Resize the preview using its bottom-right corner. SVG keeps its vector detail at any size.</p>
      <div class="export-canvas"></div>
      <div class="plot-export-controls"><label>PNG DPI scale <input type="range" min="1" max="4" step="0.5" value="2"></label><output></output><button type="button" data-format="png">Export PNG</button><button type="button" data-format="svg">Export SVG</button></div><p class="export-status" role="status"></p>`;
    document.body.append(dialog);preview=dialog.querySelector('.export-canvas');
    dialog.querySelector('.export-close').onclick=()=>dialog.close();
    dialog.querySelector('input').oninput=resolution;
    dialog.onclose=()=>{version++;ready=false;observer?.disconnect();cancelAnimationFrame(frame);if(preview.data)Plotly.purge(preview);};
    observer=new ResizeObserver(()=>{
      cancelAnimationFrame(frame);
      frame=requestAnimationFrame(()=>{if(!dialog.open)return;resolution();if(ready)Plotly.relayout(preview,dimensions());});
    });
    for(const button of dialog.querySelectorAll('[data-format]'))button.onclick=async()=>{
      if(!ready)return;
      const buttons=[...dialog.querySelectorAll('[data-format]')],status=dialog.querySelector('.export-status');
      buttons.forEach(b=>b.disabled=true);status.textContent='Preparing download…';
      try {
        const format=button.dataset.format,scale=format==='png'?Number(dialog.querySelector('input').value):1;
        const url=await Plotly.toImage(preview,{format,...dimensions(),scale});
        const link=document.createElement('a');link.href=url;link.download=`${filename}.${format}`;document.body.append(link);link.click();link.remove();
        status.textContent='Downloaded.';
      } catch {status.textContent='Export failed. Try a smaller size or resolution.';}
      finally {buttons.forEach(b=>b.disabled=false);}
    };
  }
  async function open(plot) {
    setup();const current=++version;ready=false;
    const title=plot.closest('.panel')?.querySelector('h3')?.textContent||'Graph';
    filename=title.replace(/[^\p{L}\p{N}._-]+/gu,'-').replace(/^-|-$/g,'')||'graph';
    dialog.querySelector('h3').textContent=`Export · ${title}`;
    dialog.querySelector('.export-status').textContent='';
    dialog.querySelectorAll('[data-format]').forEach(b=>b.disabled=true);
    if(!dialog.open)dialog.showModal();observer.observe(preview);resolution();
    try {
      const layout=structuredClone(plot.layout),data=structuredClone(plot.data);
      for(const key of Object.keys(plot._fullLayout||{}))if(/^[xy]axis\d*$/.test(key)){
        const axis=plot._fullLayout[key];layout[key]={...layout[key],range:[...axis.range],autorange:false};
      }
      await Plotly.newPlot(preview,data,{...layout,...dimensions(),autosize:false},{displayModeBar:false,staticPlot:true});
      if(current!==version){if(!dialog.open)Plotly.purge(preview);return;}
      ready=true;dialog.querySelectorAll('[data-format]').forEach(b=>b.disabled=false);resolution();
    } catch {dialog.querySelector('.export-status').textContent='Could not prepare this graph for export.';}
  }
  window.UWPlotExport={attach(plot){
    const tools=plot.closest('.panel')?.querySelector('.head .tools');if(!tools)return;
    let button=tools.querySelector('.plot-export');
    if(!button){button=document.createElement('button');button.type='button';button.className='plot-export';button.textContent='Export';button.title='Preview and export this graph';tools.prepend(button);}
    button.onclick=()=>open(plot);
  }};
})();
