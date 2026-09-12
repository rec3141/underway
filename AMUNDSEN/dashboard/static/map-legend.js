/* One canvas legend shared by the live map, export preview and downloads. */
(() => {
  function formatRange(range) {
    if(!range||!range.every(Number.isFinite))return ['',''];
    const format=(value,digits)=>String(Number(value.toPrecision(digits)));
    let digits=2;
    // Avoid giving a narrow but real range two identical endpoint labels.
    while(digits<12&&range[0]!==range[1]&&format(range[0],digits)===format(range[1],digits))digits++;
    return range.map(value=>format(value,digits));
  }
  function draw(ctx, width, height, legend) {
    if(!legend)return;
    const vertical=height<width, pad=8;
    ctx.save();ctx.translate(pad,pad);
    ctx.font='600 13px system-ui';
    const w=vertical?Math.min(90,Math.max(64,ctx.measureText(legend.name).width+12)):Math.min(width-16,Math.max(180,Math.min(320,ctx.measureText(legend.name).width+12)));
    const words=legend.name.split(' '), lines=[];let line='';
    for(const word of words){const next=line?`${line} ${word}`:word;if(line&&ctx.measureText(next).width>w-12){lines.push(line);line=word;}else line=next;}lines.push(line);
    const titleLines=lines.slice(0,3), top=titleLines.length*15+8;
    const length=vertical?Math.max(24,Math.min(140,height-top-64)):w-16;
    const h=legend.showScale?top+(vertical?length+34:30):top;
    ctx.fillStyle='rgba(255,255,255,.82)';ctx.fillRect(0,0,w,h);ctx.strokeStyle='rgba(80,95,110,.35)';ctx.strokeRect(.5,.5,w-1,h-1);
    ctx.fillStyle='#17202a';ctx.textAlign='center';titleLines.forEach((text,i)=>ctx.fillText(text,w/2,16+i*15,w-12));
    if(legend.showScale){
      const barTop=vertical?top+15:top, barX=vertical?(w-12)/2:8;
      const gradient=vertical?ctx.createLinearGradient(0,barTop+length,0,barTop):ctx.createLinearGradient(8,0,8+length,0);
      for(const [stop,color] of legend.stops)gradient.addColorStop(stop,color);
      ctx.fillStyle=gradient;ctx.fillRect(barX,barTop,vertical?12:length,vertical?length:10);
      ctx.font='12px system-ui';ctx.fillStyle='#17202a';
      if(vertical){ctx.textAlign='center';ctx.fillText(legend.high,w/2,top+10,w-12);ctx.fillText(legend.low,w/2,barTop+length+14,w-12);}
      else{ctx.textAlign='left';ctx.fillText(legend.low,8,top+25);ctx.textAlign='right';ctx.fillText(legend.high,w-8,top+25);}
    }
    ctx.restore();
  }
  function render(canvas, width, height, legend) {
    const ratio=window.devicePixelRatio||1;
    canvas.width=Math.round(width*ratio);canvas.height=Math.round(height*ratio);
    canvas.style.width=`${width}px`;canvas.style.height=`${height}px`;
    canvas.dataset.edge=height<width?'left':'top';
    canvas.setAttribute('aria-label',legend?`${legend.name}${legend.showScale?`: ${legend.low} to ${legend.high}`:''}`:'Map legend');
    const ctx=canvas.getContext('2d');ctx.scale(ratio,ratio);draw(ctx,width,height,legend);
  }
  window.UWMapLegend={draw,render,formatRange};
})();
