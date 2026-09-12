/* One canvas legend shared by the live map, export preview and downloads. */
(() => {
  function draw(ctx, width, height, legend) {
    if(!legend)return;
    const vertical=height<width, pad=12;
    const w=vertical?112:Math.min(360,width-24), h=vertical?Math.min(300,height-48):90;
    ctx.save();ctx.translate(pad,pad);
    ctx.fillStyle='#fff';ctx.fillRect(0,0,w,h);ctx.strokeStyle='#b5bdc5';ctx.strokeRect(.5,.5,w-1,h-1);
    ctx.fillStyle='#17202a';ctx.font='600 13px system-ui';
    const words=legend.name.split(' '), lines=[];let line='';
    for(const word of words){const next=line?`${line} ${word}`:word;if(line&&ctx.measureText(next).width>w-16){lines.push(line);line=word;}else line=next;}lines.push(line);
    const titleLines=lines.slice(0,3);titleLines.forEach((text,i)=>ctx.fillText(text,8,18+i*15,w-16));
    if(legend.showScale){
      const top=titleLines.length*15+14, length=vertical?Math.max(20,h-top-28):w-20;
      const gradient=vertical?ctx.createLinearGradient(0,top+length,0,top):ctx.createLinearGradient(10,0,10+length,0);
      for(const [stop,color] of legend.stops)gradient.addColorStop(stop,color);
      ctx.fillStyle=gradient;ctx.fillRect(10,top,vertical?14:length,vertical?length:12);
      ctx.font='12px system-ui';ctx.fillStyle='#17202a';
      if(vertical){ctx.fillText(legend.high,30,top+10,w-36);ctx.fillText(legend.low,30,top+length,w-36);}
      else{ctx.fillText(legend.low,10,top+29);ctx.textAlign='right';ctx.fillText(legend.high,w-10,top+29);}
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
  window.UWMapLegend={draw,render};
})();
