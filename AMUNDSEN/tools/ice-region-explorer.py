"""Fixed whole-region t-SNE layout, refreshed with blind Qwen annotations."""
import argparse
import importlib.util
import json
from pathlib import Path
import sys

sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from dashboard.ice_classifier import features


def render(output, responses):
    layout=output/'embedding.json'
    if not layout.exists(): return
    points=json.loads(layout.read_text())
    annotations={r['id']:dict(response=r['response'],finish_reason=r['finish_reason']) for r in responses}
    payload=json.dumps(dict(points=points,annotations=annotations)).replace('<','\\u003c')
    page='''<!doctype html><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>Region t-SNE — live Qwen review</title>
<style>body{font:16px system-ui;background:#17212b;color:#eee;margin:18px}main{display:grid;grid-template-columns:1fr 1fr;gap:16px}canvas{width:100%;background:#0d1720}img{max-width:100%}pre{white-space:pre-wrap}select{margin:8px}#tip{position:fixed;pointer-events:none;background:#17212b;padding:6px;display:none}#tip img{width:250px}@media(max-width:800px){main{grid-template-columns:1fr}}</style>
<h1>Whole-region t-SNE</h1><p>One point per approved 30° region. Fixed layout; similarity is colour/texture, not proof of ice type. Qwen annotations are unvalidated. This page reloads every 60 seconds; selection is retained in the URL.</p>
<label>Colour <select id="colour"><option value="total">Total ice %</option><option value="thin_new_ice">Thin/new %</option><option value="broken_ice">Broken/rubble %</option><option value="consolidated_ice">Consolidated %</option><option value="unknown">Unknown %</option><option value="visibility">Visibility</option></select></label><span id="status"></span>
<main><section><canvas id="map" width="800" height="650"></canvas><p>Grey: awaiting/invalid response. Ice percentages: blue (0) → yellow (100). Visibility: green clear, orange degraded, red unusable, grey uncertain.</p></section><section><h2 id="title"></h2><img id="crop"><img id="source"><pre id="response"></pre></section></main><div id="tip"></div><script>
const data=PAYLOAD, canvas=document.getElementById('map'),ctx=canvas.getContext('2d'), colour=document.getElementById('colour');
let selected=location.hash.slice(1)||data.points[0].id;
function annotation(id){try{let raw=data.annotations[id]?.response||'';return JSON.parse(raw.replace(/^```(?:json)?\\s*/,'').replace(/\\s*```$/,''));}catch{return null;}}
const xs=data.points.map(p=>p.x),ys=data.points.map(p=>p.y),minx=Math.min(...xs),maxx=Math.max(...xs),miny=Math.min(...ys),maxy=Math.max(...ys);
for(const p of data.points){p.cx=30+(p.x-minx)/(maxx-minx||1)*740;p.cy=30+(p.y-miny)/(maxy-miny||1)*590;}
function draw(){ctx.clearRect(0,0,800,650);for(const p of data.points){const a=annotation(p.id);let fill='#777';if(a){if(colour.value==='visibility'){fill=({clear:'#5c9',degraded:'#fa4',unusable:'#f55',uncertain:'#aaa'})[a.visibility]||'#777';}else{const t=a.percentages||{};let v=colour.value==='total'?t.thin_new_ice+t.broken_ice+t.consolidated_ice:t[colour.value];if(Number.isFinite(v))fill=`hsl(${220-180*Math.max(0,Math.min(100,v))/100} 80% 60%)`;}}ctx.beginPath();ctx.arc(p.cx,p.cy,p.id===selected?9:6,0,Math.PI*2);ctx.fillStyle=fill;ctx.fill();if(p.id===selected){ctx.strokeStyle='white';ctx.lineWidth=2;ctx.stroke();}}}
function choose(id){const p=data.points.find(p=>p.id===id)||data.points[0];selected=p.id;history.replaceState(null,'','#'+p.id);document.getElementById('title').textContent=p.id+' · '+p.file;document.getElementById('crop').src=p.images[1];document.getElementById('source').src=p.images[0];document.getElementById('response').textContent=data.annotations[p.id]?.response||'Awaiting Qwen';draw();}
function nearest(e){const b=canvas.getBoundingClientRect(),x=(e.clientX-b.left)*800/b.width,y=(e.clientY-b.top)*650/b.height;return data.points.find(p=>Math.hypot(p.cx-x,p.cy-y)<13);}
canvas.onclick=e=>{const p=nearest(e);if(p)choose(p.id);};canvas.onmousemove=e=>{const p=nearest(e),tip=document.getElementById('tip');tip.replaceChildren();tip.style.display=p?'block':'none';if(p){const image=document.createElement('img');image.src=p.images[1];tip.append(document.createTextNode(p.id),document.createElement('br'),image);tip.style.left=Math.min(e.clientX+15,innerWidth-275)+'px';tip.style.top=Math.max(0,Math.min(e.clientY+15,innerHeight-180))+'px';}};canvas.onmouseleave=()=>document.getElementById('tip').style.display='none';colour.onchange=draw;
document.getElementById('status').textContent=Object.keys(data.annotations).length+' / '+data.points.length+' responses';choose(selected);setTimeout(()=>location.reload(),60000);
</script>'''.replace('PAYLOAD',payload)
    tmp=output/'regions.html.tmp';tmp.write_text(page);tmp.replace(output/'regions.html')


def main():
    from sklearn.manifold import TSNE
    from sklearn.preprocessing import StandardScaler
    import numpy as np
    import base64
    import io
    from PIL import Image
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--output',type=Path,required=True)
    a=p.parse_args();a.output.mkdir(parents=True,exist_ok=True)
    spec=importlib.util.spec_from_file_location('overnight',Path(__file__).with_name('ice-overnight.py'))
    mod=importlib.util.module_from_spec(spec);spec.loader.exec_module(mod)
    data=json.loads((Path.home()/'Downloads/amundsen-ice-texture-brightness/tiles.json').read_text())
    points=[];vectors=[]
    for job in mod.jobs(data,{'tiles':[]},Path('/media/cryomics/T7 Shield/Amundsen/Camera_360/2025_LEG_04')):
        with Image.open(io.BytesIO(base64.b64decode(job['images'][1].split(',',1)[1]))) as crop:vectors.append(features(crop))
        points.append({k:job[k] for k in ('id','file','images')})
    coords=TSNE(n_components=2,perplexity=min(15,(len(points)-1)/3),random_state=42,init='pca',learning_rate='auto').fit_transform(StandardScaler().fit_transform(np.array(vectors)))
    for point,xy in zip(points,coords):point.update(x=float(xy[0]),y=float(xy[1]))
    (a.output/'embedding.json').write_text(json.dumps(points))
    results=a.output/'results.json';render(a.output,json.loads(results.read_text()) if results.exists() else [])
    print(a.output/'regions.html')


if __name__=='__main__':main()
