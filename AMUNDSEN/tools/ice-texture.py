"""Offline native-resolution sea-ROI tiles and exploratory texture embedding."""
import argparse
import base64
import io
import json
from pathlib import Path

import numpy as np
from PIL import Image, ImageOps
from scipy.ndimage import gaussian_filter, sobel
from sklearn.cluster import KMeans
from sklearn.manifold import TSNE
from sklearn.preprocessing import StandardScaler


def texture(image):
    """Contrast-normalized spatial features; no RGB or mean brightness."""
    a = np.asarray(image.convert('L'), dtype=float) / 255
    a = (a-a.mean()) / max(a.std(), .02)
    features = []
    for scale in (0, 1, 2, 4, 8):
        b = gaussian_filter(a, scale) if scale else a
        gx, gy = sobel(b, axis=1)/8, sobel(b, axis=0)/8
        mag = np.hypot(gx, gy)
        features.extend(np.quantile(mag, [.25,.5,.75,.9,.99]))
        features.append(b.std())
        hist, _ = np.histogram(np.arctan2(gy,gx), bins=8, range=(-np.pi,np.pi), weights=mag)
        features.extend(hist / max(hist.sum(), 1e-8))
    for lag in (1,2,4,8,16):
        features.extend([np.mean((a[:,lag:]-a[:,:-lag])**2),
                         np.mean((a[lag:]-a[:-lag])**2)])
    return features


def jpeg(image):
    buf = io.BytesIO()
    image.save(buf, format='JPEG', quality=85)
    return 'data:image/jpeg;base64,'+base64.b64encode(buf.getvalue()).decode()


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--source', type=Path, required=True)
    p.add_argument('--config', type=Path, required=True)
    p.add_argument('--output', type=Path, required=True)
    p.add_argument('--tile', type=int, default=100)
    p.add_argument('--clusters', type=int, default=8)
    p.add_argument('--sample-leg', action='store_true', help='Add two time-spaced camera-3 scenes per day')
    p.add_argument('--max-tiles-per-scene', type=int, default=24)
    args = p.parse_args()
    if not 32 <= args.tile <= 512:
        p.error('tile must be 32..512')
    records, features, scenes = [], [], []
    entries = json.loads(args.config.read_text())
    if args.max_tiles_per_scene < 1:
        p.error('max tiles per scene must be positive')
    if args.sample_leg:
        seen = {e['file'] for e in entries}
        for day in sorted(args.source.glob('[0-9]'*8)):
            paths = sorted(day.glob('*/Camera360_*_cam_3.jpg'))
            for index in sorted({len(paths)//4,3*len(paths)//4}) if paths else []:
                relative = str(paths[index].relative_to(args.source))
                if relative not in seen:
                    entries.append({'file':relative,'roi':[.25,.55,.65,.85]})
                    seen.add(relative)
    for scene, entry in enumerate(entries, 1):
        path = (args.source/entry['file']).resolve()
        if not path.is_relative_to(args.source.resolve()):
            raise ValueError('Source path escapes archive')
        roi = entry['roi']
        if len(roi)!=4 or not (0<=roi[0]<roi[2]<=1 and 0<=roi[1]<roi[3]<=1):
            raise ValueError('Invalid ROI')
        with Image.open(path) as im:
            w,h = im.size
            x0,y0,x1,y1 = [round(v*s) for v,s in zip(roi,(w,h,w,h))]
            crop = im.convert('RGB').crop((x0,y0,x1,y1))
        scenes.append({'scene':scene,'file':entry['file'], 'roi':roi,
                       'image':jpeg(ImageOps.contain(crop,(700,350)))})
        # Keep all five scenes, including those excluded by the older colour pilot.
        positions = [(x,y) for y in range(0,crop.height-args.tile+1,args.tile)
                     for x in range(0,crop.width-args.tile+1,args.tile)]
        if args.sample_leg and len(positions)>args.max_tiles_per_scene:
            rng = np.random.default_rng(42+scene)
            positions = [positions[i] for i in sorted(rng.choice(len(positions),args.max_tiles_per_scene,replace=False))]
        for x,y in positions:
                tile = crop.crop((x,y,x+args.tile,y+args.tile))
                features.append(texture(tile))
                records.append({'id':len(records),'scene':scene,'file':entry['file'],
                                'box':[x0+x,y0+y,x0+x+args.tile,y0+y+args.tile],
                                'image':jpeg(tile)})
    if len(records)<4 or not 2<=args.clusters<len(records):
        raise ValueError('Need at least four tiles and 2 <= clusters < tile count')
    values = StandardScaler().fit_transform(features)
    labels = KMeans(n_clusters=args.clusters,random_state=42,n_init=20).fit_predict(values)
    perplexity = min(30., (len(records)-1)/3)
    coords = TSNE(n_components=2,perplexity=perplexity,random_state=42,
                  init='pca',learning_rate='auto').fit_transform(values)
    for row, label, point in zip(records,labels,coords):
        row.update(cluster=int(label),x=float(point[0]),y=float(point[1]))
    data = {'tiles':records,'scenes':scenes,'tile_size':args.tile,'perplexity':perplexity,
            'seed':42,'clusters':args.clusters, 'sample_leg':args.sample_leg}
    args.output.mkdir(parents=True,exist_ok=True)
    (args.output/'tiles.json').write_text(json.dumps(data))
    html = TEMPLATE.replace('DATA_HERE',json.dumps(data).replace('<','\\u003c'))
    (args.output/'index.html').write_text(html)
    print(f'{len(records)} tiles; {args.output / "index.html"}')


TEMPLATE = '''<!doctype html><meta charset="utf-8"><meta name="viewport" content="width=device-width">
<title>Ice texture explorer</title><style>
body{font:16px system-ui;margin:20px;background:#17212b;color:#eee}button,select,input{font:inherit;margin:5px;padding:6px}
canvas{width:100%;max-width:900px;background:#fff;touch-action:manipulation}img{max-width:100%}.tiles{display:flex;flex-wrap:wrap;gap:8px}
.tile{width:110px;font-size:12px;cursor:pointer}.tile img{width:100px}#detail{padding:12px;background:#293846}article{display:inline-block;width:320px;margin:8px}a{color:#9de}
</style><h1>Ice texture explorer</h1>
<p>Native-pixel tiles from sea crops. Partial edge tiles omitted. All five reference scenes included.
Leg sampling adds two time-spaced scenes per day and caps tiles per scene. The reused crop needs review for ship, sky, land and poor visibility; no automatic quality exclusions.</p>
<p>t-SNE is a similarity visualization, not a concentration estimate. Groups use k-means on standardized texture features, <b>not on t-SNE coordinates</b>.
No RGB or mean-brightness features; contrast-normalized gradients, scale statistics and spatial differences can still reflect glare, perspective and camera artifacts.
Spacing, island size and group numbers have no physical meaning. Seed 42; groups are exploratory.</p>
<label>Colour by <select id="colour"><option value="cluster">Texture group</option><option value="scene">Source scene</option></select></label>
<label>Show <select id="filter"><option value="all">All groups</option></select></label>
<canvas id="plot" width="900" height="600"></canvas><div id="detail">Click a point or tile to inspect it.</div>
<p><label>Group name <input id="name" placeholder="Select a group first"></label><button id="save">Assign name</button><button id="export">Download labels</button>
Names stay in this page until exported; reload clears them.</p><div id="tiles" class="tiles"></div><h2>Source crops</h2><div id="scenes"></div>
<script>const data=DATA_HERE, names={}, palette=['#e6194b','#3cb44b','#4363d8','#f58231','#911eb4','#008b8b','#b27800','#666666','#d050a0','#668000'];
const by=id=>document.getElementById(id), c=by('plot'),ctx=c.getContext('2d');
for(let i=0;i<data.clusters;i++){let o=document.createElement('option');o.value=i;o.textContent='Group '+i;by('filter').append(o)}
const xs=data.tiles.map(t=>t.x),ys=data.tiles.map(t=>t.y), xmin=Math.min(...xs),xmax=Math.max(...xs),ymin=Math.min(...ys),ymax=Math.max(...ys);
for(const t of data.tiles){t.px=20+860*(t.x-xmin)/(xmax-xmin||1);t.py=20+560*(t.y-ymin)/(ymax-ymin||1)}
function shown(){return data.tiles.filter(t=>by('filter').value==='all'||t.cluster===+by('filter').value)}
function inspect(t){by('detail').replaceChildren();let im=new Image();im.src=t.image;im.width=200;by('detail').append(im,document.createElement('br'),
document.createTextNode(`Tile ${t.id} · scene ${t.scene} · group ${t.cluster} · ${names[t.cluster]||'unnamed'} · source box ${t.box.join(', ')} · ${t.file}`))}
function draw(){ctx.clearRect(0,0,900,600);by('tiles').replaceChildren();for(const t of shown()){
const category=by('colour').value==='scene'?t.scene-1:t.cluster;
ctx.fillStyle=category<palette.length?palette[category]:`hsl(${category*137.508%360} 65% 40%)`;ctx.beginPath();ctx.arc(t.px,t.py,5,0,7);ctx.fill();
let d=document.createElement('div');d.className='tile';let im=new Image();im.src=t.image;im.loading='lazy';d.append(im,document.createTextNode(`S${t.scene} · G${t.cluster} · #${t.id}`));d.onclick=()=>inspect(t);by('tiles').append(d)}}
c.onclick=e=>{let r=c.getBoundingClientRect(),x=(e.clientX-r.left)*900/r.width,y=(e.clientY-r.top)*600/r.height;
let t=shown().reduce((a,b)=>!a||Math.hypot(b.px-x,b.py-y)<Math.hypot(a.px-x,a.py-y)?b:a,null);if(t)inspect(t)};
by('filter').onchange=()=>{by('name').value=names[by('filter').value]||'';draw()};by('colour').onchange=draw;
by('save').onclick=()=>{if(by('filter').value==='all'){alert('Select a group first');return}names[by('filter').value]=by('name').value;};
by('export').onclick=()=>{let blob=new Blob([JSON.stringify({group_names:names,seed:data.seed,tile_size:data.tile_size,
tiles:data.tiles.map(({image,px,py,...t})=>t)},null,2)],{type:'application/json'});let a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download='ice-texture-labels.json';a.click();setTimeout(()=>URL.revokeObjectURL(a.href),1000)};
for(const s of data.scenes){let a=document.createElement('article'),im=new Image();im.src=s.image;a.append(document.createTextNode('Scene '+s.scene),im);by('scenes').append(a)}draw();</script>'''

if __name__ == '__main__':
    main()
