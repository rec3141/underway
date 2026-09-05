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


def brightness(image):
    a = np.asarray(image.convert('L'), dtype=float)/255
    return [float(a.mean()),float(a.std()),*np.quantile(a,[.05,.25,.5,.75,.95]).tolist()]


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--source', type=Path, required=True)
    p.add_argument('--config', type=Path, required=True)
    p.add_argument('--output', type=Path, required=True)
    p.add_argument('--tile', type=int, default=100)
    p.add_argument('--clusters', type=int, default=8)
    p.add_argument('--sample-leg', action='store_true', help='Add two time-spaced camera-3 scenes per day')
    p.add_argument('--max-tiles-per-scene', type=int, default=24)
    p.add_argument('--brightness-weight', type=float, default=0,
                   help='Relative block weight after standardization; 0 retains texture-only')
    p.add_argument('--labels', type=Path, help='Previous exported group labels, matched by source file and tile box')
    p.add_argument('--render-only', action='store_true', help='Refresh source previews and HTML without changing existing groups')
    args = p.parse_args()
    if not 32 <= args.tile <= 512:
        p.error('tile must be 32..512')
    if args.render_only:
        data = json.loads((args.output/'tiles.json').read_text())
        add_originals(data,args.source)
        write_report(data,args.output)
        return
    if not np.isfinite(args.brightness_weight) or not 0<=args.brightness_weight<=5:
        p.error('brightness weight must be 0..5')
    records, features, scenes, levels = [], [], [], []
    previous = json.loads(args.labels.read_text()) if args.labels else {}
    prior = {(r['file'],tuple(r['box'])):{'previous_group':r['cluster'],
             'reviewed_label':r.get('reviewed_label',''),
             'previous_label':previous.get('group_names',{}).get(str(r['cluster']), '')}
             for r in previous.get('tiles',[])}
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
                levels.append(brightness(tile))
                records.append({'id':len(records),'scene':scene,'file':entry['file'],
                                'box':[x0+x,y0+y,x0+x+args.tile,y0+y+args.tile],
                                'brightness_mean':levels[-1][0], 'image':jpeg(tile),
                                **prior.get((entry['file'],(x0+x,y0+y,x0+x+args.tile,y0+y+args.tile)),{})})
    if len(records)<4 or not 2<=args.clusters<len(records):
        raise ValueError('Need at least four tiles and 2 <= clusters < tile count')
    values = StandardScaler().fit_transform(features)
    if args.brightness_weight:
        # Equalize block dimensionality so seven brightness features are not
        # drowned out by eighty texture features. Weight is explicit in metadata.
        light = StandardScaler().fit_transform(levels)
        values = np.column_stack((values,light*args.brightness_weight*np.sqrt(values.shape[1]/light.shape[1])))
    labels = KMeans(n_clusters=args.clusters,random_state=42,n_init=20).fit_predict(values)
    perplexity = min(30., (len(records)-1)/3)
    coords = TSNE(n_components=2,perplexity=perplexity,random_state=42,
                  init='pca',learning_rate='auto').fit_transform(values)
    for row, label, point in zip(records,labels,coords):
        row.update(cluster=int(label),x=float(point[0]),y=float(point[1]))
    data = {'tiles':records,'scenes':scenes,'tile_size':args.tile,'perplexity':perplexity,
            'seed':42,'clusters':args.clusters, 'sample_leg':args.sample_leg,
            'brightness_weight':args.brightness_weight,'previous_group_names':previous.get('group_names',{})}
    add_originals(data,args.source)
    write_report(data,args.output)
    print(f'{len(records)} tiles; {args.output / "index.html"}')


def add_originals(data,source):
    for scene in data['scenes']:
        path = (source/scene['file']).resolve()
        if not path.is_relative_to(source.resolve()):
            raise ValueError('Source path escapes archive')
        with Image.open(path) as image:
            scene['original_size'] = list(image.size)
            scene['original_image'] = jpeg(ImageOps.contain(image.convert('RGB'),(1000,1000)))


def write_report(data,output):
    output.mkdir(parents=True,exist_ok=True)
    (output/'tiles.json').write_text(json.dumps(data))
    (output/'index.html').write_text(TEMPLATE.replace('DATA_HERE',json.dumps(data).replace('<','\\u003c')))


TEMPLATE = '''<!doctype html><meta charset="utf-8"><meta name="viewport" content="width=device-width">
<title>Ice texture explorer</title><style>
body{font:16px system-ui;margin:20px;background:#17212b;color:#eee}button,select,input{font:inherit;margin:5px;padding:6px}
canvas{width:100%;max-width:900px;background:#fff;touch-action:manipulation}img{max-width:100%}.tiles{display:flex;flex-wrap:wrap;gap:8px}
.tile{width:110px;font-size:12px;cursor:pointer}.tile img{width:100px}.tile.selected{outline:3px solid #ffcc33;outline-offset:2px}#detail{padding:12px;background:#293846}article{display:inline-block;width:320px;margin:8px}a{color:#9de}
</style><h1>Ice texture explorer</h1>
<p>Native-pixel tiles from sea crops. Partial edge tiles omitted. All five reference scenes included.
Leg sampling adds two time-spaced scenes per day and caps tiles per scene. The reused crop needs review for ship, sky, land and poor visibility; no automatic quality exclusions.</p>
<p>t-SNE is a similarity visualization, not a concentration estimate. Groups use k-means on standardized texture features, <b>not on t-SNE coordinates</b>.
Texture uses contrast-normalized gradients, scale statistics and spatial differences. Optional brightness uses mean, spread and quantiles.
Both can reflect glare, exposure, perspective and camera artifacts. <span id="settings"></span>
Spacing, island size and group numbers have no physical meaning. Seed 42; groups are exploratory.</p>
<label>Colour by <select id="colour"><option value="brightness">Brightness</option><option value="cluster">Texture group</option><option value="scene">Source scene</option></select></label>
<label>Show <select id="filter"><option value="all">All groups</option></select></label>
<label>Previous label <select id="prior"><option value="all">All previous groups</option></select></label>
<canvas id="plot" width="900" height="600"></canvas><div id="hover" style="display:none;position:fixed;pointer-events:none;background:#17212b;color:white;padding:8px;border:1px solid #aaa;z-index:10;width:170px"></div><div id="detail">Hover for a tile preview; click a point or tile for the full source image.</div>
<p><label>Group name <input id="name" placeholder="Select a group first"></label><button id="save">Assign name</button><button id="export">Download labels</button>
Names and individual labels stay in this page until exported; reload clears unsaved edits.</p><p>Browse tiles with ←/→ (previous/next), ↑/↓ (one library row), Home/End (first/last). X toggles ice on the selected tile. Keys follow the active filters and do not interrupt typing. <span id="position" aria-live="polite"></span><button id="markice">Toggle selected ice (X)</button><button id="clearlabel">Clear selected tile label</button></p><div id="tiles" class="tiles"></div><h2>Source crops</h2><div id="scenes"></div>
<script>const data=DATA_HERE, names={}, palette=['#e6194b','#3cb44b','#4363d8','#f58231','#911eb4','#008b8b','#b27800','#666666','#d050a0','#668000'];
const by=id=>document.getElementById(id), c=by('plot'),ctx=c.getContext('2d');
let selectedId=null;
by('settings').textContent='Brightness block weight: '+(data.brightness_weight||0)+'. Previous labels are retained per tile, not assigned to new groups.';
for(const [group,label] of Object.entries(data.previous_group_names||{})){let o=document.createElement('option');o.value=group;o.textContent=group+': '+label;by('prior').append(o)}
for(let i=0;i<data.clusters;i++){let o=document.createElement('option');o.value=i;o.textContent='Group '+i;by('filter').append(o)}
const xs=data.tiles.map(t=>t.x),ys=data.tiles.map(t=>t.y), xmin=Math.min(...xs),xmax=Math.max(...xs),ymin=Math.min(...ys),ymax=Math.max(...ys);
for(const t of data.tiles){t.px=20+860*(t.x-xmin)/(xmax-xmin||1);t.py=20+560*(t.y-ymin)/(ymax-ymin||1)}
function shown(){return data.tiles.filter(t=>(by('filter').value==='all'||t.cluster===+by('filter').value)&&(by('prior').value==='all'||t.previous_group===+by('prior').value))}
function inspect(t){selectedId=t.id;const visible=shown();by('position').textContent=`Tile ${visible.findIndex(v=>v.id===t.id)+1} of ${visible.length} · #${t.id} · individual label: ${t.reviewed_label||'unlabelled'}`;
for(const el of by('tiles').children)el.classList.toggle('selected',+el.dataset.id===t.id);
by('hover').style.display='none';by('detail').replaceChildren();let im=new Image();im.src=t.image;im.width=200;by('detail').append(im,document.createElement('br'),
document.createTextNode(`Tile ${t.id} · scene ${t.scene} · group ${t.cluster} · ${names[t.cluster]||'unnamed'} · previous: ${t.previous_label||'unlabelled'} · brightness ${(t.brightness_mean*255).toFixed(0)}/255 · source box ${t.box.join(', ')} · ${t.file}`));
const s=data.scenes.find(s=>s.scene===t.scene);if(s&&s.original_image){
const ns='http://www.w3.org/2000/svg',svg=document.createElementNS(ns,'svg'),[w,h]=s.original_size;
svg.setAttribute('viewBox',`0 0 ${w} ${h}`);svg.style.width='100%';svg.style.maxWidth='1000px';svg.style.display='block';
const full=document.createElementNS(ns,'image');full.setAttribute('href',s.original_image);full.setAttribute('width',w);full.setAttribute('height',h);svg.append(full);
function box(b,color,dashed){let rect=document.createElementNS(ns,'rect');for(const [k,v] of Object.entries({x:b[0],y:b[1],width:b[2]-b[0],height:b[3]-b[1],fill:'none',stroke:color,'stroke-width':w/300})){rect.setAttribute(k,v)}if(dashed)rect.setAttribute('stroke-dasharray',w/100);svg.append(rect)}
box(s.roi.map((v,i)=>v*(i%2?h:w)),'#00ffff',true);box(t.box,'#ff3030',false);
by('detail').append(document.createElement('br'),document.createTextNode('Full source frame (scaled preview): red = selected tile; dashed cyan = sea crop.'),svg)}}
function draw(){ctx.clearRect(0,0,900,600);by('tiles').replaceChildren();for(const t of shown()){
const category=by('colour').value==='scene'?t.scene-1:t.cluster;
const luminance=Math.round((t.brightness_mean??.5)*255);
ctx.fillStyle=by('colour').value==='brightness'?`rgb(${luminance},${luminance},${luminance})`:category<palette.length?palette[category]:`hsl(${category*137.508%360} 65% 40%)`;ctx.beginPath();ctx.arc(t.px,t.py,5,0,7);ctx.fill();if(by('colour').value==='brightness'){ctx.strokeStyle='#888';ctx.lineWidth=.6;ctx.stroke()}
let d=document.createElement('div');d.className='tile';d.dataset.id=t.id;d.classList.toggle('selected',t.id===selectedId);let im=new Image();im.src=t.image;im.loading='lazy';d.append(im,document.createTextNode(`S${t.scene} · G${t.cluster} · #${t.id}`));d.onclick=()=>inspect(t);by('tiles').append(d)}}
function nearest(e){let r=c.getBoundingClientRect(),x=(e.clientX-r.left)*900/r.width,y=(e.clientY-r.top)*600/r.height;
let t=shown().reduce((a,b)=>!a||Math.hypot(b.px-x,b.py-y)<Math.hypot(a.px-x,a.py-y)?b:a,null);return t&&Math.hypot(t.px-x,t.py-y)<15?t:null}
c.onclick=e=>{let t=nearest(e);if(t)inspect(t)};
c.onpointermove=e=>{const t=nearest(e),tip=by('hover');if(!t){tip.style.display='none';return}tip.replaceChildren();let im=new Image();im.src=t.image;im.width=160;tip.append(im,document.createElement('br'),document.createTextNode(`Tile ${t.id} · S${t.scene} · G${t.cluster} · brightness ${Math.round((t.brightness_mean??.5)*255)}/255`));tip.style.display='block';tip.style.left=Math.max(0,Math.min(e.clientX+16,innerWidth-195))+'px';tip.style.top=Math.max(0,Math.min(e.clientY+16,innerHeight-240))+'px'};
c.onpointerleave=()=>by('hover').style.display='none';
function filterChanged(){draw();const visible=shown(),t=visible.find(t=>t.id===selectedId)||visible[0];if(t)inspect(t);else{selectedId=null;by('position').textContent='No tiles match';by('detail').textContent='No tiles match these filters.'}}
function markTile(label){const t=shown().find(t=>t.id===selectedId);if(!t)return;t.reviewed_label=label;inspect(t)}
function toggleIce(){const t=shown().find(t=>t.id===selectedId);if(t)markTile(t.reviewed_label==='ice'?'':'ice')}
by('markice').onclick=toggleIce;by('clearlabel').onclick=()=>markTile('');
by('filter').onchange=()=>{by('name').value=names[by('filter').value]||'';filterChanged()};by('colour').onchange=draw;
by('prior').onchange=filterChanged;
document.addEventListener('keydown',e=>{
if(e.defaultPrevented||e.altKey||e.ctrlKey||e.metaKey||e.shiftKey||e.target.closest?.('input,textarea,select,button,[contenteditable="true"]'))return;
if(e.key.toLowerCase()==='x'){e.preventDefault();toggleIce();return}
if(!['ArrowLeft','ArrowRight','ArrowUp','ArrowDown','Home','End'].includes(e.key))return;
const visible=shown();if(!visible.length)return;e.preventDefault();
let i=visible.findIndex(t=>t.id===selectedId),cards=[...by('tiles').children];
const columns=Math.max(1,cards.filter(el=>el.offsetTop===cards[0].offsetTop).length);
if(e.key==='Home')i=0;else if(e.key==='End')i=visible.length-1;
else if(i<0)i=0;else i+=({ArrowLeft:-1,ArrowRight:1,ArrowUp:-columns,ArrowDown:columns})[e.key];
inspect(visible[Math.max(0,Math.min(visible.length-1,i))]);
});
by('save').onclick=()=>{if(by('filter').value==='all'){alert('Select a group first');return}names[by('filter').value]=by('name').value;};
by('export').onclick=()=>{let blob=new Blob([JSON.stringify({group_names:names,seed:data.seed,tile_size:data.tile_size,
tiles:data.tiles.map(({image,px,py,...t})=>t)},null,2)],{type:'application/json'});let a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download='ice-texture-labels.json';a.click();setTimeout(()=>URL.revokeObjectURL(a.href),1000)};
for(const s of data.scenes){let a=document.createElement('article'),im=new Image();im.src=s.image;a.append(document.createTextNode('Scene '+s.scene),im);by('scenes').append(a)}draw();</script>'''

if __name__ == '__main__':
    main()
