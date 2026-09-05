"""Whole-region projection explorer with live annotations and human labels."""
import argparse
import importlib.util
import json
from pathlib import Path


def render(output, responses, follow_links=True):
    layout=output/'embedding.json'
    if not layout.exists(): return
    points=json.loads(layout.read_text())
    sources=output/'annotation-sources.json'
    if sources.exists():
        prior=[]
        for file in json.loads(sources.read_text()):
            if Path(file).exists():prior.extend(json.loads(Path(file).read_text()))
        responses=[*prior,*responses]
    by_file={r['file']:r for r in responses}
    annotations={p['id']:dict(response=by_file[p['file']]['response'],finish_reason=by_file[p['file']]['finish_reason']) for p in points if p['file'] in by_file}
    projections={'tsne':dict(coords=[[p['x'],p['y']] for p in points],trees={})}
    if (output/'projections.json').exists():
        extra=json.loads((output/'projections.json').read_text())
        if extra['ids']!=[p['id'] for p in points]: raise ValueError('Projection identities differ')
        projections.update(extra['projections'])
    fixed=output/'ward-k64.json'
    fixed=json.loads(fixed.read_text()) if fixed.exists() else None
    if fixed and fixed['ids']!=[p['id'] for p in points]:raise ValueError('Fixed cut identities differ')
    payload=json.dumps(dict(points=points,annotations=annotations,projections=projections,fixed=fixed)).replace('<','\\u003c')
    script=Path(__file__).with_name('ice-region-ui.js').read_text()
    page='''<!doctype html><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>Ice region projections</title>
<style>body{font:16px system-ui;background:#17212b;color:#eee;margin:18px}main{display:grid;grid-template-columns:1fr 1fr;gap:16px}canvas{width:100%;background:#0d1720;touch-action:none}img{max-width:100%}pre{white-space:pre-wrap}.controls{display:flex;gap:10px;align-items:center;flex-wrap:wrap;margin:8px 0}button,select,input{padding:5px}#tip{position:fixed;pointer-events:none;background:#17212b;padding:6px;display:none}#tip img{width:250px}@media(max-width:800px){main{grid-template-columns:1fr}}</style>
<h1>Whole-region projections</h1><p>Visual similarity is not verified ice type. No labels automatically propagate. Scroll to zoom; drag to pan or lasso. Shift adds to selection; Alt subtracts. Overlapping selections can have multiple human labels.</p>
<div class="controls"><label>Projection <select id="projection"></select></label><label>Clustering <select id="linkage"><option value="none">None</option><option value="ward">Ward</option><option value="single">Single linkage</option></select></label><label>Tree cut <input id="height" type="range" min="0" max="1000" value="500"><output id="cut"></output></label><label>Colour <select id="colour"><option value="average">Mean RGB</option><option value="brightness">Brightness</option><option value="cluster">Cluster</option><option value="human">Human labels</option><option value="total">Total ice %</option><option value="thin_new_ice">Thin/new %</option><option value="broken_ice">Broken/rubble %</option><option value="consolidated_ice">Consolidated %</option><option value="unknown">Unknown %</option><option value="visibility">Visibility</option></select></label><span id="status"></span></div>
<div class="controls"><label>Drag <select id="mode"><option value="pan">Pan</option><option value="lasso">Lasso</option></select></label><button id="clear">Clear selection</button><span id="selection"></span><input id="label" placeholder="Label name" maxlength="100"><button id="apply">Add label</button><button id="remove">Remove this label</button><button id="export">Export labels</button><label>Import <input id="import" type="file" accept=".json"></label><button id="queue">Export Qwen queue</button><button id="refresh">Refresh annotations</button><span id="save"></span></div>
<main><section><canvas id="map" width="800" height="650"></canvas><p>Selected points outlined cyan. Grey means no colour/annotation available. Tree heights use each projection's own units and are not comparable across projections.</p></section><section><h2 id="title"></h2><p id="clusterinfo"></p><p id="human"></p><img id="crop"><img id="source"><pre id="response"></pre></section></main><div id="tip"></div><script>window.ICE_DATA=PAYLOAD;
SCRIPT</script>'''.replace('PAYLOAD',payload).replace('SCRIPT',script)
    tmp=output/'regions.html.tmp';tmp.write_text(page);tmp.replace(output/'regions.html')
    linked=output/'linked-explorers.json'
    if follow_links and linked.exists():
        for folder in json.loads(linked.read_text()):render(Path(folder),responses,False)


def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--output',type=Path,required=True)
    p.add_argument('--annotations',type=Path);p.add_argument('--render-only',action='store_true')
    a=p.parse_args();a.output.mkdir(parents=True,exist_ok=True)
    if not a.render_only:
        import sys,base64,io
        sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
        from dashboard.ice_classifier import features
        from sklearn.manifold import TSNE
        from sklearn.preprocessing import StandardScaler
        import numpy as np
        from PIL import Image
        spec=importlib.util.spec_from_file_location('overnight',Path(__file__).with_name('ice-overnight.py'))
        mod=importlib.util.module_from_spec(spec);spec.loader.exec_module(mod)
        data=json.loads((Path.home()/'Downloads/amundsen-ice-texture-brightness/tiles.json').read_text());points=[];vectors=[]
        for job in mod.jobs(data,{'tiles':[]},Path('/media/cryomics/T7 Shield/Amundsen/Camera_360/2025_LEG_04')):
            with Image.open(io.BytesIO(base64.b64decode(job['images'][1].split(',',1)[1]))) as crop: vector=features(crop)
            vectors.append(vector);points.append(dict(**{k:job[k] for k in ('id','file','images')},brightness=float(vector[21]),mean_rgb=[float(vector[i]) for i in (0,7,14)]))
        coords=TSNE(n_components=2,perplexity=min(15,(len(points)-1)/3),random_state=42,init='pca',learning_rate='auto').fit_transform(StandardScaler().fit_transform(np.array(vectors)))
        for point,xy in zip(points,coords):point.update(x=float(xy[0]),y=float(xy[1]))
        (a.output/'embedding.json').write_text(json.dumps(points))
    results=a.annotations or a.output/'results.json';render(a.output,json.loads(results.read_text()) if results.exists() else [])
    print(a.output/'regions.html')


if __name__=='__main__':main()
