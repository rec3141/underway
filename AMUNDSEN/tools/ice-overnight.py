"""Bounded, local-only exploratory review; labels never enter model prompts."""
import argparse
import concurrent.futures
import html
import importlib.util
import json
import os
from pathlib import Path
import random
import subprocess
import time
import urllib.request
from datetime import datetime, timezone
from PIL import Image, ImageDraw, ImageOps


def module(name, file):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(file))
    mod = importlib.util.module_from_spec(spec); spec.loader.exec_module(mod)
    return mod


rot = module('rot', 'ice-rotated-preview.py')
monitor = module('monitor', 'ice-monitored-review.py')
MODEL = 'ice-qwen35'
LMS = str(Path.home()/'.lmstudio/bin/lms')
PROMPT = ('Exploratory sea ice interpretation, not navigation or scientific measurement. '
          'Do not assume dark means water or bright means ice. Distinguish glare, foam, '
          'open water, dark new/nilas/grease ice, broken ice/rubble, and thicker snow-covered ice. '
          'Estimate mutually exclusive percentages of image-plane sea-surface area for these '
          'categories, plus unknown; total 100%. Give total ice percentage, confidence and '
          'alternative explanations. Unknown is acceptable. Answer under 180 words. ')

STRUCTURED_PROMPT = '''Return one JSON object only, with this schema:
{"visibility":"clear|degraded|unusable|uncertain", "visibility_causes":["fog","wet_lens","blur","darkness","glare"], "confidence":"high|moderate|low", "percentages":{"open_water":0,"thin_new_ice":0,"broken_ice":0,"consolidated_ice":0,"unknown":0}, "explanation":"short explanation"}.
First assess visibility: look for fog, droplets/water smears on the lens, defocus/motion blur, darkness and glare. Do not interpret missing texture caused by optical obstruction as open water. Cause can be uncertain; visibility_causes may be empty. Degraded or uncertain visibility is valid even if some ice can be seen.
Percentages describe image-plane area of the sea surface INSIDE the orange quadrilateral in image 1, shown extracted/rotated in image 2. Ignore everything outside it. The five mutually exclusive percentages must sum to 100. Thin_new_ice includes relatively continuous dark nilas/grease/frazil/new ice, including frost flowers. Broken_ice includes distinct floes/rubble/fragments even if snow-covered; prioritize this category over snow cover. Consolidated_ice means continuous established/snow-covered ice not assigned to the other two. Open_water includes identifiable water with glare/foam; unresolvable area is unknown. Do not assume dark means water or white means ice. If unusable, unknown=100. These are rough visual classes, not physical thickness/age determinations. Do not provide navigation/safety advice. No human labels are supplied.'''


def request(images, prompt):
    client = urllib.request.build_opener(urllib.request.ProxyHandler({}))
    payload = dict(model=MODEL, temperature=0, max_tokens=6000, messages=[dict(
        role='user', content=[dict(type='text', text=prompt)]+[
            dict(type='image_url', image_url=dict(url=i)) for i in images])])
    req = urllib.request.Request('http://127.0.0.1:1234/v1/chat/completions',
        data=json.dumps(payload).encode(), headers={'Content-Type':'application/json'})
    with client.open(req, timeout=600) as response:
        return json.load(response)


def limit_qwen_cpu():
    """Linux-only: constrain the exact LM Studio Qwen backend, not Gemma."""
    if not hasattr(os,'sched_setaffinity'): return
    cpus=set(sorted(os.sched_getaffinity(0))[:4])
    matched=False
    for proc in Path('/proc').glob('[0-9]*'):
        try:
            cmd=(proc/'cmdline').read_bytes().split(b'\0')
            if not cmd or b'/.lmstudio/extensions/backends/' not in cmd[0]: continue
            if b'/data/scratch/models/lmstudio-community/Qwen3.5-9B-GGUF/Qwen3.5-9B-Q4_K_M.gguf' not in cmd: continue
            for task in (proc/'task').iterdir(): os.sched_setaffinity(int(task.name),cpus)
            matched=True
        except ProcessLookupError: continue
    if not matched: raise RuntimeError('Could not identify Qwen backend for CPU limit')


def atomic(path, value):
    tmp = path.with_suffix(path.suffix+'.tmp'); tmp.write_text(value); tmp.replace(path)


def render(out, rows):
    if (out/'embedding.json').exists():
        module('region_explorer','ice-region-explorer.py').render(out, rows)
    page = '<!doctype html><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>Overnight ice experiments</title><style>body{font:16px system-ui;max-width:1200px;margin:auto;padding:20px}img{max-width:48%;max-height:500px}pre{white-space:pre-wrap}article{border-top:1px solid;padding:20px 0}</style><h1>Blind local Qwen experiments</h1><p>Unvalidated exploratory predictions. Human labels were withheld from prompts. Cluster descriptions are weak labels, not individual coupon truth. Region predictions use image-plane area, not perspective-corrected concentration. Refresh for new results.</p>'
    page += f'<p>{len(rows)} completed requests.</p>'
    for r in rows:
        page += '<article><h2>'+html.escape(r['id'])+'</h2>'
        page += ''.join(f'<img src="{x}">' for x in r['images'])
        page += '<p>Human comparison (not supplied to model): '+html.escape(str(r.get('human_label','No matching label')))+' </p>'
        page += '<pre>'+html.escape(r['response'])+'</pre><p>Finish: '+html.escape(str(r['finish_reason']))+'</p></article>'
    atomic(out/'index.html', page)


def jobs(data, labels, source):
    # Interleave context-region requests and exact original labelled footprints.
    groups = {}
    for tile in labels['tiles']:
        groups.setdefault(tile['cluster'], []).append(tile)
    rng = random.Random(42)
    for tiles in groups.values(): rng.shuffle(tiles)
    coupons = [groups[g][i] for i in range(24) for g in sorted(groups) if i < len(groups[g])]
    for i in range(max(len(data['scenes']), len(coupons))):
        if i < len(data['scenes']):
            s = data['scenes'][i]
            with Image.open(source/s['file']) as im:
                im = im.convert('RGB')
                affine, polygon, _ = rot.geometry(im.size, angle=-30)
                crop = im.transform((1200,600), Image.Transform.AFFINE, affine, Image.Resampling.BICUBIC)
                full = im.copy(); ImageDraw.Draw(full).line(polygon+[polygon[0]], fill='orange', width=12)
                images = [rot.uri(ImageOps.contain(full,(1400,1400))), rot.uri(crop)]
            yield dict(id=f"region-scene-{s['scene']}", file=s['file'], polygon=polygon,
                       images=images, prompt=PROMPT+'Image 1 is context: assess ONLY inside the orange quadrilateral. Image 2 is that exact region extracted and rotated. Exclude the rest of the photo. Avoid double-counting frost flowers on new ice.')
        if i < len(coupons):
            t = coupons[i]
            with Image.open(source/t['file']) as im:
                images = [rot.uri(im.convert('RGB').crop(t['box']))]
            names = labels['group_names']
            label = names[str(t['cluster'])] if isinstance(names,dict) else names[t['cluster']]
            yield dict(id=f"coupon-{t['id']}", file=t['file'], box=t['box'], cluster=t['cluster'],
                       human_label=dict(cluster_description=label, individual=t.get('reviewed_label')),
                       images=images, prompt=PROMPT+'This is an exact 100x100 pixel coupon. If scale or lack of context prevents classification, say so.')


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--hours', type=float, default=4)
    p.add_argument('--output', type=Path, required=True)
    p.add_argument('--prepare-only', action='store_true')
    p.add_argument('--regions-only', action='store_true')
    p.add_argument('--structured', action='store_true')
    a = p.parse_args()
    if not 0 < a.hours <= 4: p.error('hours must be >0 and <=4')
    out = a.output; out.mkdir(parents=True, exist_ok=True)
    data = json.loads((Path.home()/'Downloads/amundsen-ice-texture-brightness/tiles.json').read_text())
    label_path = Path.home()/'Downloads/ice-texture-labels(1).json'
    labels = json.loads(label_path.read_text())
    source = Path('/media/cryomics/T7 Shield/Amundsen/Camera_360/2025_LEG_04')
    atomic(out/'human-labels-snapshot.json', json.dumps(labels))
    existing=out/'results.json'
    rows=json.loads(existing.read_text()) if existing.exists() else []
    expected=STRUCTURED_PROMPT if a.structured else None
    if rows and (not a.structured or any(r['prompt']!=expected for r in rows)):
        raise ValueError('Output already has incompatible results; use a new directory')
    done={r['id'] for r in rows}
    render(out, rows)
    deadline = time.monotonic()+a.hours*3600
    loaded = False; trips = 0; errors = 0; force_cool=False
    def unload():
        nonlocal loaded
        subprocess.run([LMS,'unload',MODEL], timeout=45, check=False); loaded=False
    def sample():
        values = monitor.temperatures()
        with (out/'telemetry.csv').open('a') as f:
            f.write(datetime.now(timezone.utc).isoformat()+','+','.join(map(str,values))+'\n')
        return values
    pool = concurrent.futures.ThreadPoolExecutor(max_workers=1)
    try:
        for job in jobs(data, labels, source):
            if a.regions_only and not job['id'].startswith('region-'): continue
            if a.structured:
                if not a.regions_only: raise ValueError('Structured mode requires --regions-only')
                job['prompt']=STRUCTURED_PROMPT
            if job['id'] in done: continue
            if a.prepare_only:
                atomic(out/'prepared-example.json', json.dumps(job)); return
            if time.monotonic() >= deadline: break
            cpu,gpu,*_ = sample()
            if force_cool or cpu >= 88 or gpu >= 78:
                if loaded: unload()
                cooling_start = time.monotonic()
                while cpu > 75 or gpu > 65:
                    if time.monotonic() >= deadline or time.monotonic()-cooling_start > 900:
                        raise RuntimeError('Cooling timeout or run deadline')
                    time.sleep(15); cpu,gpu,*_ = sample()
                force_cool=False
            if not loaded:
                subprocess.run([LMS,'load','qwen3.5-9b','--gpu','max','--context-length','16384',
                    '--parallel','1','--identifier',MODEL,'--ttl','1800','-y'],check=True,timeout=120)
                loaded=True
                limit_qwen_cpu()
            started=time.monotonic()
            future=pool.submit(request,job['images'],job['prompt'])
            interrupted=False
            while not future.done():
                cpu,gpu,*_=sample()
                if cpu >= 95 or gpu >= 83 or time.monotonic() >= deadline:
                    unload(); interrupted=True; trips+=1; force_cool=True
                    print('Interrupted for temperature/deadline',cpu,gpu,flush=True)
                    break
                time.sleep(3)
            try:
                response=future.result(timeout=45 if interrupted else 5)
                if interrupted:
                    if trips >= 3: raise RuntimeError('Three thermal interruptions; stopping')
                    continue
                choice=response['choices'][0]
                rows.append(dict(**job,response=choice['message']['content'] or '',
                    finish_reason=choice.get('finish_reason'),usage=response.get('usage'),
                    model=MODEL,max_tokens=6000,elapsed_s=time.monotonic()-started,
                    utc=datetime.now(timezone.utc).isoformat()))
                atomic(out/'results.json',json.dumps(rows));render(out,rows)
                print(job['id'],len(rows),'completed',flush=True)
            except Exception as e:
                errors+=1;print(type(e).__name__,str(e),flush=True)
                if not future.done() or errors>=3 or trips>=3: raise
                if loaded: unload()
            time.sleep(15)
    finally:
        unload()
        pool.shutdown(wait=False,cancel_futures=True)
        atomic(out/'status.json',json.dumps(dict(completed=len(rows),ended_utc=datetime.now(timezone.utc).isoformat())))


if __name__=='__main__': main()
