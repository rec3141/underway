"""Resumable 470-photo all-high Gemma rerun, sharing one server with chat."""
import base64
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from html import escape
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import time
import urllib.request
from PIL import Image, ImageDraw

ROOT=Path('/home/cryomics/Downloads')
OUT=ROOT/'amundsen-ice-gemma-floes-v2'
TOOLS=Path('/tmp/amundsen-camera-rotated/AMUNDSEN/tools')
UNIT='ice-gemma-shared.service'
URL='http://127.0.0.1:18043'
_spec=importlib.util.spec_from_file_location('taxonomy',Path(__file__).with_name('ice-taxonomy.py'))
taxonomy=importlib.util.module_from_spec(_spec);_spec.loader.exec_module(taxonomy)

def module(name,file):
    spec=importlib.util.spec_from_file_location(name,TOOLS/file)
    value=importlib.util.module_from_spec(spec);spec.loader.exec_module(value);return value

def atomic(path,value):
    temp=path.with_suffix(path.suffix+'.tmp');temp.write_text(value);temp.replace(path)

def calm(text):
    return taxonomy.rename(text)

def audit(response,quality):
    # Existing audit implementation uses the legacy spelling; responses stay unmodified.
    parsed=json.loads(response.strip().removeprefix('```json').removesuffix('```').strip())
    surface=parsed['surface_percentages']
    if 'calm water' not in surface or 'smooth water' in surface:raise ValueError('Wrong water label')
    if not {'thin ice floe','thick ice floe'}.issubset(surface) or {'thin fyi','ice floe'} & surface.keys():raise ValueError('Wrong floe labels')
    aliases={'calm water':'smooth water','thin ice floe':'thin fyi','thick ice floe':'ice floe'}
    canonical=dict(parsed,surface_percentages={aliases.get(k,k):v for k,v in surface.items()})
    result=quality.assess(json.dumps(canonical))
    for field in ('context_photo_description','roi_description'):
        if not isinstance(parsed.get(field),str) or not parsed[field].strip():result['quality_flags'].append('Missing '+field)
    return result

def request(payload):
    client=urllib.request.build_opener(urllib.request.ProxyHandler({}))
    req=urllib.request.Request(URL+'/v1/chat/completions',data=json.dumps(payload).encode(),headers={'Content-Type':'application/json'})
    with client.open(req,timeout=300) as response:return json.load(response)

def main():
    OUT.mkdir(exist_ok=True);(OUT/'images').mkdir(exist_ok=True)
    queue_data=json.loads((ROOT/'amundsen-ice-gemma-overnight/queue.json').read_text())
    queue=queue_data['queue'];assert len(queue)==len({r['file'] for r in queue})==470
    atomic(OUT/'queue.json',json.dumps(queue_data,indent=2))
    atomic(OUT/'scale-assumptions.json',json.dumps(taxonomy.SCALE,indent=2))
    human=json.loads((ROOT/'amundsen-ice-qwen-k32-classes/labels-clean.json').read_text())
    for row in human['labels']:row['labels']=[calm(v) for v in row['labels']]
    human['labelOrder']=[calm(v) for v in human['labelOrder']]
    atomic(OUT/'labels-clean.json',json.dumps(human,indent=2))
    humans={r['file']:r['labels'] for r in human['labels']}
    baseline=json.loads((ROOT/'amundsen-ice-qwen-k32-parallel/results.json').read_text())
    old_by_file={r['file']:r for r in baseline}
    prompts=module('full_prompt','ice-context-prompt.py');rot=module('full_rot','ice-rotated-preview.py')
    quality=module('full_quality','ice-response-quality.py');thermal=module('full_thermal','ice-overnight.py')
    key_bytes=(ROOT/'ice-resolution/key-hires-floes-v2.png').read_bytes()
    key='data:image/png;base64,'+base64.b64encode(key_bytes).decode()
    rows=json.loads((OUT/'results.json').read_text()) if (OUT/'results.json').exists() else []
    done={r['file'] for r in rows if r.get('finish_reason')=='stop'}
    started=time.monotonic();deadline=started+8*3600;window=thermal.RunningTemperature()
    def status(state,**extra):
        atomic(OUT/'status.json',json.dumps(dict(state=state,completed=len(done),expected=470,utc=datetime.now(timezone.utc).isoformat(),**extra),indent=2))
    def temperature():
        cpu,gpu,*_=thermal.monitor.temperatures();average=window.add(time.monotonic(),cpu)
        with (OUT/'telemetry.csv').open('a') as f:f.write(f'{time.time()},{cpu},{gpu},{average}\n')
        if time.monotonic()>deadline:raise TimeoutError('Eight-hour run bound reached')
        return cpu,gpu,average
    def cool():
        status('cooling')
        subprocess.run(['systemctl','--user','stop',UNIT],check=True)
        while True:
            cpu,gpu,avg=temperature()
            if cpu<85 and gpu<70 and avg<85:break
            time.sleep(2)
        subprocess.run(['systemctl','--user','start',UNIT],check=True)
    def ready():
        for _ in range(90):
            cpu,gpu,avg=temperature()
            if avg>=95 or gpu>=83:cool()
            try:
                with urllib.request.urlopen(URL+'/health',timeout=2) as r:
                    if r.status==200:return
            except OSError:pass
            time.sleep(2)
        raise RuntimeError('Shared model server unavailable')
    def render():
        page='<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><meta http-equiv="refresh" content="30"><title>Gemma 470 rerun</title><style>body{font:16px system-ui;max-width:1100px;margin:2rem auto;padding:1rem}img{max-width:48%;max-height:420px}pre{white-space:pre-wrap}article{border-top:1px solid #bbb;padding:1rem 0}</style><h1>Gemma · all-high · floes v2</h1>'
        page+=f'<p>{len(done)} / 470 completed. 1120 image tokens; microbatch 2048; reasoning off. Native context, doubled ROI, rebuilt high-resolution key. Human labels withheld from prompts. Exploratory estimates only.</p>'
        page+='<h2>Approximate scale and size convention</h2><p>Camera height provisionally 9–12 m (three stories). ROI ground extent roughly 50 × 25 m, tapered rather than rectangular; allow about a factor-of-two uncertainty. Height, projection and tilt are not calibrated. Brash: individual pieces &lt;20 m across; thin/thick ice floes: coherent pieces &gt;20 m across. Thin/thick are appearance labels, not measured thickness. Key examples illustrate appearance, not independently verified sizes. Human label names were translated, not re-reviewed against the new size rule.</p>'
        page+='<h2>Reference key</h2><p><a href="ice-resolution/key-hires-floes-v2.png" target="_blank" rel="noopener">Open full-resolution key · 2928 × 3064</a></p><a href="ice-resolution/key-hires-floes-v2.png" target="_blank" rel="noopener"><img alt="Reference key with thin and thick ice floe labels" src="ice-resolution/key-hires-floes-v2.png" style="width:100%;max-width:100%;max-height:none"></a>'
        for r in rows:
            page+='<article><h2>'+escape(r['file'])+'</h2><p>Human: '+escape(', '.join(r['human_label']))+'</p>'
            page+=''.join('<img loading="lazy" src="'+OUT.name+'/'+escape(p,quote=True)+'">' for p in r['images'])
            page+='<h3>Response · '+str(round(r['elapsed_s'],1))+' seconds</h3><pre>'+escape(r['response'])+'</pre></article>'
        atomic(ROOT/'gemma.html',page+'</html>')
    status('starting');render()
    subprocess.run(['systemctl','--user','start',UNIT],check=True)
    try:
        for item in queue:
            file=item['file']
            if file in done:continue
            for attempt in range(3):
                try:
                    ready()
                    _,gpu,_=temperature()
                    while gpu>=78:time.sleep(2);_,gpu,_=temperature()
                    with Image.open(Path('/media/cryomics/T7 Shield/Amundsen/Camera_360/2025_LEG_04')/file) as im:
                        full=im.convert('RGB')
                    assert full.size==(3648,2052)
                    affine,polygon,_=rot.geometry(full.size,angle=-30)
                    crop=full.transform((1200,600),Image.Transform.AFFINE,affine,Image.Resampling.BICUBIC)
                    # Match the pilot's JPEG-native-ROI -> resize -> JPEG pipeline exactly.
                    native_uri=old_by_file[file]['images'][1] if file in old_by_file else rot.uri(crop)
                    with Image.open(io.BytesIO(base64.b64decode(native_uri.split(',',1)[1]))) as native:
                        crop=native.resize((2400,1200),Image.Resampling.LANCZOS)
                    ImageDraw.Draw(full).line(polygon+[polygon[0]],fill='orange',width=12)
                    images=[rot.uri(full),rot.uri(crop),key]
                    texts,_=prompts.build(baseline[0]['prompt'],images);texts=taxonomy.build_prompt(texts)
                    content=[]
                    for text,index in zip(texts,[0,2,1]):content.extend([dict(type='text',text=text),dict(type='image_url',image_url=dict(url=images[index]))])
                    payload=dict(model='gemma-camera',messages=[dict(role='user',content=content)],temperature=0,max_tokens=2000,stream=False,seed=42,chat_template_kwargs=dict(enable_thinking=False))
                    status('running',file=file,attempt=attempt+1);begin=time.monotonic();tripped=False
                    with ThreadPoolExecutor(max_workers=1) as pool:
                        future=pool.submit(request,payload)
                        while not future.done():
                            _,gpu,avg=temperature()
                            if avg>=95 or gpu>=83:
                                subprocess.run(['systemctl','--user','stop',UNIT],check=True);tripped=True;break
                            time.sleep(2)
                        if tripped:cool();raise RuntimeError('Request interrupted for cooling; retrying')
                        raw=future.result()
                    response=raw['choices'][0]['message']['content'];finish=raw['choices'][0]['finish_reason']
                    if finish!='stop':raise ValueError('Incomplete model response: '+finish)
                    if raw['choices'][0]['message'].get('reasoning_content'):raise ValueError('Unexpected reasoning')
                    checked=audit(response,quality)
                    paths=[]
                    for i,uri in enumerate(images[:2]):
                        path=f'images/{item["id"]}-{i}.jpg';(OUT/path).write_bytes(base64.b64decode(uri.split(',',1)[1]));paths.append(path)
                    row=dict(id=item['id'],file=file,model='gemma-budget-1120-ub2048-all-high-floes-v2',reasoning='off',images=paths,scale_assumptions=taxonomy.SCALE,
                             prompt_segments=texts,response=response,finish_reason=finish,elapsed_s=time.monotonic()-begin,
                             human_label=humans.get(file,[]),queue_metadata=item,usage=raw.get('usage'),utc=datetime.now(timezone.utc).isoformat(),**checked)
                    rows.append(row);done.add(file);atomic(OUT/'results.json',json.dumps(rows,indent=2));render()
                    print(f'{len(done)}/470 {file} {row["elapsed_s"]:.1f}s',flush=True);break
                except Exception as error:
                    status('retrying',file=file,error=str(error),attempt=attempt+1)
                    print('ERROR',file,error,flush=True)
                    if attempt==2:raise
                    time.sleep(5)
            status('running')
        status('complete')
    except BaseException as error:
        status('stopped',error=str(error));raise

if __name__=='__main__':main()
