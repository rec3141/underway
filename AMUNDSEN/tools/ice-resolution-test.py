"""Paired context-resolution experiment; unchanged native ROI, key and prompt."""
import argparse
import base64
from concurrent.futures import ThreadPoolExecutor
from html import escape
import io
import os
import json
from pathlib import Path
import subprocess
import time
import urllib.request
from PIL import Image, ImageDraw, ImageOps
import importlib.util

TOOLS = Path('/tmp/amundsen-camera-rotated/AMUNDSEN/tools')
def module(name, file):
    spec=importlib.util.spec_from_file_location(name, TOOLS/file)
    value=importlib.util.module_from_spec(spec);spec.loader.exec_module(value)
    return value

def main():
    ap=argparse.ArgumentParser();ap.add_argument('--model',choices=['gemma','qwen','muse'],required=True)
    ap.add_argument('--gemma-budget',type=int,choices=[280,1120]);ap.add_argument('--render-only',action='store_true')
    ap.add_argument('--all-high',action='store_true',help='Native context, doubled ROI, key rebuilt from native crops');args=ap.parse_args()
    if args.gemma_budget and args.model!='gemma':ap.error('Budget experiment is Gemma only')
    root=Path('/home/cryomics/Downloads');out=root/'ice-resolution';out.mkdir(exist_ok=True)
    old=json.loads((root/'amundsen-ice-qwen-k32-parallel/results.json').read_text())
    human={r['file']:r['labels'] for r in json.loads((root/'amundsen-ice-qwen-k32-classes/labels-clean.json').read_text())['labels']}
    # Fixed, human-label-selected examples: water, ambiguous smooth ice, coherent ice.
    samples=[next(r for r in old if human.get(r['file'])==[label]) for label in ['small waves','grease ice','ice floe']]
    prompts=module('resolution_prompts','ice-context-prompt.py');rot=module('resolution_rot','ice-rotated-preview.py')
    thermal=module('resolution_thermal','ice-overnight.py');quality=module('resolution_quality','ice-response-quality.py')
    key_path=out/'key-hires.png' if args.all_high else root/'ice-reference-key/ice-reference-key.png'
    key='data:image/png;base64,'+base64.b64encode(key_path.read_bytes()).decode()
    model={'gemma':'google/gemma-4-26b-a4b-qat','qwen':'qwen3.5-9b','muse':'muse-glimmer-native'}[args.model]
    slug=f'gemma-budget-{args.gemma_budget}-ub2048' if args.gemma_budget else args.model
    if args.all_high:slug+='-all-high'
    dest=out/(slug+'.json');results=json.loads(dest.read_text()) if dest.exists() else []
    server=None;server_log=None
    window=thermal.RunningTemperature()
    def check():
        cpu,gpu,*_=thermal.monitor.temperatures();avg=window.add(time.monotonic(),cpu)
        if avg>=95 or gpu>=83:
            if server:server.terminate()
            elif args.model=='muse':subprocess.run(['ollama','stop',model],check=False)
            else:subprocess.run([thermal.LMS,'unload',model],check=False)
            raise RuntimeError('Thermal cutoff')
        return dict(cpu=cpu,gpu=gpu,cpu_average=avg)
    def request(payload):
        url='http://127.0.0.1:18043/v1/chat/completions' if args.gemma_budget else ('http://127.0.0.1:11434/api/chat' if args.model=='muse' else 'http://127.0.0.1:1234/api/v1/chat')
        client=urllib.request.build_opener(urllib.request.ProxyHandler({}))
        req=urllib.request.Request(url,data=json.dumps(payload).encode(),headers={'Content-Type':'application/json'})
        with client.open(req,timeout=300) as response:return json.load(response)
    def render():
        rows=[]
        for path in out.glob('*.json'):
            rows.extend(json.loads(path.read_text()))
        html='<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Ice resolution trial</title><style>body{font:16px system-ui;max-width:1200px;margin:2rem auto;padding:1rem}pre{white-space:pre-wrap}img{max-width:100%;max-height:60vh}td,th{padding:.5rem;border-bottom:1px solid #ccc}table{border-collapse:collapse}</style><h1>Paired context-resolution trial</h1><p>Three human-label-selected photos; same native 1200×600 ROI and reference key. Only context resolution changes. Labels indicate presence, not area ground truth. Timings include different cache states; this is a pilot, not a definitive accuracy benchmark. Refresh for new results.</p><table><tr><th>Model</th><th>Human label</th><th>Context</th><th>Seconds</th><th>Ice %</th><th>Unknown %</th><th>Input tokens</th></tr>'
        for r in rows:
            html+='<tr>'+''.join('<td>'+escape(str(v))+'</td>' for v in [r['model'],', '.join(r['human']),r['variant'],round(r['elapsed_s'],1),r.get('derived_total_ice_percent'),r.get('unknown'),r.get('input_tokens')])+'</tr>'
        html+='</table>'
        high_rows=[r for r in rows if r['variant']=='all-high']
        if high_rows:
            html+='<h2>All-high versus context-only high</h2><p>Same native context and model budget on each side. Entries show total ice / unknown percentages, not a measured accuracy score.</p><table><tr><th>Model</th><th>Human label</th><th>Context-only high</th><th>All-high</th></tr>'
            for r in high_rows:
                base=next((b for b in rows if b['model']==r['model'].removesuffix('-all-high') and b['variant']=='native' and b['file']==r['file']),None)
                if base:
                    html+='<tr>'+''.join('<td>'+escape(str(v))+'</td>' for v in [base['model'],', '.join(r['human']),f'{base["derived_total_ice_percent"]} / {base["unknown"]}',f'{r["derived_total_ice_percent"]} / {r["unknown"]}'])+'</tr>'
            html+='</table><p>All-high pilot: Gemma (1120-token budget) assigns 85% grease ice and Qwen 80% grease ice to the human grease-ice example, versus 0% and 3% in their context-only-high runs. Muse still assigns 0% grease ice. This is promising for Gemma/Qwen on this one ambiguous example, not a general accuracy estimate. Qwen still misses obvious blur on the floe example. The larger key and enlarged ROI changed together, so their individual contributions remain untested.</p>'
        html+='<p><strong>All-high extension:</strong> rows marked all-high submit native 3648×2052 context, a 2400×1200 Lanczos-upscaled version of the identical 1200×600 ROI, and a 2928×3064 reference key rebuilt from the same 18 original camera crops (not enlarged thumbnails). The ROI enlargement adds no genuine camera detail. This changes all three inputs together, so it cannot isolate which image caused an answer change.</p><p><a href="ice-resolution/key-hires.png">Higher-resolution reference key</a></p>'
        html+='<h2>Unchanged target cutouts</h2>'
        for index,sample in enumerate(samples):
            html+='<h3>'+escape(', '.join(human[sample['file']]))+'</h3><img src="'+sample['images'][1]+'">'
        html+='<h2>Effective image budgets</h2><p>The installed LM Studio llama.cpp build (commit 0f3a71b) defaults Gemma to 280 visual tokens/image (~0.645 MP) and Qwen to 4096 (~4.19 MP). Muse in Ollama logs a 3,211,264-pixel ceiling (4096 × 28 × 28). These are processing budgets, not upload rejection limits: larger uploads are resized. Gemma budget trials below use the same installed engine directly at 280 or 1120 tokens, native context, identical seed and prompts; compare those two runs to isolate the budget effect.</p>'
        html+='<p><a href="https://github.com/ggml-org/llama.cpp/blob/0f3a71b/tools/mtmd/clip.cpp">Installed engine source</a> · <a href="https://ai.google.dev/gemma/docs/core/model_card_4">Gemma supported budgets</a></p>'
        html+='<h2>Interpretation of the initial context-only pilot</h2><p>This small pilot does not establish an accuracy winner. All three models keep the wave example water-only. Qwen changes strongly with context resolution: it loses agreement with the human grease label while recognizing more ice in the floe example, but also drops an obvious blur warning. Muse remains conservative about the blurred floe at both sizes. Enlarging context alone did not reliably resolve grease ice versus smooth water. The subsequent all-high experiment also changes the key and ROI; see its separate comparison above. Unknown area is not equivalent to open water; lower unknown is not automatically an improvement.</p>'
        html+='<p>The first 1120-token trial failed a non-causal attention batch assertion, not a thermal cutoff. Successful controlled budget trials have <code>ub2048</code> in their names: both use batch and microbatch sizes of 2048. The earlier <code>gemma-budget-280</code> run used default batching and is retained for transparency. Human labels were withheld from model prompts. No production settings or training labels were changed.</p>'
        for r in rows:
            html+='<h2>'+escape(r['model']+' · '+r['variant']+' · '+', '.join(r['human']))+'</h2><p>'+escape(r['file'])+'</p><img src="ice-resolution/'+r['image']+'"><details><summary>Response</summary><pre>'+escape(r['response'])+'</pre></details>'
        (root/'resolution.html').write_text(html)
    if args.render_only:
        render();return
    try:
        while check()['gpu']>=78:time.sleep(2)
        if args.gemma_budget:
            binary=Path('/home/cryomics/.lmstudio/extensions/backends/llama.cpp-linux-x86_64-nvidia-cuda12-avx2-2.33.0/llama-server')
            models=Path('/data/scratch/models/lmstudio-community/gemma-4-26B-A4B-it-QAT-GGUF')
            env=dict(os.environ,LD_LIBRARY_PATH='/home/cryomics/.lmstudio/extensions/backends/vendor/linux-llama-cuda12-vendor-v1')
            server_log=(out/(slug+'.log')).open('a')
            server=subprocess.Popen([str(binary),'--model',str(models/'gemma-4-26B-A4B-it-QAT-Q4_0.gguf'),'--mmproj',str(models/'mmproj-gemma-4-26B-A4B-it-QAT-BF16.gguf'),
                '--host','127.0.0.1','--port','18043','--no-webui','--ctx-size','16384','--n-gpu-layers','99','--threads','4','--parallel','1','--jinja','--reasoning','off',
                '--batch-size','2048','--ubatch-size','2048','--image-max-tokens',str(args.gemma_budget),'--verbosity','4'],env=env,stdout=server_log,stderr=subprocess.STDOUT)
            ready=False
            for _ in range(90):
                check()
                if server.poll() is not None:raise RuntimeError('Local server failed; see '+str(server_log.name))
                try:
                    with urllib.request.urlopen('http://127.0.0.1:18043/health',timeout=1) as r:ready=r.status==200
                except OSError:pass
                if ready:break
                time.sleep(2)
            if not ready:raise RuntimeError('Local server readiness timeout')
        elif args.model!='muse':
            subprocess.run([thermal.LMS,'load',model,'--gpu','max','--context-length','16384','--parallel','1','--identifier',model,'--ttl','1800','-y'],check=True,timeout=180)
        for index, row in enumerate(samples):
            file=row['file']
            with Image.open(Path('/media/cryomics/T7 Shield/Amundsen/Camera_360/2025_LEG_04')/file) as original:
                full=original.convert('RGB')
            _,polygon,_=rot.geometry(full.size,angle=-30)
            ImageDraw.Draw(full).line(polygon+[polygon[0]],fill='orange',width=12)
            # Reverse order on alternate photos to reduce systematic cache/order bias.
            variants=['1400','native'] if index%2==0 else ['native','1400']
            if args.gemma_budget:variants=['native']
            if args.all_high:variants=['all-high']
            for variant in variants:
                if any(r['file']==file and r['variant']==variant for r in results):continue
                context=ImageOps.contain(full,(1400,1400)) if variant=='1400' else full
                roi_uri=row['images'][1]
                if args.all_high:
                    with Image.open(io.BytesIO(base64.b64decode(roi_uri.split(',',1)[1]))) as roi:
                        assert roi.size==(1200,600)
                        roi_uri=rot.uri(roi.resize((2400,1200),Image.Resampling.LANCZOS))
                images=[rot.uri(context),roi_uri,key]
                texts,blocks=prompts.build(row['prompt'],images)
                image_name=f'{index}-{variant}.jpg';context.save(out/image_name,quality=88)
                if args.gemma_budget:
                    content=[]
                    for text,i in zip(texts,[0,2,1]):
                        content.extend([dict(type='text',text=text),dict(type='image_url',image_url=dict(url=images[i]))])
                    payload=dict(model='gemma',messages=[dict(role='user',content=content)],temperature=0,max_tokens=2000,stream=False,seed=42,chat_template_kwargs=dict(enable_thinking=False))
                elif args.model=='muse':
                    payload=dict(model=model,messages=[dict(role='user',content=t,images=[images[i].split(',',1)[1]]) for t,i in zip(texts,[0,2,1])],think=False,stream=False,keep_alive=0,options=dict(temperature=0,num_predict=2000))
                else:payload=dict(model=model,input=blocks,temperature=0,max_output_tokens=2000,reasoning='off',store=False)
                while check()['gpu']>=78:time.sleep(2)
                print('START',args.model,variant,file,flush=True);started=time.monotonic();telemetry=[]
                with ThreadPoolExecutor(max_workers=1) as pool:
                    future=pool.submit(request,payload)
                    while not future.done():
                        telemetry.append(check());time.sleep(2)
                    raw=future.result()
                response=raw['choices'][0]['message']['content'] if args.gemma_budget else (raw['message']['content'] if args.model=='muse' else '\n'.join(r['content'] for r in raw.get('output',[]) if r['type']=='message'))
                try:parsed=json.loads(response.strip().removeprefix('```json').removesuffix('```').strip())
                except ValueError:parsed={}
                result=dict(model=slug,file=file,human=human[file],variant=variant,context_size=list(context.size),image=image_name,
                            roi_size=[2400,1200] if args.all_high else [1200,600],key_size=[2928,3064] if args.all_high else [1464,1532],
                            elapsed_s=time.monotonic()-started,response=response,raw=raw,telemetry=telemetry,
                            input_tokens=raw.get('prompt_eval_count',raw.get('stats',{}).get('input_tokens',raw.get('usage',{}).get('prompt_tokens'))),
                            unknown=parsed.get('surface_percentages',{}).get('unknown'),**quality.assess(response))
                results.append(result);dest.write_text(json.dumps(results,indent=2));render()
                print('DONE',args.model,variant,round(result['elapsed_s'],1),'ice',result['derived_total_ice_percent'],'unknown',result['unknown'],flush=True)
    finally:
        if server:
            server.terminate()
            try:server.wait(timeout=15)
            except subprocess.TimeoutExpired:server.kill();server.wait()
            server_log.close()
        elif args.model=='muse':subprocess.run(['ollama','stop',model],check=False)
        else:subprocess.run([thermal.LMS,'unload',model],check=False)

if __name__=='__main__':main()
