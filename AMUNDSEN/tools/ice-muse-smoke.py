"""Bounded local Muse vision trial with the existing context/key/ROI prompt."""
import base64
from concurrent.futures import ThreadPoolExecutor
from html import escape
import importlib.util
import json
from pathlib import Path
import time
import urllib.request

TOOLS = Path('/tmp/amundsen-camera-rotated/AMUNDSEN/tools')
def module(name, file):
    spec = importlib.util.spec_from_file_location(name, TOOLS / file)
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result

def call(payload):
    client = urllib.request.build_opener(urllib.request.ProxyHandler({}))
    request = urllib.request.Request('http://127.0.0.1:11434/api/chat', data=json.dumps(payload).encode(), headers={'Content-Type': 'application/json'})
    with client.open(request, timeout=240) as response:
        return json.load(response)

def main():
    prompts = module('muse_prompts', 'ice-context-prompt.py')
    quality = module('muse_quality', 'ice-response-quality.py')
    overnight = module('muse_thermal', 'ice-overnight.py')
    window = overnight.RunningTemperature()
    root = Path('/home/cryomics/Downloads')
    rows = json.loads((root / 'amundsen-ice-qwen-k32-parallel/results.json').read_text())
    chosen = json.loads((root / 'amundsen-ice-model-comparison/manifest.json').read_text())['queue'][0]
    old = next(row for row in rows if row['file'] == chosen['file'])
    key = 'data:image/png;base64,' + base64.b64encode((root / 'ice-reference-key/ice-reference-key.png').read_bytes()).decode()
    images = old['images'][:2] + [key]
    texts, blocks = prompts.build(old['prompt'], images)
    # Separate user messages preserve the context/key/ROI sequence with image attachments.
    messages = [dict(role='user', content=text, images=[images[i].split(',', 1)[1]]) for text, i in zip(texts, [0, 2, 1])]
    cpu, gpu, *_ = overnight.monitor.temperatures()
    if cpu >= 95 or gpu >= 78:
        raise RuntimeError('Too warm to admit trial')
    window.add(time.monotonic(), cpu)
    started = time.monotonic()
    telemetry = []
    with ThreadPoolExecutor(max_workers=1) as pool:
        future = pool.submit(call, dict(model='muse-glimmer-native', messages=messages, think=False, stream=False, keep_alive=0, options=dict(num_predict=2000, temperature=0)))
        while not future.done():
            cpu, gpu, *_ = overnight.monitor.temperatures()
            avg = window.add(time.monotonic(), cpu)
            telemetry.append(dict(seconds=time.monotonic()-started, cpu=cpu, gpu=gpu, cpu_average=avg))
            if avg >= 95 or gpu >= 83:
                import subprocess
                subprocess.run(['ollama', 'stop', 'muse-glimmer-native'], check=False)
                raise RuntimeError('Thermal cutoff')
            time.sleep(2)
        raw = future.result()
    result = dict(file=old['file'], model='muse-glimmer-native', elapsed_s=time.monotonic()-started, raw=raw, telemetry=telemetry,
                  input_note='Same prompt and images, three ordered user messages for Ollama instead of LMS interleaved blocks.',
                  **quality.assess(raw['message']['content']))
    (root / 'muse.json').write_text(json.dumps(result, indent=2))
    html = '<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Muse vision test</title><style>body{font:16px system-ui;max-width:1100px;margin:2rem auto;padding:1rem}img{max-width:100%;max-height:65vh}pre{white-space:pre-wrap;overflow-wrap:anywhere}</style><h1>Muse vision test — reasoning off</h1>'
    html += '<p>'+escape(old['file'])+'</p><p>'+str(round(result['elapsed_s'], 1))+' seconds</p>'
    for title, i in [('Context', 0), ('Reference key', 2), ('ROI', 1)]:
        html += '<h2>'+title+'</h2><img src="'+images[i]+'">'
    html += '<h2>Response</h2><pre>'+escape(raw['message']['content'])+'</pre><h2>Audit</h2><pre>'+escape(json.dumps({k:v for k,v in result.items() if k not in ('raw','telemetry')}, indent=2))+'</pre></html>'
    (root / 'muse.html').write_text(html)
    print(json.dumps({k:v for k,v in result.items() if k != 'telemetry'}, indent=2), flush=True)

if __name__ == '__main__':
    main()
