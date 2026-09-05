"""Run a blind local vision-model pilot; never download models or send images off-host."""
import argparse
import html
import json
from pathlib import Path
import time
import urllib.request

PROMPT = '''Inspect this ship-camera sea-surface crop. Describe visible texture, then estimate the percentage of visible sea surface covered by any ice, including dark thin/new ice, grease ice, rubble and frost flowers. Glare on open water is not ice. Do not assume dark means water or white means ice. If the image is insufficient, say unknown. Give a rough percentage or range, likely surface types, alternative explanations and what a human should verify. These are exploratory suggestions, not navigation or scientific measurements. Be concise (under 180 words).'''


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--input', type=Path, required=True, help='Texture explorer tiles.json')
    p.add_argument('--output', type=Path, required=True)
    p.add_argument('--model', default='ice-vision')
    p.add_argument('--limit', type=int, default=5)
    p.add_argument('--max-tokens', type=int, default=1200)
    p.add_argument('--no-thinking', action='store_true')
    p.add_argument('--render-only', action='store_true', help='Rebuild HTML from existing results without inference')
    args = p.parse_args()
    if not 1 <= args.limit <= 100:
        p.error('limit must be 1..100')
    if not 128 <= args.max_tokens <= 12000:
        p.error('max tokens must be 128..12000')
    scenes = json.loads(args.input.read_text())['scenes'][:args.limit]
    args.output.mkdir(parents=True,exist_ok=True)
    rows = json.loads((args.output/'results.json').read_text()) if args.render_only else []
    # Explicitly bypass shell proxy settings: requests are loopback only.
    client = urllib.request.build_opener(urllib.request.ProxyHandler({}))
    for scene in ([] if args.render_only else scenes):
        start = time.monotonic()
        prompt = PROMPT + ('\n/no_think' if args.no_thinking else '')
        payload = {'model':args.model,'temperature':0,'max_tokens':args.max_tokens,
                   'messages':[{'role':'user','content':[
                       {'type':'text','text':prompt},
                       {'type':'image_url','image_url':{'url':scene['image']}}]}]}
        if args.no_thinking:
            payload['chat_template_kwargs'] = {'enable_thinking':False}
        request = urllib.request.Request('http://127.0.0.1:1234/v1/chat/completions',
                                         data=json.dumps(payload).encode(),headers={'Content-Type':'application/json'})
        with client.open(request,timeout=600) as response:
            result = json.load(response)
        choice = result['choices'][0]
        row = {**scene,'model':args.model,'prompt':prompt,'response':choice['message']['content'] or '',
               'finish_reason':choice.get('finish_reason'),'elapsed_s':round(time.monotonic()-start,2),
               'usage':result.get('usage'),'max_tokens':args.max_tokens}
        rows.append(row)
        (args.output/'results.json').write_text(json.dumps(rows,indent=2))
        print(f"Scene {scene['scene']}: {row['elapsed_s']}s\n{row['response']}\n",flush=True)
    page = '<!doctype html><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>Local ice vision review</title><style>body{font:17px system-ui;max-width:1000px;margin:24px auto;padding:12px}img{max-width:100%}pre{white-space:pre-wrap}article{border-top:1px solid #aaa;padding:20px 0}</style><h1>Local vision pilot — unreviewed suggestions</h1><p>Qwen3-VL-8B, Q4_K_M, temperature 0. No expert percentages or scene labels supplied. Not trained or validated for sea ice. Crops are resized previews from the texture explorer. Model text may be wrong.</p>'
    page = page.replace('Qwen3-VL-8B, Q4_K_M', html.escape(args.model))
    for row in rows:
        status = ' — INCOMPLETE: output limit reached' if row['finish_reason']=='length' else ''
        page += f'<article><h2>Scene {row["scene"]}{status}</h2><p>{html.escape(row["file"])}</p><img src="{row["image"]}"><pre>{html.escape(row["response"])}</pre></article>'
    (args.output/'index.html').write_text(page)


if __name__ == '__main__':
    main()
