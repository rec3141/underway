"""Compare logbook transcription models on the same photos.

    python tools/cruisereport_bench.py OUTDIR photo1.jpg,photo2.jpg MODEL ['MODEL|{"max_tokens":2000}' ...]

Each config is an OpenRouter model id, optionally with a reasoning setting as
JSON after "|". Every answer is saved to OUTDIR for reading against the photo;
a line per run gives time, tokens and cost. Needs OPENROUTER_REPORT_KEY.
"""
import json, sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from cruisereport import digitize  # noqa: E402
out = Path(sys.argv[1]); out.mkdir(parents=True, exist_ok=True)
images = [Path(p) for p in sys.argv[2].split(",")]
configs = sys.argv[3:]
def run(job):
    img, cfg = job
    model, _, rj = cfg.partition("|")
    tag = f"{img.stem}__{model.replace('/', '_')}{'__' + rj.replace(' ', '') if rj else ''}"
    try:
        d = digitize.transcribe(digitize.prepare(img.read_bytes()), model, reasoning=json.loads(rj) if rj else None)
    except Exception as e:
        return tag, {"error": str(e)}
    (out / (tag[:150] + ".json")).write_text(json.dumps(d, indent=1, ensure_ascii=False))
    return tag, d
jobs = [(i, c) for i in images for c in configs]
with ThreadPoolExecutor(len(jobs)) as ex:
    for tag, d in ex.map(run, jobs):
        if "error" in d:
            print(f"{tag[:90]:90s} ERROR {d['error'][:150]}", flush=True); continue
        u = d["usage"]; cells = [c for t in d["tables"] for r in t["rows"] for c in r]
        print(f"{tag[:90]:90s} {d['seconds']:6.1f}s out {u.get('completion_tokens')} (reason {(u.get('completion_tokens_details') or {}).get('reasoning_tokens')}) ${u.get('cost', 0):.4f} rows {sum(len(t['rows']) for t in d['tables'])} cells {len(cells)}", flush=True)
