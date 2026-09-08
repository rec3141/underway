"""Prepare full projection-library queue, preserving the identical 20-photo pilot."""
import json
import shutil
from pathlib import Path

root=Path('/home/cryomics/Downloads')
out=root/'amundsen-ice-gemma-leg4-size-evidence'
source=root/'amundsen-ice-gemma-size-evidence'
archive=Path('/media/cryomics/T7 Shield/Amundsen/Camera_360/2025_LEG_04')
embedding=json.loads((root/'amundsen-ice-full-leg-600x300/embedding.json').read_text())
queue=[dict(id=r['id'],file=r['file'],queue_source='full-leg4-projection') for r in embedding]
assert len(queue)==len({r['file'] for r in queue})
missing=[r['file'] for r in queue if not (archive/r['file']).is_file()]
if missing:raise RuntimeError(f'Missing {len(missing)} original photos: {missing[:3]}')
out.mkdir(exist_ok=True)
selection=out/'selection.json'
value={'queue':queue,'description':'All camera-3 photos in the 600x300 Leg 4 projection library; no human-label filtering'}
if selection.exists():assert json.loads(selection.read_text())==value
else:selection.write_text(json.dumps(value,indent=2))
if not (out/'results.json').exists():
    rows=json.loads((source/'results.json').read_text())
    assert all(r.get('prompt_variant')=='size-evidence' and r['scale_assumptions']['project_piece_cutoff_m']==40 for r in rows)
    assert {r['file'] for r in rows}<={r['file'] for r in queue}
    (out/'images').mkdir(exist_ok=True)
    for row in rows:
        for image in row['images']:shutil.copy2(source/image,out/image)
    (out/'results.json').write_text(json.dumps(rows,indent=2))
    shutil.copy2(root/'gemma.html',root/'gemma-size-trial.html')
print(f'{len(queue)} photos queued; {len(json.loads((out/"results.json").read_text()))} pilot results reused')
