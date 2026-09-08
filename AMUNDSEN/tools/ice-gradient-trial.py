"""Select a reproducible, human-label-stratified 20-photo size-gradient trial."""
import json
from pathlib import Path

ROOT=Path('/home/cryomics/Downloads')
OUT=ROOT/'amundsen-ice-gemma-gradient-40m'

def select(queue, humans):
    classes={'icy bits','brash ice','ice floe','thin fyi'}
    groups=[('icy bits',5,lambda s:s=={'icy bits'}),
            ('bits / brash transition',3,lambda s:s=={'icy bits','brash ice'}),
            ('brash',4,lambda s:s=={'brash ice'}),
            ('brash / sheet transition',2,lambda s:s=={'icy bits','brash ice','thin fyi'}),
            ('thin floes',2,lambda s:s=={'thin fyi'}),
            ('thick floes',4,lambda s:s=={'ice floe'})]
    selected=[]
    for name,count,match in groups:
        candidates=[r for r in queue if match(set(humans.get(r['file'],[])) & classes)
                    and not set(humans.get(r['file'],[])) & {'night','fog','blurry'}]
        days={}
        for _ in range(count):
            if not candidates:raise ValueError('Insufficient examples: '+name)
            row=min(candidates,key=lambda r:(days.get(r['file'][:8],0),r.get('reason')!='central',r['file']))
            candidates.remove(row);day=row['file'][:8];days[day]=days.get(day,0)+1
            selected.append(dict(row,gradient_group=name))
    assert len(selected)==len({r['file'] for r in selected})==20
    return selected

if __name__=='__main__':
    humans={r['file']:r['labels'] for r in json.loads((ROOT/'amundsen-ice-qwen-k32-classes/labels-clean.json').read_text())['labels']}
    queue=json.loads((ROOT/'amundsen-ice-gemma-overnight/queue.json').read_text())['queue']
    selected=select(queue,humans)
    OUT.mkdir(exist_ok=True)
    destination=OUT/'selection.json'
    value={'selection':'Human-label strata, excluding night/fog/blur; prefer distinct days then central representatives. Labels are not verified physical sizes and are withheld from model.',
           'project_piece_cutoff_m':40,'queue':selected}
    if destination.exists() and json.loads(destination.read_text())!=value:
        raise RuntimeError('Refusing to replace a different saved selection')
    destination.write_text(json.dumps(value,indent=2))
    for row in selected:print(row['gradient_group'],row['file'])
