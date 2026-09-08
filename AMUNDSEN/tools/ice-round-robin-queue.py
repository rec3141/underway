"""Reorder a paused full-leg queue using saved exact-k Ward cluster IDs."""
from collections import defaultdict, deque
from datetime import datetime, timezone
import json
from pathlib import Path

def reorder(queue, cluster_by_id, done):
    completed=[];groups=defaultdict(deque)
    for row in queue:
        item=dict(row,cluster=int(cluster_by_id[row['id']]))
        if row['file'] in done:completed.append(item)
        else:groups[item['cluster']].append(item)
    pending=[]
    while any(groups.values()):
        for cluster in sorted(groups):
            if groups[cluster]:pending.append(groups[cluster].popleft())
    return completed+pending

if __name__=='__main__':
    root=Path('/home/cryomics/Downloads')
    out=root/'amundsen-ice-gemma-leg4-size-evidence'
    path=out/'selection.json';original=path.read_text();data=json.loads(original)
    fixed=json.loads((root/'amundsen-ice-full-leg-600x300/ward-k64.json').read_text())
    assert fixed['k']==64 and len(set(fixed['labels']))==64
    assert len(fixed['ids'])==len(fixed['labels'])==len(set(fixed['ids']))
    mapping=dict(zip(fixed['ids'],fixed['labels']))
    rows=json.loads((out/'results.json').read_text())
    done={r['file'] for r in rows if r.get('finish_reason')=='stop'}
    queue=reorder(data['queue'],mapping,done)
    assert len(queue)==len({r['file'] for r in queue})==len(data['queue'])
    assert {r['file'] for r in queue}=={r['file'] for r in data['queue']}
    pending=[r for r in queue if r['file'] not in done]
    active={mapping[r['id']] for r in pending}
    assert len({r['cluster'] for r in pending[:len(active)]})==len(active)
    stamp=datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%S%fZ')
    (out/f'selection-before-round-robin-{stamp}.json').write_text(original)
    data.update(queue=queue,order='Completed first, then round-robin through saved t-SNE Ward k=64 clusters; preserve previous order within each cluster',reordered_utc=stamp)
    temp=path.with_suffix('.tmp');temp.write_text(json.dumps(data,indent=2));temp.replace(path)
    print(json.dumps(dict(total=len(queue),completed=len(done),remaining=len(pending),active_clusters=len(active),first_cycle=[r['cluster'] for r in pending[:len(active)]])))
