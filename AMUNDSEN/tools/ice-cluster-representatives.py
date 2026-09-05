"""Cut the t-SNE Ward tree at exact k and select up to three distinct photos each."""
import argparse
import json
from pathlib import Path
import numpy as np
from scipy.cluster.hierarchy import cut_tree


def representatives(coords,labels):
    chosen=[]
    for cluster in sorted(set(labels)):
        members=np.flatnonzero(labels==cluster);xy=coords[members]
        picks=[int(members[np.argmin(np.linalg.norm(xy-xy.mean(axis=0),axis=1))])]
        while len(picks)<min(3,len(members)):
            distance=np.linalg.norm(xy[:,None,:]-coords[picks][None,:,:],axis=2).min(axis=1)
            distance[np.isin(members,picks)]=-1
            picks.append(int(members[np.argmax(distance)]))
        chosen.extend((int(cluster),int(i),len(members),'central' if j==0 else 'contrasting') for j,i in enumerate(picks))
    return chosen


def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--portal',type=Path,required=True)
    p.add_argument('--output',type=Path,required=True);p.add_argument('--k',type=int,default=64);a=p.parse_args()
    points=json.loads((a.portal/'embedding.json').read_text());state=json.loads((a.portal/'projections.json').read_text())
    if state['ids']!=[p['id'] for p in points] or not 1<=a.k<=len(points):raise ValueError('Invalid tree identities or k')
    tree=np.array(state['projections']['tsne']['trees']['ward']);coords=np.array(state['projections']['tsne']['coords'])
    labels=cut_tree(tree,n_clusters=[a.k]).ravel()
    if len(set(labels))!=a.k:raise ValueError('Cut did not produce exact k')
    queue=[dict(cluster=c,id=points[i]['id'],file=points[i]['file'],cluster_size=n,reason=reason) for c,i,n,reason in representatives(coords,labels)]
    a.output.parent.mkdir(parents=True,exist_ok=True)
    a.output.write_text(json.dumps(dict(projection='tsne',linkage='ward',k=a.k,
        feature_size=json.loads((a.portal/'layout-state.json').read_text()).get('feature_size'),
        note='Distinct representatives, no labels propagated. Small clusters contribute all members.',queue=queue),indent=2))
    (a.portal/'ward-k64.json').write_text(json.dumps(dict(k=a.k,ids=state['ids'],labels=labels.tolist())))
    import importlib.util
    spec=importlib.util.spec_from_file_location('explorer',Path(__file__).with_name('ice-region-explorer.py'))
    mod=importlib.util.module_from_spec(spec);spec.loader.exec_module(mod);mod.render(a.portal,[])
    print(f'{a.k} clusters; {len(queue)} distinct representatives',flush=True)


if __name__=='__main__':main()
