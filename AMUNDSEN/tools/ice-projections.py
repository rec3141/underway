"""Additional full-data projections and exact linkage trees of their 2-D coordinates."""
import argparse
import importlib.util
import json
from pathlib import Path
import sqlite3
import subprocess
import signal
import sys
import time
import numpy as np


def load(name,file):
    spec=importlib.util.spec_from_file_location(name,Path(__file__).with_name(file))
    mod=importlib.util.module_from_spec(spec);spec.loader.exec_module(mod);return mod


def atomic(path,value):
    tmp=path.with_suffix('.tmp');tmp.write_text(json.dumps(value));tmp.replace(path)


def worker(a):
    from scipy.cluster.hierarchy import linkage
    from sklearn.preprocessing import StandardScaler
    from threadpoolctl import threadpool_limits
    threadpool_limits(limits=1)
    points=json.loads((a.output/'embedding.json').read_text())
    conn=sqlite3.connect(f'file:{a.output / "features.sqlite"}?mode=ro',uri=True)
    features={file:json.loads(v) for file,v in conn.execute('SELECT file,vector FROM features WHERE error IS NULL')};conn.close()
    x=np.array([features[p['file']] for p in points],dtype=np.float32)
    for p,v in zip(points,x):p.update(mean_rgb=[float(v[i]) for i in (0,7,14)],brightness=float(v[21]))
    atomic(a.output/'embedding.json',points)
    path=a.output/'projections.json'
    state=json.loads(path.read_text()) if path.exists() else dict(ids=[p['id'] for p in points],projections={})
    if state['ids']!=[p['id'] for p in points]:raise ValueError('Stale projection identities')
    def publish():
        atomic(path,state)
        rows=json.loads(a.annotations.read_text()) if a.annotations.exists() else []
        load('explorer','ice-region-explorer.py').render(a.output,rows)
    publish()
    x=StandardScaler().fit_transform(x)
    for name in a.methods.split(','):
        try:
            if name not in state['projections']:
                print('Projection',name,'all',len(x),'rows',flush=True)
                if name=='tsne':coords=np.array([[p['x'],p['y']] for p in points])
                elif name=='umap':
                    from umap import UMAP
                    coords=UMAP(n_components=2,n_neighbors=30,min_dist=.1,random_state=42,n_jobs=1).fit_transform(x)
                elif name=='dbmap':
                    import dbmap
                    from umap import UMAP
                    np.random.seed(42)
                    diff=dbmap.diffusion.Diffusor(n_components=30,n_neighbors=30,n_jobs=1,
                        ann=False,knn_dist='euclidean',kernel_use='decay',eigengap=False)
                    diff.fit_transform(x)
                    eigenvalues=np.asarray(diff.res['EigenValues'])
                    # Exclude the stationary component before multiscale weighting.
                    keep=(eigenvalues>0)&(eigenvalues<1-1e-6)
                    basis=np.asarray(diff.res['EigenVectors'])[:,keep]*(eigenvalues[keep]/(1-eigenvalues[keep]))
                    if basis.shape[1]<2 or not np.isfinite(basis).all():raise ValueError('Invalid diffusion basis')
                    coords=UMAP(n_components=2,n_neighbors=30,min_dist=.1,random_state=42,n_jobs=1).fit_transform(basis)
                else:raise ValueError('Unknown projection')
                if coords.shape!=(len(x),2) or not np.isfinite(coords).all():raise ValueError('Invalid projection')
                state['projections'][name]=dict(coords=coords.tolist(),trees={},
                    method='dbMAP Diffusor + modern UMAP layout; compatibility variant, stationary component excluded' if name=='dbmap' else name);publish()
            projection=state['projections'][name]
            for method in ('ward','single'):
                if method not in projection['trees']:
                    print('Tree',name,method,flush=True)
                    projection['trees'][method]=linkage(np.asarray(projection['coords']),method=method,metric='euclidean').tolist();publish()
        except Exception as e:
            print(name,'FAILED',repr(e),flush=True)
            errors=state.setdefault('errors',{});errors[name]=repr(e);publish()
    print('Projection work finished',flush=True)


def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--output',type=Path,required=True)
    p.add_argument('--annotations',type=Path,required=True);p.add_argument('--methods',default='tsne,umap')
    p.add_argument('--worker',action='store_true');a=p.parse_args()
    if a.worker:return worker(a)
    monitor=load('monitor','ice-monitored-review.py');child=None;paused=False
    try:
        while True:
            cpu,gpu,*_=monitor.temperatures()
            if child is None:
                if cpu>=80 or gpu>=78:time.sleep(5);continue
                child=subprocess.Popen([sys.executable,'-u',__file__,*sys.argv[1:],'--worker'])
            if child.poll() is not None:return child.returncode
            if not paused and (cpu>=88 or gpu>=80):
                child.send_signal(signal.SIGSTOP);paused=True;print('Cooling pause',cpu,gpu,flush=True)
            elif paused and cpu<=75 and gpu<=68:
                child.send_signal(signal.SIGCONT);paused=False;print('Resuming',cpu,gpu,flush=True)
            time.sleep(5)
    finally:
        if child is not None and child.poll() is None:
            if paused:child.send_signal(signal.SIGCONT)
            child.terminate()
            try:child.wait(timeout=10)
            except subprocess.TimeoutExpired:child.kill();child.wait()


if __name__=='__main__':sys.exit(main())
