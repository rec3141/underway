"""Resumable full-leg region features and CPU t-SNE with thermal pauses."""
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import signal
import sqlite3
import subprocess
import sys
import time

sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
import numpy as np
from PIL import Image,ImageDraw,ImageOps
from dashboard.ice_classifier import VERSION,crop_region,features


def load(name,file):
    spec=importlib.util.spec_from_file_location(name,Path(__file__).with_name(file))
    mod=importlib.util.module_from_spec(spec);spec.loader.exec_module(mod);return mod


def write(path,obj):
    tmp=path.with_suffix(path.suffix+'.tmp');tmp.write_text(json.dumps(obj));tmp.replace(path)


def worker(a):
    from sklearn.manifold import TSNE
    from sklearn.preprocessing import StandardScaler
    from threadpoolctl import threadpool_limits
    threadpool_limits(limits=1)
    feature_size=tuple(getattr(a,'feature_size',(240,120)))
    a.output.mkdir(parents=True,exist_ok=True);(a.output/'previews').mkdir(exist_ok=True)
    db=sqlite3.connect(a.output/'features.sqlite')
    db.execute('CREATE TABLE IF NOT EXISTS features (file TEXT PRIMARY KEY, fingerprint TEXT, vector TEXT, point TEXT, error TEXT)')
    paths=sorted(a.source.glob('????????/*/Camera360_*_cam_3.jpg'))
    if not paths: raise ValueError('No camera-3 images found')
    print('Discovered',len(paths),'camera-3 images',flush=True)
    valid_files=set()
    for i,path in enumerate(paths):
        relative=str(path.relative_to(a.source));valid_files.add(relative)
        stat=path.stat();fingerprint=f'{VERSION}:{feature_size}:{path.resolve()}:{stat.st_size}:{stat.st_mtime_ns}'
        old=db.execute('SELECT fingerprint,error FROM features WHERE file=?',(relative,)).fetchone()
        if old and old[0]==fingerprint and not old[1]: continue
        identity=hashlib.sha256(relative.encode()).hexdigest()[:20]
        try:
            with Image.open(path) as im:
                im=im.convert('RGB');crop=crop_region(im);vector=features(crop,size=feature_size)
                _,polygon,_=load('rotation','ice-rotated-preview.py').geometry(im.size,angle=-30)
                ImageDraw.Draw(im).line(polygon+[polygon[0]],fill='orange',width=12)
                ImageOps.contain(im,(900,900)).save(a.output/'previews'/f'{identity}-source.jpg',quality=75)
                ImageOps.contain(crop,(480,240)).save(a.output/'previews'/f'{identity}-region.jpg',quality=80)
            point=dict(id=identity,file=relative,brightness=float(vector[21]),
                images=[f'previews/{identity}-source.jpg',f'previews/{identity}-region.jpg'])
            db.execute('INSERT OR REPLACE INTO features VALUES (?,?,?,?,?)',
                (relative,fingerprint,json.dumps(vector.tolist()),json.dumps(point),None))
        except (OSError,ValueError) as e:
            db.execute('INSERT OR REPLACE INTO features VALUES (?,?,?,?,?)',
                (relative,fingerprint,None,None,str(e)))
        db.commit()
        if i%100==0:
            write(a.output/'progress.json',dict(stage='features',processed=i+1,total=len(paths)))
            print('Features',i+1,'/',len(paths),flush=True)
    rows=[r for r in db.execute('SELECT file,vector,point,error FROM features ORDER BY file') if r[0] in valid_files]
    excluded=[dict(file=r[0],error=r[3]) for r in rows if r[3]]
    good=[r for r in rows if not r[3]]
    if len(good)<4: raise ValueError('Too few readable images')
    write(a.output/'excluded.json',excluded)
    x=np.array([json.loads(r[1]) for r in good],dtype=np.float32)
    points=[json.loads(r[2]) for r in good]
    signature=hashlib.sha256(x.tobytes()).hexdigest()
    state=a.output/'layout-state.json';layout=a.output/'embedding.json'
    previous=json.loads(state.read_text()) if state.exists() else {}
    if not layout.exists() or previous.get('signature')!=signature:
        write(a.output/'progress.json',dict(stage='tsne',included=len(good),excluded=len(excluded)))
        print('Fitting t-SNE on ALL',len(good),'regions',flush=True)
        coords=TSNE(n_components=2,perplexity=min(40,(len(good)-1)/3),
            random_state=42,init='pca',learning_rate='auto',method='barnes_hut',
            n_jobs=1,verbose=1).fit_transform(StandardScaler().fit_transform(x))
        for point,xy in zip(points,coords):point.update(x=float(xy[0]),y=float(xy[1]))
        write(layout,points);write(state,dict(signature=signature,seed=42,features=VERSION,feature_size=feature_size))
    responses=json.loads(a.annotations.read_text()) if a.annotations.exists() else []
    load('explorer','ice-region-explorer.py').render(a.output,responses)
    write(a.output/'progress.json',dict(stage='complete',included=len(good),excluded=len(excluded)))
    print(a.output/'regions.html',flush=True)


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--source',type=Path,required=True);p.add_argument('--output',type=Path,required=True)
    p.add_argument('--annotations',type=Path,required=True);p.add_argument('--worker',action='store_true')
    p.add_argument('--feature-size',type=int,nargs=2,default=[240,120],metavar=('WIDTH','HEIGHT'))
    a=p.parse_args()
    if min(a.feature_size)<=16 or max(a.feature_size)>1200:p.error('Feature dimensions must be 17..1200')
    if a.worker: return worker(a)
    a.output.mkdir(parents=True,exist_ok=True)
    monitor=load('monitor','ice-monitored-review.py')
    child=None;paused=False
    try:
        while True:
            cpu,gpu,*_=monitor.temperatures()
            if child is None:
                if cpu>=80 or gpu>=78:
                    time.sleep(5);continue
                child=subprocess.Popen([sys.executable,'-u',__file__,*sys.argv[1:],'--worker'])
            if child.poll() is not None: return child.returncode
            if not paused and (cpu>=88 or gpu>=80):
                child.send_signal(signal.SIGSTOP);paused=True
                print('Cooling pause',cpu,gpu,flush=True)
            elif paused and cpu<=75 and gpu<=68:
                child.send_signal(signal.SIGCONT);paused=False
                print('Resuming',cpu,gpu,flush=True)
            time.sleep(5)
    finally:
        if child is not None and child.poll() is None:
            if paused:child.send_signal(signal.SIGCONT)
            child.terminate()
            try:child.wait(timeout=10)
            except subprocess.TimeoutExpired:child.kill();child.wait()


if __name__=='__main__':sys.exit(main())
