"""Incremental camera-3 features; fixed 2025 PCA axes, atomic publication.

Runs independently of the underway build. A missing share or partial JPEG never
replaces the last good product. No model calls and no GPU are required.
"""
import argparse
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import re
import sqlite3
import time

import numpy as np
from PIL import Image
from .camera_features import crop_region, features, quality
from .camera_model import infer

def atomic(path, data):
    tmp=path.with_suffix(path.suffix+'.tmp')
    tmp.write_text(json.dumps(data,separators=(',',':'),allow_nan=False))
    tmp.replace(path)

def update(source, output, cache, pca_path, limit=60, presence_model=None, area_model=None):
    if not source.is_dir():raise ValueError('Camera share unavailable')
    pca=json.loads(pca_path.read_text())
    mean,scale,axes=map(np.asarray,(pca['mean'],pca['scale'],pca['axes']))
    assert mean.shape==scale.shape==(89,) and axes.shape==(4,89)
    identity=hashlib.sha256(pca_path.read_bytes()).hexdigest()
    cache.parent.mkdir(parents=True,exist_ok=True)
    started=time.monotonic();processed=0;errors=[]
    with sqlite3.connect(cache) as db:
        db.execute('CREATE TABLE IF NOT EXISTS photos (leg TEXT,file TEXT,stamp TEXT,vector TEXT,photo TEXT,PRIMARY KEY(leg,file))')
        legs=sorted(source.glob('2026_LEG_*'),reverse=True)
        for leg in legs:
            if not leg.is_dir():continue
            target=output.parent.parent/'photos'/leg.name
            target.parent.mkdir(parents=True,exist_ok=True)
            if not target.exists() and not target.is_symlink():target.symlink_to(leg,target_is_directory=True)
            for day in sorted(leg.glob('2026????'),reverse=True):
                if not day.is_dir():continue
                for path in sorted(day.glob('*/Camera360_*_cam_3.jpg'),reverse=True):
                    match=re.fullmatch(r'Camera360_(\d{14})_cam_3\.jpg',path.name)
                    if not match:continue
                    file=path.relative_to(leg).as_posix()
                    try:
                        stat=path.stat()
                        if time.time()-stat.st_mtime<30:continue
                        stamp=f'{stat.st_size}:{stat.st_mtime_ns}:{identity}:600x300:original-roi-v1'
                        old=db.execute('SELECT stamp FROM photos WHERE leg=? AND file=?',(leg.name,file)).fetchone()
                        if old and old[0]==stamp:continue
                        if processed>=limit or time.monotonic()-started>90:break
                        processed+=1
                        with Image.open(path) as im:
                            crop=crop_region(im);vector=features(crop,size=(600,300));q=quality(crop)
                        after=path.stat()
                        if (stat.st_size,stat.st_mtime_ns)!=(after.st_size,after.st_mtime_ns):continue
                        pcs=((vector-mean)/scale)@axes.T
                        photo=dict(file=file,leg=leg.name,time=datetime.strptime(match[1],'%Y%m%d%H%M%S').replace(tzinfo=timezone.utc).isoformat(),
                                   rgb=[round(float(vector[i])*255) for i in (0,7,14)],pcs=pcs.tolist(),pc1=float(pcs[0]),quality=q)
                        db.execute('INSERT OR REPLACE INTO photos VALUES (?,?,?,?,?)',(leg.name,file,stamp,json.dumps(vector.tolist()),json.dumps(photo)))
                    except (OSError,ValueError) as e:errors.append(dict(file=str(path),error=str(e)))
                if processed>=limit or time.monotonic()-started>90:break
            if processed>=limit or time.monotonic()-started>90:break
        db.commit()
        models={name:json.loads(path.read_text()) for name,path in [('human',presence_model),('area',area_model)] if path and path.is_file()}
        current=[]
        for raw,vector in db.execute('SELECT photo,vector FROM photos ORDER BY leg,file'):
            photo=json.loads(raw);x=json.loads(vector)
            for name,model in models.items():
                prediction=infer(x,model)
                reasons=[]
                if prediction['outside_training_features']:reasons.append('outside_training_features')
                if 'too_dark' in photo['quality']['flags']:reasons.append('too_dark')
                if name=='human':
                    if any(prediction['scores'].get(k,0)>.5 for k in ('blurry','fog','night')):reasons.append('possible_visibility_problem')
                    if .35<prediction['scores']['any ice']<.65:reasons.append('ambiguous_ice_presence')
                else:
                    if prediction['scores'].get('unknown',0)>20:reasons.append('substantial_unknown_area')
                    if prediction['tree_spread']>25:reasons.append('model_disagreement')
                prediction['review_reasons']=reasons
                photo[name+'_ice']=prediction
            current.append(photo)
    existing=json.loads(output.read_text())
    old_photos=[p for p in existing['photos'] if not p['leg'].startswith('2026_')]
    existing.update(photos=old_photos+current,updated_utc=datetime.now(timezone.utc).isoformat(),
                    live_camera=dict(processed=processed,cached=len(current),errors=errors[:20],pca_sha256=identity,
                        feature_size=[600,300],geometry='original-roi-v1',note='Fixed 2025 PCA; not ice concentration. Archive gaps are not filled.'))
    atomic(output,existing)
    print(json.dumps(existing['live_camera']),flush=True)

if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--source',type=Path,required=True);p.add_argument('--output',type=Path,required=True)
    p.add_argument('--cache',type=Path,required=True);p.add_argument('--pca-path',type=Path,required=True)
    p.add_argument('--limit',type=int,default=60)
    p.add_argument('--presence-model',type=Path);p.add_argument('--area-model',type=Path)
    update(**vars(p.parse_args()))
