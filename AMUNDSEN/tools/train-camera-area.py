"""Distil teacher area estimates with human contradiction checks and day holdouts."""
import argparse
import hashlib
import json
from pathlib import Path
import sqlite3
import sys
import time
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
import numpy as np
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import GroupKFold
from dashboard.camera_model import infer, ICE
from dashboard.camera_live import atomic

def parse(text):
    return json.loads(text.strip().removeprefix('```json').removesuffix('```').strip())

def target(row, human):
    if row.get('finish_reason')!='stop':raise ValueError('unfinished response')
    obj=parse(row['response']);surface=obj['surface_percentages']
    names=['whitecap','small waves','smooth water',*ICE,'unknown']
    y=np.array([surface[k] for k in names],dtype=float)
    if not np.isfinite(y).all() or (y<0).any() or (y>100).any() or abs(y.sum()-100)>.01:raise ValueError('invalid surface budget')
    if obj['visibility'] in ('unusable','uncertain') or obj['confidence']=='low':raise ValueError('teacher uncertain')
    if surface['unknown']>40:raise ValueError('too much unknown')
    if any(obj['artifact_percentages'][k]>40 for k in ('blurry','fog','night')):raise ValueError('visibility artifact')
    h=set(human);ice=sum(surface[k] for k in ICE)
    if h and h.issubset(set(ICE)) and ice<20:raise ValueError('contradicts human ice-only labels')
    if h and h.issubset({'whitecap','small waves','smooth water'}) and ice>60:raise ValueError('contradicts human water-only labels')
    return y

def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--results',type=Path,default=Path('/home/cryomics/Downloads/amundsen-ice-gemma-overnight/gemma4/results.json'))
    p.add_argument('--output',type=Path,default=Path('/home/cryomics/Downloads/amundsen-ice-area-classifier'))
    a=p.parse_args();a.output.mkdir(exist_ok=True)
    if not a.results.exists():print('Waiting for teacher results');return
    raw=a.results.read_bytes();rows=json.loads(raw)
    human_path=Path('/home/cryomics/Downloads/amundsen-ice-qwen-k32-classes/labels-clean.json')
    human={r['file']:r['labels'] for r in json.loads(human_path.read_text())['labels']}
    with sqlite3.connect('/home/cryomics/Downloads/amundsen-ice-full-leg-600x300/features.sqlite') as db:
        vectors={f:json.loads(v) for f,v in db.execute('SELECT file,vector FROM features WHERE error IS NULL AND vector IS NOT NULL')}
    accepted=[];skipped=[];x=[];y=[];groups=[];seen=set()
    for row in rows:
        file=row['file']
        if file in seen:continue
        seen.add(file)
        try:
            value=target(row,human.get(file,[]));vector=vectors[file]
            x.append(vector);y.append(value);groups.append(file[:8]);accepted.append(file)
        except (ValueError,KeyError,TypeError) as e:skipped.append(dict(file=file,reason=str(e)))
    report=dict(teacher_rows=len(rows),accepted=len(x),days=len(set(groups)),skipped=skipped,
                teacher_source=str(a.results),human_source=str(human_path),published=False,
                note='Whole-day holdout measures teacher agreement, not scientific truth. Human presence labels only veto strong contradictions; they are never converted into area fractions.')
    if len(x)<30 or len(set(groups))<5:
        report['status']='Waiting for 30 usable photos across at least five days'
        atomic(a.output/'evaluation.json',report);print(report['status'],len(x),flush=True);return
    x=np.asarray(x);y=np.asarray(y);groups=np.asarray(groups);held=np.empty_like(y);constant=np.empty_like(y)
    def forest():return RandomForestRegressor(n_estimators=48,max_depth=7,min_samples_leaf=2,random_state=42,n_jobs=1)
    for train,test in GroupKFold(n_splits=5).split(x,y,groups):
        held[test]=forest().fit(x[train],y[train]).predict(x[test]);constant[test]=y[train].mean(axis=0)
    total=lambda v:v[:,3:9].sum(axis=1)
    mae=float(np.abs(total(held)-total(y)).mean());base=float(np.abs(total(constant)-total(y)).mean())
    fitted=forest().fit(x,y);trees=[]
    for estimator in fitted.estimators_:
        t=estimator.tree_;trees.append(dict(left=t.children_left.tolist(),right=t.children_right.tolist(),feature=t.feature.tolist(),threshold=t.threshold.tolist(),value=t.value[:,:,0].tolist()))
    names=['whitecap','small waves','smooth water',*ICE,'unknown']
    model=dict(id=hashlib.sha256(raw+human_path.read_bytes()+b'area-600-v1').hexdigest()[:16],kind='teacher-area-regression',
               feature_size=[600,300],geometry='original-roi-v1',outputs=names,trees=trees,
               mean=x.mean(0).tolist(),scale=np.maximum(x.std(0),.01).tolist())
    start=time.perf_counter();runtime=np.array([list(infer(v,model)['scores'].values()) for v in x])
    elapsed=time.perf_counter()-start;np.testing.assert_allclose(runtime,fitted.predict(x),atol=1e-8)
    support={name:int((y[:,names.index(name)]>=10).sum()) for name in ICE}
    publish=len(x)>=50 and len(set(groups))>=8 and mae<20 and mae<base*.9 and min(support.values())>=3
    report.update(heldout_teacher_ice_mae_pp=mae,constant_baseline_mae_pp=base,per_type_mae_pp=dict(zip(names,np.abs(held-y).mean(axis=0).tolist())),
                  class_support=support,inference_ms=elapsed/len(x)*1000,model_id=model['id'],published=publish,
                  status='Published experimental area model' if publish else 'Candidate only: publication gates not all met',
                  publication_gates='50 usable rows, 8 days, >=3 examples at >=10% for every ice type, ice MAE <20pp and at least 10% better than constant baseline')
    atomic(a.output/'candidate-model.json',model);atomic(a.output/'evaluation.json',report)
    if publish:atomic(a.output/'model.json',model)
    print(json.dumps({k:v for k,v in report.items() if k!='skipped'},indent=2),flush=True)

if __name__=='__main__':main()
