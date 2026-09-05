"""Distil explicit, confident region estimates; hold out entire days."""
import argparse
import base64
import hashlib
import io
import json
from pathlib import Path
import re
import sys
import time

sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
import numpy as np
from PIL import Image
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import GroupKFold
from dashboard.ice_classifier import VERSION, TYPES, features, predict, quality


def structured_target(row):
    if not row['id'].startswith('region-') or row.get('finish_reason')!='stop': return None
    try:
        text=re.sub(r'^```(?:json)?\s*|\s*```$','',row['response'].strip())
        label=json.loads(text)
        if label['visibility']!='clear' or label['confidence'] not in ('high','moderate'): return None
        values=[label['percentages'][k] for k in TYPES]
        if any(type(v) not in (int,float) or not np.isfinite(v) or not 0<=v<=100 for v in values): return None
        if abs(sum(values)-100)>.1 or values[-1]>20: return None
        return values
    except (ValueError,KeyError,TypeError): return None


def target(row):
    if not row['id'].startswith('region-') or row.get('finish_reason')!='stop': return None
    text=row['response'].replace('*','').replace('–','-').replace('—','-')
    # Accept only explicit total-ice lines, never infer totals from prose/types.
    confidence=re.search(r'Confidence\s*:\s*([^\n.]+)',text,re.I)
    if not confidence or re.search(r'low|uncertain',confidence[1],re.I): return None
    match=re.search(r'Total Ice(?: Percentage)?\s*:\s*[~≈]?\s*([<>]?)\s*(\d+(?:\.\d+)?)\s*(?:%?\s*-\s*(\d+(?:\.\d+)?))?\s*%',text,re.I)
    if not match: return None
    low=float(match[2]); high=float(match[3] or match[2])
    if match[1]=='<': low=0
    if match[1]=='>': high=100
    if not 0<=low<=high<=100 or high-low>25: return None
    return (low+high)/2


def export(forest,x,identity):
    trees=[]
    for estimator in forest.estimators_:
        t=estimator.tree_
        trees.append(dict(left=t.children_left.tolist(),right=t.children_right.tolist(),
            feature=t.feature.tolist(),threshold=t.threshold.tolist(),
            value=(t.value[:,:,0] if forest.n_outputs_>1 else t.value[:,0,0]).tolist()))
    return dict(id=identity,features=VERSION,trees=trees,mean=x.mean(0).tolist(),
        scale=np.maximum(x.std(0),.01).tolist(),
        note='Qwen weak-label distillation on 2025 Leg 4; not human-validated; camera 3 only')


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--results',type=Path,required=True)
    p.add_argument('--output',type=Path,required=True)
    p.add_argument('--structured',action='store_true')
    a=p.parse_args();a.output.mkdir(parents=True,exist_ok=True)
    rows=json.loads(a.results.read_text());x=[];y=[];groups=[];accepted=[];skipped=[];crops=[]
    for r in rows:
        value=structured_target(r) if a.structured else target(r)
        # Eric identified poor visibility here; no asserted cause or ice truth.
        reason='human flagged poor visibility' if r['id']=='region-scene-34' else None
        if value is None or reason:
            skipped.append(dict(id=r['id'],reason=reason or 'not an explicit confident region target'));continue
        with Image.open(io.BytesIO(base64.b64decode(r['images'][1].split(',',1)[1]))) as im:
            crop=im.convert('RGB')
        crops.append(crop);x.append(features(crop));y.append(value);groups.append(r['file'].split('/')[0])
        accepted.append(dict(id=r['id'],file=r['file'],teacher_percent=value,quality=quality(crop)))
    x=np.asarray(x);y=np.asarray(y,dtype=float);groups=np.asarray(groups)
    if len(set(groups))<5 or len(y)<15: raise ValueError('Insufficient independent days/labels')
    def forest(): return RandomForestRegressor(n_estimators=48,max_depth=5,min_samples_leaf=2,random_state=42,n_jobs=1)
    held=np.empty_like(y)
    for train,test in GroupKFold(n_splits=5).split(x,y,groups):
        model=forest().fit(x[train],y[train]);held[test]=model.predict(x[test])
    model=forest().fit(x,y)
    identity=hashlib.sha256(a.results.read_bytes()+VERSION.encode()+str(a.structured).encode()).hexdigest()[:16]
    exported=export(model,x,identity)
    if a.structured: exported['outputs']=list(TYPES)
    # Verify runtime agrees with sklearn before writing its portable model.
    runtime=np.array([predict(c,exported)['candidate_percent'] for c in crops])
    expected=model.predict(x)
    if a.structured: expected=expected[:,1:4].sum(axis=1)
    if not np.allclose(runtime,expected,atol=.006): raise AssertionError('Export mismatch')
    times=[]
    for crop in crops:
        start=time.perf_counter();predict(crop,exported);times.append(time.perf_counter()-start)
    for r,pred in zip(accepted,held):r['heldout_percent']=pred.tolist()
    total_y=y[:,1:4].sum(axis=1) if a.structured else y
    total_held=held[:,1:4].sum(axis=1) if a.structured else held
    report=dict(training_rows=len(y),days=len(set(groups)),
        heldout_teacher_mae_pp=float(np.mean(np.abs(total_held-total_y))),
        per_type_mae_pp=dict(zip(TYPES,np.mean(np.abs(held-y),axis=0).tolist())) if a.structured else None,
        heldout_teacher_class_accuracy=float(np.mean(np.digitize(total_held,[20,80])==np.digitize(total_y,[20,80]))),
        inference_median_ms=float(np.median(times)*1000),inference_max_ms=float(max(times)*1000),
        note='Five-fold whole-day holdout measures agreement with Qwen, NOT correctness. Timing excludes source JPEG decoding.',
        accepted=accepted,skipped=skipped)
    (a.output/'model.json').write_text(json.dumps(exported))
    (a.output/'evaluation.json').write_text(json.dumps(report,indent=2))
    print(json.dumps({k:v for k,v in report.items() if k not in ('accepted','skipped')},indent=2))


if __name__=='__main__': main()
