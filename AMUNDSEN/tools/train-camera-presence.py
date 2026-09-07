"""Train against human label presence; never interpret presence as coverage."""
import hashlib
import json
from pathlib import Path
import sqlite3
import sys
import time
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
import numpy as np
from sklearn.ensemble import ExtraTreesRegressor
from sklearn.model_selection import GroupKFold
from sklearn.metrics import f1_score
from dashboard.camera_model import infer, ICE

root=Path('/home/cryomics/Downloads/amundsen-ice-full-leg-600x300')
labels_path=Path('/home/cryomics/Downloads/amundsen-ice-qwen-k32-classes/labels-clean.json')
out=Path('/home/cryomics/Downloads/amundsen-ice-human-classifier');out.mkdir(exist_ok=True)
labels=json.loads(labels_path.read_text());by_file={r['file']:r['labels'] for r in labels['labels'] if r['labels']}
names=['whitecap','small waves','smooth water',*ICE,'blurry','fog','reflection','night','any ice']
with sqlite3.connect(root/'features.sqlite') as db:
    rows=[(f,json.loads(v)) for f,v in db.execute('SELECT file,vector FROM features WHERE error IS NULL AND vector IS NOT NULL ORDER BY file') if f in by_file]
x=np.asarray([v for _,v in rows]);groups=np.asarray([f[:8] for f,_ in rows])
y=np.asarray([[float(n in by_file[f]) for n in names[:-1]]+[float(bool(set(by_file[f])&set(ICE)))] for f,_ in rows])
def forest():return ExtraTreesRegressor(n_estimators=48,max_depth=10,min_samples_leaf=5,random_state=42,n_jobs=1)
held=np.empty_like(y)
for fold,(train,test) in enumerate(GroupKFold(n_splits=5).split(x,y,groups),1):
    held[test]=forest().fit(x[train],y[train]).predict(x[test]);print('Held-out fold',fold,flush=True)
fitted=forest().fit(x,y);trees=[]
for estimator in fitted.estimators_:
    t=estimator.tree_
    trees.append(dict(left=t.children_left.tolist(),right=t.children_right.tolist(),feature=t.feature.tolist(),
                      threshold=t.threshold.tolist(),value=t.value[:,:,0].tolist()))
model=dict(id=hashlib.sha256(labels_path.read_bytes()+b'presence-600-v1').hexdigest()[:16],kind='human-label-presence',
           feature_size=[600,300],geometry='original-roi-v1',outputs=names,trees=trees,
           mean=x.mean(0).tolist(),scale=np.maximum(x.std(0),.01).tolist())
start=time.perf_counter();predicted=np.array([list(infer(v,model)['scores'].values()) for v in x[:100]])
elapsed=time.perf_counter()-start
np.testing.assert_allclose(predicted,fitted.predict(x[:100]),atol=1e-10)
report=dict(rows=len(x),days=len(set(groups)),labels_sha256=hashlib.sha256(labels_path.read_bytes()).hexdigest(),
            per_label_f1=dict(zip(names,f1_score(y,held>=.5,average=None,zero_division=0).tolist())),
            macro_f1=float(f1_score(y,held>=.5,average='macro',zero_division=0)),
            any_ice_accuracy=float(np.mean((held[:,-1]>=.5)==y[:,-1])),inference_ms=elapsed/100*1000,
            note='Five-fold whole-day holdout against human presence labels. Batch labels are correlated; not independent scientific validation. Presence does not establish coverage.')
(out/'model.json').write_text(json.dumps(model,separators=(',',':')))
(out/'evaluation.json').write_text(json.dumps(report,indent=2));print(json.dumps(report,indent=2),flush=True)
