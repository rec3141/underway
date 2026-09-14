"""Conservative, reversible seawater triage. Failures always route to Gemma."""
import hashlib
import json
from pathlib import Path
import numpy as np
from .camera_features import features
from .camera_model import infer

def image_guards(crop):
    gray=np.asarray(crop.convert('L'),dtype=float)/255
    if not .10<float(gray.mean())<.65:return ['brightness outside reviewed range']
    if float(gray.std())<.018:return ['smooth or low-contrast surface']
    # Tiny bright objects may be ice or foam: either deserves a model look.
    if int((gray>max(.7,float(np.median(gray))+.35)).sum())>=3:
        return ['bright fragments, foam, or glare']
    edges=[]
    for strip in np.array_split(gray,4):
        for tile in np.array_split(strip,4,axis=1):
            edges.append(float((np.abs(np.diff(tile,axis=0)).mean()+np.abs(np.diff(tile,axis=1)).mean())/2))
    if min(edges)<.25*np.median(edges):return ['local blur or smooth patch']
    return []

def audit_sample(file,model_id):
    return int(hashlib.sha256((model_id+file).encode()).hexdigest()[:8],16)%10==0

class Filter:
    def __init__(self,config):
        self.config=Path(config);self.cached=None;self.stamp=None
    def decide(self,file,crop):
        try:
            stamp=self.config.stat().st_mtime_ns
            if stamp!=self.stamp:
                cfg=json.loads(self.config.read_text());model=json.loads(Path(cfg['model']).read_text())
                self.cached=(cfg,model);self.stamp=stamp
            cfg,model=self.cached
            if not cfg.get('enabled') or Path(cfg['disabled_flag']).exists():return dict(route='gemma',reason='filter disabled')
            guard=image_guards(crop)
            if guard:return dict(route='gemma',reason=guard[0])
            prediction=infer(features(crop,size=(600,300)),model)
            score=prediction['scores']['clear_seawater_score']
            if prediction['outside_training_features'] or score<cfg['threshold']:return dict(route='gemma',reason='uncertain or unfamiliar features',score=score)
            return dict(route='audit' if audit_sample(file,model['id']) else 'filter',score=score,model_id=model['id'],threshold=cfg['threshold'],reason='high-score seawater candidate; image guards passed')
        except Exception as error:return dict(route='gemma',reason='filter unavailable: '+str(error))

def audit_contradiction(response):
    d=json.loads(response.strip().removeprefix('```json').removesuffix('```').strip())
    s=d['surface_percentages']
    ice=sum(s.get(k,0) for k in ['grease ice','nilas','thin ice floe','thick ice floe','icy bits','brash ice'])
    # The full taxonomy prompt often reserves 15% unknown for distinguishing
    # ripples from calm water even while explicitly reporting clear, ice-free water.
    # Permit that small budget ambiguity only for clear/high-confidence audits.
    tolerated_unknown=20 if d.get('visibility')=='clear' and d.get('confidence')=='high' else 9
    return ice>0 or s.get('unknown',100)>tolerated_unknown or max(d['artifact_percentages'].values(),default=100)>20
