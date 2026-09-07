"""Portable tree inference on the reviewed 600x300 feature contract."""
import numpy as np

ICE = ('grease ice','nilas','thin fyi','icy bits','brash ice','ice floe')

def infer(vector, model):
    if model['feature_size'] != [600,300] or model['geometry'] != 'original-roi-v1':
        raise ValueError('Unsupported camera model input contract')
    # sklearn's tree predictors round inputs to float32 before testing splits.
    # Round inputs to float32, then promote for comparison with float64 thresholds.
    # Comparing NumPy float32 scalars directly can also round the threshold.
    x=np.asarray(vector,dtype=np.float32).astype(np.float64)
    if x.shape!=(89,) or not np.isfinite(x).all():raise ValueError('Invalid camera features')
    values=[]
    for tree in model['trees']:
        node=0
        while tree['left'][node]!=-1:
            node=tree['left'][node] if x[tree['feature'][node]]<=tree['threshold'][node] else tree['right'][node]
        values.append(tree['value'][node])
    values=np.asarray(values);means=values.mean(axis=0)
    scores=dict(zip(model['outputs'],map(float,means)))
    distance=float(np.max(np.abs((x-np.asarray(model['mean']))/np.asarray(model['scale']))))
    result=dict(model_id=model['id'],kind=model['kind'],scores=scores,
                outside_training_features=distance>6,max_standardized_distance=distance)
    if model['kind']=='human-label-presence':
        result['note']='Label presence scores, NOT image-area fractions or calibrated probabilities.'
    elif model['kind']=='teacher-area-regression':
        result['ice_percent']=sum(scores[k] for k in ICE)
        result['note']='Exploratory teacher-distilled image-area estimates, not scientific concentration.'
        result['tree_spread']=float(values[:,[model['outputs'].index(k) for k in ICE]].sum(axis=1).std())
    else:raise ValueError('Unknown camera model kind')
    return result
