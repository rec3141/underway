"""CPU-only Qwen-distilled region proxy. Not a scientific concentration product."""
from __future__ import annotations

import hashlib
import json
import math
from pathlib import Path
import re
import time

import numpy as np
from PIL import Image

VERSION = 'region-colour-texture-v1'
NAME = 'Camera visible ice · experimental (%)'
TYPES = ('open_water','thin_new_ice','broken_ice','consolidated_ice','unknown')
TYPE_NAMES = {'thin_new_ice':'Camera thin/new ice · experimental (%)',
              'broken_ice':'Camera broken ice · experimental (%)',
              'consolidated_ice':'Camera consolidated ice · experimental (%)'}


def crop_region(image):
    # Same camera-3 footprint as the reviewed 2025 experiment. Refuse other
    # dimensions rather than silently moving the footprint onto ship/sky.
    if image.size != (3648, 2052):
        raise ValueError('Unreviewed camera dimensions; expected 3648x2052 camera 3')
    u, v = math.cos(math.radians(-30)), math.sin(math.radians(-30))
    ox = .45*image.width-600*u+300*v
    oy = .62*image.height-600*v-300*u
    return image.convert('RGB').transform((1200,600), Image.Transform.AFFINE,
        (u,-v,ox,v,u,oy), Image.Resampling.BICUBIC)


def features(crop):
    """Fixed low-resolution colour, contrast, edge and spatial features."""
    rgb = np.asarray(crop.convert('RGB').resize((240,120),Image.Resampling.BOX),dtype=float)/255
    gray = rgb @ np.array([.299,.587,.114])
    result = []
    for plane in (*rgb.transpose(2,0,1),gray,rgb.max(2)-rgb.min(2)):
        result.extend([plane.mean(),plane.std(),*np.quantile(plane,[.05,.25,.5,.75,.95])])
    for lag in (1,2,4,8,16):
        for axis in (0,1):
            diff = np.abs(np.diff(gray,axis=axis)) if lag==1 else np.abs(
                np.take(gray,range(lag,gray.shape[axis]),axis=axis)-
                np.take(gray,range(gray.shape[axis]-lag),axis=axis))
            result.extend([diff.mean(),diff.std(),np.quantile(diff,.9)])
    for block in np.array_split(gray,3,axis=0):
        for tile in np.array_split(block,4,axis=1):
            result.extend([tile.mean(),tile.std()])
    return np.asarray(result)


def quality(crop):
    gray = np.asarray(crop.convert('L').resize((240,120),Image.Resampling.BOX),dtype=float)/255
    contrast = float(np.quantile(gray,.95)-np.quantile(gray,.05))
    edge = float((np.abs(np.diff(gray,axis=0)).mean()+np.abs(np.diff(gray,axis=1)).mean())/2)
    flags = []
    if np.quantile(gray,.95) < .08: flags.append('too_dark')
    if contrast < .035: flags.append('low_contrast_or_featureless')
    if edge < .002: flags.append('blur_or_smooth_surface')
    return dict(flags=flags,contrast=contrast,edge=edge,
        note='Heuristic screening only: fog/wet lens not reliably distinguished from smooth ice/water')


def predict(crop, model):
    if model['features'] != VERSION: raise ValueError('Unsupported ice feature version')
    x = features(crop)
    values=[]
    for tree in model['trees']:
        node=0
        while tree['left'][node] != -1:
            node=(tree['left'] if x[tree['feature'][node]]<=tree['threshold'][node] else tree['right'])[node]
        values.append(tree['value'][node])
    types=None
    if model.get('outputs'):
        means=np.mean(values,axis=0)
        types=dict(zip(model['outputs'],map(float,means)))
        mean=sum(types[k] for k in TYPES[1:4])
        spread=float(np.std(np.asarray(values)[:,1:4].sum(axis=1)))
    else:
        mean=float(np.mean(values)); spread=float(np.std(values))
    q=quality(crop)
    distance=float(np.max(np.abs((x-np.array(model['mean']))/np.array(model['scale']))))
    reasons=list(q['flags'])
    if distance>6: reasons.append('outside_training_features')
    if spread>25: reasons.append('model_disagreement')
    if types and types['unknown']>20: reasons.append('substantial_unknown_area')
    return dict(ice_percent=None if reasons else round(mean,2),
        type_percentages=None if reasons else types,
        candidate_percent=round(mean,2), tree_spread=round(spread,2),
        surface_class='unknown' if reasons else 'mostly_water' if mean<20 else 'mostly_ice' if mean>=80 else 'mixed',
        quality=q, reasons=reasons, needs_review=bool(reasons), model_id=model['id'],
        note='Unvalidated image-area proxy; tree spread is not calibrated uncertainty')


def camera_frame(index, source, model_path, cache_path, limit=20):
    """Incremental bounded work, exact UTC minute bins, never forward-fill gaps.

    Source is a single camera archive with YYYYMMDD/HHMMSS/camera-3 files.
    At most 20 new photos per dashboard build; newest first. Cache keys include
    model bytes, absolute image path, file size and mtime. Bad images retry later.
    """
    import pandas as pd
    if not source.is_dir(): raise ValueError('Camera archive is unavailable')
    raw=model_path.read_bytes(); model=json.loads(raw)
    digest=hashlib.sha256(raw).hexdigest()
    try: cache=json.loads(cache_path.read_text())
    except (FileNotFoundError,ValueError): cache={}
    result=pd.DataFrame(np.nan,index=index,columns=[NAME,*TYPE_NAMES.values()])
    # Only minutes with dashboard observations; no full-leg recursive walk.
    stamps={int(t.timestamp())//60 for t in index}
    used=0; start=time.monotonic(); samples={}
    for day in sorted({t.strftime('%Y%m%d') for t in index},reverse=True):
        for path in sorted((source/day).glob('*/Camera360_*_cam_3.jpg'),reverse=True):
            match=re.fullmatch(r'Camera360_(\d{14})_cam_3.jpg',path.name)
            if not match: continue
            timestamp=pd.to_datetime(match[1],format='%Y%m%d%H%M%S',utc=True)
            minute=int(timestamp.timestamp())//60
            if minute not in stamps: continue
            try:
                stat=path.stat()
                key=hashlib.sha256(f'{digest}:{path.resolve()}:{stat.st_size}:{stat.st_mtime_ns}'.encode()).hexdigest()
                if key not in cache:
                    if used>=limit or time.monotonic()-start>5: continue
                    used+=1
                    with Image.open(path) as im: prediction=predict(crop_region(im),model)
                    cache[key]=dict(**prediction,file=str(path),time=timestamp.isoformat())
                samples.setdefault(minute,[]).append(cache[key])
            except (OSError,ValueError):
                continue  # partial camera writes never break dashboard builds
    minute_index=np.array([int(t.timestamp())//60 for t in index])
    for minute, values in samples.items():
        # Any rejected photo in a bin leaves a gap, rather than hiding poor visibility.
        if all(v['ice_percent'] is not None for v in values):
            result.loc[minute_index==minute,NAME]=float(np.mean([v['ice_percent'] for v in values]))
            if all(v.get('type_percentages') for v in values):
                for key,name in TYPE_NAMES.items():
                    result.loc[minute_index==minute,name]=float(np.mean([v['type_percentages'][key] for v in values]))
    cache_path.parent.mkdir(parents=True,exist_ok=True)
    tmp=cache_path.with_suffix('.tmp');tmp.write_text(json.dumps(cache));tmp.replace(cache_path)
    return result
