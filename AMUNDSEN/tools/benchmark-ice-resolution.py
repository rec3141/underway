"""Benchmark the existing feature family on Eric's water/ice comparison pair."""
import argparse
import json
from pathlib import Path
import sys
import time
import numpy as np
from PIL import Image
from threadpoolctl import threadpool_limits

sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from dashboard.ice_classifier import features,crop_region


def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--output',type=Path,required=True);a=p.parse_args()
    threadpool_limits(limits=1)
    source=Path('/media/cryomics/T7 Shield/Amundsen/Camera_360/2025_LEG_04')
    files=['20250921/213750/Camera360_20250921213750_cam_3.jpg','20250915/212750/Camera360_20250915212750_cam_3.jpg']
    crops=[];preparation=[]
    for file in files:
        start=time.perf_counter()
        with Image.open(source/file) as im:
            im.load();decoded=time.perf_counter();crop=crop_region(im)
        preparation.append(dict(file=file,decode_ms=(decoded-start)*1000,crop_ms=(time.perf_counter()-decoded)*1000))
        crops.append(crop)
    rows=[]
    for size in [(120,60),(240,120),(480,240),(600,300),(960,480),None]:
        times=[[],[]];vectors=[]
        for crop in crops:features(crop,size=size)  # warm-up, not measured
        for repeat in range(7):
            for i in ([0,1] if repeat%2==0 else [1,0]):
                start=time.perf_counter();vector=features(crops[i],size=size);times[i].append((time.perf_counter()-start)*1000)
                if len(vector)!=89 or not np.isfinite(vector).all():raise ValueError('Invalid features')
            time.sleep(.15)
        for crop in crops:vectors.append(features(crop,size=size).tolist())
        row=dict(size=list(size or crops[0].size),native=size is None,features=89,repeats=7,
            images=[dict(file=file,median_ms=float(np.median(t)),min_ms=min(t),max_ms=max(t),vector=v) for file,t,v in zip(files,times,vectors)])
        rows.append(row);print(json.dumps({**row,'images':[{k:v for k,v in r.items() if k!='vector'} for r in row['images']]}),flush=True)
    a.output.parent.mkdir(parents=True,exist_ok=True)
    a.output.write_text(json.dumps(dict(note='Single numerical thread; warm-up plus seven alternating repetitions. Feature times include resize/conversion, exclude file I/O and crop. Fixed pixel lags; not a validation of discrimination.',preparation=preparation,results=rows),indent=2))


if __name__=='__main__':main()
