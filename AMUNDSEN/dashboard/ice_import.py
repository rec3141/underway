"""Explicit import of completed dev products, never historical work queues."""
import argparse
from datetime import datetime,timezone
import json
from pathlib import Path
import re
from PIL import Image
from . import ice_store

def run(source,root,since,metadata_only=False,repair_images=False):
    db=ice_store.connect(root);(root/'images').mkdir(exist_ok=True);count=0
    records=json.loads((source/'results.json').read_text());full={r['file'] for r in records}
    skips=json.loads((source/'seawater-skips.json').read_text()) if (source/'seawater-skips.json').exists() else []
    for r in records+[r for r in skips if r['file'] not in full]:
        m=re.search(r'Camera360_(\d{14})_',r['file'])
        if not m:continue
        stamp=datetime.strptime(m[1],'%Y%m%d%H%M%S').replace(tzinfo=timezone.utc).timestamp()
        if stamp<since:continue
        existing=db.execute("SELECT 1 FROM photos WHERE id=? AND status!='pending'",(r['id'],)).fetchone()
        if existing and (not repair_images or all(ice_store.photo_path(r['id'],k,root).exists() for k in ('source','roi','slice'))):continue
        try:
            if 'response' in r:
                answer=json.loads(r['response'].strip().removeprefix('```json').removesuffix('```').strip());values=[answer['surface_percentages'][k] for k in ice_store.TYPES];status='gemma';paths=r['images']
            else:values=[0]*6;status='filtered';paths=[None,r['image']]
            if any(type(v) not in (int,float) or not 0<=v<=100 for v in values) or sum(values)>100:continue
            if metadata_only:
                db.execute('INSERT OR IGNORE INTO photos(id,file,t,leg) VALUES (?,?,?,?)',(r['id'],r['file'],stamp,r['file'].split('/')[0]))
                ice_store.complete(db,r['id'],status,values,dict(imported=True,record=r));count+=1;continue
            with Image.open(source/paths[1]) as im:roi=im.convert('RGB').resize((1200,600))
            products=[('roi',roi),('slice',roi.crop((598,0,602,600)).resize((4,180)))]
            if paths[0]:
                with Image.open(source/paths[0]) as im:products.append(('source',im.convert('RGB')))
            else:
                # Legacy skips did not retain context; only use an explicitly supplied existing cache.
                original=Path('/data/scratch/camera-originals')/r['file']
                if original.is_file():
                    with Image.open(original) as im:products.append(('source',im.convert('RGB')))
            for kind,im in products:
                target=ice_store.photo_path(r['id'],kind,root);temp=target.with_suffix('.tmp');im.save(temp,'JPEG',quality=90);temp.replace(target)
            db.execute('INSERT OR IGNORE INTO photos(id,file,t,leg) VALUES (?,?,?,?)',(r['id'],r['file'],stamp,r['file'].split('/')[0]))
            ice_store.complete(db,r['id'],status,values,dict(imported=True,record=r));count+=1
        except OSError as e:
            # Valid estimates remain useful even if a crash damaged the JPEG.
            db.execute('INSERT OR IGNORE INTO photos(id,file,t,leg) VALUES (?,?,?,?)',(r['id'],r['file'],stamp,r['file'].split('/')[0]))
            ice_store.complete(db,r['id'],status,values,dict(imported=True,record=r,image_error=str(e)));count+=1
            print('Imported estimate without image:',r['file'],flush=True)
        except (ValueError,KeyError,TypeError) as e:print('Skipped invalid product:',r['file'],e)
    print(f'Imported {count} completed products; no pending rows created')

def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--source',type=Path,required=True);p.add_argument('--root',type=Path,default=ice_store.ROOT);p.add_argument('--since',required=True,help='ISO timestamp with UTC offset')
    mode=p.add_mutually_exclusive_group();mode.add_argument('--metadata-only',action='store_true');mode.add_argument('--repair-images',action='store_true');a=p.parse_args()
    stamp=datetime.fromisoformat(a.since)
    if stamp.tzinfo is None:p.error('--since requires a UTC offset')
    run(a.source,a.root,stamp.timestamp(),a.metadata_only,a.repair_images)

if __name__=='__main__':main()
