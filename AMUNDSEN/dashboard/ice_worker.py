"""New-arrival-only Gemma camera worker. Backfill belongs in the dev repository."""
import argparse
import base64
from collections import deque
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime,timezone
import fcntl
import hashlib
import io
import json
import math
import os
from pathlib import Path
import re
import subprocess
import time
import urllib.request
from PIL import Image,ImageDraw
from . import camera_batch,ice_store
from .camera_features import crop_region
from .camera_seawater import Filter,audit_contradiction

ASSETS=Path(__file__).with_name('ice_assets')

def uri(im):
    out=io.BytesIO();im.save(out,'JPEG',quality=92)
    return 'data:image/jpeg;base64,'+base64.b64encode(out.getvalue()).decode()

def discover(db,source,now=None):
    """First invocation records a start boundary. Never queues pre-boundary photos."""
    now=time.time() if now is None else now
    row=db.execute("SELECT value FROM meta WHERE key='arrival_start'").fetchone()
    if row is None:
        db.execute('INSERT INTO meta VALUES (?,?)',('arrival_start',str(now)));db.commit();return 0
    cutoff=float(row[0]);first=datetime.fromtimestamp(cutoff,timezone.utc).strftime('%Y%m%d');count=0
    if not source.is_dir():raise OSError('Camera source unavailable')
    for leg in sorted(source.glob('*_LEG_*'),reverse=True):
        for day in sorted(leg.glob('20??????'),reverse=True):
            if day.name<first:continue
            for p in day.glob('*/Camera360_*_cam_3.jpg'):
                m=re.fullmatch(r'Camera360_(\d{14})_cam_3.jpg',p.name)
                if not m:continue
                stamp=datetime.strptime(m[1],'%Y%m%d%H%M%S').replace(tzinfo=timezone.utc).timestamp()
                if stamp<cutoff or stamp>now or now-p.stat().st_mtime<60:continue
                file=p.relative_to(source).as_posix();identifier=hashlib.sha256(file.encode()).hexdigest()[:20]
                count+=db.execute('INSERT OR IGNORE INTO photos(id,file,t,leg) VALUES (?,?,?,?)',(identifier,file,stamp,leg.name)).rowcount
    db.commit();return count

def validate(raw):
    choice=raw['choices'][0]
    if choice['finish_reason']!='stop' or choice['message'].get('reasoning_content'):raise ValueError('Incomplete or reasoning response')
    text=choice['message']['content'].strip().removeprefix('```json').removesuffix('```').strip();record=json.loads(text)
    surface=record['surface_percentages'];required=ice_store.TYPES+['whitecap','small waves','calm water','unknown']
    if set(surface)!=set(required):raise ValueError('Unexpected surface taxonomy')
    if any(type(surface[k]) not in (int,float) or not math.isfinite(surface[k]) or not 0<=surface[k]<=100 for k in required):raise ValueError('Invalid percentages')
    if abs(sum(surface.values())-100)>2:raise ValueError('Surface percentages do not sum to 100')
    if not all(isinstance(record.get(k),str) and record[k].strip() for k in ('context_photo_description','roi_description')):raise ValueError('Missing descriptions')
    return [surface[k] for k in ice_store.TYPES],record

class Client:
    def __init__(self,config,db):self.config=config;self.db=db;self.samples=deque()
    def temperature(self):
        values=[]
        for d in Path('/sys/class/hwmon').glob('hwmon*'):
            if (d/'name').read_text().strip() in {'coretemp','k10temp'}:values.extend(float(p.read_text())/1000 for p in d.glob('temp*_input'))
        if not values:raise RuntimeError('No supported CPU temperature sensor; refusing inference')
        gpu=max(float(x) for x in subprocess.check_output(['nvidia-smi','--query-gpu=temperature.gpu','--format=csv,noheader,nounits'],text=True,timeout=10).splitlines())
        now=time.monotonic();cpu=max(values);self.samples.append((now,cpu));cutoff=now-120
        while len(self.samples)>1 and self.samples[1][0]<=cutoff:self.samples.popleft()
        start=max(cutoff,self.samples[0][0]);total=sum(v*max(0,b-max(a,start)) for (a,v),(b,_) in zip(self.samples,list(self.samples)[1:]));avg=total/(now-start) if now>start else cpu
        self.db.execute('INSERT INTO telemetry VALUES (?,?,?,?)',(time.time(),cpu,gpu,avg));self.db.execute('DELETE FROM telemetry WHERE t<?',(time.time()-604800,));self.db.commit()
        return cpu,gpu,avg
    def request(self,payload):
        payload['model']=self.config.get('model','gemma-camera')
        req=urllib.request.Request(self.config['url'].rstrip('/')+'/v1/chat/completions',json.dumps(payload).encode(),{'Content-Type':'application/json'})
        with urllib.request.build_opener(urllib.request.ProxyHandler({})).open(req,timeout=300) as response:return json.load(response)
    def __call__(self,payload):
        cpu,gpu,avg=self.temperature()
        while avg>=95 or gpu>=78:time.sleep(2);cpu,gpu,avg=self.temperature()
        # The owned server must be stoppable: canceling HTTP alone does not stop GPU inference.
        unit=self.config['server_unit']
        with ThreadPoolExecutor(max_workers=1) as pool:
            future=pool.submit(self.request,payload)
            while not future.done():
                try:cpu,gpu,avg=self.temperature()
                except Exception:
                    subprocess.run(['systemctl','--user','stop',unit],check=True);raise
                if avg>=95 or gpu>=83:
                    subprocess.run(['systemctl','--user','stop',unit],check=True)
                    while cpu>=85 or gpu>=70 or avg>=85:time.sleep(2);cpu,gpu,avg=self.temperature()
                    subprocess.run(['systemctl','--user','start',unit],check=True)
                    raise RuntimeError('Thermal cooldown; retry later')
                time.sleep(2)
            return future.result()

def images(row,source,root):
    path=source/row['file'];before=path.stat()
    with Image.open(path) as im:full=im.convert('RGB');roi=crop_region(full)
    after=path.stat()
    if (before.st_size,before.st_mtime_ns)!=(after.st_size,after.st_mtime_ns):raise OSError('Image still changing')
    folder=root/'images';folder.mkdir(exist_ok=True)
    angle=-math.pi/6;u,v=math.cos(angle),math.sin(angle);ox=.45*full.width-600*u+300*v;oy=.62*full.height-600*v-300*u
    corners=[(ox,oy),(ox+1200*u,oy+1200*v),(ox+1200*u-600*v,oy+1200*v+600*u),(ox-600*v,oy+600*u)]
    ImageDraw.Draw(full).line(corners+[corners[0]],fill='orange',width=12)
    for kind,im in [('source',full),('roi',roi),('slice',roi.crop((598,0,602,600)).resize((4,180)))]:
        target=ice_store.photo_path(row['id'],kind,root);tmp=target.with_suffix('.tmp');im.save(tmp,'JPEG',quality=92);tmp.replace(target)
    return full,roi

def process(db,config,root,client=None):
    rows=db.execute("SELECT * FROM photos WHERE status='pending' AND retry_after<=? ORDER BY t DESC LIMIT 16",(time.time(),)).fetchall()
    if not rows:return
    client=client or Client(config,db);filter_=Filter(root/'seawater-filter.json');cases=[];pictures={};decisions={}
    for row in rows:
        try:
            pictures[row['file']]=images(row,Path(config['source']),root);roi=pictures[row['file']][1]
            decision=filter_.decide(row['file'],roi);decision['stage']='local';decisions[row['file']]=decision
            if decision['route']=='gemma':cases.append(dict(file=row['file'],crop=roi))
        except Exception as e:
            db.execute('UPDATE photos SET attempts=attempts+1,retry_after=?,detail=? WHERE id=?',(time.time()+300,json.dumps(dict(error=str(e))),row['id']));db.commit()
    if len(cases)>=2:
        try:
            batch,answers=camera_batch.run(cases,client,root/'batches')
            first=db.execute("SELECT value FROM meta WHERE key='batch_audit_started'").fetchone() is None
            for file,answer in answers.items():
                audit=answer['water'] and (first or int(hashlib.sha256(file.encode()).hexdigest()[:8],16)%100==0)
                if audit:first=False;db.execute("INSERT OR REPLACE INTO meta VALUES ('batch_audit_started','1')");db.commit()
                decisions[file]=dict(stage='batch',batch=batch,**answer,route=('audit' if audit else 'filter') if answer['water'] else 'gemma')
        except Exception as e:print('Batch failed open:',e,flush=True)
    prompt=json.loads((ASSETS/'prompt.json').read_text())
    key='data:image/png;base64,'+base64.b64encode((ASSETS/'reference-key.png').read_bytes()).decode()
    for row in rows:
        if row['file'] not in pictures:continue
        decision=decisions[row['file']]
        try:
            if decision['route']=='filter':ice_store.complete(db,row['id'],'filtered',[0]*6,dict(triage=decision));continue
            full,roi=pictures[row['file']];content=[]
            for text,image in zip(prompt['segments'],[uri(full),key,uri(roi.resize((2400,1200),Image.Resampling.LANCZOS))]):content.extend([dict(type='text',text=text),dict(type='image_url',image_url=dict(url=image))])
            raw=client(dict(messages=[dict(role='user',content=content)],temperature=0,seed=42,max_tokens=2000,stream=False,chat_template_kwargs=dict(enable_thinking=False)))
            values,answer=validate(raw);record=dict(answer=answer,triage=decision,version=prompt['version'],usage=raw.get('usage'),utc=datetime.now(timezone.utc).isoformat())
            if decision['route']=='audit' and audit_contradiction(json.dumps(answer)):
                db.execute('INSERT OR REPLACE INTO audits VALUES (?,?)',(row['id'],json.dumps(dict(action='report only',**record))))
            ice_store.complete(db,row['id'],'gemma',values,record)
        except Exception as e:
            db.execute('UPDATE photos SET attempts=attempts+1,retry_after=?,detail=? WHERE id=?',(time.time()+300,json.dumps(dict(error=str(e))),row['id']));db.commit();print('Retry later:',row['file'],e,flush=True)

def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--config',type=Path,required=True);parser.add_argument('--once',action='store_true');args=parser.parse_args()
    config=json.loads(args.config.read_text());root=Path(config['root']);root.mkdir(parents=True,exist_ok=True)
    with (root/'worker.lock').open('w') as lock:
        fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
        db=ice_store.connect(root)
        client=Client(config,db)
        if not (root/'seawater-filter.json').exists():
            (root/'seawater-filter.json').write_text(json.dumps(dict(enabled=True,model=str(ASSETS/'seawater-v2.json'),threshold=.995,disabled_flag=str(root/'filter-disabled'))))
        while True:
            try:discover(db,Path(config['source']));process(db,config,root,client)
            except Exception as e:print('Worker waiting:',e,flush=True)
            if args.once:break
            time.sleep(30)

if __name__=='__main__':main()
