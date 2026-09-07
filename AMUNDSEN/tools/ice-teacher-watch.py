"""Bounded overnight watchdog: saved progress, cool-only retries, explicit pause."""
from datetime import datetime,timezone
import json
from pathlib import Path
import subprocess
import time
import importlib.util
import os

ROOT=Path(os.environ.get('ICE_TEACHER_ROOT','/home/cryomics/Downloads/amundsen-ice-gemma-overnight'))
STATE=Path(os.environ.get('ICE_TEACHER_STATE','/home/cryomics/.cache/underway/teacher-watch.json'))
PAUSE=Path(os.environ.get('ICE_TEACHER_PAUSE','/home/cryomics/.cache/underway/teacher-paused'))
UNIT=os.environ.get('ICE_TEACHER_UNIT','ice-gemma-overnight.service')
TIMER=os.environ.get('ICE_TEACHER_TIMER','ice-teacher-watch.timer')
RESULTS=ROOT/os.environ.get('ICE_TEACHER_RESULTS','gemma4/results.json')
spec=importlib.util.spec_from_file_location('monitor','/tmp/amundsen-camera-rotated/AMUNDSEN/tools/ice-monitored-review.py')
m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)
state=json.loads(STATE.read_text()) if STATE.exists() else dict(started=time.time(),retries=0,cool_checks=0)
try:
    rows=json.loads(RESULTS.read_text())
except FileNotFoundError:rows=[]
expected=len(json.loads((ROOT/'queue.json').read_text())['queue'])
saved=len({r['file'] for r in rows if r.get('finish_reason')=='stop'})
active=subprocess.run(['systemctl','--user','is-active',UNIT],capture_output=True,text=True).stdout.strip()
cpu,gpu,_,_=m.temperatures()
state.update(saved=saved,expected=expected,service=active,cpu=cpu,gpu=gpu,checked_utc=datetime.now(timezone.utc).isoformat())
if saved>=expected:
    state['status']='complete';subprocess.run(['systemctl','--user','stop',TIMER],check=False)
elif time.time()-state['started']>=float(os.environ.get('ICE_TEACHER_MAX_HOURS','8'))*3600:
    state['status']='overnight deadline reached';subprocess.run(['systemctl','--user','stop',UNIT],check=False)
    subprocess.run(['systemctl','--user','stop',TIMER],check=False)
elif PAUSE.exists():state['status']='explicitly paused'
elif active in ('active','activating'):state['status']='running';state['cool_checks']=0
else:
    state['cool_checks']=state.get('cool_checks',0)+1 if cpu<85 and gpu<70 else 0
    state['status']='stopped; waiting for three cool checks'
    if state['cool_checks']>=3 and state['retries']<2:
        subprocess.run(['systemctl','--user','restart',UNIT],check=True)
        state['retries']+=1;state['cool_checks']=0;state['status']='resumed saved queue'
    elif state['retries']>=2:state['status']='retry limit reached; manual review needed'
tmp=STATE.with_suffix('.tmp');tmp.write_text(json.dumps(state,indent=2));tmp.replace(STATE)
(ROOT/'monitor.json').write_text(json.dumps(state,indent=2));print(json.dumps(state),flush=True)
