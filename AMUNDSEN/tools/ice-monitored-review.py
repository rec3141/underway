"""Run local review with periodic telemetry and conservative experiment cutoffs."""
import csv
from datetime import datetime, timezone
from pathlib import Path
import subprocess
import sys
import time


def temperatures():
    cpu=[]
    for folder in Path('/sys/class/hwmon').glob('hwmon*'):
        if (folder/'name').read_text().strip()!='coretemp':
            continue
        cpu.extend(float(p.read_text())/1000 for p in folder.glob('temp*_input'))
    if not cpu:
        raise RuntimeError('CPU temperature sensors unavailable')
    gpu=subprocess.check_output(['nvidia-smi','--query-gpu=temperature.gpu,power.draw,memory.used',
        '--format=csv,noheader,nounits'],text=True,timeout=10).strip().splitlines()
    values=[list(map(float,row.split(','))) for row in gpu]
    return max(cpu),max(v[0] for v in values),sum(v[1] for v in values),sum(v[2] for v in values)


def main():
    # All remaining arguments are passed to the fixed review script, never a shell.
    args=sys.argv[1:]
    output=Path(args[args.index('--output')+1])
    model=args[args.index('--model')+1]
    output.mkdir(parents=True,exist_ok=True)
    lms=str(Path.home()/'.lmstudio/bin/lms')
    child=None
    try:
        with (output/'telemetry.csv').open('w',newline='') as f:
            writer=csv.writer(f);writer.writerow(['utc','cpu_max_c','gpu_c','gpu_w','gpu_memory_mib']);f.flush()
            while True:
                values=temperatures()
                writer.writerow([datetime.now(timezone.utc).isoformat(),*values]);f.flush()
                # These are deliberately conservative trial limits, not claims
                # about official hardware damage/shutdown thresholds.
                if values[0]>=95 or values[1]>=83:
                    raise RuntimeError(f'Trial cutoff reached: CPU {values[0]}C, GPU {values[1]}C')
                if child is None:
                    child=subprocess.Popen([sys.executable,'-u',str(Path(__file__).with_name('ice-vision-review.py')),*args])
                result=child.poll()
                if result is not None:
                    return result
                time.sleep(3)
    finally:
        if child is not None and child.poll() is None:
            child.terminate()
            try:child.wait(timeout=10)
            except subprocess.TimeoutExpired:child.kill();child.wait()
        # Cancels any in-flight inference and releases its resources on all exits.
        subprocess.run([lms,'unload',model],timeout=30,check=False)


if __name__=='__main__':
    raise SystemExit(main())
