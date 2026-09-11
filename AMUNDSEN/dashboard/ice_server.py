"""Launch an explicitly configured local llama.cpp server; never download models."""
import json
import os
from pathlib import Path
import sys

def main():
    config=json.loads(Path(sys.argv[1]).read_text())
    binary=config['llama_binary'];model=config['model_path'];projector=config['projector_path']
    for path in (binary,model,projector):
        if not Path(path).is_file():raise FileNotFoundError(path)
    if config.get('library_path'):os.environ['LD_LIBRARY_PATH']=config['library_path']
    args=[binary,'--model',model,'--mmproj',projector,'--alias',config.get('model','gemma-camera'),
          '--host','127.0.0.1','--port',str(config.get('port',18043)),'--ctx-size','16384',
          '--n-gpu-layers','99','--threads','4','--parallel','1','--jinja','--reasoning','off',
          '--batch-size','2048','--ubatch-size','2048','--image-max-tokens','1120']
    os.execv(binary,args)

if __name__=='__main__':main()
