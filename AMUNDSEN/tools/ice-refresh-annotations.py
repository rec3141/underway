"""Publish new teacher annotations without modifying human labels or projections."""
import importlib.util
import json
from pathlib import Path

root=Path('/home/cryomics/Downloads')
source=root/'amundsen-ice-gemma-floes-v2/results.json'
if source.exists():
    spec=importlib.util.spec_from_file_location('explorer','/tmp/amundsen-camera-rotated/AMUNDSEN/tools/ice-region-explorer.py')
    explorer=importlib.util.module_from_spec(spec);spec.loader.exec_module(explorer)
    for name in ['amundsen-ice-full-leg-600x300','amundsen-ice-full-leg-tsne']:
        portal=root/name;path=portal/'annotation-sources.json'
        if not path.exists() or not (portal/'embedding.json').exists():
            print('Skipping absent/archived portal',name);continue
        sources=json.loads(path.read_text())
        if str(source) not in sources:
            sources.append(str(source));temp=path.with_suffix('.tmp');temp.write_text(json.dumps(sources));temp.replace(path)
        explorer.render(portal,[],False)
    print('Updated projection annotations from new Gemma run')
