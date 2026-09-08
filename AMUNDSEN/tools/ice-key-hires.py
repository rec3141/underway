"""Rebuild the same reference figure from native source crops at double density."""
import base64
import argparse
import importlib.util
import io
import json
from pathlib import Path
import re
import subprocess
from PIL import Image

root=Path('/home/cryomics/Downloads/ice-reference-key')
parser=argparse.ArgumentParser();parser.add_argument('--floes-v2',action='store_true');args=parser.parse_args()
out=Path('/home/cryomics/Downloads/ice-resolution');out.mkdir(exist_ok=True)
spec=importlib.util.spec_from_file_location('rot','/tmp/amundsen-camera-rotated/AMUNDSEN/tools/ice-rotated-preview.py')
rot=importlib.util.module_from_spec(spec);spec.loader.exec_module(rot)
examples=json.loads((root/'selected-examples.json').read_text())['examples']
uris=[]
for row in examples:
    source=Path('/media/cryomics/T7 Shield/Amundsen/Camera_360/2025_LEG_04')/row['file']
    with Image.open(source) as image:
        affine,_,_=rot.geometry(image.size,angle=-30)
        crop=image.convert('RGB').transform((1200,600),Image.Transform.AFFINE,affine,Image.Resampling.BICUBIC)
        buffer=io.BytesIO();crop.save(buffer,format='PNG')
        uris.append('data:image/png;base64,'+base64.b64encode(buffer.getvalue()).decode())
page=(root/'ice-reference-key.html').read_text()
assert len(re.findall(r'data:image/jpeg;base64,[^"]+',page))==len(uris)==18
iterator=iter(uris)
page=re.sub(r'data:image/jpeg;base64,[^"]+',lambda _:next(iterator),page)
stem='key-hires-floes-v2' if args.floes_v2 else 'key-hires'
if args.floes_v2:
    spec=importlib.util.spec_from_file_location('taxonomy',Path(__file__).with_name('ice-taxonomy.py'))
    taxonomy=importlib.util.module_from_spec(spec);spec.loader.exec_module(taxonomy)
    page=taxonomy.rename(page)
html=out/(stem+'.html');html.write_text(page)
subprocess.run(['/home/cryomics/.cache/ms-playwright/chromium_headless_shell-1234/chrome-headless-shell-linux64/chrome-headless-shell',
    '--no-sandbox','--headless','--disable-gpu','--hide-scrollbars','--force-device-scale-factor=2','--window-size=1464,1532',
    '--screenshot='+str(out/(stem+'.png')),html.as_uri()],check=True,timeout=60,capture_output=True)
with Image.open(out/(stem+'.png')) as image:
    assert image.size==(2928,3064),image.size
    print('High-resolution key:',image.size,'same 18 examples, original 1200x600 crops')
