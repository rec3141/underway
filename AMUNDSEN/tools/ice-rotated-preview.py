"""Compare a boat-aligned candidate sea ROI with the old axis-aligned crop."""
import argparse
import base64
import html
import io
import json
import math
from pathlib import Path
from PIL import Image, ImageOps


def geometry(size, center=(.45,.62), length=1200, width=600, angle=-55):
    theta=math.radians(angle)
    ux,uy=math.cos(theta),math.sin(theta)
    vx,vy=-uy,ux
    cx,cy=center[0]*size[0],center[1]*size[1]
    ox,oy=cx-length*ux/2-width*vx/2,cy-length*uy/2-width*vy/2
    def point(x,y):
        return [ox+x*ux+y*vx,oy+x*uy+y*vy]
    polygon=[point(0,0),point(length,0),point(length,width),point(0,width)]
    if any(not(0<=x<size[0] and 0<=y<size[1]) for x,y in polygon):
        raise ValueError('Candidate region extends outside image')
    return (ux,vx,ox,uy,vy,oy),polygon,point


def uri(image):
    buf=io.BytesIO();image.save(buf,format='JPEG',quality=88)
    return 'data:image/jpeg;base64,'+base64.b64encode(buf.getvalue()).decode()


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--input',type=Path,required=True)
    p.add_argument('--source',type=Path,required=True)
    p.add_argument('--output',type=Path,required=True)
    p.add_argument('--angle',type=float,default=-55)
    args=p.parse_args()
    data=json.loads(args.input.read_text())
    selected=[s for s in data['scenes'] if s['scene'] in (7,11,18,23,38,40)]
    args.output.mkdir(parents=True,exist_ok=True)
    page='''<!doctype html><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>Boat-aligned crop trial</title>
<style>body{font:17px system-ui;background:#17212b;color:#eee;margin:20px}.pair{display:grid;grid-template-columns:1.3fr 1fr;gap:16px}svg,img{width:100%}article{border-top:1px solid #aaa;padding:15px 0}a{color:#9df}@media(max-width:750px){.pair{grid-template-columns:1fr}}</style>
<h1>Boat-aligned crop trial</h1><p>Cyan dashed: old crop. Orange: candidate rotated rectangle. Right: extracted candidate, with 100×100-pixel grid.
One common placement across six scenes; not automatically fitted to each image. No existing labels changed. Rotation resamples pixels, so new tile labels must stay separate.</p>'''
    manifest=[]
    for s in selected:
        path=(args.source/s['file']).resolve()
        if not path.is_relative_to(args.source.resolve()):raise ValueError('Outside source')
        with Image.open(path) as im:
            im=im.convert('RGB');w,h=im.size
            affine,polygon,point=geometry(im.size,angle=args.angle)
            crop=im.transform((1200,600),Image.Transform.AFFINE,affine,Image.Resampling.BICUBIC)
            full=uri(ImageOps.contain(im,(1200,1200)))
        crop.save(args.output/f'scene-{s["scene"]}-rotated.jpg',quality=92)
        pts=' '.join(f'{x:.2f},{y:.2f}' for x,y in polygon)
        x0,y0,x1,y1=[v*n for v,n in zip(s['roi'],(w,h,w,h))]
        grid=''.join(f'<path d="M{x} 0 V600"/>' for x in range(100,1200,100))+''.join(f'<path d="M0 {y} H1200"/>' for y in range(100,600,100))
        page+=f'''<article><h2>Scene {s['scene']} · {html.escape(s['file'])}</h2><div class="pair">
<svg viewBox="0 0 {w} {h}"><image href="{full}" width="{w}" height="{h}"/><rect x="{x0}" y="{y0}" width="{x1-x0}" height="{y1-y0}" fill="none" stroke="cyan" stroke-width="12" stroke-dasharray="35"/><polygon points="{pts}" fill="none" stroke="#ffb020" stroke-width="12"/></svg>
<svg viewBox="0 0 1200 600"><image href="{uri(crop)}" width="1200" height="600"/><g stroke="#ffb020" stroke-width="1" opacity=".6">{grid}</g></svg></div></article>'''
        manifest.append({'scene':s['scene'],'file':s['file'],'angle':args.angle,'center':[.45,.62],
                         'size':[1200,600],'source_polygon':polygon,'output_to_source_affine':affine,
                         'tiles':[{'crop_box':[x,y,x+100,y+100],'source_polygon':[point(x,y),point(x+100,y),point(x+100,y+100),point(x,y+100)]}
                                  for y in range(0,600,100) for x in range(0,1200,100)]})
    (args.output/'regions.json').write_text(json.dumps(manifest,indent=2))
    (args.output/'index.html').write_text(page)
    print(args.output/'index.html')


if __name__=='__main__':main()
