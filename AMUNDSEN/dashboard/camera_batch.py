"""Two-order contact-sheet triage, with strict validation and saved evidence."""
import base64
import hashlib
import io
import json
from PIL import Image,ImageDraw,ImageFont,ImageOps

PROMPT='''This is a numbered contact sheet of {n} separate sea-surface ROI photos.
For EACH numbered image independently, classify it as "water", "ice", or "unclear".
Water includes waves and ordinary foam. Ice includes any visible ice fragments or
coherent ice surface. Use unclear for ship/deck, severe wet-lens blur, fog, darkness,
or an ambiguous smooth surface. Mild blur need not obscure otherwise clear water.
Inspect the entire tile including its edges. Do not let neighbouring tiles affect
the label. Return only JSON: {{"items":[{{"id":1,"label":"water"}}, ...]}}.
Include all {n} ids exactly once. No percentages and no explanation.'''

def labels(raw,cases):
    choice=raw['choices'][0]
    if choice['finish_reason']!='stop':raise ValueError('Incomplete batch response')
    text=choice['message']['content'];data=json.loads(text.strip().removeprefix('```json').removesuffix('```').strip())
    items=data['items']
    if any(type(r.get('id')) is not int for r in items):raise ValueError('Noninteger tile id')
    if sorted(r['id'] for r in items)!=list(range(1,len(cases)+1)):raise ValueError('Missing/duplicate batch ids')
    if any(r.get('label') not in {'water','ice','unclear'} for r in items):raise ValueError('Invalid batch label')
    return {cases[r['id']-1]['file']:r['label'] for r in items}

def consensus(a,b):
    if set(a)!=set(b):raise ValueError('Different photo sets')
    return {file:dict(first=a[file],reversed=b[file],water=a[file]==b[file]=='water') for file in a}

def run(cases,call,out):
    if not 2<=len(cases)<=16:raise ValueError('Batch requires 2–16 photos')
    if len({r['file'] for r in cases})!=len(cases):raise ValueError('Duplicate photos')
    batch_id=hashlib.sha256(('batch-v1|'+'|'.join(r['file'] for r in cases)).encode()).hexdigest()[:20]
    out.mkdir(exist_ok=True);answers=[]
    for order,group in [('forward',cases),('reversed',list(reversed(cases)))]:
        sheet=Image.new('RGB',(1600,230*((len(group)+3)//4)),'#17212b');draw=ImageDraw.Draw(sheet)
        try:font=ImageFont.truetype('/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf',22)
        except OSError:font=ImageFont.load_default()
        for i,r in enumerate(group):
            x=i%4*400;y=i//4*230;sheet.paste(ImageOps.contain(r['crop'],(396,198)),(x,y+30));draw.text((x+4,y+2),str(i+1),font=font,fill='#00ff66')
        buffer=io.BytesIO();sheet.save(buffer,format='JPEG',quality=92);blob=buffer.getvalue()
        (out/f'{batch_id}-{order}.jpg').write_bytes(blob)
        prompt=PROMPT.format(n=len(group))
        payload=dict(model='gemma-camera',messages=[dict(role='user',content=[dict(type='text',text=prompt),dict(type='image_url',image_url=dict(url='data:image/jpeg;base64,'+base64.b64encode(blob).decode()))])],temperature=0,seed=42,max_tokens=1000,stream=False,chat_template_kwargs=dict(enable_thinking=False))
        raw=call(payload)
        report=out/f'{batch_id}-{order}.json'
        temporary=report.with_suffix('.json.tmp')
        temporary.write_text(json.dumps(dict(files=[r['file'] for r in group],prompt=prompt,response=raw),indent=2))
        temporary.replace(report)
        answers.append(labels(raw,group))
    return batch_id,consensus(*answers)
