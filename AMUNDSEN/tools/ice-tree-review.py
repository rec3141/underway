"""Build a standalone k=16 Ward-tree proofsheet labeling page."""
import argparse
import json
from pathlib import Path


def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--portal',type=Path,required=True);a=p.parse_args()
    points=json.loads((a.portal/'embedding.json').read_text())
    state=json.loads((a.portal/'projections.json').read_text())
    if state['ids']!=[p['id'] for p in points]:raise ValueError('Tree identities differ')
    tree=state['projections']['tsne']['trees']['ward']
    n=len(points);roots=list(range(n))
    active=set(roots)
    for i,row in enumerate(tree[:n-16]):
        active.remove(int(row[0]));active.remove(int(row[1]));active.add(n+i)
    data=dict(points=points,tree=tree,roots=sorted(active))
    payload=json.dumps(data).replace('<','\\u003c')
    js=Path(__file__).with_name('ice-tree-review.js').read_text()
    page='''<!doctype html><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>Ice tree proofsheet</title><style>body{font:17px system-ui;background:#17212b;color:#eee;margin:16px}header{position:sticky;top:0;background:#17212b;padding:10px;z-index:2}button,input{font:inherit;margin:4px}#sheet{display:grid;grid-template-columns:repeat(auto-fill,minmax(var(--tile,160px),1fr));gap:5px}figure{margin:0}img{width:100%;display:block}figcaption{font:10px system-ui;overflow-wrap:anywhere}dialog{max-width:650px}#preview img{max-height:70vh;width:auto;max-width:85vw}a{color:#8df}</style>
<header><h1>Ward-tree proofsheet · t-SNE · initial k=16</h1><p>Down: split one level. Up: parent (excluding labeled photos). Left/Right: previous/next pending subset. L: label entire displayed batch. Enter: save and advance. Esc: cancel. Click a thumbnail for a larger view.</p><p id="status"></p><button id="left">← Previous</button><button id="up">↑ Parent</button><button id="down">↓ Split</button><button id="right">Next →</button><button id="label">L · Label batch</button><button id="undo">Undo</button><button id="export">Export labels/progress</button><label>Import <input id="import" type="file" accept=".json"></label><label>Thumbnail size <input id="size" type="range" min="80" max="320" value="160"></label><p id="save"></p></header>
<main id="sheet"></main><dialog id="labelbox"><form id="form"><p id="confirm"></p><label>Label <input id="name" required maxlength="100" autocomplete="off"></label><button type="submit">Save and next</button><button id="cancel" type="button">Cancel</button></form></dialog><dialog id="preview"><img id="large"><p id="file"></p><button id="close">Close</button></dialog><script>window.TREE_DATA=PAYLOAD;
SCRIPT</script>'''.replace('PAYLOAD',payload).replace('SCRIPT',js)
    target=a.portal/'tree-review.html';temp=target.with_suffix('.tmp');temp.write_text(page);temp.replace(target);print(target)


if __name__=='__main__':main()
