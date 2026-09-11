"""Populate compact map colours from existing slices; no inference or downloads."""
import argparse
from pathlib import Path
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from dashboard import ice_store

def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root',type=Path,default=ice_store.ROOT)
    args=parser.parse_args()
    db=ice_store.connect(args.root)
    ids=[r[0] for r in db.execute('SELECT photos.id FROM photos LEFT JOIN photo_rgb USING(id) WHERE photo_rgb.id IS NULL')]
    count=missing=0
    for identifier in ids:
        try:
            ice_store.cache_slice_rgb(db,identifier,args.root);count+=1
        except OSError:
            missing+=1
        if count and count%500==0:db.commit()
    db.commit();db.close()
    print(f'Cached {count} slice colours; {missing} missing or unreadable slices')

if __name__=='__main__':main()
