import json
import os
from pathlib import Path
import tempfile
import unittest
from dashboard import ice_store,ice_worker

class IceTests(unittest.TestCase):
    def test_new_arrivals_only_and_resume(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);db=ice_store.connect(root/'out');source=root/'share';source.mkdir()
            cutoff=1789084800 # 2026-09-11 00:00 UTC
            self.assertEqual(ice_worker.discover(db,source,cutoff),0)
            for stamp in ('20260910235800','20260911000200'):
                p=source/'2026_LEG_03'/stamp[:8]/stamp[8:]/f'Camera360_{stamp}_cam_3.jpg';p.parent.mkdir(parents=True);p.write_bytes(b'pending');os.utime(p,(cutoff-100,cutoff-100))
            self.assertEqual(ice_worker.discover(db,source,cutoff+300),1)
            self.assertEqual(ice_worker.discover(db,source,cutoff+600),0)
            row=db.execute('select * from photos').fetchone();self.assertIn('000200',row['file'])
            product=ice_store.track(cutoff,cutoff+600,root/'out')['photos'][0];self.assertIsNone(product['ice'])
            ice_store.complete(db,row['id'],'filtered',[0]*6,{'triage':'water'})
            self.assertEqual(ice_store.track(cutoff,cutoff+600,root/'out')['photos'][0]['ice'],0)
    def test_paths(self):
        for identifier in ('../secret','/etc/passwd','a'*21):
            with self.assertRaises(ValueError):ice_store.photo_path(identifier,'roi')
        with self.assertRaises(ValueError):ice_store.photo_path('a'*20,'../source')
    def test_model_response(self):
        s={k:0 for k in ice_store.TYPES+['whitecap','small waves','calm water','unknown']};s['calm water']=100
        a=dict(surface_percentages=s,context_photo_description='Ocean',roi_description='Water')
        raw={'choices':[{'finish_reason':'stop','message':{'content':json.dumps(a)}}]}
        self.assertEqual(ice_worker.validate(raw)[0],[0]*6)
        s['nilas']=50;raw['choices'][0]['message']['content']=json.dumps(a)
        with self.assertRaises(ValueError):ice_worker.validate(raw)
    def test_packaged_assets(self):
        p=json.loads((ice_worker.ASSETS/'prompt.json').read_text());self.assertEqual(len(p['segments']),3)
        self.assertTrue((ice_worker.ASSETS/'reference-key.png').is_file())
        model=json.loads((ice_worker.ASSETS/'seawater-v2.json').read_text());self.assertEqual(model['feature_size'],[600,300])

if __name__=='__main__':unittest.main()
