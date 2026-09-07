import json
from pathlib import Path
import tempfile
import unittest
import numpy as np
from PIL import Image
from dashboard.camera_live import update
from dashboard.camera_features import crop_region, features

class LiveCameraTests(unittest.TestCase):
    def test_feature_contract(self):
        x=features(crop_region(Image.new('RGB',(3648,2052),(20,40,60))),size=(600,300))
        self.assertEqual(x.shape,(89,));self.assertTrue(np.isfinite(x).all())
        np.testing.assert_allclose(x[[0,7,14]],np.array([20,40,60])/255)
        with self.assertRaises(ValueError):crop_region(Image.new('RGB',(100,100)))

    def test_missing_share_preserves_output(self):
        with tempfile.TemporaryDirectory() as temp:
            root=Path(temp);out=root/'camera.json';out.write_text('{"photos":[]}')
            with self.assertRaises(ValueError):update(root/'missing',out,root/'cache.db',root/'pca.json')
            self.assertEqual(out.read_text(),'{"photos":[]}')

    def test_bad_image_preserves_archive(self):
        with tempfile.TemporaryDirectory() as temp:
            root=Path(temp);source=root/'source';day=source/'2026_LEG_03/20260906/010000';day.mkdir(parents=True)
            path=day/'Camera360_20260906010000_cam_3.jpg';path.write_bytes(b'partial jpeg')
            import os
            os.utime(path,(1,1))
            out=root/'www/data/camera.json';out.parent.mkdir(parents=True)
            old=dict(leg='2025_LEG_04',file='old.jpg');out.write_text(json.dumps(dict(photos=[old])))
            pca=root/'pca.json';pca.write_text(json.dumps(dict(mean=[0]*89,scale=[1]*89,axes=np.eye(89)[:4].tolist())))
            update(source,out,root/'cache.db',pca)
            result=json.loads(out.read_text());self.assertEqual(result['photos'],[old]);self.assertEqual(len(result['live_camera']['errors']),1)

if __name__=='__main__':unittest.main()
