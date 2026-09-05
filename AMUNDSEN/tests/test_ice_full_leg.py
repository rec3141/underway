import argparse
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import numpy as np
from PIL import Image

spec=importlib.util.spec_from_file_location('full_leg',Path(__file__).parents[1]/'tools/ice-full-leg-tsne.py')
full=importlib.util.module_from_spec(spec);spec.loader.exec_module(full)


class FullLegTests(unittest.TestCase):
    def test_all_images_and_resume(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);source=root/'archive';folder=source/'20250910'/'120000';folder.mkdir(parents=True)
            for i in range(4):
                Image.new('RGB',(3648,2052),(40+i*20,80,100)).save(folder/f'Camera360_20250910120{i}00_cam_3.jpg')
            (folder/'Camera360_20250910120900_cam_3.jpg').write_bytes(b'bad')
            a=argparse.Namespace(source=source,output=root/'out',annotations=root/'absent.json')
            with patch('sklearn.manifold.TSNE') as tsne:
                tsne.return_value.fit_transform.return_value=np.arange(8).reshape(4,2)
                full.worker(a)
                self.assertEqual(tsne.return_value.fit_transform.call_args[0][0].shape[0],4)
            progress=json.loads((a.output/'progress.json').read_text())
            self.assertEqual(progress['included'],4);self.assertEqual(progress['excluded'],1)
            with patch('sklearn.manifold.TSNE') as tsne:
                full.worker(a);tsne.assert_not_called()
            self.assertIn('previews/',(a.output/'regions.html').read_text())


if __name__=='__main__':unittest.main()
