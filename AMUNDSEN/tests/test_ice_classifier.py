import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
import numpy as np
import pandas as pd
from PIL import Image
from dashboard.ice_classifier import VERSION, NAME, TYPES, TYPE_NAMES, features, predict, crop_region, camera_frame

spec=importlib.util.spec_from_file_location('trainer',Path(__file__).parents[1]/'tools/train-ice-classifier.py')
trainer=importlib.util.module_from_spec(spec);spec.loader.exec_module(trainer)


class ClassifierTests(unittest.TestCase):
    def model(self, crop):
        return dict(id='test',features=VERSION,outputs=list(TYPES),
            trees=[dict(left=[-1],right=[-1],feature=[-2],threshold=[-2],value=[[10,20,30,40,0]])],
            mean=features(crop).tolist(),scale=np.ones(len(features(crop))).tolist())

    def image(self):
        return Image.fromarray(np.random.default_rng(42).integers(20,240,(600,1200,3),dtype=np.uint8))

    def test_vector_outputs_add_to_total(self):
        crop=self.image();result=predict(crop,self.model(crop))
        self.assertEqual(result['ice_percent'],90)
        self.assertEqual(sum(result['type_percentages'].values()),100)

    def test_darkness_is_unknown_not_water(self):
        crop=Image.new('RGB',(1200,600),'black')
        result=predict(crop,self.model(crop))
        self.assertIsNone(result['ice_percent'])
        self.assertIsNone(result['type_percentages'])
        self.assertIn('too_dark',result['reasons'])

    def test_camera_geometry_rejects_other_views(self):
        with self.assertRaises(ValueError):crop_region(Image.new('RGB',(640,480)))
        self.assertEqual(crop_region(Image.new('RGB',(3648,2052))).size,(1200,600))

    def test_structured_targets_reject_bad_quality_or_totals(self):
        label=dict(visibility='clear',confidence='high',percentages=dict(zip(TYPES,[10,20,30,40,0])))
        row=dict(id='region-scene-2',finish_reason='stop',response=json.dumps(label))
        self.assertEqual(trainer.structured_target(row),[10,20,30,40,0])
        label['visibility']='degraded';row['response']=json.dumps(label)
        self.assertIsNone(trainer.structured_target(row))
        label['visibility']='clear';label['percentages']['unknown']=90;row['response']=json.dumps(label)
        self.assertIsNone(trainer.structured_target(row))

    def test_cache_and_missing_minutes(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);folder=root/'20250910'/'120000';folder.mkdir(parents=True)
            path=folder/'Camera360_20250910120000_cam_3.jpg'
            self.image().resize((3648,2052)).save(path)
            with Image.open(path) as im:model=self.model(crop_region(im))
            model_path=root/'model.json';model_path.write_text(json.dumps(model))
            cache=root/'cache.json';index=pd.date_range('2025-09-10T12:00Z',periods=2,freq='min')
            first=camera_frame(index,root,model_path,cache)
            self.assertEqual(first[NAME].iloc[0],90)
            self.assertTrue(first[NAME].isna().iloc[1])
            self.assertEqual(first[TYPE_NAMES['broken_ice']].iloc[0],30)
            second=camera_frame(index,root,model_path,cache,limit=0)
            pd.testing.assert_frame_equal(first,second)


if __name__=='__main__':unittest.main()
