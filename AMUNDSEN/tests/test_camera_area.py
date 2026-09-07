import importlib.util
import json
from pathlib import Path
import unittest

spec=importlib.util.spec_from_file_location('area',Path(__file__).resolve().parents[1]/'tools/train-camera-area.py')
area=importlib.util.module_from_spec(spec);spec.loader.exec_module(area)

class AreaTargetTests(unittest.TestCase):
    def row(self):
        return dict(visibility='clear',confidence='high',surface_percentages=dict(zip(
            ['whitecap','small waves','smooth water',*area.ICE,'unknown'],[0,0,10,0,60,0,30,0,0,0])),
            artifact_percentages=dict(blurry=0,fog=0,reflection=0,night=0),total_ice_percent=7)
    def evaluate(self,obj,labels=()):return area.target(dict(response=json.dumps(obj),finish_reason='stop'),labels)
    def test_derive_budget_not_reported_total(self):
        self.assertEqual(self.evaluate(self.row(),['nilas'])[3:9].sum(),90)
    def test_bad_budget(self):
        row=self.row();row['surface_percentages']['smooth water']=25
        with self.assertRaises(ValueError):self.evaluate(row)
    def test_human_presence_not_percentages(self):
        with self.assertRaisesRegex(ValueError,'contradicts'):self.evaluate(self.row(),['smooth water'])
        self.assertEqual(self.evaluate(self.row(),['smooth water','nilas'])[4],60)
    def test_visibility_and_uncertainty(self):
        for field,value in [('visibility','uncertain'),('confidence','low')]:
            row=self.row();row[field]=value
            with self.assertRaises(ValueError):self.evaluate(row)
        row=self.row();row['artifact_percentages']['blurry']=60
        with self.assertRaises(ValueError):self.evaluate(row)

if __name__=='__main__':unittest.main()
