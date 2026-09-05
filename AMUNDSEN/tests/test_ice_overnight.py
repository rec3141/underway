import importlib.util
from pathlib import Path
import tempfile
import unittest
from PIL import Image

spec = importlib.util.spec_from_file_location('overnight', Path(__file__).parents[1]/'tools/ice-overnight.py')
overnight = importlib.util.module_from_spec(spec)
spec.loader.exec_module(overnight)


class OvernightTests(unittest.TestCase):
    def test_exact_coupon_labels_are_blind_and_region_is_separate(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            Image.new('RGB',(3648,2052),'gray').save(root/'test.jpg')
            data = {'scenes':[{'scene':1,'file':'test.jpg'}]}
            labels = {'group_names':{'0':'SECRET HUMAN LABEL'},'tiles':[
                {'id':3,'file':'test.jpg','box':[100,100,200,200],'cluster':0}]}
            region,coupon = list(overnight.jobs(data,labels,root))
            self.assertEqual(len(region['images']),2)
            self.assertNotIn('human_label',region)
            self.assertEqual(coupon['box'],[100,100,200,200])
            self.assertEqual(coupon['human_label']['cluster_description'],'SECRET HUMAN LABEL')
            self.assertNotIn('SECRET',coupon['prompt'])

    def test_report_escapes_predictions(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            overnight.render(root,[dict(id='test',images=[],response='<script>',finish_reason='length')])
            self.assertIn('&lt;script&gt;',(root/'index.html').read_text())


if __name__ == '__main__': unittest.main()
