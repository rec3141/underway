import importlib.util
from pathlib import Path
import unittest

spec=importlib.util.spec_from_file_location('taxonomy',Path(__file__).resolve().parents[1]/'tools/ice-taxonomy.py')
taxonomy=importlib.util.module_from_spec(spec);spec.loader.exec_module(taxonomy)

class TaxonomyTests(unittest.TestCase):
    def test_size_evidence_is_opt_in_and_roi_only(self):
        base=['context','key','roi']
        self.assertNotIn('piece_size_assessment',' '.join(taxonomy.build_prompt(base)))
        result=taxonomy.build_prompt(base,size_evidence=True)
        self.assertNotIn('piece_size_assessment',' '.join(result[:2]))
        self.assertIn('piece_size_assessment',result[2])
        self.assertIn('null if unresolvable',result[2])
        self.assertIn('interval crosses 40 m',result[2])

    def test_single_pass_renames(self):
        self.assertEqual(taxonomy.rename('thin fyi, ice floe, smooth water'),'thin ice floe, thick ice floe, calm water')

    def test_scale_only_introduced_with_roi(self):
        texts=taxonomy.build_prompt(['Image 1 context','old key','Image 3 ROI; thin fyi and ice floe'])
        self.assertNotIn('50 m',texts[0]);self.assertNotIn('50 m',texts[1])
        self.assertIn('50 m',texts[2]);self.assertIn('factor-of-two',texts[2])
        self.assertIn('LESS\n  THAN 40 m',texts[1]);self.assertIn('MORE THAN 40 m',texts[1])
        self.assertNotIn('thin thick ice floe',' '.join(texts))

if __name__=='__main__':unittest.main()
