import runpy
from pathlib import Path
import unittest

try:
    import numpy as np
    from PIL import Image
    MODULE = runpy.run_path(str(Path(__file__).resolve().parents[1]/'tools/ice-texture.py'))
except ImportError:
    MODULE = None


@unittest.skipIf(MODULE is None, 'Optional texture dependencies not installed')
class TextureFeaturesTests(unittest.TestCase):
    def test_brightness_distinguishes_uniform_surfaces(self):
        dark = Image.new('RGB',(100,100),(20,20,20))
        light = Image.new('RGB',(100,100),(220,220,220))
        np.testing.assert_allclose(MODULE['texture'](dark),MODULE['texture'](light),atol=1e-10)
        self.assertGreater(MODULE['brightness'](light)[0]-MODULE['brightness'](dark)[0],.7)

    def test_features_finite_and_dimensions_stable(self):
        pixels = np.random.default_rng(42).integers(0,256,(100,100,3),dtype=np.uint8)
        image = Image.fromarray(pixels)
        self.assertEqual(len(MODULE['texture'](image)),80)
        levels = MODULE['brightness'](image)
        self.assertEqual(len(levels),7)
        self.assertTrue(np.isfinite(MODULE['texture'](image)).all())
        self.assertTrue(all(0<=v<=1 for v in levels))
