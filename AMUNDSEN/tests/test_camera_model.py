import unittest
from dashboard.camera_model import infer

class CameraModelTests(unittest.TestCase):
    def model(self):
        return dict(id='test',kind='human-label-presence',feature_size=[600,300],geometry='original-roi-v1',
                    outputs=['any ice'],mean=[0]*89,scale=[1]*89,
                    trees=[dict(left=[1,-1,-1],right=[2,-1,-1],feature=[0,-2,-2],threshold=[.5,-2,-2],value=[[.5],[.1],[.9]])])
    def test_split_and_contract(self):
        model=self.model();self.assertAlmostEqual(infer([0]*89,model)['scores']['any ice'],.1)
        self.assertAlmostEqual(infer([1]*89,model)['scores']['any ice'],.9)
        self.assertTrue(infer([8]*89,model)['outside_training_features'])
        model['feature_size']=[240,120]
        with self.assertRaises(ValueError):infer([0]*89,model)
    def test_float32_split_boundary(self):
        model=self.model();model['trees'][0]['threshold'][0]=0.5
        # Above the threshold as float64, but exactly 0.5 at sklearn precision.
        x=[0.0]*89;x[0]=0.500000001
        self.assertAlmostEqual(infer(x,model)['scores']['any ice'],.1)

    def test_threshold_keeps_float64_precision(self):
        model=self.model();model['trees'][0]['threshold'][0]=0.499999999
        x=[0.0]*89;x[0]=0.5
        self.assertAlmostEqual(infer(x,model)['scores']['any ice'],.9)

if __name__=='__main__':unittest.main()
