import importlib.util
from pathlib import Path
import unittest
import numpy as np

spec=importlib.util.spec_from_file_location('reps',Path(__file__).parents[1]/'tools/ice-cluster-representatives.py')
reps=importlib.util.module_from_spec(spec);spec.loader.exec_module(reps)


class RepresentativeTests(unittest.TestCase):
    def test_distinct_up_to_three_per_cluster(self):
        coords=np.array([[0,0],[0,1],[0,2],[0,3],[10,0],[20,0],[20,1]])
        rows=reps.representatives(coords,np.array([0,0,0,0,1,2,2]))
        self.assertEqual(len(rows),6)
        self.assertEqual(len({r[1] for r in rows}),6)
        self.assertEqual([sum(r[0]==c for r in rows) for c in range(3)],[3,1,2])


if __name__=='__main__':unittest.main()
