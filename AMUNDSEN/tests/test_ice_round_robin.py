import importlib.util
from pathlib import Path
import unittest

spec=importlib.util.spec_from_file_location('rr',Path(__file__).resolve().parents[1]/'tools/ice-round-robin-queue.py')
rr=importlib.util.module_from_spec(spec);spec.loader.exec_module(rr)

class RoundRobinTests(unittest.TestCase):
    def test_completed_preserved_and_uneven_clusters(self):
        queue=[{'id':str(i),'file':str(i)} for i in range(7)]
        clusters=dict(zip(map(str,range(7)),[0,0,0,1,1,2,2]))
        result=rr.reorder(queue,clusters,{'0','5'})
        self.assertEqual([r['file'] for r in result],['0','5','1','3','6','2','4'])
        self.assertEqual(len({r['file'] for r in result}),7)
