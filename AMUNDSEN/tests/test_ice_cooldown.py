import importlib.util
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

spec=importlib.util.spec_from_file_location('overnight',Path(__file__).parents[1]/'tools/ice-overnight.py')
mod=importlib.util.module_from_spec(spec);spec.loader.exec_module(mod)


class CooldownTests(unittest.TestCase):
    def test_spike_is_not_sustained_heat(self):
        avg=mod.RunningTemperature()
        self.assertEqual(avg.add(0,70),70)
        avg.add(117,99)
        self.assertAlmostEqual(avg.add(120,70),70.725)

    def test_window_trims_and_weights_irregular_intervals(self):
        avg=mod.RunningTemperature();avg.add(0,60);avg.add(100,96)
        self.assertEqual(avg.add(220,96),96)

    def test_disappearing_process_is_skipped(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);(root/'1').mkdir();good=root/'2';(good/'task'/'100').mkdir(parents=True)
            (good/'cmdline').write_bytes(b'/home/test/.lmstudio/extensions/backends/llama-server\0--model\0/data/scratch/models/lmstudio-community/Qwen3.5-9B-GGUF/Qwen3.5-9B-Q4_K_M.gguf\0')
            with patch.object(mod.os,'sched_getaffinity',return_value=set(range(8))),patch.object(mod.os,'sched_setaffinity') as affinity:
                mod.limit_qwen_cpu(root);affinity.assert_called_once_with(100,{0,1,2,3})


if __name__=='__main__':unittest.main()
