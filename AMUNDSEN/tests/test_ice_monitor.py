import importlib.util
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch, Mock

spec=importlib.util.spec_from_file_location('ice_monitor',Path(__file__).resolve().parents[1]/'tools/ice-monitored-review.py')
monitor=importlib.util.module_from_spec(spec)
spec.loader.exec_module(monitor)


class MonitorTests(unittest.TestCase):
    def test_hot_start_does_not_launch_and_unloads(self):
        with tempfile.TemporaryDirectory() as tmp, patch.object(monitor.sys,'argv',['monitor','--output',tmp,'--model','test']), \
                patch.object(monitor,'temperatures',return_value=(96,40,10,100)), \
                patch.object(monitor.subprocess,'Popen') as start, patch.object(monitor.subprocess,'run') as unload:
            with self.assertRaisesRegex(RuntimeError,'cutoff'):
                monitor.main()
            start.assert_not_called()
            self.assertEqual(unload.call_args.args[0][-2:],['unload','test'])

    def test_completion_preserves_status_and_telemetry(self):
        child=Mock();child.poll.return_value=0
        with tempfile.TemporaryDirectory() as tmp, patch.object(monitor.sys,'argv',['monitor','--output',tmp,'--model','test']), \
                patch.object(monitor,'temperatures',return_value=(60,45,20,100)), \
                patch.object(monitor.subprocess,'Popen',return_value=child), patch.object(monitor.subprocess,'run'):
            self.assertEqual(monitor.main(),0)
            self.assertIn('60,45,20,100',(Path(tmp)/'telemetry.csv').read_text())
            child.terminate.assert_not_called()
