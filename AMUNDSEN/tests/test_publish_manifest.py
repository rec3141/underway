"""Public page rendering uses the dashboard environment, not the history one."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


class PublicManifestTests(unittest.TestCase):
    def test_dashboard_python_renders_current_track_manifest(self):
        script = (Path(__file__).resolve().parents[1] / 'tools/publish-web.sh').read_text()
        function = script[script.index('public_manifest() {'):script.index('case "${1:-}" in')]
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'data').mkdir()
            (root / 'dashboard/templates').mkdir(parents=True)
            (root / 'dashboard/templates/index.html.j2').write_text('{{ m.track.version }} {{ site.default_window }}')
            manifest = root / 'data/manifest.json'
            manifest.write_text(json.dumps({'default_window': 'leg', 'track': {'version': 1}, 'cameras': ['private']}))
            (root / 'index.html').write_text('window.__SITE__ = {};')
            env = dict(os.environ, HERE=str(root), MIRROR=str(root), PY='/bin/false', TRACK_PY=sys.executable)
            result = subprocess.run(['bash', '-ec', function + '\npublic_manifest'], env=env, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual((root / 'index.html').read_text(), '1 leg')
            self.assertEqual(json.loads(manifest.read_text())['cameras'], [])
            self.assertTrue(json.loads(manifest.read_text())['public'])
