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
            (root / 'dashboard/templates/index.html.j2').write_text('{{ m.track.version }} {{ site.default_window }} {{ site.asset_version }}')
            (root / 'static').mkdir()
            (root / 'static/app.js').write_text('new script')
            manifest = root / 'data/manifest.json'
            manifest.write_text(json.dumps({'default_window': 'leg', 'track': {'version': 1}, 'cameras': ['private']}))
            (root / 'index.html').write_text('window.__SITE__ = {};')
            env = dict(os.environ, HERE=str(root), MIRROR=str(root), PY='/bin/false', TRACK_PY=sys.executable)
            result = subprocess.run(['bash', '-ec', function + '\npublic_manifest'], env=env, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            rendered = (root / 'index.html').read_text()
            self.assertTrue(rendered.startswith('1 leg '))
            self.assertRegex(rendered.split()[-1], r'^[0-9a-f]{10}$')
            self.assertEqual(json.loads(manifest.read_text())['cameras'], [])
            self.assertTrue(json.loads(manifest.read_text())['public'])

    def test_public_json_compression_and_chunk_cache_configuration(self):
        script = (Path(__file__).resolve().parents[1] / 'tools/publish-web.sh').read_text()
        function = script[script.index('web_files() {'):script.index('public_manifest() {')]
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            env = dict(os.environ, MIRROR=str(root), TARGET='example:/site/underway')
            subprocess.run(['bash', '-ec', function + '\nweb_files'], env=env, check=True)
            self.assertIn('AddOutputFilterByType DEFLATE application/json application/geo+json', (root / '.htaccess').read_text())
            self.assertIn('public, max-age=31536000, immutable', (root / 'data/track/.htaccess').read_text())
