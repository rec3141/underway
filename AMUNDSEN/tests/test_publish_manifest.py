"""Public page rendering uses the dashboard environment, not the history one."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


class PublicManifestTests(unittest.TestCase):
    def test_history_manifest_uses_grid_export_and_preserves_other_fields(self):
        script = (Path(__file__).resolve().parents[1] / 'tools/publish-web.sh').read_text()
        function = script[script.index('history_layer() {'):script.index('publish_locale_data() {')]
        for entry in (
            {'pages': 42, 'locales': {'fr-CA': {'base': 'data/history/locales/fr-CA/grid/'}}},
            {'pages': 42},
        ):
            with self.subTest(entry=entry), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                (root / 'db/history').mkdir(parents=True)
                (root / 'db/history/history.sqlite').touch()
                (root / 'data').mkdir()
                (root / 'dashboard').mkdir()
                (root / 'dashboard/__init__.py').touch()
                (root / 'dashboard/history.py').write_text(
                    'import json, os\ndef publish(root):\n'
                    '    return json.loads(os.environ["EXPORT_ENTRY"])\n')
                original = {'default_window': 'leg', 'track': {'version': 1},
                            'history': {'pages': 10, 'locales': {'fr-CA': {'base': 'ship/'}}},
                            'cameras': ['private']}
                manifest = root / 'data/manifest.json'
                manifest.write_text(json.dumps(original))
                manifest.chmod(0o644)
                env = dict(os.environ, HERE=str(root), HIST=str(root), MIRROR=str(root),
                           PY=sys.executable, EXPORT_ENTRY=json.dumps(entry))
                result = subprocess.run(['bash', '-ec', function + '\nhistory_layer'],
                                        cwd=root, env=env, capture_output=True, text=True)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(json.loads(manifest.read_text()), dict(original, history=entry))
                self.assertEqual(manifest.stat().st_mode & 0o777, 0o644)
                self.assertEqual(list((root / 'data').glob('*.tmp')), [])

    def test_locale_payload_transfer_precedes_advertisement_and_retains_old_revisions(self):
        script = (Path(__file__).resolve().parents[1] / 'tools/publish-web.sh').read_text()
        function = script[script.index('publish_locale_data() {'):script.index('static_assets() {')]
        deployment = script[script.index('  deploy|rebuild-deploy)'):]
        self.assertLess(deployment.index('    publish_locale_data\n'), deployment.index('    $RSYNC --delete'))
        self.assertIn("--exclude 'data/history/locales/'", deployment)
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'data/history/locales/fr-CA/revision').mkdir(parents=True)
            env = dict(os.environ, MIRROR=str(root), TARGET='example:/site/underway', RSYNC='mock_rsync')
            mocks = ('ssh() { printf "ssh:%s\\n" "$*"; }; '
                     'mock_rsync() { printf "rsync:%s\\n" "$*"; return "${MOCK_FAILURE:-0}"; }; '
                     'stats() { cat; };\n')
            command = mocks + function + '\npublish_locale_data\nprintf "advertise\\n"\n'
            result = subprocess.run(['bash', '-euc', 'set -o pipefail\n' + command],
                                    env=env, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn('/data/history/locales/', result.stdout)
            self.assertNotIn('--delete', result.stdout)
            self.assertTrue(result.stdout.endswith('advertise\n'))
            result = subprocess.run(['bash', '-euc', 'set -o pipefail\n' + command],
                                    env=dict(env, MOCK_FAILURE='1'), capture_output=True, text=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertNotIn('advertise', result.stdout)

    def test_dashboard_python_renders_current_track_manifest(self):
        script = (Path(__file__).resolve().parents[1] / 'tools/publish-web.sh').read_text()
        function = script[script.index('public_manifest() {'):script.index('case "${1:-}" in')]
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'data').mkdir()
            (root / 'dashboard/templates').mkdir(parents=True)
            (root / 'dashboard/templates/index.html.j2').write_text('{{ m.track.version }} {{ site.default_window }} {{ m.history.locales["fr-CA"].base }} {{ site.asset_version }}')
            (root / 'static').mkdir()
            (root / 'static/app.js').write_text('new script')
            manifest = root / 'data/manifest.json'
            manifest.write_text(json.dumps({'default_window': 'leg', 'track': {'version': 1}, 'cameras': ['private'],
                                            'history': {'locales': {'fr-CA': {'base': 'data/history/locales/fr-CA/grid/'}}}}))
            (root / 'index.html').write_text('window.__SITE__ = {};')
            env = dict(os.environ, HERE=str(root), MIRROR=str(root), PY='/bin/false', TRACK_PY=sys.executable)
            result = subprocess.run(['bash', '-ec', function + '\npublic_manifest'], env=env, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            rendered = (root / 'index.html').read_text()
            self.assertTrue(rendered.startswith('1 leg data/history/locales/fr-CA/grid/ '))
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
