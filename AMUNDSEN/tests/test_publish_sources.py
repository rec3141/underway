"""Exercise real rsync filters against isolated local source/destination trees."""
import os
import shlex
from pathlib import Path
import subprocess
import tempfile
import unittest

SCRIPT = Path(__file__).resolve().parents[1] / 'tools/publish-sources.sh'


class SourceSyncTests(unittest.TestCase):
    def test_selection_and_remote_deletion_are_scoped_to_sources(self):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            source, target, db, bins = (base / n for n in ('ship', 'grid/source', 'db', 'bin'))
            for p in (source, target, db, bins):
                p.mkdir(parents=True, exist_ok=True)
            def put(root, name, text='data'):
                p = root / name; p.parent.mkdir(parents=True, exist_ok=True); p.write_text(text)
            keep = ['Data/FULL_CSV/2026_LEG_03/ACSD_20260913.csv', 'Share/2025/2025_LEG_01/ACSD_20250701.csv',
                    'Data/TSG/2026_LEG_03/tsg_convdata_20260913.cnv', 'Data/MVP/profile.m1',
                    'Data/EventLog/Eventlog_2026.xls', 'Data/Rosette/Logs/log.csv']
            reject = ['Data/external_proprietary/CTD/cast.cnv', 'Data/Rosette/cast.CNV',
                      'Data/Rosette/plots/cast_raw_data.html', 'data/w-4y-fine.json']
            for name in keep + reject: put(source, name)
            for name in reject: put(target, name, 'old copy')
            put(db, 'live_scrape/20260913.jsonl'); put(db, 'provisional_tsg.csv'); put(db, 'alerts.json', 'private')
            put(target.parent, 'www/index.html', 'public site')
            put(target, 'runtime/live_scrape/20260913.jsonl', 'old')
            put(target, 'runtime/alerts.json', 'unwanted')
            (bins/'ssh').write_text('#!/bin/sh\nshift\nexec sh -c "$*"\n')
            (bins/'rsync').write_text('#!/usr/bin/python3\nimport os,sys\na=[x.split(":",1)[1] if x.startswith("grid:") else x for x in sys.argv[1:]]\nos.execv("/usr/bin/rsync",["rsync"]+a)\n')
            for p in bins.iterdir(): p.chmod(0o755)
            env = dict(os.environ, PATH=str(bins)+':'+os.environ['PATH'], UNDERWAY_SITE=str(base/'absent'),
                       UNDERWAY_MIRROR=str(source), UNDERWAY_DB_DIR=str(db), UNDERWAY_PUBLISH_SOURCE_REMOTE='grid:'+str(target))
            preview = subprocess.run([str(SCRIPT), '--dry-run'], env=env, capture_output=True, text=True)
            self.assertEqual(preview.returncode, 0, preview.stderr)
            self.assertTrue((target/reject[0]).exists())
            result = subprocess.run([str(SCRIPT)], env=env, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            for name in keep: self.assertTrue((target/name).exists(), name)
            for name in reject: self.assertFalse((target/name).exists(), name)
            self.assertEqual((target.parent/'www/index.html').read_text(), 'public site')
            self.assertTrue((target/'runtime/live_scrape/20260913.jsonl').exists())
            self.assertFalse((target/'runtime/alerts.json').exists())
            self.assertTrue((source/reject[0]).exists())

    def test_site_push_excludes_generated_chunks_and_preserves_grid_data(self):
        # Exercise the actual push filter arguments with real local rsync.
        script = SCRIPT.with_name('publish-web.sh').read_text().replace('\\\n', ' ')
        command = next(line.strip() for line in script.splitlines() if line.strip().startswith('$RSYNC --delete '))
        words = shlex.split(command)
        filters = words[1:words.index('$WEBROOT/')]
        with tempfile.TemporaryDirectory() as tmp:
            source, target = Path(tmp) / 'ship', Path(tmp) / 'grid'
            for root in (source, target):
                (root / 'data/track').mkdir(parents=True)
            (source / 'index.html').write_text('new page')
            (source / 'data/manifest.json').write_text('new metadata')
            (source / 'data/w-1h.json').write_text('ship chart')
            (source / 'data/track/ship.json').write_text('large generated chunk')
            (target / 'data/w-1h.json').write_text('grid chart')
            (target / 'data/track/grid.json').write_text('grid chunk')
            result = subprocess.run(['/usr/bin/rsync', '-a', *filters, str(source) + '/', str(target) + '/'], capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual((target / 'index.html').read_text(), 'new page')
            self.assertEqual((target / 'data/manifest.json').read_text(), 'new metadata')
            self.assertEqual((target / 'data/w-1h.json').read_text(), 'grid chart')
            self.assertEqual((target / 'data/track/grid.json').read_text(), 'grid chunk')
            self.assertFalse((target / 'data/track/ship.json').exists())
