"""Exercise deployment restart decisions with local Git repos and a fake systemctl."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

SCRIPT = Path(__file__).resolve().parents[1] / 'tools/deploy-pull.sh'


class DeployPullTests(unittest.TestCase):
    def check_change(self, files, restart, codex_enabled=True):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            upstream, deployed, bins = (base / name for name in ('upstream', 'deployed', 'bin'))
            upstream.mkdir()
            bins.mkdir()
            calls = base / 'systemctl.log'
            fake = bins / 'systemctl'
            fake.write_text('#!/bin/sh\n'
                            'printf "%s\\n" "$*" >> "$TEST_SYSTEMCTL_LOG"\n'
                            'if [ "$1" = is-enabled ]; then exit "$TEST_CODEX_STATUS"; fi\n'
                            'exit 0\n')
            fake.chmod(0o755)
            env = dict(os.environ, PATH=str(bins) + ':' + os.environ['PATH'],
                       UNDERWAY_APP=str(deployed), TEST_SYSTEMCTL_LOG=str(calls),
                       TEST_CODEX_STATUS='0' if codex_enabled else '1',
                       GIT_CONFIG_NOSYSTEM='1', GIT_CONFIG_GLOBAL=os.devnull)

            def git(repo, *args):
                return subprocess.run(['git', '-C', str(repo), *args], env=env,
                                      capture_output=True, text=True, check=True).stdout.strip()

            git(upstream, 'init', '-q', '-b', 'master')
            git(upstream, 'config', 'user.name', 'Deployment test')
            git(upstream, 'config', 'user.email', 'deploy-test@example.invalid')
            (upstream / 'README.md').write_text('Test repository\n')
            git(upstream, 'add', '.')
            git(upstream, 'commit', '-qm', 'Initial fixture')
            git(base, 'clone', '-q', str(upstream), str(deployed))
            before = git(deployed, 'rev-parse', 'HEAD')
            for name in files:
                target = upstream / name
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_text('# Fixture change\n')
            git(upstream, 'add', '.')
            git(upstream, 'commit', '-qm', 'Deploy fixture changes')
            expected = git(upstream, 'rev-parse', 'HEAD')
            result = subprocess.run(['bash', str(SCRIPT)], env=env, capture_output=True,
                                    text=True, timeout=20)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertNotEqual(before, expected)
            self.assertEqual(git(deployed, 'rev-parse', 'HEAD'), expected)
            self.assertEqual(git(deployed, 'status', '--porcelain'), '')
            for name in files:
                self.assertEqual((deployed / name).read_text(), '# Fixture change\n')
            observed = calls.read_text().splitlines() if calls.exists() else []
            expected_calls = []
            if restart:
                expected_calls = ['restart underway-dashboard underway-telegram',
                                  'is-enabled --quiet underway-codex.service']
                if codex_enabled:
                    expected_calls.append('restart underway-codex.service')
            self.assertEqual(observed, expected_calls)
            # An unchanged checkout must not repeat any service operations.
            again = subprocess.run(['bash', str(SCRIPT)], env=env, capture_output=True,
                                   text=True, timeout=20)
            self.assertEqual(again.returncode, 0, again.stderr)
            self.assertEqual(calls.read_text().splitlines() if calls.exists() else [], expected_calls)

    def test_cast_import_only_fast_forwards_without_service_operations(self):
        self.check_change(['AMUNDSEN/dashboard/casts.py'], restart=False)

    def test_other_dashboard_python_restarts_serving_processes(self):
        self.check_change(['AMUNDSEN/dashboard/serve.py'], restart=True)

    def test_mixed_cast_and_live_python_still_restarts(self):
        self.check_change(['AMUNDSEN/dashboard/casts.py', 'AMUNDSEN/dashboard/live.py'], restart=True)

    def test_disabled_codex_is_not_restarted(self):
        self.check_change(['AMUNDSEN/dashboard/live.py'], restart=True, codex_enabled=False)

    def test_static_only_changes_do_not_restart(self):
        self.check_change(['AMUNDSEN/dashboard/static/app.js'], restart=False)


if __name__ == '__main__':
    unittest.main()
