import importlib.util
import json
import shutil
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('ui_catalog', ROOT/'tools/build-ui-catalog.py')
catalog = importlib.util.module_from_spec(spec)
spec.loader.exec_module(catalog)

class CatalogTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        shutil.copytree(ROOT/'locales', self.root/'locales')
        self.variant = self.root/'locales/variants/fr-CA/editorial-fr-ca-v1.json'

    def edit(self, path, change):
        data = json.loads(path.read_text()); change(data)
        path.write_text(json.dumps(data, ensure_ascii=False))

    def test_generated_browser_asset_is_current(self):
        data, coverage = catalog.compile_catalog()
        self.assertEqual((ROOT/'dashboard/static/i18n-catalog.js').read_text(), catalog.javascript(data))
        self.assertFalse(coverage['fr-CA']['fallbacks'])

    def test_source_edit_rejects_stale_translation(self):
        self.edit(self.root/'locales/en.json', lambda d:d['messages']['nav.map'].update(text='Ocean map'))
        data, coverage = catalog.compile_catalog(self.root)
        self.assertNotIn('nav.map',data['locales']['fr-CA']['messages'])
        self.assertEqual(coverage['fr-CA']['fallbacks']['nav.map'],'stale')

    def test_placeholders_must_survive(self):
        self.edit(self.variant,lambda d:d['messages']['feedback.retry'].update(text='Erreur'))
        with self.assertRaisesRegex(ValueError,'placeholders changed'):catalog.compile_catalog(self.root)

    def test_multiple_translators_and_selection_do_not_overwrite_candidates(self):
        alt = json.loads(self.variant.read_text()); alt['profile']['id']='alternate'
        alt['messages']['nav.map']['text']='Carte alternative'
        path=self.variant.parent/'alternate.json'; path.write_text(json.dumps(alt))
        self.edit(self.root/'locales/selection.json',lambda d:d['locales']['fr-CA']['overrides'].update({'nav.map':'alternate'}))
        data,_=catalog.compile_catalog(self.root)
        self.assertEqual(data['locales']['fr-CA']['messages']['nav.map'],'Carte alternative')
        self.assertEqual(data['locales']['fr-CA']['provenance']['nav.map']['profile'],'alternate')
        self.assertEqual(json.loads(self.variant.read_text())['messages']['nav.map']['text'],'Carte')

if __name__ == '__main__':unittest.main()
