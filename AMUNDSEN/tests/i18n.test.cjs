const {test} = require('node:test');
const assert = require('node:assert/strict');
const vm = require('node:vm');
const fs = require('node:fs');
const path = require('node:path');
const script = fs.readFileSync(path.join(__dirname,'../dashboard/static/i18n.js'),'utf8');
function setup(search='', stored=null) {
  const document={readyState:'loading',addEventListener(){},documentElement:{lang:'en'},querySelectorAll(){return []}};
  const window={UW_UI_CATALOG:{sourceLocale:'en',locales:{
    en:{messages:{'pages.example':'Label {value}', 'pages.only':'Source-only {value}', 'variable.salinity':'Salinity (PSU)',hello:'Hello {name}',only:'English only',bottles:{one:'{count} bottle',other:'{count} bottles'},fallback:{one:'{count} bottle',other:'{count} bottles'}}},
    'fr-CA':{messages:{'pages.example':'<b>"Libellé"</b> {value}', 'variable.salinity':'Salinité (PSU)',hello:'Bonjour {name}',bottles:{one:'{count} bouteille',other:'{count} bouteilles'}}}
  }},dispatchEvent(){}};
  const context={window,document,localStorage:{getItem(){return stored},setItem(k,v){stored=v}},location:{search,href:'https://example.test/underway/'+search},history:{replaceState(){}},URL,URLSearchParams,Intl,CustomEvent:class {constructor(type,options){this.type=type;this.detail=options.detail}}};
  vm.runInNewContext(script,context);
  return window.UWI18n;
}
test('URL language wins over saved choice; missing messages use English',()=>{
  const i18n=setup('?lang=fr-CA','"en"');
  assert.equal(i18n.locale,'fr-CA'); assert.equal(i18n.t('only'),'English only');
  assert.equal(i18n.t('hello',{name:'<script>x</script>'}),'Bonjour <script>x</script>');
  assert.equal(i18n.t('__proto__'),'__proto__');
  assert.equal(i18n.setLocale('xx-invalid'),false); assert.equal(i18n.locale,'fr-CA');
});
test('plural rules follow message language, including English fallback',()=>{
  const i18n=setup('?lang=fr-CA');
  assert.equal(i18n.t('bottles',{count:0}),'0 bouteille');
  assert.equal(i18n.t('bottles',{count:2}),'2 bouteilles');
  assert.equal(i18n.t('fallback',{count:0}),'0 bottles');
  i18n.setLocale('en'); assert.equal(i18n.t('bottles',{count:0}),'0 bottles');
});
test('bad saved language safely falls back; valid saved language persists',()=>{
  assert.equal(setup('','invalid JSON').locale,'en');
  assert.equal(setup('','"fr-CA"').locale,'fr-CA');
});
test('measurement translation is display-only, with unknown and missing catalog fallbacks',()=>{
  const i18n=setup('?lang=fr-CA');
  assert.equal(i18n.variable('Salinity (PSU)'),'Salinité (PSU)');
  assert.equal(i18n.variable('SST (°C)'),'SST (°C)');
  assert.equal(i18n.variable('custom instrument (V)'),'custom instrument (V)');
  assert.equal(i18n.variable('__proto__'),'__proto__');
  i18n.setLocale('en');
  assert.equal(i18n.variable('Salinity (PSU)'),'Salinity (PSU)');
});
test('authored source messages retain fallback and escape translator markup before trusted interpolation',()=>{
  const i18n=setup('?lang=fr-CA');
  assert.equal(i18n.text('Label {value}',{value:3}),'<b>"Libellé"</b> 3');
  assert.equal(i18n.html('Label {value}',{value:'<em>known markup</em>'}),'&lt;b&gt;&quot;Libellé&quot;&lt;/b&gt; <em>known markup</em>');
  assert.equal(i18n.text('Source-only {value}',{value:0}),'Source-only 0');
  assert.equal(i18n.text('Unknown {value}',{value:'<user text>'}),'Unknown <user text>');
  i18n.setLocale('en');assert.equal(i18n.text('Label {value}',{value:3}),'Label 3');
});
