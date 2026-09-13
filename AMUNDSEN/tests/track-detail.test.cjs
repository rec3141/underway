const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync(require('node:path').join(__dirname, '../dashboard/static/app.js'), 'utf8');
function setup() {
  const context = {state: {}, store: {set() {}}, $: () => null, M: {windows: []}};
  vm.createContext(context);
  vm.runInContext(source.slice(source.indexOf('  const TRACK_STEPS'), source.indexOf('  // the ship\'s position:')) +
    '\nthis.api = {detailFor, windowFile, setTrackDetail, thinTrack};', context);
  return context;
}
test('long spans set comfortable defaults but allow every finer choice', () => {
  const {api, state} = setup();
  api.setTrackDetail(api.detailFor(24 * 730));
  assert.equal(state.trackKm, 20);
  for (const km of [5, 1, 0.5, 0]) {
    api.setTrackDetail(km);
    assert.equal(state.trackKm, km);
    assert.equal(api.windowFile({file: 'coarse', fine_file: 'native'}), 'native');
  }
});
test('spacing preserves aligned values, gap starts, and the final fix without a point cap', () => {
  const {api} = setup();
  const n = 50000;
  const data = {t: Array.from({length:n}, (_,i) => i), lat: Array(n).fill(70),
    dist_km: Array.from({length:n}, (_,i) => i / 2), vars: {temperature: Array.from({length:n}, (_,i) => i)}};
  assert.equal(api.thinTrack(data, 0), data);
  assert.equal(api.thinTrack(data, 0.5).shown, n);
  data.lat[1] = null;
  const result = api.thinTrack(data, 5);
  assert.deepEqual(Array.from(result.t.slice(0,3)), [0,1,2]);
  assert.equal(result.t.at(-1), n-1);
  assert.deepEqual(Array.from(result.vars.temperature), Array.from(result.t));
});
