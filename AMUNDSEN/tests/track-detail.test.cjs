const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync(require('node:path').join(__dirname, '../dashboard/static/app.js'), 'utf8');
function setup() {
  const context = {state: {}, store: {set() {}}, $: () => null, M: {windows: []}};
  vm.createContext(context);
  vm.runInContext(source.slice(source.indexOf('  const TRACK_STEPS'), source.indexOf('  // the ship\'s position:')) +
    '\nthis.api = {detailFor, windowFile, setTrackDetail};', context);
  return context;
}
test('all spans default to automatic viewport detail, with no setting coarser than 100 m', () => {
  const {api, state} = setup();
  api.setTrackDetail(api.detailFor(24 * 730));
  assert.equal(state.trackKm, 'auto');
  for (const km of [0.1, 0.025, 0.005, 0]) {
    api.setTrackDetail(km);
    assert.equal(state.trackKm, km);
  }
  api.setTrackDetail(20);
  assert.equal(state.trackKm, 'auto');
});
test('charts never download the full fine file, including old manifests', () => {
  const {api} = setup();
  assert.equal(api.windowFile({file: 'coarse', fine_file: 'native'}), 'coarse');
  assert.equal(api.windowFile({file: 'coarse'}), 'coarse');
});
test('merged chart windows keep their actual distance offset for native map points', () => {
  const context = vm.createContext({lastFinite: a => a.filter(Number.isFinite).at(-1)});
  vm.runInContext(source.slice(source.indexOf('  function mergeWindows('), source.indexOf('  // the graphs\' x range')) +
    '\nthis.merge = mergeWindows;', context);
  const make = (t, distances, origin) => ({t, dist_km:distances, dist_origin_km:origin,
    lat:[70,70],lon:[-60,-60],leg:[0,0],
    vars:{temperature:[1,2],'Distance travelled (km)':distances,'Time elapsed (h)':[0,1/3600e3]},n:2});
  const result = context.merge(make([1,2],[0,2],100), make([3,4],[0,1],105));
  assert.deepEqual(Array.from(result.dist_km),[0,2,5,6]);
  assert.equal(result.dist_origin_km,100);
  assert.deepEqual(Array.from(result.vars['Distance travelled (km)']),[0,2,5,6]);
  assert.deepEqual(Array.from(result.vars['Time elapsed (h)']),[0,1/3600e3,2/3600e3,3/3600e3]);
});
