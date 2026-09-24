/* Runs the status page's real inline script against an isolated DOM and clock. */
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const {test} = require('node:test');
const html = fs.readFileSync(path.join(__dirname, '../dashboard/static/status.html'), 'utf8');
const script = html.match(/<script>([\s\S]*?)<\/script>/)[1];
const now = Date.parse('2026-09-24T12:00:00Z');
class Clock extends Date {
  constructor(...args) { super(...(args.length ? args : [now])); }
  static now() { return now; }
}
class Node {
  constructor(tag) { this.tag = tag; this.children = []; this.attrs = {}; this.style = {}; this.textContent = ''; }
  setAttribute(key, value) { this.attrs[key] = value; }
  append(...nodes) { this.children.push(...nodes); }
  replaceChildren(...nodes) { this.children = nodes; }
  insertRow() { const row = new Node('tr'); this.append(row); return row; }
  insertCell() { const cell = new Node('td'); this.append(cell); return cell; }
}
const walk = node => [node, ...node.children.flatMap(walk)];
function freeze(value) {
  if (value && typeof value === 'object') { Object.values(value).forEach(freeze); Object.freeze(value); }
  return value;
}
const dayAgo = n => new Date(now - n * 86400000).toISOString().slice(0, 10);
async function render(input) {
  const snapshot = JSON.stringify(input), source = freeze(input), nodes = new Map();
  const get = id => { if (!nodes.has(id)) nodes.set(id, new Node(id)); return nodes.get(id); };
  get('days').value = '7';
  get('error').hidden = true;
  const requests = [];
  vm.runInNewContext(script, {
    document: {getElementById: get, createElement: tag => new Node(tag), createElementNS: (_, tag) => new Node(tag), createTextNode: text => Object.assign(new Node('#text'), {textContent: text})},
    Date: Clock, AbortSignal, setInterval() {},
    fetch: async (url, options) => { requests.push({url, options}); return {ok: true, json: async () => source}; },
  });
  await new Promise(resolve => setImmediate(resolve));
  assert.equal(get('error').hidden, true, get('error').textContent);
  assert.equal(requests.length, 1);
  assert.equal(requests[0].url, 'status.html?format=json');
  return {
    period(days) { get('days').value = String(days); get('days').onchange(); },
    titles() { return walk(get('view-chart')).filter(n => n.tag === 'title').map(n => n.textContent); },
    unknownLabels() { return walk(get('view-chart')).filter(n => n.attrs.class === 'lang' && n.textContent === '?').length; },
    table() { return get('views').children.map(row => row.children.map(cell => cell.textContent)); },
    note() { return get('view-language-since').textContent; },
    unchanged() { assert.equal(JSON.stringify(source), snapshot); },
  };
}
const base = () => ({checks: [], events: [], views: [], views_by_language: {rows: []}, pending_email: 0});

test('historical and partial language days retain all views in 7/30/90 day charts', async () => {
  const data = base();
  const add = (age, page, views) => data.views.push({day: dayAgo(age), page, views});
  add(0, 'underway', 10); add(0, 'casts', 4); add(1, 'wiki', 6);
  add(6, 'stations', 8); add(7, 'photos', 9); add(29, 'calendar', 11);
  add(30, 'sources', 12); add(89, 'underway', 13); add(90, 'casts', 14);
  data.views_by_language.rows = [
    {day: dayAgo(0), page: 'underway', language: 'en', views: 3},
    {day: dayAgo(0), page: 'underway', language: 'fr-CA', views: 2},
    {day: dayAgo(0), page: 'casts', language: 'en', views: 4},
    {day: dayAgo(1), page: 'wiki', language: 'en', views: 2},
    {day: dayAgo(1), page: 'wiki', language: 'fr-CA', views: 4},
  ];
  const page = await render(data);
  for (const days of [7, 30, 90]) {
    page.period(days);
    const expected = data.views.filter(row => row.day >= dayAgo(days - 1));
    const titles = page.titles();
    assert.equal(titles.reduce((n, text) => n + Number(text.match(/: (\d+)$/)[1]), 0), expected.reduce((n, row) => n + row.views, 0), `${days}-day sum`);
    assert.deepEqual(page.table(), expected.map(row => [row.day, row.page, row.views]));
    assert.ok(titles.every(text => text.slice(0, 10) >= dayAgo(days - 1)));
    assert.equal(page.unknownLabels(), days === 7 ? 2 : days === 30 ? 4 : 6);
    assert.ok(titles.includes(`${dayAgo(0)} · Language unrecorded · underway: 5`));
    assert.ok(!titles.some(text => text.includes('Language unrecorded · casts')));
    assert.ok(!titles.some(text => text.startsWith(dayAgo(1)) && text.includes('Language unrecorded')));
    page.unchanged();
  }
});

test('fully attributed language counts show no unrecorded bars', async () => {
  const data = base();
  data.views = [{day: dayAgo(0), page: 'wiki', views: 7}];
  data.views_by_language.rows = [
    {day: dayAgo(0), page: 'wiki', language: 'en', views: 3},
    {day: dayAgo(0), page: 'wiki', language: 'fr-CA', views: 4},
  ];
  const page = await render(data);
  assert.equal(page.unknownLabels(), 0);
  assert.equal(page.titles().length, 2);
  assert.match(page.note(), /0 views in this period have no recorded language/);
  page.unchanged();
});

test('totals without any language payload remain visible', async () => {
  const data = base();
  delete data.views_by_language;
  data.views = [{day: dayAgo(2), page: 'photos', views: 19}];
  const page = await render(data);
  assert.deepEqual(page.titles(), [`${dayAgo(2)} · Language unrecorded · photos: 19`]);
  assert.equal(page.unknownLabels(), 1);
  assert.match(page.note(), /19 views in this period have no recorded language/);
  page.unchanged();
});
