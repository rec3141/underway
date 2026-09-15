const {test}=require('node:test');
const assert=require('node:assert/strict');
const fs=require('node:fs'),path=require('node:path'),vm=require('node:vm');
const context={window:{},URL};
vm.runInNewContext(fs.readFileSync(path.join(__dirname,'../dashboard/static/ice-charts.js'),'utf8'),context);
const {defaultChart,colour,code,safeLink}=context.window.UW.iceChartHelpers;
test('automatic chart choice never represents future charts as historical conditions',()=>{
 const charts=[{id:'new',date:'2026-09-14'},{id:'old',date:'2026-09-07'}];
 assert.equal(defaultChart(charts,'2026-09-10').id,'old');
 assert.equal(defaultChart(charts,'2026-09-14').id,'new');
 assert.equal(defaultChart(charts,'2025-09-14'),null);
 assert.equal(charts[0].id,'new');
});
test('unknown concentration is distinct from open water and source links cannot execute code',()=>{
 assert.notEqual(colour(null),colour(0));
 assert.equal(code('-9'),'—'); assert.equal(code('0'),'0');
 assert.equal(safeLink('javascript:alert(1)'),null);assert.equal(safeLink('file:///tmp/a'),null);
 assert.equal(safeLink('https://ice-glaces.ec.gc.ca/prods/sigrids/'),'https://ice-glaces.ec.gc.ca/prods/sigrids/');
});

test('same-day chart valid later than the ship record is not automatically selected',()=>{
 const charts=[{id:'today',date:'2026-09-14',valid_time:'2026-09-14T18:00:00Z'},{id:'previous',date:'2026-09-07',valid_time:'2026-09-07T18:00:00Z'}];
 assert.equal(defaultChart(charts,'2026-09-14T12:00:00Z').id,'previous');
 assert.equal(defaultChart(charts,'2026-09-14T18:00:00Z').id,'today');
});
