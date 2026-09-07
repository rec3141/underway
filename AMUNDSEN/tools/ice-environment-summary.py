"""Local, descriptive camera-label/environment joins; no independent-photo p-values."""
import json
from html import escape
from pathlib import Path
import sqlite3
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import pandas as pd
from dashboard.config import DB_DIR


def main():
    source = Path('/home/cryomics/Downloads/amundsen-ice-qwen-k32-classes/labels-clean.json')
    output = Path('/home/cryomics/Downloads/amundsen-ice-environment')
    output.mkdir(exist_ok=True)
    labels = json.loads(source.read_text())
    photos = pd.DataFrame(labels['labels'])
    photos['time'] = pd.to_datetime(photos.file.str.extract(r'Camera360_(\d{14})_')[0], format='%Y%m%d%H%M%S', utc=True)
    # Exact instrument names, resolved through the archive metadata, not column offsets.
    wanted = {
        'water_temperature_C': 'TSG — Hull temperature (deg C)',
        'salinity_psu': 'TSG — Salinity (psu)',
        'air_temperature_C': 'AVOS — Air temperature (deg C)',
        'relative_wind_knots': 'AVOS — Relative wind speed (knt)',
        'visibility_m': 'ATS_CS125 — Visibility distance (m)',
        'latitude': 'POSMV — Latitude (deg N)',
        'longitude': 'POSMV — Longitude (deg E)',
    }
    with sqlite3.connect(f'file:{DB_DIR / "2025_LEG_04.db"}?mode=ro', uri=True) as db:
        columns = {display: col for col, display in db.execute('SELECT col,display FROM columns')}
        selected = {name: columns[display] for name, display in wanted.items()}
        query = 'SELECT t,' + ','.join(f'"{col}" AS "{name}"' for name, col in selected.items()) + ' FROM obs ORDER BY t'
        env = pd.read_sql_query(query, db)
    env['observation_time'] = pd.to_datetime(env.pop('t'), unit='s', utc=True).astype('datetime64[us, UTC]')
    env = env.groupby('observation_time', as_index=False).median(numeric_only=True)
    joined = pd.merge_asof(photos.sort_values('time'), env, left_on='time', right_on='observation_time', direction='nearest', tolerance=pd.Timedelta(seconds=120))
    joined['match_seconds'] = (joined.time - joined.observation_time).dt.total_seconds().abs()
    joined['day'] = joined.time.dt.strftime('%Y-%m-%d')
    report = dict(source=str(source), archive=str(DB_DIR / '2025_LEG_04.db'), photos=len(joined), matched=int(joined.observation_time.notna().sum()), tolerance_seconds=120,
                  caveats=['Human labels are overlapping presence, not area fractions.',
                           'TSG flow is unverified: this archive has no pump voltage column. Raw sensor values are not independently QC validated.',
                           'Descriptive associations only; neighboring photos and batch labels are correlated.',
                           'Day-median summaries give each represented day equal weight, but geography and season remain confounded.',
                           'Relative wind includes ship motion; it is not true wind.'], labels={})
    for label in labels['labelOrder']:
        group = joined[joined.labels.map(lambda values: label in values)]
        daily = group.groupby('day')[list(wanted)].median()
        report['labels'][label] = dict(photos=len(group), days=len(daily), variables={})
        for name in wanted:
            values = group[name].dropna()
            day_values = daily[name].dropna()
            report['labels'][label]['variables'][name] = dict(n=len(values), median=float(values.median()) if len(values) else None,
                p10=float(values.quantile(.1)) if len(values) else None, p90=float(values.quantile(.9)) if len(values) else None,
                median_of_day_medians=float(day_values.median()) if len(day_values) else None)
    joined.assign(labels=joined.labels.map(lambda v: '|'.join(v))).to_csv(output / 'photo-environment.csv', index=False)
    (output / 'summary.json').write_text(json.dumps(report, indent=2, allow_nan=False))
    lines = ['# Ice labels and underway conditions', '', f'{report["matched"]:,} / {len(joined):,} photos matched within 120 seconds.', '', *['- '+s for s in report['caveats']], '', '| Label | Photos | Days | Water °C median (10–90%) | Day-balanced water °C |', '|---|---:|---:|---:|---:|']
    for name, row in report['labels'].items():
        v = row['variables']['water_temperature_C']
        if v['n']:
            lines.append(f'| {name} | {row["photos"]} | {row["days"]} | {v["median"]:.2f} ({v["p10"]:.2f}–{v["p90"]:.2f}) | {v["median_of_day_medians"]:.2f} |')
    (output / 'README.md').write_text('\n'.join(lines)+'\n')
    sections = []
    for variable in wanted:
        rows = []
        for label, group in report['labels'].items():
            v = group['variables'][variable]
            if not v['n']:
                continue
            rows.append(f'<tr><th scope="row">{escape(label)}</th><td>{v["n"]}</td><td>{group["days"]}</td>'
                        f'<td>{v["median"]:.2f}</td><td>{v["p10"]:.2f}–{v["p90"]:.2f}</td>'
                        f'<td>{v["median_of_day_medians"]:.2f}</td></tr>')
        sections.append(f'<h2>{escape(variable.replace("_", " "))}</h2><div class="table"><table>'
                        '<thead><tr><th>Label</th><th>Valid photos</th><th>Label days</th><th>Median</th>'
                        '<th>10–90% range</th><th>Median of day medians</th></tr></thead><tbody>'
                        + ''.join(rows) + '</tbody></table></div>')
    html = ('<!doctype html><html lang="en"><meta charset="utf-8">'
            '<meta name="viewport" content="width=device-width,initial-scale=1">'
            '<title>Ice labels and underway conditions</title><style>'
            'body{font:16px system-ui,sans-serif;max-width:1100px;margin:2rem auto;padding:0 1rem;color:#182a35;background:#f5f8fa}'
            'li{margin:.5rem 0}.table{overflow-x:auto}table{border-collapse:collapse;width:100%;background:white}'
            'th,td{padding:.6rem;text-align:right;border-bottom:1px solid #d7e0e6}th:first-child{text-align:left}'
            'h2{margin-top:2rem}a{color:#075c99}</style><h1>Ice labels and underway conditions</h1>'
            f'<p>{report["matched"]:,} / {len(joined):,} photos matched within 120 seconds.</p><ul>'
            + ''.join('<li>'+escape(c)+'</li>' for c in report['caveats']) + '</ul>'
            + ''.join(sections) + '</html>')
    (output / 'index.html').write_text(html)
    # Short standalone browser entry point avoids wrapped long paths in the chat harness.
    (output.parent / 'ice.html').write_text(html)
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
