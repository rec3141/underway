"""Resumable 50 m radar archive along the recorded ship track; no model work."""
import argparse
from datetime import datetime, timedelta, timezone
import json
import logging
import math
import os
from pathlib import Path
import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from dashboard import satellite as sat


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--since', required=True)
    parser.add_argument('--track', type=Path, required=True)
    args = parser.parse_args()
    logging.basicConfig(level=logging.INFO, format='%(asctime)s %(message)s')
    since = datetime.fromisoformat(args.since).replace(tzinfo=timezone.utc)
    now = datetime.now(timezone.utc)
    track = json.loads(args.track.read_text())
    groups = {}
    for t, lat, lon in zip(track['t'], track['lat'], track['lon']):
        if lat is None or lon is None:
            continue
        stamp = datetime.fromtimestamp(t / 1000, timezone.utc)
        if since <= stamp <= now:
            groups.setdefault(stamp.date().isoformat(), []).append((lat, lon))
    if not groups:
        raise SystemExit('No ship positions in the requested interval')
    creds = sat.credentials()
    if not creds:
        raise SystemExit('Satellite credentials unavailable')
    token = sat.token(creds)
    sat.preserve_near_cache()
    kind = 's1near'
    sp = sat.SENSORS[kind]
    failures = 0
    for date, positions in sorted(groups.items()):
        start = datetime.fromisoformat(date).replace(tzinfo=timezone.utc)
        end = min(start + timedelta(days=1), now)
        centres = [positions[0]]
        for position in positions[1:]:
            if sat.distance_km(*position, *centres[-1]) >= sat.NEAR_MOVE_KM:
                centres.append(position)
        for lat, lon in centres:
            bbox = sat.box_around(lat, lon, sp['box_km'])
            bounds = sat.corners(bbox)
            try:
                token = sat.token(creds)  # short-lived tokens; renders can be slow
                scene = sat.newest_scene(token, kind, bbox, start, end, raise_errors=True)
                if not scene:
                    logging.info('%s %.3f %.3f: no radar scene', date, lat, lon)
                    continue
                info = sat.load_info()
                if any(e.get('scene') == scene and e.get('corners') == bounds for e in info.get('archive', {}).get(kind, [])):
                    logging.info('%s %.3f %.3f: already retained', date, lat, lon)
                    continue
                data, cost, size = sat.render(token, kind, bbox, end, 50 / math.cos(math.radians(lat)))
                # Reload after the network request, retaining any intervening updates.
                with sat.archive_lock():
                    info = sat.load_info()
                    sat.archive(info, kind, data, scene, now, metadata=dict(corners=bounds, centre=[lat, lon], size=list(size), label=sp['label'], days=sp['days']))
                    info['cost_pu_total'] = round(float(info.get('cost_pu_total') or 0) + cost, 2)
                    temp = sat.sat_dir() / 'sat.json.tmp'
                    temp.write_text(json.dumps(info, indent=1))
                    os.replace(temp, sat.sat_dir() / 'sat.json')
                logging.info('%s %.3f %.3f: archived %s, %d bytes, %.1f PU', date, lat, lon, scene, len(data), cost)
            except Exception:
                failures += 1
                logging.exception('%s %.3f %.3f failed', date, lat, lon)
    if failures:
        raise SystemExit(f'{failures} boxes failed; rerun to retry')


if __name__ == '__main__':
    main()
