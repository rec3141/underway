"""Read-only summary for the unlinked status page."""
import json
from datetime import datetime, timezone

from . import uptime, usage


def report():
    try:
        state = json.loads((uptime.DB_DIR / 'uptime.json').read_text())
    except (OSError, ValueError):
        state = {}
    checked = state.get('checked_at')
    try:
        stale = (datetime.now(timezone.utc) - datetime.fromisoformat(checked)).total_seconds() > 180
    except (TypeError, ValueError):
        stale = True
    return {'checked_at': checked, 'stale': stale,
            'checks': [{'url': url, 'ok': check.get('result', {}).get('ok'),
                        'ms': check.get('result', {}).get('ms'), 'failures': check.get('failures', 0),
                        'down': check.get('down', False), 'last_check': check.get('last_check')}
                       for url, check in state.get('checks', {}).items()],
            'events': state.get('events', []), 'pending_email': len(state.get('pending', [])),
            'views': usage.report(), 'ips': usage.report_ips()}
