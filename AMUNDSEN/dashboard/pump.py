"""Observed low-flow periods; missing telemetry is not evidence of pump-off."""
import pandas as pd

from .config import LOW_FLOW_V

FLOW = "TSG flow (V)"
MIN_MINUTES = 10        # shorter dips still flag their bins on the plots, but make no event


def pump_events(frame, legs, min_minutes: int = MIN_MINUTES):
    """One event per run of minutes with the intake flow below the cutoff,
    per leg; runs shorter than ``min_minutes`` (a transition, a blip while
    priming) are left out so the calendars only carry real stops."""
    if frame is None or FLOW not in frame or "leg" not in frame:
        return []
    events = []
    # Flow is available at minute resolution, even when underway data are faster.
    for code, part in frame.groupby("leg"):
        if not 0 <= int(code) < len(legs):
            continue
        flow = part[FLOW].sort_index().resample("1min").mean()
        low = flow.notna() & (flow < LOW_FLOW_V)
        starts = low & ~low.shift(fill_value=False)
        ends = flow.index[low & ~low.shift(-1, fill_value=False)] + pd.Timedelta(minutes=1)
        for start, end in zip(flow.index[starts], ends):
            if (end - start) < pd.Timedelta(minutes=min_minutes):
                continue
            reason = "flow restored" if pd.notna(flow.get(end)) else "flow coverage ended"
            ident = f"pump|{legs[int(code)].id}|{start.isoformat()}"
            events.append({"id": ident, "leg": legs[int(code)].id,
                           "time_utc": start.isoformat(), "end_utc": end.isoformat(),
                           "activity": "TSG pump", "event": "Pump off / low intake flow",
                           "comment": f"Intake flow below {LOW_FLOW_V:g} V (minute means); {reason}. "
                                      "Pump stopped or intake restricted; TSG measurements retained but flagged."})
    return events
