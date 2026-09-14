import pandas as pd
from dashboard.derive import build_analysis, resolve_variables


def test_winch_rates_and_units():
    index = pd.to_datetime(['2026-09-11T00:00:00Z', '2026-09-11T00:01:00Z', '2026-09-11T01:00:00Z'])
    frame = pd.DataFrame({'ctd-rosette — rosette depth (m)': [10, 40, 50],
                          '500hp — winch cable length (m)': [20, 80, 90],
                          '500hp — winch cable speed (m/min)': [60, -30, 0]}, index=index)
    resolved = resolve_variables(list(frame.columns), {c:c for c in frame.columns})
    result = build_analysis(frame, resolved, [], [], {})
    assert result.frame['Cable rate (m/s)'].tolist() == [1, -.5, 0]
    assert result.frame['Rosette rate (m/s)'].iloc[1] == .5
    assert pd.isna(result.frame['Rosette rate (m/s)'].iloc[2])
