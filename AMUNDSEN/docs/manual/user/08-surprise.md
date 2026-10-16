# The surprise score

The surprise score answers one question about the sea water under the ship:
**does the water right now look different from the water the ship has just
been in?** It is a single number, worked out every minute, that stays near
zero while nothing changes and jumps when something does: the ship crosses a
front, runs into river water, enters a bloom, or a sensor misbehaves.

You find it at the top of the [Underway tab](underway.html), in the
**Surprise** card.

## What it looks at

The score watches five measurements together:

- the sea surface temperature (**SST (°C)**),
- the salinity (**Salinity (PSU)**),
- the fluorescence, a sign of phytoplankton (**Fluorescence (µg/L)**),
- the dissolved oxygen (**Oxygen (mL/L)**),
- the air temperature (**Air temperature (°C)**).

It does not look at each one alone. It looks at how they usually go
together. Water that is a bit warmer *and* a bit fresher *and* greener than
usual all at once is more surprising than water where just one of them
wobbles.

## How to read the number

| Score | What it means |
|---|---|
| 0 to 1 | Nothing unusual. Most of the time the score is here. |
| 1 to 2 | A small change: worth a glance. |
| 2 to 3 | A clear change. |
| above 3 | Very unusual: the water is unlike anything in the recent past. The graph shades this band. |
| 6 | The top of the scale. Each time scale stops at 6. |

The number counts zeros. A score of **1** means "about one minute in ten
would look this different by chance"; **2**, one in a hundred; **3**, one in
a thousand. Each step up is ten times rarer.

!!! note
    A high score means *different*, not *wrong* or *important*. It points you
    to a moment worth looking at. Then look at the Lab panels to see which
    measurement changed, and at the map to see where.

## The time scales

"Different from the recent past" depends on how far back you look. The score
is worked out five times, each with its own memory:

| Row in the card | Compares the water with |
|---|---|
| **15 min** | roughly the last quarter of an hour |
| **1 h** | roughly the last hour |
| **3 h** | roughly the last three hours |
| **12 h** | roughly the last half day |
| **48 h** | roughly the last two days |
| **Combined** | the average of the five |

The memories are not hard cut-offs: recent minutes count more, older ones
fade away gradually.

The short scales react at once when the ship crosses into new water, then
calm down within minutes, because they quickly get used to it. The long
scales stay raised as long as the ship is in water unlike that of the last
day or two. So the **Combined** score jumps at a crossing and then fades
slowly.

## The Surprise card and its panels

![The Surprise card: Combined and the five time scales, each with its latest value](img/08-surprise-card.webp)

The card works like every other group card (see
[The Underway tab](underway.html#the-group-cards)):

- Each row shows the newest value of one score.
- Click a row to open its panel. Click again to fold it.
- The **▲** at the top right opens all six panels at once.

The **Combined** panel is the one most people keep open. Here it is over a
month of Leg 3 (September 2026), with **Show last** set to **1 mo**.

![The combined surprise score over September 2026: mostly below 1, with a few spikes into the shaded band above 3](img/08-surprise-month.webp)

The shaded band at the top marks scores above 3. Most of the month is
calm, with a dozen or so moments that stand out.

To find out what happened at a spike:

1. Hold the mouse over the spike to read its time.
2. Click it. The map moves to where the ship was at that moment.
3. Open **Salinity (PSU)**, **SST (°C)** and the other Lab panels. They
   share the time axis, so the spike lines up with the change that caused
   it.
4. Zoom in on the spike (select the panel, then scroll) to see it minute by
   minute.

## Example: fresh water at the surface

On 10 September 2026 the ship was working at the GF stations in Leg 3. In
the afternoon it left the station and steamed off at 10 knots.

![The combined score, the 15 min and 48 h scales, and salinity on 10 September 2026](img/08-surprise-example.webp)

Read the four panels from the bottom up:

- **Salinity (PSU)**: at about 14:20 ship time, the salinity falls from about
  30.5 to under 23 PSU in a quarter of an hour. The ship has run into a
  fresh surface layer.
- **Surprise · 48 h**: reaches nearly 3. Compared with the last two days,
  this water is very unusual.
- **Surprise · 15 min**: hits the top of the scale, 6, at the moment of the
  drop, and is back to zero fifteen minutes later. It has already got used
  to the new water.
- **Surprise (−log10 p)** (Combined): about 3.8, then fading.

At about 16:00 the ship crosses fresh water a second time and the salinity
falls almost as far. This time the combined score barely moves: the ship
was in that water an hour ago, so it is no longer a surprise. That is the
score doing its job: it flags the *first* time something new turns up.

## Things that are not the sea

The score sees everything the sensors report, including problems:

- **The intake pump stops.** The water in the pipe warms and changes slowly,
  and when the pump restarts the sea water arrives all at once. Both can
  score high. Check the **TSG flow (V)** panel and look for grey points (see
  [Grey points: the pump was off](underway.html#grey-points-the-pump-was-off)).
- **A sensor is cleaned or replaced.** A sudden step in one variable alone
  can mean work in the lab rather than a change in the sea.
- **The data stop.** After a gap with no data, the score needs a while to
  learn the water again before it gives values.

!!! tip
    The **Underway Updates** calendar on the [Schedule tab](schedule.html)
    lists pump stops and surprising episodes, so you can find them without
    scrolling through the graph.

## How it is worked out (optional)

For readers who want the method: each minute, the five measurements are
put on a common scale. For each time scale, the dashboard keeps a running,
exponentially weighted average and covariance of the minutes *before* the
current one, with a half-life of 15 minutes, 1 hour, 3 hours, 12 hours or
48 hours. It measures how far the current minute lies from that average,
allowing for how the variables usually vary together (a Mahalanobis
distance), and turns that distance into a probability with a chi-square
test. The score is −log10 of that probability, capped at 6; the combined
score is the mean over the five scales. Fluorescence is taken on a log
scale, so that a bloom does not dominate. A minute with some sensors
missing is scored on the ones it has.

## In short

- The surprise score says how different the water is from the water just
  before. 0 is ordinary; above 3 (shaded) is very unusual; 6 is the top.
- Five time scales, from 15 minutes to 48 hours; **Combined** is their
  average.
- A spike is a pointer: open the Lab panels and the map to see what changed.
- Pump stops and sensor work can score high too.
