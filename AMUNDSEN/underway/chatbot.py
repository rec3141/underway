"""Crew members in the ship chat, played by a local model through Ollama.

Each persona has an @handle. They answer when addressed (``@capn``,
``@polly``, ``@doc``) and, every so often while someone has the page open,
one of them peeks at the current data and says something unprompted — sooner
when a surprise episode or a schedule change has just appeared. They see a
compact summary of what the dashboard shows (latest values, position, intake
flow, surprise, schedule, whiteboard) and the recent chat.

Everything here is best-effort: no Ollama, no GPU, or a slow reply simply
means silence. One generation runs at a time.
"""

from __future__ import annotations

import json
import logging
import os
import random
import re
import threading
import time
from datetime import datetime, timezone
from pathlib import Path

log = logging.getLogger(__name__)

LLM_URL = os.environ.get("UNDERWAY_LLM_URL", "http://127.0.0.1:11434")
LLM_MODEL = os.environ.get("UNDERWAY_LLM_MODEL", "gemma4-local")
CHIME_MIN_S = 45 * 60          # unprompted remarks at most this often …
EVENT_MIN_S = 15 * 60          # … except after a notable event
IDLE_S = 30 * 60               # only while someone has had the page open this recently
MAX_TOKENS = 500
NUM_CTX = 16384               # room for the dashboard summary and a long chat
TIMEOUT = 240

PERSONAS = {
    "capn": {"name": "Cap'n Barnacle", "emoji": "🏴‍☠️",
             "voice": ("a swarthy old sea captain who has sailed the Arctic for forty years: gruff, salty, full of tall tales and "
                       "nautical idiom, always has an opinion and a hunch, calls people 'lad' or 'lass', never more than three "
                       "sentences. Happy to guess and to be wrong with style.")},
    "polly": {"name": "Polly", "emoji": "🦜",
              "voice": ("the ship's parrot: squawky one-liners, repeats the key number twice, 'SQUAWK', 'pretty bird', mangles a "
                        "word now and then, cheeky. One or two lines at most.")},
    "doc": {"name": "Doc", "emoji": "🔬",
            "voice": ("the ship's biologist in the mould of Ed Ricketts of Cannery Row: warm, unhurried, endlessly curious, a "
                      "tide-pool naturalist who sees the whole ecology in one number and drifts happily from a salinity reading to "
                      "Thoreau, Bach, beer and the holistic 'breaking through'. Quick with a back-of-the-envelope estimate done out "
                      "loud, gentle humour, generous with the young scientists, never pompous. Two to four sentences, more if the "
                      "question earns it.")},
}
HANDLE_RX = re.compile(r"@(\w+)")


def _num(x, nd=2):
    try:
        return f"{float(x):.{nd}f}"
    except (TypeError, ValueError):
        return "?"


class Crew:
    def __init__(self, root: Path, post, read):
        self.root = root
        self.post = post                    # (name, emoji, text) -> None
        self.read = read                    # () -> dict with messages/online
        self.lock = threading.Lock()        # one generation at a time
        self.typing: set[str] = set()
        self.last_bot = 0.0
        self.seen_update = None
        self.seen_surprise = None
        self.enabled = os.environ.get("UNDERWAY_LLM", "1") == "1"

    # ------------------------------------------------------------ context
    def _last(self, d: dict, name: str, nd=2) -> str:
        ys = d.get("vars", {}).get(name) or []
        for y in reversed(ys):
            if y is not None:
                return _num(y, nd)
        return "n/a"

    def context(self) -> str:
        try:
            m = json.loads((self.root / "data" / "manifest.json").read_text())
        except Exception:                   # noqa: BLE001
            return "The dashboard data is unavailable right now."
        lines = []
        end = m.get("data_range", {}).get("end", "")
        lat = m.get("latest", {}).get("lat"); lon = m.get("latest", {}).get("lon")
        live = next((l["label"] for l in m.get("legs", []) if l.get("id") == m.get("live")), None)
        from zoneinfo import ZoneInfo
        local = datetime.now(ZoneInfo("America/Toronto")).strftime("%Y-%m-%d %H:%M %Z")
        lines.append(f"Now (UTC): {datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M')} (ship time {local}). Latest data at {end[:16].replace('T', ' ')} UTC"
                     + (f", position {_num(lat, 3)}, {_num(lon, 3)}" if lat is not None else "") + (f", leg {live}" if live else "") + ".")
        try:
            w = next(x for x in m["windows"] if x["label"] == "3h")
            d = json.loads((self.root / w["file"]).read_text())
            keys = [("SST (°C)", 2), ("Salinity (PSU)", 2), ("Fluorescence (µg/L)", 2), ("Oxygen (mL/L)", 2), ("Air temperature (°C)", 1),
                    ("Relative wind speed (kn)", 0), ("Ship speed (kn)", 1), ("Bottom depth (m)", 0), ("TSG flow (V)", 2), ("TSG line warming (°C)", 2),
                    ("Sea state · 4σ heave (m)", 2), ("Roll & pitch RMS (°)", 2), ("Surprise · 3 h", 1), ("Surprise · 48 h", 1)]
            lines.append("Latest values: " + "; ".join(f"{k} = {self._last(d, k, nd)}" for k, nd in keys if k in d.get("vars", {})) + ".")
            sst = [y for y in d["vars"].get("SST (°C)", []) if y is not None]; sal = [y for y in d["vars"].get("Salinity (PSU)", []) if y is not None]
            if len(sst) > 10:
                lines.append(f"Over the last 3 h: SST {_num(min(sst))}..{_num(max(sst))} °C, salinity {_num(min(sal))}..{_num(max(sal))} PSU.")
        except Exception as e:              # noqa: BLE001
            lines.append(f"(window data unavailable: {e})")
        try:
            c = json.loads((self.root / "data" / "calendar.json").read_text())
            s = c.get("schedule", {})
            rows = s.get("rows", [])
            if rows:
                lines.append("Operations schedule: " + "; ".join(f"{r.get('station')} {r.get('operation')} {r.get('date')} {r.get('start')}-{r.get('end')} ({r.get('status')})" for r in rows[:6]) + ".")
            if s.get("whiteboard"):
                lines.append(f"Whiteboard: {s['whiteboard']}")
            ev = c.get("events", [])[-3:]
            if ev:
                lines.append("Last logged events: " + "; ".join(f"{e.get('time_utc', '')[:16]} {e.get('station', '')} {e.get('activity', '')} {e.get('event', '')}" for e in ev) + ".")
        except Exception:                   # noqa: BLE001
            pass
        note = m.get("surprise", {}).get("note", "")
        if "flow below" in note:
            lines.append("Note: the surprise score ignores TSG readings while the intake flow is below 0.5 V (pump off or line choked).")
        return "\n".join(lines)

    # ------------------------------------------------------------ generation
    def _generate(self, handle: str, task: str) -> str | None:
        import requests
        p = PERSONAS[handle]
        chat = self.read()
        recent = "\n".join(f"{x.get('emoji', '')} {x['name']}: {x['text']}" for x in chat.get("messages", [])[-40:])
        system = (f"You are {p['name']}, {p['voice']} You are one of several crew members in the chat of the CCGS Amundsen underway "
                  f"dashboard, read by the scientists aboard, who like a laugh. Speak as your character in plain text, no markdown, no "
                  f"lists. Be entertaining first and useful second. Use everything you know: general oceanography, rules of thumb, "
                  f"astronomy, arithmetic from the numbers at hand (a saturation from temperature and salinity, sunset from the "
                  f"position and date, an ETA from speed and distance). Always give a best guess rather than a refusal, and just "
                  f"say it is a guess. The dashboard summary below is the truth about current ship readings; do not make up "
                  f"readings that are not in it, but estimate freely beyond it. Questions about anything else — history, "
                  f"science, the Arctic, life aboard, the world — you answer fully from your own knowledge, at the length the "
                  f"question deserves (a few paragraphs for a real one), still in character. The recent chat is the conversation "
                  f"so far: a follow-up refers to it, so continue rather than restart.\n\n"
                  f"DASHBOARD SUMMARY\n{self.context()}\n\nRECENT CHAT (oldest first)\n{recent}")
        body = {"model": LLM_MODEL, "stream": False, "think": False, "keep_alive": "3h",
                "options": {"num_predict": MAX_TOKENS, "num_ctx": NUM_CTX, "temperature": 1.0},
                "messages": [{"role": "system", "content": system}, {"role": "user", "content": task}]}
        r = requests.post(f"{LLM_URL}/api/chat", json=body, timeout=TIMEOUT)
        r.raise_for_status()
        text = (r.json().get("message") or {}).get("content", "").strip()
        text = re.sub(r"^\W*" + re.escape(p["name"]) + r"\s*:\s*", "", text)      # no self-labelling
        return text[:2500] or None

    def _speak(self, handle: str, task: str) -> None:
        if not self.enabled:
            return
        p = PERSONAS[handle]
        with self.lock:
            self.typing.add(handle)
            try:
                text = self._generate(handle, task)
                if text:
                    self.post(p["name"], p["emoji"], text)
                    self.last_bot = time.time()
            except Exception as e:          # noqa: BLE001
                log.info("crew %s stayed quiet (%s)", handle, e)
            finally:
                self.typing.discard(handle)

    # ------------------------------------------------------------ triggers
    def on_message(self, name: str, text: str) -> None:
        """Called after a human message is stored."""
        if name in {p["name"] for p in PERSONAS.values()}:
            return
        handles = [h.lower() for h in HANDLE_RX.findall(text)]
        if "crew" in handles or "all" in handles:
            handles = list(PERSONAS)
        for h in dict.fromkeys(h for h in handles if h in PERSONAS):
            threading.Thread(target=self._speak, args=(h, f"{name} just wrote: \"{text}\". Reply to them as yourself."), daemon=True).start()

    def _events(self) -> str | None:
        """A notable change since the last look, as a short description, or None."""
        try:
            m = json.loads((self.root / "data" / "manifest.json").read_text())
        except Exception:                   # noqa: BLE001
            return None
        upd = (m.get("calendar") or {}).get("update") or {}
        if upd.get("changed_utc") and upd["changed_utc"] != self.seen_update:
            first = self.seen_update is None
            self.seen_update = upd["changed_utc"]
            if not first:
                return f"The intranet schedule page just changed: {upd.get('text', '')}"
        try:
            w = next(x for x in m["windows"] if x["label"] == "3h")
            d = json.loads((self.root / w["file"]).read_text())
            s3 = [y for y in d["vars"].get("Surprise · 3 h", []) if y is not None]
            hot = s3 and max(s3[-10:]) > 2.5
            key = (d.get("end") or "")[:13] if hot else None
            if hot and key != self.seen_surprise:
                first = self.seen_surprise is None
                self.seen_surprise = key
                if not first:
                    return f"The 3 h surprise score just rose to {max(s3[-10:]):.1f} — something in the water (or the intake) changed."
        except Exception:                   # noqa: BLE001
            pass
        return None

    def loop(self) -> None:
        time.sleep(90)
        while True:
            try:
                now = time.time()
                chat = self.read()
                someone = bool(chat.get("online"))
                event = self._events()
                if someone and event and now - self.last_bot > EVENT_MIN_S:
                    h = random.choice(list(PERSONAS))
                    self._speak(h, f"{event}. Remark on it for the crew in your own way; be brief and cite the relevant number.")
                elif someone and now - self.last_bot > CHIME_MIN_S:
                    h = random.choice(list(PERSONAS))
                    self._speak(h, "Peek at the dashboard summary and chime in with one short, characterful remark for the crew about the "
                                   "current conditions — pick one detail worth noticing and cite its number. Do not greet, do not ask questions.")
            except Exception as e:          # noqa: BLE001
                log.info("crew loop: %s", e)
            time.sleep(300)

    def start(self) -> None:
        if not self.enabled:
            return
        threading.Thread(target=self.loop, daemon=True).start()
        log.info("crew online: %s via %s (%s)", ", ".join("@" + h for h in PERSONAS), LLM_URL, LLM_MODEL)
