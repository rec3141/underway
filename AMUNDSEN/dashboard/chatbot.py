"""Crew members in the ship chat, played by a local model through Ollama.

Each persona has an @handle and a beat, and sees only their own slice of the
dashboard: the Cap'n has the schedule, the weather and the logistics; Doc has
the water and the air; the Librarian has the History wiki, with what happened
on this date and near the ship; Polly reports on the reporting, riffing on what
the others just said. They answer when addressed (``@capn``, ``@doc``,
``@ada``, ``@polly``) and, every so often while someone has the page open, the
one whose beat has news says something unprompted, sooner when a surprise
episode or a schedule change has just appeared; now and then Polly, or another,
riffs on what was said. All four are the one local model with different
prompts; there is one GPU.

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
LLM_API = os.environ.get("UNDERWAY_LLM_API", "ollama")
LLM_CONFIG = Path.home() / '.config/underway/chat-model.json'
CHIME_MIN_S = 45 * 60          # unprompted remarks at most this often …
EVENT_MIN_S = 15 * 60          # … except after a notable event
IDLE_S = 30 * 60               # only while someone has had the page open this recently
BANTER_P = 0.4                 # chance another crew member riffs on a crew remark (one hop only)
MAX_TOKENS = 500
NUM_CTX = 16384               # room for the dashboard summary and a long chat
TIMEOUT = 240

PERSONAS = {
    "capn": {"name": "Cap'n Barnacle", "emoji": "🏴‍☠️", "beat": "schedule",
             "type": ("ESTJ, the Executive: organiser, decider, keeper of the plan; measures the day in tasks done. Kegan stage 3, "
                      "the socialised mind: the ship's standing, the crew's regard and the way things are properly done are what "
                      "the Cap'n is made of, and a plan kept is a point of honour"),
             "voice": ("an actual barnacle, a crusty old acorn barnacle cemented to the Amundsen's hull below the waterline, who "
                       "has ridden her through the Arctic for forty years and captains her in every sense but the paperwork: gruff, "
                       "salty, full of tall tales and nautical idiom, always has an opinion and a hunch, calls people 'shipmate' or "
                       "by name, never more than three sentences. Feels the sea state and the speed through the shell, filter-feeds "
                       "on the plankton going past, never sets foot on deck, having none. Happy to guess and to be wrong with style."),
             "brief": ("Your beat is the running of the ship: the operations schedule and what is next, the weather and the sea state, "
                       "the wind, the ship's speed and heading, distances and ETAs, the whiteboard, the logistics of getting the work "
                       "done. Water chemistry is Doc's, the past is the Librarian's: point people to @doc or @ada for those.")},
    "doc": {"name": "Doc", "emoji": "🔬", "beat": "environment",
            "type": ("INFP, the Mediator: the idealist naturalist who reads meaning in a number and wanders, gladly, off the point. "
                     "Kegan stage 4, the self-authoring mind: Doc has his own framework for what matters and judges the day by it, "
                     "unbothered by whether the ship agrees"),
            "voice": ("the ship's biologist in the mould of Ed Ricketts of Cannery Row: warm, unhurried, endlessly curious, a "
                      "tide-pool naturalist who sees the whole ecology in one number and drifts happily from a salinity reading to "
                      "Thoreau, Bach, beer and the holistic 'breaking through'. Quick with a back-of-the-envelope estimate done out "
                      "loud, gentle humour, generous with the young scientists, never pompous. Two to four sentences, more if the "
                      "question earns it."),
            "brief": ("Your beat is the environment the ship is moving through: the sea surface temperature, salinity, fluorescence, "
                      "oxygen, the air, the surprise score and what a change in the water means ecologically. The schedule is the "
                      "Cap'n's and the past is the Librarian's: point people to @capn or @ada for those.")},
    "ada": {"name": "Ada", "emoji": "📚", "beat": "history",
            "type": ("ISTJ, the Logistician: exact, dutiful, trusting of the record over the anecdote, and quietly delighted by a good "
                     "primary source. Kegan stage 4, the self-authoring mind: the librarian has settled principles about evidence "
                     "and provenance and applies them to captains and parrots alike"),
            "voice": ("the ship's librarian: precise, dry, fond of a date and a page number, never pompous and never long. Reads the "
                      "ship's own History wiki, which the research crew wrote from journals, logs and Inuit testimony, and says "
                      "where a thing comes from. Two to four sentences; a little more for a real question, still with the source "
                      "named."),
            "brief": ("Your beat is the past of these waters: what happened on this date in other years, who wintered or wrecked or "
                      "wandered near where the ship is now, and the people, Inuit and European, whose record it is. Answer from the "
                      "WIKI EXCERPTS below when they bear on the question, and cite the page by its title in square brackets; say so "
                      "when the wiki is silent and then give what you know, marked as such. Current readings are Doc's and the "
                      "schedule is the Cap'n's: point people to @doc or @capn for those.")},
    "polly": {"name": "Polly", "emoji": "🦜", "beat": "meta",
              "type": ("ENTP, the Debater: quick, contrary, allergic to a hedge, cannot let a claim go by unremarked. Kegan stage 5, the "
                       "self-transforming mind, in the trickster's key: Polly holds every frame at once, the Cap'n's rules, Doc's "
                       "meanings, the librarian's sources, and plays them off each other, loyal to none and fond of all"),
              "voice": ("the ship's parrot: squawky one-liners, repeats the key number twice, 'SQUAWK', 'pretty bird', mangles a "
                        "word now and then, cheeky. One or two lines at most."),
              "brief": ("Your beat is the reporting itself: you report on what the other crew members just said, echo the number "
                        "that mattered, needle a hedge, applaud a good line, notice when two of them disagree. You have no data of "
                        "your own beyond the recent chat; when asked a real question, squawk it on to @capn, @doc or @ada.")},
}
HANDLE_RX = re.compile(r"@(\w+)")


def complete(system: str, user: str, max_tokens: int = MAX_TOKENS, temperature: float = 1.0,
             num_ctx: int = NUM_CTX, timeout: int = TIMEOUT) -> str:
    """One answer from the local model. The chat crew and the History tab's
    historian both come through here, so the backend choice (Ollama, or the
    OpenAI-style server the camera pipeline runs) is made in one place."""
    import requests
    config = json.loads(LLM_CONFIG.read_text()) if LLM_CONFIG.exists() else {}
    backend = config.get('api', LLM_API)
    url = config.get('url', LLM_URL).rstrip('/')
    model = config.get('model', LLM_MODEL)
    messages = [{"role": "system", "content": system}, {"role": "user", "content": user}]
    if backend == 'openai':
        body = dict(model=model, messages=messages, stream=False, max_tokens=max_tokens, temperature=temperature,
                    chat_template_kwargs={'enable_thinking': False})
        endpoint = '/v1/chat/completions'
    elif backend == 'ollama':
        body = {"model": model, "stream": False, "think": False, "keep_alive": "3h",
                "options": {"num_predict": max_tokens, "num_ctx": num_ctx, "temperature": temperature},
                "messages": messages}
        endpoint = '/api/chat'
    else:
        raise ValueError('Unknown chat API backend')
    r = requests.post(url + endpoint, json=body, timeout=timeout)
    r.raise_for_status()
    result = r.json()
    message = result['choices'][0]['message'] if backend == 'openai' else result.get('message') or {}
    return (message.get('content') or '').strip()


_wiki_cache: dict = {"stamp": None, "pages": []}
_WORD_RX = re.compile(r"[a-zà-ÿ0-9']{3,}")
_STOP = set("the and for with that this from were was are have has had not but his her their they them then than into "
            "over under about after before between which what when where who whom whose why how does did done been being "
            "also there here these those such some any all more most much many very just only both each other".split())


def wiki_pages(root: Path) -> list[dict]:
    """Every published History page, held in memory until the build publishes anew."""
    idx = root / "data" / "history" / "index.json"
    if not idx.is_file():
        return []
    stamp = idx.stat().st_mtime
    if _wiki_cache["stamp"] == stamp:
        return _wiki_cache["pages"]
    pages = []
    for f in sorted((root / "data" / "history" / "pages").glob("*.json")):
        try:
            d = json.loads(f.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            continue
        text = re.sub(r"<[^>]+>", " ", d.get("html", ""))
        text = re.sub(r"\]\([^)]*\)", "]", text)              # link targets are noise for matching
        d["_text"] = text
        d["_words"] = _WORD_RX.findall((d.get("title", "") + " " + d.get("summary", "") + " " + text).lower())
        pages.append(d)
    _wiki_cache.update(stamp=stamp, pages=pages)
    return pages


def wiki_excerpts(root: Path, question: str, slug: str = "", limit: int = 8, budget: int = 28000) -> list[dict]:
    """The wiki pages that bear on a question, best first: matched on words,
    with the page being read and its neighbours favoured, narrative pages
    weighted up. Each comes back with an ``excerpt`` sized to the budget."""
    pages = wiki_pages(root)
    if not pages:
        return []
    q = [w for w in _WORD_RX.findall(question.lower()) if w not in _STOP]
    by_slug = {p["slug"]: p for p in pages}
    current = by_slug.get(slug)
    scored = []
    for p in pages:
        words = p["_words"]
        if not words:
            continue
        hits = sum(words.count(w) for w in q)
        title_hits = sum(1 for w in q if w in p.get("title", "").lower())
        score = hits / (len(words) ** 0.5) + 3 * title_hits
        if current and (p["slug"] == slug or p["slug"] in current.get("backlinks", []) or p["slug"] in current.get("html", "")):
            score += 2.0
        if p.get("kind") == "page":
            score *= 1.5                                    # the narrative pages carry the story
        if score > 0:
            scored.append((score, p))
    scored.sort(key=lambda x: -x[0])
    chosen = []
    if current:
        chosen.append(current)
        budget -= len(current["_text"])
    for _, p in scored:
        if p in chosen:
            continue
        if len(chosen) >= limit or budget <= 0:
            break
        chosen.append(p)
        budget -= min(len(p["_text"]), 6000)
    return [{"slug": p["slug"], "title": p["title"], "kind": p["kind"],
             "excerpt": (p["_text"] if p is current else p["_text"][:6000]).strip()} for p in chosen]


def excerpt_block(excerpts: list[dict]) -> str:
    return "\n\n".join(f"### {e['title']}  [{e['kind']} · {e['slug']}]\n{e['excerpt']}" for e in excerpts)


def history_lines(root: Path, lat, lon, now: datetime | None = None) -> list[str]:
    """Two lines for the crew's context from the published history layer:
    today's date in other years, and the nearest artifacts to the ship."""
    import math
    tl_file = root / "data" / "history" / "timeline.json"
    art_file = root / "data" / "history" / "artifacts.json"
    if not tl_file.is_file():
        return []
    now = now or datetime.now(timezone.utc)
    mmdd = now.strftime("-%m-%d")
    rows = json.loads(tl_file.read_text()).get("timeline", [])
    today = []
    for r in rows:
        if r.get("precision") != "day":
            continue
        for key, word in (("date", "began" if r.get("date_end") else ""), ("date_end", "ended")):
            d = r.get(key) or ""
            if d.endswith(mmdd):
                place = r.get("place") or ""
                where = f" at {place}" if place and place.lower() not in (r.get("label") or "").lower() else ""
                today.append(f"{d[:4]}: {r.get('label', '')}{' ' + word if word else ''}{where}")
    out = []
    if today:
        out.append("On this day in other years: " + "; ".join(sorted(today)[:5]) + ".")
    if lat is not None and lon is not None and art_file.is_file():
        def km(a, b, c, d):
            r = math.pi / 180
            x = math.sin((c - a) * r / 2) ** 2 + math.cos(a * r) * math.cos(c * r) * math.sin((d - b) * r / 2) ** 2
            return 2 * 6371 * math.asin(math.sqrt(x))
        near = []
        for a in json.loads(art_file.read_text()).get("artifacts", []):
            pts = [(w["lat"], w["lon"]) for w in a.get("waypoints", []) if w.get("lat") is not None] if a.get("type") == "track" else ([(a["lat"], a["lon"])] if a.get("lat") is not None else [])
            if pts:
                near.append((min(km(float(lat), float(lon), p[0], p[1]) for p in pts), a))
        # the dated events carry positions too: a wintering, a wreck, a death
        seen = set()
        for r in rows:
            if r.get("lat") is None or r.get("entity_kind") != "event" or r.get("label") in seen:
                continue
            seen.add(r.get("label"))
            when = r.get("date", "") + (" to " + r["date_end"] if r.get("date_end") else "")
            near.append((km(float(lat), float(lon), r["lat"], r["lon"]), {"title": r.get("label", ""), "date_text": when}))
        near.sort(key=lambda x: x[0])
        near = [(d, a) for d, a in near if d <= 300][:6]
        if near:
            out.append("History near the ship: " + "; ".join(f"{a['title']} ({a.get('date_text', '')}, {d:.0f} km away)" for d, a in near) + ".")
        # the named places, with their other names: a station, a harbour, a wintering site
        pl_file = root / "data" / "history" / "places.json"
        if pl_file.is_file():
            places = []
            for p in json.loads(pl_file.read_text()).get("places", []):
                if p.get("lat") is None:
                    continue
                names = ", ".join(n for n in (p.get("inuktitut"), p.get("historic")) if n and n != p["name"])
                places.append((km(float(lat), float(lon), p["lat"], p["lon"]), p, names))
            places.sort(key=lambda x: x[0])
            places = [x for x in places if x[0] <= 250][:5]
            if places:
                out.append("Named places near the ship: " + "; ".join(
                    f"{p['name']}{' (' + names + ')' if names else ''}, {p.get('kind') or 'place'}, {d:.0f} km" for d, p, names in places) + ".")
    return out


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
        self.pause_file = Path.home() / '.config/underway/chat-paused'
        self.enabled = os.environ.get("UNDERWAY_LLM", "1") == "1" and not self.pause_file.exists()

    # ------------------------------------------------------------ context
    def _last(self, d: dict, name: str, nd=2) -> str:
        ys = d.get("vars", {}).get(name) or []
        for y in reversed(ys):
            if y is not None:
                return _num(y, nd)
        return "n/a"

    def context(self, beat: str = "all", task: str = "") -> str:
        """The dashboard summary for one beat: the Cap'n sees the schedule and
        the weather, Doc the water, the Librarian the past, Polly nothing but
        the clock. ``all`` is everything, for tests and for a look."""
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
        if beat == "meta":
            return "\n".join(lines)
        want = {"schedule": [("Air temperature (°C)", 1), ("Relative wind speed (kn)", 0), ("Ship speed (kn)", 1), ("Bottom depth (m)", 0),
                             ("Sea state · 4σ heave (m)", 2), ("Roll & pitch RMS (°)", 2)],
                "environment": [("SST (°C)", 2), ("Salinity (PSU)", 2), ("Fluorescence (µg/L)", 2), ("Oxygen (mL/L)", 2), ("Air temperature (°C)", 1),
                                ("Bottom depth (m)", 0), ("TSG flow (V)", 2), ("TSG line warming (°C)", 2), ("Surprise · 3 h", 1), ("Surprise · 48 h", 1)],
                "history": [("Bottom depth (m)", 0), ("Ship speed (kn)", 1)]}
        keys = want.get(beat) or sorted(set(sum(want.values(), [])))
        if beat in ("all", "schedule", "environment", "history"):
            try:
                w = next(x for x in m["windows"] if x["label"] == "3h")
                d = json.loads((self.root / w["file"]).read_text())
                lines.append("Latest values: " + "; ".join(f"{k} = {self._last(d, k, nd)}" for k, nd in keys if k in d.get("vars", {})) + ".")
                if beat in ("all", "environment"):
                    sst = [y for y in d["vars"].get("SST (°C)", []) if y is not None]; sal = [y for y in d["vars"].get("Salinity (PSU)", []) if y is not None]
                    if len(sst) > 10:
                        lines.append(f"Over the last 3 h: SST {_num(min(sst))}..{_num(max(sst))} °C, salinity {_num(min(sal))}..{_num(max(sal))} PSU.")
                    note = m.get("surprise", {}).get("note", "")
                    if "flow below" in note:
                        lines.append("Note: the surprise score ignores TSG readings while the intake flow is below 0.5 V (pump off or line choked).")
            except Exception as e:              # noqa: BLE001
                lines.append(f"(window data unavailable: {e})")
        if beat in ("all", "schedule"):
            try:
                c = json.loads((self.root / "data" / "calendar.json").read_text())
                sch = c.get("schedule", {})
                rows = sch.get("rows", [])
                if rows:
                    lines.append("Operations schedule: " + "; ".join(f"{r.get('station')} {r.get('operation')} {r.get('date')} {r.get('start')}-{r.get('end')} ({r.get('status')})" for r in rows[:6]) + ".")
                if sch.get("whiteboard"):
                    lines.append(f"Whiteboard: {sch['whiteboard']}")
                ev = c.get("events", [])[-3:]
                if ev:
                    lines.append("Last logged events: " + "; ".join(f"{e.get('time_utc', '')[:16]} {e.get('station', '')} {e.get('activity', '')} {e.get('event', '')}" for e in ev) + ".")
            except Exception:                   # noqa: BLE001
                pass
        if beat in ("all", "history"):
            # the History tab's vignettes: this date in other years, and what
            # lies near the ship; then the wiki pages that bear on the task
            try:
                lines += history_lines(self.root, lat, lon)
            except Exception:                   # noqa: BLE001
                pass
            if beat == "history":
                try:
                    ex = wiki_excerpts(self.root, task or " ".join(lines[-2:]), limit=5, budget=14000)
                    if ex:
                        lines.append("\nWIKI EXCERPTS\n\n" + excerpt_block(ex))
                except Exception:               # noqa: BLE001
                    pass
        return "\n".join(lines)

    # ------------------------------------------------------------ generation
    def _generate(self, handle: str, task: str) -> str | None:
        p = PERSONAS[handle]
        chat = self.read()
        recent = "\n".join(f"{x.get('emoji', '')} {x['name']}: {x['text']}" for x in chat.get("messages", [])[-40:])
        others = ", ".join(f"@{h} ({q['name']}: {q['beat']})" for h, q in PERSONAS.items() if h != handle)
        system = (f"You are {p['name']}, {p['voice']} Your type is {p['type']}. {p['brief']} The rest of the crew: {others}. "
                  f"You are one of four crew members in the chat of the CCGS Amundsen underway "
                  f"dashboard, read by the scientists aboard, who like a laugh. The crew is mixed, and you never assume anyone's gender: "
                  f"address people by name or as shipmate, and speak of others in neutral terms unless they have said otherwise. "
                  f"Speak as your character in plain text, no markdown, no "
                  f"lists. Be entertaining first and useful second. Use everything you know: general oceanography, rules of thumb, "
                  f"astronomy, arithmetic from the numbers at hand (a saturation from temperature and salinity, sunset from the "
                  f"position and date, an ETA from speed and distance). Always give a best guess rather than a refusal, and just "
                  f"say it is a guess. The dashboard summary below is the truth about current ship readings; do not make up "
                  f"readings that are not in it, but estimate freely beyond it. Questions about anything else — history, "
                  f"science, the Arctic, life aboard, the world — you answer fully from your own knowledge, at the length the "
                  f"question deserves (a few paragraphs for a real one), still in character. The recent chat is the conversation "
                  f"so far: a follow-up refers to it, so continue rather than restart.\n\n"
                  f"DASHBOARD SUMMARY (your beat's slice)\n{self.context(p['beat'], task)}\n\nRECENT CHAT (oldest first)\n{recent}")
        text = complete(system, task, MAX_TOKENS, 1.0)
        text = re.sub(r"^\W*" + re.escape(p["name"]) + r"\s*:\s*", "", text)      # no self-labelling
        return text[:2500] or None

    def _speak(self, handle: str, task: str, banter: bool = True) -> None:
        """Generate and post one remark; sometimes another crew member then
        riffs on it (never more than one hop, so they cannot chain forever)."""
        if not self.enabled:
            return
        p = PERSONAS[handle]
        text = None
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
        if text and banter and random.random() < BANTER_P:
            # the reporting gets reported on: Polly, usually; another now and then
            other = "polly" if handle != "polly" and random.random() < 0.7 else random.choice([h for h in PERSONAS if h not in (handle, "polly")])
            time.sleep(random.uniform(8, 25))
            self._speak(other, f"{p['name']} just said in the chat: \"{text}\". Riff on it in your own voice — agree, needle them, "
                               f"correct them, or add a detail — in one or two sentences. Do not repeat their numbers back unless you dispute them.",
                        banter=False)

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
                    h = "capn" if "schedule" in event else "doc"
                    self._speak(h, f"{event}. Remark on it for the crew in your own way; be brief and cite the relevant number.")
                elif someone and now - self.last_bot > CHIME_MIN_S:
                    # the three with a beat take turns; Polly only ever reports on them
                    h = random.choice(["capn", "doc", "ada"])
                    self._speak(h, {"ada": "Peek at your slice of the summary and chime in with one short remark for the crew about the past: "
                                           "something that happened on this date in another year, or near where the ship is now, with its "
                                           "year and its source. If there is nothing, pick the most striking thing in the wiki excerpts. "
                                           "Do not greet, do not ask questions."}.get(h,
                                   "Peek at your slice of the dashboard summary and chime in with one short, characterful remark for the crew "
                                   "about the current conditions on your beat — pick one detail worth noticing and cite its number. Do not "
                                   "greet, do not ask questions."))
            except Exception as e:          # noqa: BLE001
                log.info("crew loop: %s", e)
            time.sleep(300)

    def start(self) -> None:
        if not self.enabled:
            return
        threading.Thread(target=self.loop, daemon=True).start()
        log.info("crew online: %s via %s (%s)", ", ".join("@" + h for h in PERSONAS), LLM_URL, LLM_MODEL)
