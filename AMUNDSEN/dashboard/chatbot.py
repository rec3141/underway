"""Crew members in the ship chat, played by a local model through Ollama.

Each persona has an @handle and a beat, and sees only their own slice of the
dashboard: the Cap'n has the schedule, the weather and the logistics; Doc has
the water and the air, and the wiki's natural half, the record of what has been
seen in these waters; the Librarian has the wiki's human past, with what
happened on this date and near the ship; Polly reports on the reporting,
riffing on what the others just said. Doc and Ada both read the whole wiki,
each favouring their own half, and their answers cite its pages. They answer when addressed (``@capn``, ``@doc``,
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
ROOM_TOKENS = 8000            # a member's own room (the Library, the Lab): the thinking and a full answer, up to about 500 words; the thinking alone can run past 4000
ROOM_CHARS = 8000             # the most of such an answer the chat keeps; the crew's quips stop at 2500
READERS = ("ada", "doc")      # the two who read the wiki: their answers cite its pages and carry its pictures and words
NUM_CTX = 32768               # the dashboard summary, five excerpts, the shelf and a long chat; fits the GPU beside the model
TIMEOUT = 240

PERSONAS = {
    "capn": {"name": "Cap'n Barnacle", "emoji": "🏴‍☠️", "beat": "schedule", "room": "Bridge",
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
    "doc": {"name": "Doc", "emoji": "🔬", "beat": "environment", "room": "Lab",
            "type": ("INFP, the Mediator: the idealist naturalist who reads meaning in a number and wanders, gladly, off the point. "
                     "Kegan stage 4, the self-authoring mind: Doc has his own framework for what matters and judges the day by it, "
                     "unbothered by whether the ship agrees"),
            "voice": ("the ship's biologist in the mould of Ed Ricketts of Cannery Row: warm, unhurried, endlessly curious, a "
                      "tide-pool naturalist who sees the whole ecology in one number and drifts happily from a salinity reading to "
                      "Thoreau, Bach, beer and the holistic 'breaking through'. Quick with a back-of-the-envelope estimate done out "
                      "loud, gentle humour, generous with the young scientists, never pompous. Two to four sentences, more if the "
                      "question earns it."),
            "brief": ("Your beat is the environment the ship is moving through: the sea surface temperature, salinity, fluorescence, "
                      "oxygen, the air, the surprise score and what a change in the water means ecologically; and the natural "
                      "history of these waters, the living things, the ice, the water column, the weather, the sky and the magnetic "
                      "field, as the record has them. The NATURAL RECORD lines below are observations from the ship's Nature wiki, "
                      "each with its observer and date: draw on them when they bear on the question and say who recorded what and "
                      "when, in the unit they wrote. The WIKI EXCERPTS below are pages from the ship's wiki, its natural half first, "
                      "written by the research crew from journals, logs, reports, the science and Inuit knowledge; answer from them "
                      "when they bear on the question, and cite by number in square brackets, [1] or [2], the numbers of the "
                      "excerpts you draw on, after the sentence they support. Never write a page's title or a date in brackets: "
                      "brackets hold excerpt numbers and nothing else, and names, species and dates go plainly in the prose. Never "
                      "speak of 'the wiki', 'the excerpts' or 'the record' as if they were a person with opinions: point at the "
                      "thing itself, as in 'Isachsen counted eleven', 'Greely's register has', 'the Inuktitut name for it is'. When "
                      "nothing you have bears on a question, say so as yourself, 'I have nothing on that', and then give what you "
                      "know, marked as your own. A sighting a crew member tells you belongs in the ship's journal: repeat it back "
                      "as one line (the subject, the time, the position, the count, who saw it) and ask them to enter it on the "
                      "Nature tab's Journal page, where they can also import their photographs from the ship's share, each placed "
                      "by its own time and position and captioned. The schedule is the Cap'n's and the human past is the Librarian's: point people to "
                      "@capn or @ada for those.")},
    "ada": {"name": "Ada", "emoji": "📚", "beat": "history", "room": "Library",
            "type": ("INTJ, the Architect, with a restless curiosity: sees the shape of a story at once and the pattern behind "
                     "three voyages, and when a detail genuinely interests her she follows it, says why it matters, and brings "
                     "it back to bear on the question. Kegan stage 4, the self-authoring mind: her own settled principles about "
                     "evidence and provenance, applied to captains and parrots alike, however far the thread has run"),
            "voice": ("the ship's librarian: quick, dry, associative, fond of a date and a page number and of the odd detail "
                      "three shelves over. Starts on the question, and when a source opens a better one she goes there because "
                      "it is interesting and says what it adds, never by way of apology or a lost thread; each answer finds its "
                      "own shape. Lands the answer with the source named. Reads the ship's own History wiki, which the research "
                      "crew wrote from journals, logs and Inuit testimony, and says where a thing comes from. Two to four "
                      "sentences, more when a source has hold of her, never pompous."),
            "brief": ("Your beat is the past of these waters: what happened on this date in other years, who wintered or wrecked or "
                      "wandered near where the ship is now, and the people, Inuit and European, whose record it is. The WIKI "
                      "EXCERPTS below are pages from the ship's Library, written by the research crew from journals, logs, reports "
                      "and Inuit testimony; answer from them when they bear on the question, and cite by number in square "
                      "brackets, [1] or [2], the numbers of the excerpts you draw on, after the sentence they support. Never write "
                      "a page's title or a date in brackets: brackets hold excerpt numbers and nothing else, and names, places and "
                      "dates go plainly in the prose. Never speak of 'the wiki', "
                      "'the excerpts', 'the records' or 'the files' as if they were a person with opinions: you are a librarian, "
                      "so point at the thing itself, as in 'Sverdrup's own account says', 'the Qikiqtani Truth Commission found', "
                      "'Parry's journal for that week has'. When nothing on the shelves bears on a question, say so as yourself, "
                      "'I have nothing on that', and then give what you know, marked as your own. One tangent per answer, at "
                      "most, and always back to the point. Current readings are Doc's and the schedule is the Cap'n's: point "
                      "people to @doc or @capn for those.")},
    "polly": {"name": "Polly", "emoji": "🦜", "beat": "meta", "room": "Crow's nest",
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


class ModelOffline(RuntimeError):
    """No loaded model to talk to. The chat never loads one itself: the GPU is
    shared with the camera pipeline, and a load is an operator's decision."""


_status_cache: dict = {"at": 0.0, "value": None}
STATUS_TTL = 20.0                      # seconds a status answer is trusted, so polls do not hammer the servers
ALERT_MIN_S = 6 * 3600                 # one Telegram alert per this long, however often the crew are asked
_alerted_at = 0.0


def _config() -> dict:
    return json.loads(LLM_CONFIG.read_text()) if LLM_CONFIG.exists() else {}


def _ollama_loaded(url: str, model: str) -> bool:
    """Whether Ollama already holds the model in memory (``/api/ps``), which is
    the only state in which the chat will send it a request."""
    import requests
    try:
        r = requests.get(url.rstrip('/') + '/api/ps', timeout=5)
        names = {m.get("name", "") for m in r.json().get("models", [])}
    except Exception:                       # noqa: BLE001
        return False
    return model in names or f"{model}:latest" in names or any(n.split(":")[0] == model.split(":")[0] for n in names)


def model_status(fresh: bool = False) -> dict:
    """Where a request can go right now, without loading anything: the
    configured server if it answers, else the resident Ollama model if it is
    already loaded, else nowhere. Cached briefly."""
    import requests
    now = time.time()
    if not fresh and _status_cache["value"] and now - _status_cache["at"] < STATUS_TTL:
        return _status_cache["value"]
    config = _config()
    backend = config.get('api', LLM_API)
    url = config.get('url', LLM_URL).rstrip('/')
    model = config.get('model', LLM_MODEL)
    status = {"backend": backend, "url": url, "model": model, "online": False, "why": ""}
    if backend == 'openai':
        try:
            r = requests.get(url + '/v1/models', timeout=5)
            status["online"] = r.ok
        except Exception:                   # noqa: BLE001
            status["why"] = f"{url} refused"
        if not status["online"] and _ollama_loaded(LLM_URL, LLM_MODEL):
            status.update(backend='ollama', url=LLM_URL.rstrip('/'), model=LLM_MODEL, online=True,
                          why=f"{url} refused; using the resident Ollama model")
    elif backend == 'ollama':
        status["online"] = _ollama_loaded(url, model)
        if not status["online"]:
            status["why"] = f"{model} is not loaded in Ollama at {url}"
    else:
        status["why"] = f"unknown chat API backend {backend!r}"
    if not status["online"] and not status["why"]:
        status["why"] = "no model loaded"
    _status_cache.update(at=now, value=status)
    return status


def _ops_telegram() -> tuple[str, str]:
    """The bot token and the operator's chat id: from the environment, else
    from ~/.config/underway/underway.env, which the server unit does not load."""
    token = next((os.environ[k] for k in ("UNDERWAY_TELEGRAM_TOKEN", "TELEGRAM_KEY", "TELEGRAM_BOT_TOKEN") if os.environ.get(k)), "")
    chat_id = os.environ.get("TELEGRAM_ID", "")
    env = Path.home() / '.config/underway/underway.env'
    if (not token or not chat_id) and env.is_file():
        for line in env.read_text().splitlines():
            k, _, v = line.partition("=")
            k, v = k.strip(), v.strip().strip('"').strip("'")
            if k == "TELEGRAM_KEY" and not token:
                token = v
            if k == "TELEGRAM_ID" and not chat_id:
                chat_id = v
    if not token:
        try:
            from .alerts import telegram_token
            token = telegram_token()
        except Exception:                   # noqa: BLE001
            pass
    return token, chat_id


def alert_offline(status: dict, what: str = "the chat crew") -> None:
    """Tell the operator, once in a long while, that the crew have no model:
    loading one is theirs to decide, given the GPU."""
    global _alerted_at
    now = time.time()
    if now - _alerted_at < ALERT_MIN_S:
        return
    _alerted_at = now
    token, chat_id = _ops_telegram()
    if not token or not chat_id:
        log.warning("%s have no model (%s) and no Telegram to say so", what, status.get("why"))
        return
    try:
        from .alerts import Telegram
        Telegram(token).send(chat_id, f"Amundsen dashboard: {what} have no model to talk to ({status.get('why')}). "
                                      f"The chat never loads one itself. Load gemma4-local in Ollama with keep_alive -1, "
                                      f"or start the shared server, and they will answer again.")
        log.info("Telegram alert sent: no chat model (%s)", status.get("why"))
    except Exception as e:                  # noqa: BLE001
        log.warning("Telegram alert failed: %s", e)


def complete(system: str, user: str, max_tokens: int = MAX_TOKENS, temperature: float = 1.0,
             num_ctx: int = NUM_CTX, timeout: int = TIMEOUT, think: bool = False) -> str:
    """One answer from the local model. The chat crew and the historian both
    come through here, so the backend choice (the shared OpenAI-style server
    the camera pipeline runs, or the resident Ollama model) is made in one
    place, and the rule that the chat never loads a model is kept here: a
    request goes only to a server that is up or a model that is already in
    memory, and ``keep_alive`` -1 leaves a resident model resident (unload it
    with ``ollama stop``). ``think`` lets the model reason before it answers;
    ``max_tokens`` then covers the reasoning and the answer together."""
    import requests
    status = model_status()
    if not status["online"]:
        raise ModelOffline(status["why"])
    backend, url, model = status["backend"], status["url"], status["model"]
    messages = [{"role": "system", "content": system}, {"role": "user", "content": user}]
    if backend == 'openai':
        body = dict(model=model, messages=messages, stream=False, max_tokens=max_tokens, temperature=temperature,
                    chat_template_kwargs={'enable_thinking': think})
        endpoint = '/v1/chat/completions'
    else:
        body = {"model": model, "stream": False, "think": think, "keep_alive": -1,
                "options": {"num_predict": max_tokens, "num_ctx": num_ctx, "temperature": temperature},
                "messages": messages}
        endpoint = '/api/chat'
    try:
        r = requests.post(url + endpoint, json=body, timeout=timeout)
    except requests.ConnectionError as e:
        _status_cache["at"] = 0.0               # the picture has changed; the next call looks again
        raise ModelOffline(f"{url} refused mid-conversation") from e
    r.raise_for_status()
    result = r.json()
    message = result['choices'][0]['message'] if backend == 'openai' else result.get('message') or {}
    content = (message.get('content') or '').strip()
    if think:
        thought = message.get('thinking') or message.get('reasoning') or ''
        log.info("thought %d chars, answered %d, %s tokens in %.0f s", len(thought), len(content),
                 result.get("eval_count", "?"), (result.get("eval_duration") or 0) / 1e9)
        if not content:
            # the thinking used the whole budget and no answer followed: say
            # so, and answer again without it rather than fall silent
            log.warning("the model thought for %d chars and gave no answer within %d tokens; answering again without thinking", len(thought), max_tokens)
            return complete(system, user, max_tokens, temperature, num_ctx, timeout, think=False)
    return content


_wiki_cache: dict = {"stamp": None, "pages": []}
_WORD_RX = re.compile(r"[a-zà-ÿ0-9']{3,}")
_STOP = set("the and for with that this from were was are have has had not but his her their they them then than into "
            "over under about after before between which what when where who whom whose why how does did done been being "
            "also there here these those such some any all more most much many very just only both each other "
            "history historical happened happen tell know place places near nearby around current location "
            "ship vessel today year years time now your our closest nearest database records record archive archives "
            "wiki mention mentioned anything something about".split())


def _read_rows(root: Path, name: str) -> list[dict]:
    f = root / "data" / "history" / f"{name}.json"
    try:
        return json.loads(f.read_text(encoding="utf-8")).get(name, []) if f.is_file() else []
    except (OSError, ValueError):
        return []


def row_facts(root: Path) -> dict[str, str]:
    """One line of facts per generated page, by slug, from the published rows:
    what the page body no longer carries and the historian must still see."""
    out: dict[str, str] = {}
    def line(parts) -> str:
        return "; ".join(f"{k}: {v}" if k else str(v) for k, v in parts if v not in (None, "", [], "[]"))
    def span(r) -> str:
        return f"{r.get('born') or '?'}–{r.get('died') or ''}" if (r.get("born") or r.get("died")) else ""
    def pos(r) -> str:
        return f"{float(r['lat']):.3f}, {float(r['lon']):.3f}" if r.get("lat") is not None and r.get("lon") is not None else ""
    for a in _read_rows(root, "artifacts"):
        out[a.get("page", f"artifact/{a.get('id')}")] = line([("kind", a.get("type")), ("date", a.get("date_text") or a.get("date_start")),
            ("creator", a.get("creator")), ("position", pos(a)), ("credit", a.get("credit")), ("source", a.get("bibkey")),
            ("pages", a.get("pages")), ("people", ", ".join(a.get("people") or []))])
    for r in _read_rows(root, "places"):
        out[r.get("page", "")] = line([("kind", r.get("kind")), ("Inuktitut name", r.get("inuktitut")), ("historic name", r.get("historic")),
            ("position", pos(r)), ("source", r.get("bibkey"))])
    for r in _read_rows(root, "people"):
        out[r.get("page", "")] = line([("role", r.get("role")), ("affiliation", r.get("affiliation")), ("lived", span(r)),
            ("also written", r.get("also")), ("Indigenous", "yes" if r.get("indigenous") else ""), ("source", r.get("bibkey"))])
    for r in _read_rows(root, "animals"):
        out[r.get("page", "")] = line([("kind", r.get("kind")), ("role", r.get("role")), ("affiliation", r.get("affiliation")),
            ("lived", span(r)), ("also called", r.get("also")), ("source", r.get("bibkey"))])
    for r in _read_rows(root, "vessels"):
        out[r.get("page", "")] = line([("kind", ", ".join(x for x in (r.get("kind_label") or r.get("kind"), r.get("kind_note")) if x)),
            ("role", r.get("role")), ("built", r.get("built")), ("fate", r.get("lost")), ("affiliation", r.get("affiliation")),
            ("tonnage", r.get("tonnage")), ("also", r.get("also")), ("source", r.get("bibkey"))])
    for e in _read_rows(root, "events"):
        when = e.get("date_text") or " to ".join(x for x in (e.get("date_start"), e.get("date_end")) if x)
        out[f"event/{e.get('id')}"] = line([("date", when), ("place", e.get("place")), ("position", pos(e)),
            ("people", ", ".join(e.get("people") or [])), ("source", e.get("bibkey"))])
    return {k: v for k, v in out.items() if k and v}


def wiki_pages(root: Path) -> list[dict]:
    """Every published wiki page, both halves, held in memory until the build
    publishes anew. Each carries its ``_domain``, history or nature, by the
    domain of its topic (a subject or an observation is nature's)."""
    idx = root / "data" / "history" / "index.json"
    if not idx.is_file():
        return []
    stamp = idx.stat().st_mtime
    if _wiki_cache["stamp"] == stamp:
        return _wiki_cache["pages"]
    try:
        domain = {t.get("slug"): t.get("domain") or "history" for t in json.loads(idx.read_text(encoding="utf-8")).get("topics", [])}
    except (OSError, ValueError):
        domain = {}
    pages = []
    facts = row_facts(root)
    for f in sorted((root / "data" / "history" / "pages").glob("*.json")):
        try:
            d = json.loads(f.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            continue
        text = re.sub(r"<[^>]+>", " ", d.get("html", ""))
        text = re.sub(r"\]\([^)]*\)", "]", text)              # link targets are noise for matching
        # a subject page reads as the publish's excerpt of it (the note and
        # its record, written for a small model), when there is one
        ex = root / "data" / "history" / "excerpts" / (str(d.get("slug", "")).replace("/", "__") + ".txt")
        if d.get("kind") == "subject" and ex.is_file():
            try:
                text = ex.read_text(encoding="utf-8")
            except OSError:
                pass
        d["_domain"] = "nature" if d.get("kind") in ("subject", "observation") else domain.get(d.get("topic"), "history")
        # a generated page's body is its row's prose alone; the row's other
        # fields (the date, the position, the source, the people) are what
        # the historian needs to answer where and when, so they go back on
        if d.get("slug") in facts:
            text = (text.rstrip() + "\n" + facts[d["slug"]]).strip()
        d["_text"] = text
        d["_words"] = _WORD_RX.findall((d.get("title", "") + " " + d.get("summary", "") + " " + text).lower())
        pages.append(d)
    # document frequency, so a word on every page (bay, ship, ice) counts for little
    import math
    df: dict[str, int] = {}
    for d in pages:
        for w in set(d["_words"]):
            df[w] = df.get(w, 0) + 1
    n = max(1, len(pages))
    avg = sum(len(d["_words"]) for d in pages) / n
    _wiki_cache.update(stamp=stamp, pages=pages, idf={w: math.log(1 + (n - c + 0.5) / (c + 0.5)) for w, c in df.items()}, n=n, avglen=avg)
    return pages


# the region's acronyms, as the scientists type them; expanded for retrieval
# and spelled out to the librarian so QEI is not read as a Qikiqtani body
ACRONYMS = {
    "qei": "Queen Elizabeth Islands", "qeis": "Queen Elizabeth Islands", "nwp": "Northwest Passage",
    "kwi": "King William Island", "qtc": "Qikiqtani Truth Commission", "qia": "Qikiqtani Inuit Association",
    "hbc": "Hudson's Bay Company", "rcmp": "Royal Canadian Mounted Police", "ccgs": "Canadian Coast Guard Ship",
    "dew": "Distant Early Warning Line", "jaws": "Joint Arctic Weather Stations", "pcsp": "Polar Continental Shelf Project",
    "ipy": "International Polar Year", "hms": "His Majesty's Ship", "nwt": "Northwest Territories",
    "itk": "Inuit Tapiriit Kanatami", "cae": "Canadian Arctic Expedition", "pearl": "Polar Environment Atmospheric Research Laboratory",
    "chars": "Canadian High Arctic Research Station",
}
_ACRO_RX = re.compile(r"\b([A-Za-z]{2,6})\b")


def expand_acronyms(text: str) -> tuple[str, list[str]]:
    """The text with each known acronym followed by its expansion, and the
    expansions used, for a glossary line."""
    used = []

    def sub(m):
        full = ACRONYMS.get(m.group(1).lower())
        if not full or full in used:
            return m.group(0) if not full else m.group(0)
        used.append(full)
        return f"{m.group(0)} ({full})"
    return _ACRO_RX.sub(sub, text), used


def wiki_excerpts(root: Path, question: str, slug: str = "", limit: int = 8, budget: int = 28000, prefer: str = "") -> list[dict]:
    """The wiki pages that bear on a question, best first: matched on words,
    with the page being read and its neighbours favoured, narrative pages
    and the reader's own half of the wiki (``prefer``: history or nature)
    weighted up. Each comes back with an ``excerpt`` sized to the budget."""
    pages = wiki_pages(root)
    if not pages:
        return []
    import math
    idf = _wiki_cache.get("idf") or {}
    n = _wiki_cache.get("n") or 1
    avg = _wiki_cache.get("avglen") or 1.0
    question = expand_acronyms(question)[0]
    q = list(dict.fromkeys(w for w in _WORD_RX.findall(question.lower()) if w not in _STOP))
    weight = {w: idf.get(w, math.log(n + 1)) for w in q}       # a word the wiki has never seen is rare by definition
    by_slug = {p["slug"]: p for p in pages}
    current = by_slug.get(slug)
    scored = []
    k1, b = 1.2, 0.75                                          # BM25: a term counts less the longer the page
    for p in pages:
        words = p["_words"]
        if not words:
            continue
        # a forty-word quote page must not outscore a chapter on a single word:
        # short pages are normalised as if they were of a modest length
        norm = k1 * (1 - b + b * max(len(words), 300) / avg)
        score = 0.0
        for w in q:
            tf = words.count(w)
            if tf:
                score += weight[w] * tf * (k1 + 1) / (tf + norm)
        title = p.get("title", "").lower()
        score += sum(weight[w] for w in q if w in title)
        if current and (p["slug"] == slug or p["slug"] in current.get("backlinks", []) or p["slug"] in current.get("html", "")):
            score += 2.0
        if p.get("kind") == "page":
            score *= 1.5                                    # the narrative pages carry the story
        if prefer and p.get("_domain") == prefer:
            score *= 1.5                                    # the reader's own half of the wiki comes first
        if score > 0:
            scored.append((score, p))
    scored.sort(key=lambda x: -x[0])
    best = scored[0][0] if scored else 0.0
    chosen = []
    if current:
        chosen.append(current)
        budget -= len(current["_text"])
    for sc, p in scored:
        if p in chosen:
            continue
        if len(chosen) >= limit or budget <= 0:
            break
        # a page has to earn its place: nothing weaker than a quarter of the
        # best match, and nothing at all when even the best is feeble, so the
        # historian says the wiki is silent rather than reading six random bays
        if sc < max(0.25 * best, 10.0):
            break
        chosen.append(p)
        budget -= min(len(p["_text"]), 6000)
    return [{"slug": p["slug"], "title": p["title"], "kind": p["kind"],
             "excerpt": (p["_text"] if p is current else p["_text"][:6000]).strip()} for p in chosen]


def places_named(root: Path, text: str) -> list[dict]:
    """The published places whose names (modern, historic or Inuktitut)
    appear in a text, with their positions: for 'where is X' and 'what
    happened at X', where the wiki's prose alone may not say."""
    f = root / "data" / "history" / "places.json"
    if not f.is_file():
        return []
    low = text.lower()
    out = []
    for p in json.loads(f.read_text()).get("places", []):
        names = [x for x in (p.get("name"), p.get("historic"), p.get("inuktitut")) if x and len(x) >= 4]
        if any(x.lower() in low for x in names):
            out.append(p)
    return out[:8]


def nature_lines(root: Path, lat, lon, slug: str = "", now: datetime | None = None, near_km: float = 300, limit: int = 8) -> list[str]:
    """Doc's slice of the natural half: the observations nearest the ship, those
    made on this date in other years, the ship's own journal lines, and the
    whole record of the subject (or the one observation) open on the Nature
    tab. Each line names the subject, the number as written, the observer,
    the date and the place, so an answer can say who recorded what."""
    import math
    h = root / "data" / "history"
    try:
        subjects = {s["name"]: s for s in json.loads((h / "subjects.json").read_text()).get("subjects", [])} if (h / "subjects.json").is_file() else {}
        obs = json.loads((h / "observations.json").read_text()).get("observations", []) if (h / "observations.json").is_file() else []
    except (OSError, ValueError):
        subjects, obs = {}, []
    try:
        from .nature import entries
        seen = {str(o.get("id")) for o in obs}
        journal = [{**e, "date_start": e.get("date", "")[:10], "_journal": True} for e in entries(50) if str(e.get("id")) not in seen]
    except Exception:                   # noqa: BLE001
        journal = []
    rows = obs + journal
    if not rows:
        return []
    def name(o):
        s = subjects.get(o.get("subject", ""))
        return f"{s['english']} ({s['name']})" if s and s.get("english") and s["english"] != s["name"] else o.get("subject", "")
    def number(o):
        if o.get("value") not in (None, ""):
            return f"{o['value']} {o.get('unit', '')}".strip()
        return str(o.get("count") or o.get("qualifier") or "")
    def line(o, extra=""):
        bits = [name(o), number(o), o.get("date_text") or o.get("date_start") or o.get("date", ""),
                ("by " + o["observer"]) if o.get("observer") else "", o.get("place", ""), extra,
                "(the ship's journal, not yet on grid)" if o.get("_journal") else ""]
        return "- " + ", ".join(b for b in bits if b) + (f": {o['detail'][:160]}" if o.get("detail") else "")
    out = []
    if lat is not None and lon is not None:
        def km(o):
            r = math.pi / 180
            a = math.sin((o["lat"] - lat) * r / 2) ** 2 + math.cos(lat * r) * math.cos(o["lat"] * r) * math.sin((o["lon"] - lon) * r / 2) ** 2
            return 2 * 6371 * math.asin(math.sqrt(a))
        near = sorted((o for o in rows if o.get("lat") is not None and o.get("lon") is not None), key=km)
        near = [o for o in near if km(o) <= near_km][:limit]
        if near:
            out.append(f"NATURAL RECORD near the ship (within {int(near_km)} km):\n" + "\n".join(line(o, f"{km(o):.0f} km away") for o in near))
    now = now or datetime.now(timezone.utc)
    mmdd = now.strftime("-%m-%d")
    today = [o for o in rows if len(str(o.get("date_start", ""))) == 10 and str(o["date_start"]).endswith(mmdd)][:limit]
    if today:
        out.append("NATURAL RECORD on this date in other years:\n" + "\n".join(line(o) for o in today))
    if journal:
        out.append("THE SHIP'S JOURNAL, latest lines:\n" + "\n".join(line(o) for o in journal[:6]))
    if slug.startswith("subject/"):
        s = next((x for x in subjects.values() if x.get("page") == slug), None)
        excerpt = h / "excerpts" / (slug.replace("/", "__") + ".txt")     # the publish writes one text per subject for a small model
        if excerpt.is_file():
            out.append("THE PAGE OPEN ON THE NATURE TAB:\n" + excerpt.read_text(encoding="utf-8")[:6000])
        elif s:
            rec = sorted((o for o in rows if o.get("subject") == s["name"]), key=lambda o: str(o.get("date_start", "")))
            names = "; ".join(f"{k}: {s[k]}" for k in ("english", "french", "inuktitut", "kalaallisut") if s.get(k))
            out.append(f"THE PAGE OPEN ON THE NATURE TAB: {s['name']}" + (f" ({names})" if names else "") + (f". {s['note'][:800]}" if s.get("note") else "")
                       + ("\nIts record:\n" + "\n".join(line(o) for o in rec[:30]) if rec else "\nNo observations of it in the record yet."))
    elif slug.startswith("observation/"):
        o = next((x for x in rows if str(x.get("id")) == slug[12:]), None)
        if o:
            out.append("THE OBSERVATION OPEN ON THE NATURE TAB:\n" + line(o))
    return out


def excerpt_block(excerpts: list[dict]) -> str:
    # numbered, so the answer can cite [n]; the header carries no slug, or the
    # model cites the slug
    return "\n\n".join(f"### [{i}] {e['title']}  (a {e['kind']} page)\n{e['excerpt']}" for i, e in enumerate(excerpts, 1))


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
    def __init__(self, root: Path, post=None, read=None):
        from . import chat
        self.root = root
        self.post = post or (lambda name, emoji, text, channel, meta=None: chat.post("crew", name, text, emoji, channel, bot=True, meta=meta))
        self.read = read or chat.context    # (channel) -> the room's recent messages, oldest first
        self._pages: list[dict] = []        # the wiki pages the last context drew on
        self._shelf: list[dict] = []        # the pictures and words behind them, for the chips
        self.lock = threading.Lock()        # one generation at a time
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

    def context(self, beat: str = "all", task: str = "", slug: str = "") -> str:
        """The dashboard summary for one beat: the Cap'n sees the schedule and
        the weather, Doc the water and the natural record, the Librarian the
        past, Polly nothing but the clock. ``all`` is everything, for tests
        and for a look."""
        self._pages, self._shelf = [], []   # only a reader's context fills these
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
        if beat in ("all", "environment"):
            # the natural half of the history layer: the record near the ship and on
            # this date, the ship's own journal, and the page open on the Nature tab;
            # then the wiki pages that bear on the task, the natural half favoured
            wiki = self._wiki(task, slug, "nature", " ".join(lines[-2:])) if beat == "environment" else []
            try:
                # the open page comes once: through the excerpts when they hold it
                lines += nature_lines(self.root, lat, lon, "" if any(e["slug"] == slug for e in self._pages) else slug)
            except Exception:                   # noqa: BLE001
                pass
            lines += wiki
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
                lines += self._wiki(task, slug, "history", " ".join(lines[-2:]))
        return "\n".join(lines)

    def _wiki(self, task: str, slug: str, prefer: str, fallback: str = "") -> list[str]:
        """The wiki's part of a reader's context: a glossary of the acronyms in
        the question, the excerpts that bear on it, numbered to be cited, with
        the page open on the tab first, and the shelf of pictures and words
        behind them. Keeps the pages and the shelf the answer is linked against."""
        out = []
        self._pages, self._shelf = [], []
        _, used = expand_acronyms(task or "")
        if used:
            out.append("Acronyms in the question: " + "; ".join(f"{k.upper()} is the {v}" for k, v in ACRONYMS.items() if v in used) + ".")
        try:
            ex = wiki_excerpts(self.root, task or fallback, slug, limit=5, budget=14000, prefer=prefer)
            if ex:
                out.append("\nWIKI EXCERPTS\n\n" + excerpt_block(ex))
                self._pages = ex
                from . import chat
                self._shelf = chat.artifact_shelf(ex)
                if self._shelf:
                    out.append("\nPICTURES AND QUOTATIONS from those pages. When one shows what a paragraph of your answer "
                               "says, set its tag, such as {P2}, at the end of that paragraph: it appears beside your words. "
                               "At most one tag per paragraph, and only when it truly illustrates the paragraph; none is fine.\n"
                               + chat.shelf_lines(self._shelf))
        except Exception:                       # noqa: BLE001
            pass
        return out

    @staticmethod
    def own_room(handle: str, channel: str) -> bool:
        """Ada's Library, or Doc's Lab (a direct message with Doc alone): a
        room where every message is a question to that one member, answered
        at length with the pages cited."""
        if handle == "ada":
            return channel == "ada"
        from . import chat
        return handle == "doc" and channel.startswith("dm:") and chat.bots_in(channel) == ["doc"]

    # ------------------------------------------------------------ generation
    def _generate(self, handle: str, task: str, channel: str = "ship", query: str = "", long: bool = False, slug: str = "") -> tuple[str | None, list[dict]]:
        """One remark in a room. The crew member sees its beat's slice of the
        dashboard and the last few kilobytes of that room, nothing else."""
        p = PERSONAS[handle]
        recent_rows = self.read(channel)
        recent = "\n".join(f"{x.get('emoji', '')} {x['name']}: {x['text']}" for x in recent_rows)
        others = ", ".join(f"@{h} ({q['name']}: {q['beat']})" for h, q in PERSONAS.items() if h != handle)
        own = self.own_room(handle, channel)
        room = {"ship": "the ship's public room, where you speak only when addressed",
                "crew": "the crew's own room, where the four of you talk among yourselves and with whoever drops in"}.get(channel,
               f"the {p['room']}, your own room, where every message is a question put to you and deserves a full "
               f"answer, up to about 500 words, with the pages cited" if own else
               "a private room with one person; only the two of you see it, and you may speak first")
        system = (f"You are {p['name']}, {p['voice']} Your type is {p['type']}. {p['brief']} The rest of the crew: {others}. "
                  f"You are one of four crew members in the chat of the CCGS Amundsen underway "
                  f"dashboard, read by the scientists aboard, who like a laugh. This is {room}. "
                  f"The crew is mixed, and you never assume anyone's gender: "
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
                  f"DASHBOARD SUMMARY (your beat's slice)\n{self.context(p['beat'], query or task, slug)}\n\nRECENT CHAT (oldest first)\n{recent}")
        pages = list(self._pages)
        # a member's own room: the answer runs long (thinking costs minutes and adds little)
        text = complete(system, task, ROOM_TOKENS if own else MAX_TOKENS * (2 if long else 1), 0.5 if own else 1.0)
        text = re.sub(r"^\W*" + re.escape(p["name"]) + r"\s*:\s*", "", text)      # no self-labelling
        return (text[:ROOM_CHARS if own else 2500] or None), pages

    def _pick_chips(self, handle: str, text: str, shelf: list[dict]) -> tuple[str, list[dict]]:
        """A reader's second look: the answer paragraph by paragraph beside the
        shelf, and which item, if any, shows what each paragraph says. A
        short call with no thinking, a couple of seconds; on any failure the
        answer stands without chips."""
        from . import chat
        paras = [p for p in re.split(r"\n\s*\n", text or "") if p.strip()]
        if not paras:
            return text, []
        user = ("THE ANSWER, paragraph by paragraph\n" + "\n".join(f"paragraph {i + 1}: {p[:400]}" for i, p in enumerate(paras))
                + "\n\nTHE SHELF\n" + chat.shelf_lines(shelf)
                + "\n\nWhich item on the shelf shows what a paragraph says? One line per match, 'paragraph N: Pn', at most one item "
                  "per paragraph and at most three matches; the picture or the words must genuinely illustrate that paragraph. "
                  "If nothing fits, write 'none'. Lines only, nothing else.")
        try:
            reply = complete(f"You are {PERSONAS[handle]['name']}, the ship's {'librarian' if handle == 'ada' else 'naturalist'}, "
                             f"choosing illustrations for an answer of your own.", user, 160, 0.0)
        except Exception as e:                  # noqa: BLE001
            log.info("%s's second look failed (%s); no chips", PERSONAS[handle]["name"], e)
            return text, []
        return chat.chosen_chips(chat.apply_picks(text, reply), shelf)

    def _speak(self, handle: str, task: str, channel: str = "ship", query: str = "", banter: bool = True, long: bool = False, slug: str = "", hop: int = 0) -> None:
        """Generate and post one remark in a room. A member the remark
        @mentions answers it, and in the crew's room another member
        sometimes riffs on it, usually Polly; either is one hop only
        (`hop`), so they cannot chain forever."""
        if not self.enabled:
            return
        from . import chat
        p = PERSONAS[handle]
        text = None
        with self.lock:
            chat.typing(channel, handle, True)
            try:
                text, pages = self._generate(handle, task, channel, query, long, slug)
                if text:
                    meta = None
                    chips = []
                    reader = handle in READERS
                    if reader:
                        shelf = self._shelf
                        text, chips = chat.chosen_chips(text, shelf)   # the pictures and words they picked, before the brackets are read
                        if shelf and not chips:
                            text, chips = self._pick_chips(handle, text, shelf)   # they seldom tag as they write: a second look at the answer
                    if pages:
                        text, refs = chat.link_citations(text, [{"slug": e["slug"], "title": e["title"], "kind": e["kind"]} for e in pages])
                        meta = {"refs": refs} if refs else None
                    if reader:
                        text = chat.link_entities(text)     # every person and place they name, to its page
                        if chips:
                            meta = {**(meta or {}), "chips": chips}
                    self.post(p["name"], p["emoji"], text, channel, meta)
                    self.last_bot = time.time()
            except ModelOffline as e:
                log.info("crew %s stayed quiet: %s", handle, e)
                alert_offline(model_status(), "the chat crew")
            except Exception as e:          # noqa: BLE001
                log.info("crew %s stayed quiet (%s)", handle, e)
            finally:
                chat.typing(channel, handle, False)
        if text and hop == 0:
            # the members they spoke to by handle answer, where they can be in that room
            room_bots = chat.bots_in(channel)
            asked = [h.lower() for h in HANDLE_RX.findall(text)]
            asked = [h for h in dict.fromkeys(asked) if h in PERSONAS and h != handle and (h in room_bots or channel == "ship")]
            for other in asked:
                self._speak(other, f"{p['name']} just said to you in the chat: \"{text}\". Reply to them as yourself.", channel, banter=False, hop=1)
            if asked:
                return
        if text and banter and channel == "crew" and random.random() < BANTER_P:
            # the reporting gets reported on: Polly, usually; another now and then
            other = "polly" if handle != "polly" and random.random() < 0.7 else random.choice([h for h in PERSONAS if h not in (handle, "polly")])
            time.sleep(random.uniform(8, 25))
            self._speak(other, f"{p['name']} just said in the chat: \"{text}\". Riff on it in your own voice — agree, needle them, "
                               f"correct them, or add a detail — in one or two sentences. Do not repeat their numbers back unless you dispute them.",
                        channel, banter=False, hop=1)

    # ------------------------------------------------------------ triggers
    def on_message(self, name: str, text: str, channel: str = "ship", slug: str = "") -> None:
        """Called after a human message is stored. Who answers depends on the
        room: in the public room only a member @mentioned; in the crew's room
        whoever is mentioned, else one of them; in Ada's room, Ada, at length,
        and in Doc's Lab (a private room with him alone) Doc likewise; in any
        other private room, the member it is with."""
        from . import chat
        if name in {p["name"] for p in PERSONAS.values()}:
            return
        handles = [h.lower() for h in HANDLE_RX.findall(text)]
        if "crew" in handles or "all" in handles:
            handles = list(PERSONAS)
        handles = list(dict.fromkeys(h for h in handles if h in PERSONAS))
        room_bots = chat.bots_in(channel)
        task = f"{name} just wrote: \"{text}\". Reply to them as yourself."
        long = False
        if channel == "ship":
            speakers = handles
        elif channel == "crew":
            speakers = handles or ([random.choice(room_bots)] if room_bots else [])
        elif channel == "ada" or (channel.startswith("dm:") and room_bots == ["doc"]):
            h = "ada" if channel == "ada" else "doc"
            speakers = [h] if h in room_bots else []
            task = (f"{name} asks in the {PERSONAS[h]['room']}: \"{text}\". Answer fully from {'the record and ' if h == 'doc' else ''}the "
                    f"pages you have, citing each you draw on by its number in square brackets after the sentence it supports, never "
                    f"by title; speak of the sources by name, never of 'the wiki' or 'the excerpts'; and where you have nothing, say "
                    f"so as yourself. Where a picture or a quotation on the shelf shows what a paragraph of yours says, end that "
                    f"paragraph with its tag, {{P2}} say, so it appears beside your words.")
            long = True
        else:
            speakers = room_bots
        for h in speakers:
            threading.Thread(target=self._speak, args=(h, task, channel, text), kwargs={"long": long, "slug": slug}, daemon=True).start()

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
        """Unprompted remarks, only into rooms someone has open where the crew
        belong: the crew's room, or a private room with one member. Never the
        public room, never Ada's reading room."""
        from . import chat
        time.sleep(90)
        while True:
            try:
                now = time.time()
                open_rooms = [(ch, who) for ch, who in chat.open_rooms() if ch not in ("ship", "ada") and chat.bots_in(ch)]
                if open_rooms and model_status()["online"]:
                    event = self._events()
                    if event and now - self.last_bot > EVENT_MIN_S:
                        h = "capn" if "schedule" in event else "doc"
                        rooms = [ch for ch, _ in open_rooms if h in chat.bots_in(ch)]
                        if rooms:
                            self._speak(h, f"{event}. Remark on it for the crew in your own way; be brief and cite the relevant number.",
                                        random.choice(rooms))
                    elif now - self.last_bot > CHIME_MIN_S:
                        ch, _ = random.choice(open_rooms)
                        # the three with a beat take turns; Polly only ever reports on them
                        choices = [h for h in chat.bots_in(ch) if h != "polly"] or chat.bots_in(ch)
                        h = random.choice(choices)
                        self._speak(h, {"ada": "Peek at your slice of the summary and chime in with one short remark about the past: "
                                               "something that happened on this date in another year, or near where the ship is now, with its "
                                               "year and its source. If there is nothing, pick the most striking thing in the wiki excerpts. "
                                               "Do not greet, do not ask questions.",
                                        "polly": "Report on the last thing anyone said in this room, in one squawky line."}.get(h,
                                       "Peek at your slice of the dashboard summary and chime in with one short, characterful remark "
                                       "about the current conditions on your beat — pick one detail worth noticing and cite its number. Do not "
                                       "greet, do not ask questions."), ch)
            except Exception as e:          # noqa: BLE001
                log.info("crew loop: %s", e)
            time.sleep(300)

    def start(self) -> None:
        if not self.enabled:
            return
        threading.Thread(target=self.loop, daemon=True).start()
        log.info("crew online: %s via %s (%s)", ", ".join("@" + h for h in PERSONAS), LLM_URL, LLM_MODEL)
