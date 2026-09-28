"""Which model each part of the dashboard talks to: OpenRouter when it has a key.

The parts that use a language model share three settings (the /settings page
lists them, from USES):

    dashboard  the chat crew, the Wiki's answers, the photo tags, the
               underway-water alert advice and the crew at the Hearts table
               (amundsen-game)                                   text and images
    ice        the ice camera's frame classifications            images
    report     the cruise report builder's sheet digitizing      images

A call names its part (chat, wiki, photos, alerts, game, ice, report); PART maps
it to one of the three. The dashboard's key, ``OPENROUTER_API_KEY``, also serves
the cruise report when it has none of its own (``OPENROUTER_REPORT_KEY``); the ice
camera sends pictures all day and runs only on its own, ``OPENROUTER_ICE_KEY``.
A part with a key sends its requests to OpenRouter with its model
(``OPENROUTER_MODEL``, ``OPENROUTER_ICE_MODEL``, ``CRUISE_DIGITIZE_MODEL``). A
part with no key keeps the local arrangement: the resident Ollama model or the
shared server (chatbot.model_status), which never loads a model itself.

The game reads the dashboard's two variables itself (crew_chat.py in
amundsen-game); the cruise report builder reads its own (cruisereport/digitize.py).
"""
from __future__ import annotations

import os
from dataclasses import dataclass

OPENROUTER = "https://openrouter.ai/api"      # + /v1/chat/completions, like the local OpenAI-style server
SHARED_KEY = "OPENROUTER_API_KEY"               # the dashboard's key, also the report's fallback
FLASH = "google/gemini-3.8-flash"
# the model the ship runs locally for the ice camera (gemma-camera), as OpenRouter names it
GEMMA = "google/gemma-4-26b-a4b-it"


@dataclass(frozen=True)
class Use:
    name: str
    label: str
    images: bool
    default: str
    key_var: str
    model_var: str
    help: str


USES: tuple[Use, ...] = (
    Use("dashboard", "Dashboard AI", True, FLASH, SHARED_KEY, "OPENROUTER_MODEL",
        "The chat crew, answers in the Wiki, tags for the Photos tab, advice on underway-water alerts, "
        "and the crew at the Hearts table in Games. Needs a model that reads images."),
    Use("ice", "Ice camera", True, GEMMA, "OPENROUTER_ICE_KEY", "OPENROUTER_ICE_MODEL",
        "Classifies the ice in the 360° camera's pictures. Needs a model that reads images. "
        "It sends pictures all day and costs the most, so it runs only with a key of its own here."),
    Use("report", "Cruise report sheets", True, FLASH, "OPENROUTER_REPORT_KEY", "CRUISE_DIGITIZE_MODEL",
        "Reads scanned log sheets in the cruise report builder. Needs a model that reads images. "
        "Leave the key empty to use the Dashboard AI key."),
)
BY_NAME = {u.name: u for u in USES}
# the part a call names -> the setting it uses
PART = {"chat": "dashboard", "wiki": "dashboard", "photos": "dashboard", "alerts": "dashboard", "game": "dashboard",
        "dashboard": "dashboard", "ice": "ice", "report": "report"}

# the ice camera sends pictures all day, so it runs only on a key given to it alone
OWN_KEY_ONLY = {"ice"}


def key(use: str) -> str:
    u = BY_NAME[PART[use]]
    shared = "" if u.name in OWN_KEY_ONLY else os.environ.get(SHARED_KEY)
    return (os.environ.get(u.key_var) or shared or "").strip()


def model(use: str) -> str:
    u = BY_NAME[PART[use]]
    return (os.environ.get(u.model_var) or u.default).strip()


def remote(use: str) -> dict | None:
    """The OpenRouter route for this part, or None when it has no key (it then stays local).
    The dict has the shape of chatbot.model_status()'s answer, so callers treat both alike."""
    k = key(use)
    if not k:
        return None
    return {"backend": "openrouter", "url": OPENROUTER, "model": model(use), "online": True, "why": "",
            "key": k, "use": use}


def headers(status: dict) -> dict:
    """The HTTP headers for a chat request along this route."""
    h = {"Content-Type": "application/json"}
    if status.get("backend") == "openrouter":
        h["Authorization"] = f"Bearer {status['key']}"
        h["X-Title"] = "Underway dashboard"
    return h


def body(status: dict, payload: dict) -> dict:
    """An OpenAI-style request body fitted to the route: the local server's template switch
    becomes OpenRouter's reasoning setting, and the model is the route's."""
    out = dict(payload, model=status["model"])
    if status.get("backend") == "openrouter":
        kwargs = out.pop("chat_template_kwargs", None) or {}
        out.pop("keep_alive", None)
        if "enable_thinking" in kwargs:
            out["reasoning"] = {"enabled": bool(kwargs["enable_thinking"])}
    return out


def uses_remote() -> bool:
    """Whether any part has an OpenRouter key, so the crew can talk though the local GPU is off."""
    return any(key(u.name) for u in USES)
