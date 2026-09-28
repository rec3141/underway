"""Which model each part of the dashboard talks to: OpenRouter when it has a key.

Seven parts use a language model, each with its own key and model setting (the
/settings page lists them, from USES):

    chat      the crew in the Chat tab                        text
    wiki      the Wiki's "ask" answers                          text
    photos    tags for the Photos tab                           images
    alerts    the recommendation on an underway-water alert     images
    ice       the ice camera's frame classifications            images
    game      the crew at the Hearts table (amundsen-game)      text
    report    the cruise report builder's sheet digitizing      images

A part whose key is set (``OPENROUTER_<PART>_KEY``, or failing that the shared
``OPENROUTER_API_KEY``; the ice camera takes only its own) sends its requests to OpenRouter with the model in
``OPENROUTER_<PART>_MODEL``, whose default is the model the ship runs locally.
A part with no key keeps the local arrangement: the resident Ollama model or
the shared server (chatbot.model_status), which never loads a model itself.

The game reads its two variables itself (crew_chat.py in amundsen-game).
The cruise report builder keeps its own names, ``OPENROUTER_REPORT_KEY`` and
``CRUISE_DIGITIZE_MODEL`` (cruisereport/digitize.py).
"""
from __future__ import annotations

import os
from dataclasses import dataclass

OPENROUTER = "https://openrouter.ai/api"      # + /v1/chat/completions, like the local OpenAI-style server
SHARED_KEY = "OPENROUTER_API_KEY"
# the model the ship runs locally (gemma4-local, gemma-camera), as OpenRouter names it
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
    Use("chat", "Chat crew", False, GEMMA, "OPENROUTER_CHAT_KEY", "OPENROUTER_CHAT_MODEL",
        "The crew who answer in the Chat tab."),
    Use("wiki", "Wiki answers", False, GEMMA, "OPENROUTER_WIKI_KEY", "OPENROUTER_WIKI_MODEL",
        "Answers to questions asked in the Wiki."),
    Use("photos", "Photo tags", True, GEMMA, "OPENROUTER_PHOTOS_KEY", "OPENROUTER_PHOTOS_MODEL",
        "Describes and tags the pictures in the Photos tab. Needs a model that reads images."),
    Use("alerts", "Water alert advice", True, GEMMA, "OPENROUTER_ALERTS_KEY", "OPENROUTER_ALERTS_MODEL",
        "Looks at an underway-water alert's chart and says whether it matters. Needs a model that reads images."),
    Use("ice", "Ice camera", True, GEMMA, "OPENROUTER_ICE_KEY", "OPENROUTER_ICE_MODEL",
        "Classifies the ice in the 360° camera's pictures. Needs a model that reads images. "
        "It sends pictures all day and costs the most, so it never uses the shared key: it runs only with a key here."),
    Use("game", "Card-table crew", False, GEMMA, "OPENROUTER_GAME_KEY", "OPENROUTER_GAME_MODEL",
        "The crew who play and chat at the Hearts table in Games."),
    Use("report", "Cruise report sheets", True, "google/gemini-3.8-flash", "OPENROUTER_REPORT_KEY",
        "CRUISE_DIGITIZE_MODEL", "Reads scanned log sheets in the cruise report builder. Needs a model that reads images."),
)
BY_NAME = {u.name: u for u in USES}


# the ice camera sends pictures all day, so it runs only on a key given to it alone
OWN_KEY_ONLY = {"ice"}


def key(use: str) -> str:
    u = BY_NAME[use]
    shared = "" if use in OWN_KEY_ONLY else os.environ.get(SHARED_KEY)
    return (os.environ.get(u.key_var) or shared or "").strip()


def model(use: str) -> str:
    u = BY_NAME[use]
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
