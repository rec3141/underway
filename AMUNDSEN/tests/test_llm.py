"""Which model each part talks to (dashboard/llm.py), and the AI rows of /settings."""

import io
import json

import pytest

from dashboard import chatbot, llm
from dashboard import settings as S

ALL_VARS = [llm.SHARED_KEY] + [v for u in llm.USES for v in (u.key_var, u.model_var)]


@pytest.fixture(autouse=True)
def clean_env(monkeypatch, tmp_path):
    for v in ALL_VARS:
        monkeypatch.delenv(v, raising=False)
    d = tmp_path / "conf"
    d.mkdir()
    monkeypatch.setattr(S, "CONFIG_DIR", d)
    listing = [{"id": llm.GEMMA, "in": ["image", "text"], "reasoning": {"mandatory": False}},
               {"id": llm.FLASH, "in": ["image", "text"],
                "reasoning": {"mandatory": True, "supported_efforts": ["high", "medium", "low"]}},
               {"id": "some/text-only", "in": ["text"]}]
    monkeypatch.setattr(llm, "models", lambda cache_dir=None: listing)
    monkeypatch.setattr(S, "_models_list", lambda: [{"id": llm.GEMMA, "in": ["image", "text"]},
                                                    {"id": llm.FLASH, "in": ["image", "text"]},
                                                    {"id": "some/text-only", "in": ["text"]}])
    return d


def test_no_key_stays_local():
    assert llm.remote("chat") is None
    assert not llm.uses_remote()


def test_three_settings_serve_every_part(monkeypatch):
    monkeypatch.setenv(llm.SHARED_KEY, "dash")
    monkeypatch.setenv("OPENROUTER_ICE_KEY", "ice-only")
    for part in ("chat", "wiki", "photos", "alerts", "game"):
        assert llm.remote(part)["key"] == "dash" and llm.remote(part)["model"] == llm.FLASH
    assert llm.remote("ice")["key"] == "ice-only" and llm.remote("ice")["model"] == llm.GEMMA
    assert llm.remote("report")["key"] == "dash", "the report falls back on the dashboard key"
    monkeypatch.setenv("OPENROUTER_REPORT_KEY", "rep")
    assert llm.remote("report")["key"] == "rep"
    monkeypatch.setenv("OPENROUTER_MODEL", "some/other")
    assert llm.remote("wiki")["model"] == "some/other"
    assert llm.model("report") == llm.FLASH
    monkeypatch.delenv("OPENROUTER_ICE_KEY")
    assert llm.remote("ice") is None, "the ice camera never runs on the shared key"


def test_body_turns_the_template_switch_into_reasoning():
    st = {"backend": "openrouter", "model": llm.GEMMA, "key": "k"}
    b = llm.body(st, {"model": "local", "keep_alive": -1, "chat_template_kwargs": {"enable_thinking": False}, "messages": []})
    assert b == {"model": llm.GEMMA, "messages": [], "reasoning": {"enabled": False}}
    flash = dict(st, model=llm.FLASH)
    off = llm.body(flash, {"chat_template_kwargs": {"enable_thinking": False}})
    assert off["reasoning"] == {"effort": "low"}, "Gemini refuses enabled:false; it gets its lowest effort"
    assert llm.body(flash, {"chat_template_kwargs": {"enable_thinking": True}})["reasoning"] == {"enabled": True}
    unknown = dict(st, model="not/listed")
    assert llm.body(unknown, {"chat_template_kwargs": {"enable_thinking": False}})["reasoning"] == {"enabled": False}
    assert llm.headers(st)["Authorization"] == "Bearer k"
    local = {"backend": "openai", "model": "gemma-camera"}
    assert "chat_template_kwargs" in llm.body(local, {"chat_template_kwargs": {"enable_thinking": False}})
    assert "Authorization" not in llm.headers(local)


class FakeResponse:
    def __init__(self, code=200, payload=None):
        self.status_code, self.payload = code, payload or {"choices": [{"message": {"content": "hello"}}]}

    def raise_for_status(self):
        if self.status_code >= 400:
            raise RuntimeError(self.status_code)

    def json(self):
        return self.payload


def test_complete_goes_to_openrouter_with_the_parts_model(monkeypatch):
    import requests
    sent = {}

    def post(url, json=None, headers=None, timeout=None):
        sent.update(url=url, json=json, headers=headers)
        return FakeResponse()

    monkeypatch.setattr(requests, "post", post)
    monkeypatch.setenv(llm.SHARED_KEY, "wk")
    monkeypatch.setenv("OPENROUTER_MODEL", "some/wiki-model")
    assert chatbot.complete("sys", "q", use="wiki") == "hello"
    assert sent["url"] == "https://openrouter.ai/api/v1/chat/completions"
    assert sent["headers"]["Authorization"] == "Bearer wk"
    assert sent["json"]["model"] == "some/wiki-model"
    assert "chat_template_kwargs" not in sent["json"]


def test_a_refused_key_reads_as_offline(monkeypatch):
    import requests
    monkeypatch.setattr(requests, "post", lambda *a, **k: FakeResponse(401))
    monkeypatch.setenv(llm.SHARED_KEY, "bad")
    with pytest.raises(chatbot.ModelOffline, match="refused"):
        chatbot.complete("sys", "q")


def test_crew_wakes_with_a_key_even_without_the_local_gpu(monkeypatch, tmp_path):
    monkeypatch.setenv("UNDERWAY_LLM", "0")
    monkeypatch.setattr(chatbot, "CONFIG_DIR", tmp_path)
    crew = lambda: chatbot.Crew(tmp_path, lambda *a, **k: None, lambda *a, **k: [])   # noqa: E731
    assert not crew().enabled
    monkeypatch.setenv(llm.SHARED_KEY, "ck")
    assert crew().enabled


def test_model_box_left_at_the_default_is_not_written(clean_env):
    S.save({"OPENROUTER_MODEL": llm.FLASH, "OPENROUTER_ICE_MODEL": "some/other"}, set())
    site = S.read_env(clean_env / "site.env")
    assert "OPENROUTER_MODEL" not in site
    assert site["OPENROUTER_ICE_MODEL"] == "some/other"


def test_page_prefills_defaults_and_lists_models(clean_env):
    S.set_password("correct horse")
    html = S.render(S.check_session(S.make_session()))
    assert f'name="f_OPENROUTER_MODEL" value="{llm.FLASH}"' in html
    assert f'name="f_OPENROUTER_ICE_MODEL" value="{llm.GEMMA}"' in html
    assert 'list="models-images"' in html and '<datalist id="models-text">' in html
    assert "AI models:</b> off: add an OpenRouter key" in html


def test_check_flags_unknown_and_text_only_models(clean_env, monkeypatch):
    S.save({llm.SHARED_KEY: "sk-or-abc12345", "OPENROUTER_MODEL": "some/text-only",
            "CRUISE_DIGITIZE_MODEL": "no/such"}, set())
    import urllib.request
    monkeypatch.setattr(urllib.request, "urlopen",
                        lambda req, timeout=None: io.BytesIO(json.dumps({"data": {"limit_remaining": 4.5}}).encode()))
    state, words = S.check_ai()
    assert state == "bad"
    assert "works" in words and "4.50 credit left" in words
    assert "Cruise report sheets: OpenRouter has no model called no/such" in words
    assert "Dashboard AI: some/text-only does not read images" in words


def test_an_empty_model_list_is_never_cached(tmp_path, monkeypatch):
    import os
    import urllib.request
    monkeypatch.undo()                      # the real llm.models, not the fixture's listing
    good = [{"id": "a/b", "in": ["text"], "reasoning": {}}]
    (tmp_path / llm.MODELS_FILE).write_text(json.dumps(good))
    os.utime(tmp_path / llm.MODELS_FILE, (0, 0))      # stale: a fetch is due
    monkeypatch.setattr(urllib.request, "urlopen", lambda *a, **k: io.BytesIO(b'{"error": "busy"}'))
    assert llm.models(tmp_path) == good, "falls back on the stale list"
    assert json.loads((tmp_path / llm.MODELS_FILE).read_text()) == good, "and never overwrites it with []"
