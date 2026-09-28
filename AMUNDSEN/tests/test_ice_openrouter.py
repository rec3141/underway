"""The ice camera's OpenRouter route, against a fake HTTP layer (no real requests)."""
import io
import json
import sys
import urllib.error

import pytest

from dashboard import ice_store, ice_worker

ANSWER = {"choices": [{"finish_reason": "stop", "message": {"content": "{}"}}]}
PAYLOAD = dict(messages=[dict(role="user", content="hi")], temperature=0, stream=False,
               chat_template_kwargs=dict(enable_thinking=False))


@pytest.fixture
def no_keys(monkeypatch):
    for k in ("OPENROUTER_ICE_KEY", "OPENROUTER_API_KEY", "OPENROUTER_ICE_MODEL"):
        monkeypatch.delenv(k, raising=False)
    # the model list is fetched with the same urlopen these tests fake
    monkeypatch.setattr(ice_worker.llm, "models", lambda cache_dir=None: [])


@pytest.fixture
def owned_gpu_forbidden(monkeypatch):
    def boom(*a, **k):
        raise AssertionError("touched the owned GPU server")
    monkeypatch.setattr(ice_worker.Client, "temperature", boom)
    monkeypatch.setattr(ice_worker.subprocess, "run", boom)
    monkeypatch.setattr(ice_worker.subprocess, "check_output", boom)


class Fake:
    """Stands in for urlopen: records each request and answers from a list (a dict or an HTTP status)."""
    def __init__(self, *answers):
        self.answers = list(answers)
        self.requests = []

    def __call__(self, req, timeout=None):
        self.requests.append(req)
        a = self.answers.pop(0)
        if isinstance(a, int):
            raise urllib.error.HTTPError(req.full_url, a, "no", {"Retry-After": "7"} if a == 429 else {}, io.BytesIO(b"{}"))
        return io.BytesIO(json.dumps(a).encode())


def client(tmp_path, config=None):
    return ice_worker.Client(config or {}, ice_store.connect(tmp_path))


def test_remote_sends_key_and_model_and_skips_gpu(tmp_path, monkeypatch, no_keys, owned_gpu_forbidden):
    monkeypatch.setenv("OPENROUTER_ICE_KEY", "sk-test")
    fake = Fake(ANSWER)
    monkeypatch.setattr(ice_worker.urllib.request, "urlopen", fake)
    c = client(tmp_path)
    assert c(dict(PAYLOAD)) == ANSWER
    req = fake.requests[0]
    assert req.full_url == "https://openrouter.ai/api/v1/chat/completions"
    assert req.get_header("Authorization") == "Bearer sk-test"
    body = json.loads(req.data)
    assert body["model"] == "google/gemma-4-26b-a4b-it"
    assert body["reasoning"] == {"enabled": False} and "chat_template_kwargs" not in body
    assert c.ready()


def test_own_key_and_model_setting_but_never_the_shared_key(tmp_path, monkeypatch, no_keys, owned_gpu_forbidden):
    monkeypatch.setenv("OPENROUTER_API_KEY", "sk-shared")
    assert ice_worker.llm.remote("ice") is None
    monkeypatch.setenv("OPENROUTER_ICE_KEY", "sk-ice")
    monkeypatch.setenv("OPENROUTER_ICE_MODEL", "some/other-model")
    fake = Fake(ANSWER)
    monkeypatch.setattr(ice_worker.urllib.request, "urlopen", fake)
    client(tmp_path)(dict(PAYLOAD))
    assert fake.requests[0].get_header("Authorization") == "Bearer sk-ice"
    assert json.loads(fake.requests[0].data)["model"] == "some/other-model"


def test_no_key_keeps_local_path(tmp_path, monkeypatch, no_keys):
    calls = []
    monkeypatch.setattr(ice_worker.Client, "temperature", lambda self: calls.append("temp") or (50, 50, 50))
    monkeypatch.setattr(ice_worker.urllib.request, "urlopen", lambda *a, **k: pytest.fail("remote call"))
    seen = []

    class Opener:
        def open(self, req, timeout=None):
            seen.append(req)
            return io.BytesIO(json.dumps(ANSWER).encode())
    monkeypatch.setattr(ice_worker.urllib.request, "build_opener", lambda *a: Opener())
    c = client(tmp_path, dict(url="http://127.0.0.1:18043/", server_unit="underway-ice-model.service"))
    assert c.remote is None
    assert c(dict(PAYLOAD)) == ANSWER
    assert "temp" in calls
    assert seen[0].full_url == "http://127.0.0.1:18043/v1/chat/completions"
    assert seen[0].get_header("Authorization") is None
    body = json.loads(seen[0].data)
    assert body["model"] == "gemma-camera" and body["chat_template_kwargs"] == {"enable_thinking": False}


@pytest.mark.parametrize("code", [401, 402])
def test_refused_key_stops_trying(tmp_path, monkeypatch, capsys, no_keys, owned_gpu_forbidden, code):
    monkeypatch.setenv("OPENROUTER_ICE_KEY", "sk-bad")
    fake = Fake(code)
    monkeypatch.setattr(ice_worker.urllib.request, "urlopen", fake)
    c = client(tmp_path)
    with pytest.raises(RuntimeError):
        c(dict(PAYLOAD))
    assert "OpenRouter refused the ice camera's key" in capsys.readouterr().out
    assert not c.ready()
    with pytest.raises(RuntimeError):
        c(dict(PAYLOAD))
    assert len(fake.requests) == 1


def test_refusal_in_a_200_body(tmp_path, monkeypatch, capsys, no_keys, owned_gpu_forbidden):
    monkeypatch.setenv("OPENROUTER_ICE_KEY", "sk-bad")
    monkeypatch.setattr(ice_worker.urllib.request, "urlopen", Fake({"error": {"code": 402, "message": "Insufficient credits"}}))
    c = client(tmp_path)
    with pytest.raises(RuntimeError):
        c(dict(PAYLOAD))
    assert c.refused == 402 and "Insufficient credits" in capsys.readouterr().out


@pytest.mark.parametrize("code,wait", [(429, 7), (503, 60)])
def test_busy_backs_off(tmp_path, monkeypatch, no_keys, owned_gpu_forbidden, code, wait):
    monkeypatch.setenv("OPENROUTER_ICE_KEY", "sk-test")
    fake = Fake(code, ANSWER)
    monkeypatch.setattr(ice_worker.urllib.request, "urlopen", fake)
    now = [1000.0]
    monkeypatch.setattr(ice_worker.time, "time", lambda: now[0])
    c = client(tmp_path)
    with pytest.raises(RuntimeError):
        c(dict(PAYLOAD))
    assert not c.ready() and c.pause_until == 1000 + wait
    with pytest.raises(RuntimeError):            # still waiting: no request goes out
        c(dict(PAYLOAD))
    assert len(fake.requests) == 1
    now[0] += wait
    assert c.ready() and c(dict(PAYLOAD)) == ANSWER and c.backoff == 0


def test_network_down_backs_off(tmp_path, monkeypatch, no_keys, owned_gpu_forbidden):
    monkeypatch.setenv("OPENROUTER_ICE_KEY", "sk-test")

    def down(*a, **k):
        raise urllib.error.URLError("no route")
    monkeypatch.setattr(ice_worker.urllib.request, "urlopen", down)
    c = client(tmp_path)
    with pytest.raises(RuntimeError, match="no route"):
        c(dict(PAYLOAD))
    assert not c.ready()


def test_main_without_config_uses_env(tmp_path, monkeypatch, no_keys, owned_gpu_forbidden):
    source = tmp_path / "Camera_360"
    source.mkdir()
    monkeypatch.setenv("OPENROUTER_ICE_KEY", "sk-test")
    monkeypatch.setenv("UNDERWAY_CAMERA_SOURCE", str(source))
    monkeypatch.setattr(ice_worker.ice_store, "ROOT", tmp_path / "ice")
    monkeypatch.setattr(sys, "argv", ["ice_worker", "--once"])
    ice_worker.main()
    assert (tmp_path / "ice/ice.sqlite").is_file() and (tmp_path / "ice/seawater-filter.json").is_file()


def test_main_refuses_without_key_or_server(tmp_path, monkeypatch, no_keys):
    monkeypatch.setenv("UNDERWAY_CAMERA_SOURCE", str(tmp_path))
    monkeypatch.setattr(sys, "argv", ["ice_worker", "--once"])
    with pytest.raises(SystemExit):
        ice_worker.main()


def test_store_root_follows_underway_home(monkeypatch):
    import importlib
    monkeypatch.delenv("UNDERWAY_ICE_ROOT", raising=False)
    monkeypatch.setenv("UNDERWAY_HOME", "/underway")
    try:
        assert str(importlib.reload(ice_store).ROOT) == "/underway/ice"
        monkeypatch.setenv("UNDERWAY_ICE_ROOT", "/elsewhere")
        assert str(importlib.reload(ice_store).ROOT) == "/elsewhere"
    finally:
        monkeypatch.undo()
        importlib.reload(ice_store)
