"""Transient conversations never enter the persistent shared-room log."""
import pytest

from dashboard import chat, chatbot


@pytest.fixture
def temporary_chat(tmp_path, monkeypatch):
    monkeypatch.setattr(chat, "CHAT_DB", tmp_path / "chat.sqlite")
    for key in ("_dms", "_dm_starts", "_online", "_last_post"):
        monkeypatch.setattr(chat, key, {})
    monkeypatch.setattr(chat, "CREW", None)
    monkeypatch.setattr(chat, "bots", lambda: {})
    return "dm:alice|bob"


def test_dm_memory_auth_expiry_and_clear(temporary_chat, monkeypatch):
    room = temporary_chat
    monkeypatch.setattr(chat, "CHAT_PAGE", 2)
    first = chat.post("a", "Alice", "first", channel=room, token="alice-token")
    assert "id" in first
    # Retention trims messages but preserves the conversation generation.
    for i in range(3):
        assert "id" in chat.post(str(i), "Bob", str(i), channel=room, token="bob-token")
    current = chat.read(0, "Alice", "alice-token", channel=room)
    assert len(current["messages"]) == 2
    assert current["history_start"] == first["id"]
    assert chat.read(0, "Alice", "wrong-token", channel=room)["messages"] == []
    assert chat.read(0, "Eve", "eve-token", channel=room)["messages"] == []
    with chat.conn() as c:
        assert c.execute("SELECT COUNT(*) FROM messages").fetchone()[0] == 0
    assert chat.clear(room, "Bob", "bob-token")["deleted"] == 2
    assert chat.read(0, "Alice", "alice-token", channel=room)["history_start"] is None
    new = chat.post("new", "Alice", "new", channel=room, token="alice-token")
    assert new["id"] != first["id"]
    monkeypatch.setattr(chat.time, "time", lambda: new["t"] + chat.DM_TTL + 1)
    assert chat.read(0, "Alice", "alice-token", channel=room)["messages"] == []
    assert room not in chat._dm_starts


def test_migration_removes_only_saved_dms(temporary_chat):
    with chat.conn() as c:
        c.execute("UPDATE schema SET version=2")
        c.executemany("INSERT INTO messages (t,name,text,channel) VALUES (0,'Alice','hello',?)", [(temporary_chat,), ("ship",)])
    with chat.conn() as c:
        assert c.execute("SELECT channel FROM messages").fetchall() == [("ship",)]


def test_navigation_position():
    assert chatbot.navigation_position(78.167, -92.822) == "78° 10.020′ N, 92° 49.320′ W"
    assert chatbot.navigation_position(-89.9999999, 179.9999999) == "90° 00.000′ S, 180° 00.000′ E"
