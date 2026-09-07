"""Schedule alerts: subscriptions, what is due, Telegram commands, one
message per subscription per run. Senders are fakes; nothing leaves."""
import json
import tempfile
import unittest
from datetime import datetime, timedelta, timezone
from pathlib import Path
from unittest.mock import patch

from dashboard import alerts


def _row(station, op, start, end, status="", comment=""):
    return {"station": station, "operation": op, "status": status, "start_utc": start, "end_utc": end,
            "comment": comment, "key": f"{station}|{op}"}


class FakeTelegram:
    def __init__(self, updates=()):
        self.updates_q = list(updates); self.sent = []
    def me(self): return "amundsen_bot"
    def updates(self, offset): u = [x for x in self.updates_q if x["update_id"] >= offset]; return u
    def send(self, chat_id, text): self.sent.append((chat_id, text))


class AlertTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(); self.addCleanup(self.tmp.cleanup)
        self.db = Path(self.tmp.name)
        p = patch.object(alerts, "DB_DIR", self.db); p.start(); self.addCleanup(p.stop)
        self.now = datetime(2026, 9, 6, 13, 0, tzinfo=timezone.utc)

    def rows(self, *rs):
        (self.db / "schedule.json").write_text(json.dumps({"rows": [
            {"station": r["station"], "operation": r["operation"], "status": r["status"], "comment": r["comment"],
             "date": "06/09/26", "start": datetime.fromisoformat(r["start_utc"]).astimezone(alerts.TZ).strftime("%H:%M"),
             "end": datetime.fromisoformat(r["end_utc"]).astimezone(alerts.TZ).strftime("%H:%M"), "duration_h": None} for r in rs]}))

    def test_subscribe_validates_and_updates_in_place(self):
        with self.assertRaises(ValueError):
            alerts.subscribe("email", "not an address")
        with self.assertRaises(ValueError):
            alerts.subscribe("carrier pigeon", "x")
        s = alerts.subscribe("email", "Bob@Example.org", "CardS-3; ctd", "abc" if False else 45, ["upcoming", "bogus"], "Bob")
        self.assertEqual((s["match"], s["lead_min"], s["events"]), ("CardS-3, ctd", 45, ["upcoming"]))
        again = alerts.subscribe("email", "bob@example.org", "", 30, None)
        self.assertEqual(again["id"], s["id"])
        self.assertEqual(len(alerts.load_subs()), 1)
        self.assertEqual(again["events"], list(alerts.DEFAULT_EVENTS))
        self.assertTrue(alerts.unsubscribe(s["id"]))
        self.assertIsNone(alerts.unsubscribe(s["id"]))
        self.assertEqual(alerts.load_subs(), [])

    def test_matching(self):
        row = _row("CardS-3", "CTD-Rosette", "", "")
        self.assertTrue(alerts.matches({"match": ""}, row))
        self.assertTrue(alerts.matches({"match": "cards-3"}, row))
        self.assertTrue(alerts.matches({"match": "JSW-01, ctd"}, row))
        self.assertFalse(alerts.matches({"match": "JSW-01, Box Core"}, row))

    def test_due_events_and_one_message_per_subscription(self):
        t = lambda m: (self.now + timedelta(minutes=m)).isoformat(timespec="minutes")
        state = alerts.load_state()
        rows = [_row("CardS-3", "CTD-Rosette", t(20), t(80)), _row("CardS-3", "TM-Rosette", t(90), t(150)),
                _row("JSW-01", "Box Core", t(-60), t(-10), status="In progress")]
        ev = alerts.due_events(rows, state, self.now)
        self.assertEqual([e[0] for e in ev], ["upcoming", "upcoming"])         # nothing "started": no earlier version to compare with
        subs = [alerts.subscribe("email", "a@example.org", "", 30, ["upcoming", "started", "finished", "moved"]),
                alerts.subscribe("telegram", "42", "JSW", 30, ["upcoming", "started", "finished", "moved"])]
        msgs = alerts.messages_for(subs, ev, state, self.now)
        self.assertEqual([(s["to"], len(lines)) for s, lines in msgs], [("a@example.org", 1)])   # TM-Rosette is beyond the 30 min lead
        self.assertIn("Starting in 20 min: CardS-3 — CTD-Rosette", msgs[0][1][0])
        self.assertEqual(alerts.messages_for(subs, ev, state, self.now), [])    # never twice
        # later: the CTD is under way, the box core is done, the TM rosette slips an hour
        later = self.now + timedelta(minutes=25)
        rows = [_row("CardS-3", "CTD-Rosette", t(20), t(80), status="In progress"), _row("CardS-3", "TM-Rosette", t(150), t(210)),
                _row("JSW-01", "Box Core", t(-60), t(-10), status="Completed")]
        ev = alerts.due_events(rows, state, later)
        self.assertEqual(sorted(e[0] for e in ev if e[0] != "upcoming"), ["finished", "moved", "started"])   # upcoming repeats each run; messages_for dedupes it
        msgs = dict((s["to"], lines) for s, lines in alerts.messages_for(subs, ev, state, later))
        self.assertEqual(len(msgs["a@example.org"]), 3)
        self.assertEqual(msgs["42"], ["Completed: JSW-01 — Box Core"]) if "42" in msgs else self.fail("telegram subscriber should hear about JSW-01")
        self.assertIn("Moved later by 60 min", " ".join(msgs["a@example.org"]))

    def test_telegram_commands_and_run(self):
        t = lambda m: (self.now + timedelta(minutes=m)).isoformat(timespec="minutes")
        self.rows(_row("CardS-3", "CTD-Rosette", t(20), t(80)))
        upd = [{"update_id": 7, "message": {"chat": {"id": 42}, "from": {"first_name": "Ann"}, "text": "/start"}},
               {"update_id": 8, "message": {"chat": {"id": 42}, "text": "/lead 60"}},
               {"update_id": 9, "message": {"chat": {"id": 43}, "text": "/only JSW"}},
               {"update_id": 10, "message": {"chat": {"id": 43}, "text": "/status"}}]
        tg = FakeTelegram(upd)
        emails = []
        r = alerts.run(self.now, tg=tg, email=lambda cfg, to, subject, body: emails.append((to, subject, body)))
        self.assertEqual(r["commands"], 4)
        subs = {s["to"]: s for s in alerts.load_subs()}
        self.assertEqual((subs["42"]["lead_min"], subs["42"]["match"], subs["42"]["name"]), (60, "", "Ann"))
        self.assertEqual(subs["43"]["match"], "JSW")
        alert_msgs = [x for x in tg.sent if "🔔" in x[1]]
        self.assertEqual(len(alert_msgs), 1)                                    # chat 42 hears about the CTD; 43 wants JSW only
        self.assertEqual(alert_msgs[0][0], "42"); self.assertIn("Starting in 20 min", alert_msgs[0][1])
        self.assertTrue(any("/status" in x[1] or "Subscribed:" in x[1] for x in tg.sent))
        self.assertEqual(alerts.load_state()["telegram_offset"], 11)
        self.assertEqual(alerts.load_state()["telegram_username"], "amundsen_bot")
        self.assertEqual(emails, [])
        # a second run sends nothing new
        r = alerts.run(self.now + timedelta(minutes=1), tg=FakeTelegram(), email=lambda *a: emails.append(a))
        self.assertEqual((r["sent"], r["commands"]), (0, 0))
        # an email subscriber gets the unsubscribe link
        alerts.subscribe("email", "ann@example.org", "", 30, None)
        with patch.object(alerts, "smtp_config", lambda: {"host": "x", "user": "u", "password": "p"}):
            r = alerts.run(self.now + timedelta(minutes=2), tg=FakeTelegram(), email=lambda cfg, to, subject, body: emails.append((to, subject, body)))
        self.assertEqual(r["sent"], 1)
        self.assertIn("/api/alerts/unsubscribe?token=", emails[0][2])


if __name__ == "__main__":
    unittest.main()
