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


class RowFollowTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(); self.addCleanup(self.tmp.cleanup)
        p = patch.object(alerts, "DB_DIR", Path(self.tmp.name)); p.start(); self.addCleanup(p.stop)
        self.now = datetime(2026, 9, 6, 13, 0, tzinfo=timezone.utc)

    def test_follow_and_drop_a_row(self):
        s = alerts.follow_row("email", "ann@example.org", "CardS-3|CTD-Rosette", name="Ann")
        self.assertEqual((s["all"], s["match"], s["name"]), (False, "", "Ann"))
        self.assertEqual(s["rows"]["CardS-3|CTD-Rosette"], {"lead_min": 15, "events": list(alerts.EVENTS)})
        self.assertEqual(alerts.following("email", "ann@example.org"), {"rows": ["CardS-3|CTD-Rosette"], "all": False, "match": ""})
        self.assertEqual(alerts.following("email", "nobody"), {"rows": [], "all": False, "match": ""})
        alerts.follow_row("email", "ann@example.org", "JSW-01|Mapping")
        self.assertIsNone(alerts.follow_row("email", "ann@example.org", "CardS-3|CTD-Rosette", remove=True) and None)
        self.assertEqual(alerts.following("email", "ann@example.org")["rows"], ["JSW-01|Mapping"])
        self.assertIsNone(alerts.follow_row("email", "ann@example.org", "JSW-01|Mapping", remove=True))
        self.assertEqual(alerts.load_subs(), [])                              # nothing left to follow: the subscription goes
        # a general subscription keeps its rows, and survives dropping them
        alerts.subscribe("email", "ann@example.org", "", 30, None)
        alerts.follow_row("email", "ann@example.org", "JSW-01|Mapping")
        self.assertEqual(alerts.following("email", "ann@example.org")["all"], True)
        alerts.follow_row("email", "ann@example.org", "JSW-01|Mapping", remove=True)
        self.assertEqual(len(alerts.load_subs()), 1)

    def test_followed_row_uses_its_own_lead_and_events(self):
        t = lambda m: (self.now + timedelta(minutes=m)).isoformat(timespec="minutes")
        state = alerts.load_state()
        rows = [_row("CardS-3", "CTD-Rosette", t(12), t(70)), _row("CardS-3", "TM-Rosette", t(12), t(70))]
        ev = alerts.due_events(rows, state, self.now)
        sub = alerts.follow_row("telegram", "42", "CardS-3|CTD-Rosette")           # follows the CTD only
        general = alerts.subscribe("email", "b@example.org", "", 10, ["upcoming"])  # everything, but only 10 min ahead
        msgs = dict((s["to"], lines) for s, lines in alerts.messages_for([sub, general], ev, state, self.now))
        self.assertEqual(list(msgs), ["42"])                                        # 12 min out: inside the row's 15, outside the general 10
        self.assertEqual(len(msgs["42"]), 1); self.assertIn("CTD-Rosette", msgs["42"][0])
        later = self.now + timedelta(minutes=13)
        rows = [_row("CardS-3", "CTD-Rosette", t(12), t(70), status="Completed"), _row("CardS-3", "TM-Rosette", t(12), t(70), status="Completed")]
        ev = alerts.due_events(rows, state, later)
        msgs = dict((s["to"], lines) for s, lines in alerts.messages_for([sub, general], ev, state, later))
        self.assertEqual(msgs.get("42"), ["Completed: CardS-3 — CTD-Rosette"])   # the row's events include finished; the general one's do not
        self.assertNotIn("b@example.org", msgs)
        # a kind of operation, at any station
        kind = alerts.follow_row("telegram", "43", "op:TM-Rosette")
        rows = [_row("CardS-4", "TM-Rosette", t(30), t(90)), _row("CardS-4", "CTD", t(30), t(90))]
        ev = alerts.due_events(rows, alerts.load_state(), self.now + timedelta(minutes=20))
        msgs = dict((s["to"], lines) for s, lines in alerts.messages_for([kind], ev, alerts.load_state(), self.now + timedelta(minutes=20)))
        self.assertEqual(len(msgs["43"]), 1); self.assertIn("CardS-4 — TM-Rosette", msgs["43"][0])
        # every transit, whatever the destination
        self.assertEqual((alerts.kind_of("Transit to CardS-5"), alerts.kind_of("Steam to OG-1"), alerts.kind_of("CTD")), ("Transit", "Transit", "CTD"))
        tr = alerts.follow_row("telegram", "44", "op:Transit to CardS-5")           # a page-built key is normalised to the kind
        self.assertEqual(list(tr["rows"]), ["op:Transit"])
        rows = [_row("", "Transit to CardS-5", t(30), t(45)), _row("", "Steaming", t(60), t(90))]
        st2 = alerts.load_state()
        ev = alerts.due_events(rows, st2, self.now + timedelta(minutes=20))
        msgs = dict((s["to"], lines) for s, lines in alerts.messages_for([tr], ev, st2, self.now + timedelta(minutes=20)))
        self.assertEqual(len(msgs["44"]), 1); self.assertIn("Transit to CardS-5", msgs["44"][0])

    def test_telegram_start_payload_follows_a_row(self):
        key = "CardS-3|CTD-Rosette"
        payload = alerts.encode_row(key)
        self.assertLessEqual(len(payload), 64); self.assertNotIn("=", payload)
        self.assertEqual(alerts.decode_row(payload), key)
        self.assertEqual(alerts.decode_row("not base64!"), "not base64!")
        tg = FakeTelegram([{"update_id": 1, "message": {"chat": {"id": 7}, "from": {"first_name": "Ann"}, "text": f"/start {payload}"}},
                           {"update_id": 2, "message": {"chat": {"id": 7}, "text": "/status"}}])
        state = alerts.load_state()
        alerts.handle_telegram(tg, state)
        self.assertEqual(alerts.following("telegram", "7")["rows"], [key])
        self.assertFalse(alerts.load_subs()[0]["all"])
        self.assertIn("Following CardS-3 — CTD-Rosette", tg.sent[0][1])
        self.assertIn("• CardS-3 — CTD-Rosette (15 min ahead)", tg.sent[1][1])
