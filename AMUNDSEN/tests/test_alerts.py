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
    def updates(self, offset, wait=0): return [x for x in self.updates_q if x["update_id"] >= offset]
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

    def test_web_channel_queues_for_the_browser(self):
        t = lambda m: (self.now + timedelta(minutes=m)).isoformat(timespec="minutes")
        self.rows(_row("CardS-3", "CTD-Rosette", t(20), t(80)))
        with self.assertRaises(ValueError):
            alerts.subscribe("web", "short", "", 30, None)
        alerts.subscribe("web", "b0f1c2d3e4f5a6b7", "", 30, None)
        r = alerts.run(self.now, tg=FakeTelegram(), email=lambda *a: None)
        self.assertEqual(r["sent"], 1)                                            # queued for the browser counts as delivered
        box = alerts.inbox("b0f1c2d3e4f5a6b7")
        self.assertEqual(len(box), 1); self.assertIn("Starting in 20 min", box[0]["text"])
        self.assertEqual(alerts.inbox("b0f1c2d3e4f5a6b7", since=box[0]["t"]), [])   # seen
        self.assertEqual(alerts.inbox("other0000000000"), [])

    def test_telegram_commands_and_run(self):
        t = lambda m: (self.now + timedelta(minutes=m)).isoformat(timespec="minutes")
        self.rows(_row("CardS-3", "CTD-Rosette", t(20), t(80)))
        upd = [{"update_id": 7, "message": {"chat": {"id": 42}, "from": {"first_name": "Ann"}, "text": "/all"}},
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
        self.assertEqual(alerts.load_offset(), 11)
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

    def test_whiteboard_command_and_change_notice(self):
        db = alerts.DB_DIR
        def board(text):
            (db / "schedule.json").write_text(json.dumps({"rows": [], "whiteboard": text}))
        board("Toolbox 08:30")
        tg = FakeTelegram([{"update_id": 1, "message": {"chat": {"id": 42}, "from": {"first_name": "Ann"}, "text": "/whiteboard"}}])
        alerts.run(self.now, tg=tg, email=lambda *a: None)
        self.assertIn("Toolbox 08:30", tg.sent[0][1])                              # the command answers with the board
        self.assertTrue(alerts.load_subs()[0].get("whiteboard"))
        tg = FakeTelegram(); alerts.run(self.now + timedelta(minutes=2), tg=tg, email=lambda *a: None)
        self.assertEqual(tg.sent, [])                                             # unchanged: quiet
        board("Toolbox 08:30\nHelicopter brief 13:00")
        tg = FakeTelegram(); r = alerts.run(self.now + timedelta(minutes=4), tg=tg, email=lambda *a: None)
        self.assertEqual(r["sent"], 1); self.assertIn("Helicopter brief", tg.sent[0][1])
        tg = FakeTelegram([{"update_id": 2, "message": {"chat": {"id": 42}, "text": "/whiteboard off"}}])
        alerts.run(self.now + timedelta(minutes=6), tg=tg, email=lambda *a: None)
        self.assertEqual(alerts.load_subs(), [])                                  # nothing else was subscribed: gone
        board("cleared again")
        tg = FakeTelegram(); alerts.run(self.now + timedelta(minutes=8), tg=tg, email=lambda *a: None)
        self.assertEqual(tg.sent, [])

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
        alerts.handle_telegram(tg)
        self.assertEqual(alerts.following("telegram", "7")["rows"], [key])
        self.assertFalse(alerts.load_subs()[0]["all"])
        self.assertIn("Following CardS-3 — CTD-Rosette", tg.sent[0][1])
        self.assertIn("• CardS-3 — CTD-Rosette (15 min ahead)", tg.sent[1][1])


class ChangesTests(unittest.TestCase):
    """Following ``changes``: an operation added, taken off before it started,
    moved or canceled, and nothing else."""
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(); self.addCleanup(self.tmp.cleanup)
        self.db = Path(self.tmp.name)
        p = patch.object(alerts, "DB_DIR", self.db); p.start(); self.addCleanup(p.stop)
        self.now = datetime(2026, 9, 6, 13, 0, tzinfo=timezone.utc)

    def t(self, m):
        return (self.now + timedelta(minutes=m)).isoformat(timespec="minutes")

    def test_added_removed_moved_canceled(self):
        state = alerts.load_state()
        ctd, net, core = _row("S1", "CTD", self.t(60), self.t(120)), _row("S2", "Net", self.t(180), self.t(240)), _row("S0", "Core", self.t(-120), self.t(-60), status="Completed")
        self.assertEqual([e for e, _, _ in alerts.due_events([ctd, net, core], state, self.now) if e != "upcoming"], [])   # the first run only records
        box = _row("S3", "Box Core", self.t(300), self.t(360))
        moved = dict(ctd, start_utc=self.t(90), end_utc=self.t(150))
        canceled = {k: v for k, v in net.items() if k not in ("start_utc", "end_utc")} | {"status": "Canceled"}   # the page drops a canceled row's times
        ev = {(e, r["key"]) for e, r, _ in alerts.due_events([moved, canceled, box], state, self.now) if e != "upcoming"}   # the core has scrolled off
        self.assertEqual(ev, {("moved", "S1|CTD"), ("finished", "S2|Net"), ("added", "S3|Box Core")})
        self.assertNotIn("S0|Core", state["rows"])                                                                # forgotten, not reported
        ev = [(e, r["key"], text) for e, r, text in alerts.due_events([moved, canceled], state, self.now)]
        self.assertEqual([(e, k) for e, k, _ in ev if e != "upcoming"], [("removed", "S3|Box Core")])
        self.assertIn("Taken off the schedule: S3 — Box Core", ev[-1][2])
        self.assertEqual(alerts.due_events([], state, self.now), [])                                               # a failed read removes nothing
        self.assertIn("S1|CTD", state["rows"])

    def test_changes_only_subscriber(self):
        state = alerts.load_state()
        alerts.due_events([_row("S1", "CTD", self.t(20), self.t(80)), _row("S2", "Net", self.t(180), self.t(240))], state, self.now)
        sub = alerts.set_changes("telegram", "42", True, "Ann")
        everyone = alerts.subscribe("telegram", "43", "", 30, None)
        rows = [_row("S1", "CTD", self.t(20), self.t(80), status="In progress"), _row("S2", "Net", self.t(180), self.t(240), status="Completed"),
                _row("S3", "Box Core", self.t(300), self.t(360))]
        msgs = {s["to"]: lines for s, lines in alerts.messages_for([sub, everyone], alerts.due_events(rows, state, self.now), state, self.now)}
        self.assertEqual(msgs["42"], ["Added: S3 — Box Core (" + alerts._when(rows[2]) + ")"])   # no heads-up, no start, no completion
        self.assertTrue(any(l.startswith("Now in progress") for l in msgs["43"]))
        self.assertFalse(any(l.startswith("Added") for l in msgs["43"]))                        # a general subscription does not hear of additions

    def test_following_the_bell_and_telegram(self):
        alerts.set_whiteboard("email", "ann@example.org", True)
        alerts.follow_row("email", "ann@example.org", alerts.CHANGES_KEY)
        self.assertEqual(alerts.following("email", "ann@example.org")["rows"], ["changes"])
        self.assertIsNotNone(alerts.follow_row("email", "ann@example.org", alerts.CHANGES_KEY, remove=True))   # the whiteboard keeps it
        self.assertEqual(alerts.following("email", "ann@example.org")["rows"], [])
        upd = [{"update_id": 1, "message": {"chat": {"id": 42}, "text": "/start " + alerts.encode_row(alerts.CHANGES_KEY)}},
               {"update_id": 2, "message": {"chat": {"id": 42}, "text": "/status"}},
               {"update_id": 3, "message": {"chat": {"id": 42}, "text": "/changes off"}}]
        tg = FakeTelegram(upd)
        alerts.handle_telegram(tg)
        self.assertIn("whenever the schedule changes", tg.sent[0][1])
        self.assertIn("Schedule changes: yes", tg.sent[1][1])
        self.assertIsNone(alerts._find(alerts.load_subs(), "telegram", "42"))                    # nothing left to follow


class SmtpConfigTests(unittest.TestCase):
    def test_account_from_the_environment(self):
        with patch.dict("os.environ", {}, clear=True):
            self.assertIsNone(alerts.smtp_config())
        with patch.dict("os.environ", {"SMTP_HOST": "smtp.example.org", "SMTP_USER": "keeper@example.org"}, clear=True):
            c = alerts.smtp_config()
            self.assertEqual((c["host"], c["user"], c["ssl"], c["from"]), ("smtp.example.org", "keeper@example.org", True, ""))
            with patch.object(alerts, "OPS_EMAIL", ""):
                self.assertEqual(alerts.ops_targets()[0], "keeper@example.org")
        with patch.dict("os.environ", {"SMTP_HOST": "h", "SMTP_USER": "u", "SMTP_SSL": "0", "SMTP_PORT": "587"}, clear=True):
            c = alerts.smtp_config()
            self.assertEqual((c["ssl"], c["port"]), (False, "587"))


class BotTests(unittest.TestCase):
    def test_timer_leaves_commands_to_a_live_bot(self):
        with tempfile.TemporaryDirectory() as tmp, patch.object(alerts, "DB_DIR", Path(tmp)), patch.object(alerts, "WEBROOT", Path(tmp)):
            (Path(tmp) / "schedule.json").write_text(json.dumps({"rows": []}))
            tg = FakeTelegram([{"update_id": 3, "message": {"chat": {"id": 8}, "text": "/all"}}])
            (Path(tmp) / "telegram_bot.alive").touch()                   # the bot service is polling
            r = alerts.run(datetime(2026, 9, 7, 3, 0, tzinfo=timezone.utc), tg=tg, email=lambda *a: None)
            self.assertEqual(r["commands"], 0); self.assertEqual(alerts.load_subs(), [])
            self.assertFalse(alerts.bot_alive.__wrapped__() if hasattr(alerts.bot_alive, "__wrapped__") else not alerts.bot_alive())
            import os
            os.utime(Path(tmp) / "telegram_bot.alive", (0, 0))            # the heartbeat has gone stale: the timer takes over
            r = alerts.run(datetime(2026, 9, 7, 3, 2, tzinfo=timezone.utc), tg=tg, email=lambda *a: None)
            self.assertEqual(r["commands"], 1); self.assertEqual(alerts.load_subs()[0]["to"], "8")
            self.assertEqual(alerts.load_offset(), 4)


class OpsTests(unittest.TestCase):
    def test_stale_record_is_reported_once_and_recovery_once(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp); (root / "data").mkdir()
            now = datetime(2026, 9, 7, 3, 0, tzinfo=timezone.utc)
            def manifest(last): (root / "data" / "manifest.json").write_text(json.dumps({"sources": {"full_csv": last}}))
            mails, tgs = [], []
            class TG:
                def send(self, chat, text): tgs.append((chat, text))
            with patch.object(alerts, "WEBROOT", root), patch.object(alerts, "DB_DIR", root), \
                    patch.object(alerts, "ops_targets", lambda: ("keeper@example.org", {"host": "x", "user": "u", "password": "p"}, "99")):
                state = alerts.load_state()
                manifest((now - timedelta(minutes=12)).isoformat())
                self.assertEqual(alerts.ops_check(state, now, TG(), lambda cfg, to, subject, body: mails.append((to, subject))), [])
                manifest((now - timedelta(minutes=54)).isoformat())
                sent = alerts.ops_check(state, now, TG(), lambda cfg, to, subject, body: mails.append((to, subject)))
                self.assertEqual(len(sent), 1); self.assertIn("54 min ago", sent[0])
                self.assertEqual(mails, [("keeper@example.org", "Underway dashboard: FULL_CSV stale")])
                self.assertEqual(tgs[0][0], "99")
                self.assertEqual(alerts.ops_check(state, now + timedelta(minutes=10), TG(), lambda *a: mails.append(a)), [])   # not again
                manifest((now + timedelta(minutes=20)).isoformat())
                sent = alerts.ops_check(state, now + timedelta(minutes=22), TG(), lambda cfg, to, subject, body: mails.append((to, subject)))
                self.assertEqual(len(sent), 1); self.assertIn("recovered", sent[0])
                self.assertIsNone(state["ops"]["stale_since"])


class FlagTests(unittest.TestCase):
    """Review flags on History artifacts: raised with a note by anyone,
    withdrawn by the raiser alone or an admin, rate limited, reported once."""
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(); self.addCleanup(self.tmp.cleanup)
        p = patch.object(alerts, "DB_DIR", Path(self.tmp.name)); p.start(); self.addCleanup(p.stop)
        p = patch.object(alerts, "is_admin", lambda name, token: (name, token) == ("Keeper", "ktok")); p.start(); self.addCleanup(p.stop)
        p = patch.object(alerts, "ops_targets", lambda: ("keeper@example.org", {"host": "x", "user": "u", "password": "p"}, "42")); p.start(); self.addCleanup(p.stop)
        self.now = datetime(2026, 9, 9, 13, 0, tzinfo=timezone.utc)

    def test_raise_withdraw_and_who_may(self):
        r = alerts.set_flag("thule-023", True, "tokA", "Ann", "Port Refuge", "artifact/thule-023", "wrong coordinates", self.now)
        self.assertEqual([(f["id"], f["mine"], f["raisers"][0]["note"]) for f in r["flags"]], [("thule-023", True, "wrong coordinates")])
        self.assertFalse(r["admin"])
        self.assertNotIn("tokA", (Path(self.tmp.name) / "history_flags.json").read_text())      # the token itself is never written
        self.assertFalse(alerts.flagged("tokB", "Bob")["flags"][0]["mine"])
        with self.assertRaises(PermissionError):                                                 # not Bob's to withdraw
            alerts.set_flag("thule-023", False, "tokB", "Bob", now=self.now)
        alerts.set_flag("thule-023", True, "tokB", "Bob", note="agree", now=self.now)          # Bob adds his own
        self.assertEqual(len(alerts.flagged()["flags"][0]["raisers"]), 2)
        alerts.set_flag("thule-023", True, "tokB", "Bob", note="again", now=self.now)          # once per device
        self.assertEqual(len(alerts.flagged()["flags"][0]["raisers"]), 2)
        with self.assertRaises(PermissionError):                                                 # two people flagged: Ann alone cannot clear
            alerts.set_flag("thule-023", False, "tokA", "Ann", now=self.now)
        r = alerts.set_flag("thule-023", False, "ktok", "Keeper", now=self.now)
        self.assertEqual((r["flags"], r["admin"]), ([], True))
        with self.assertRaises(ValueError):
            alerts.set_flag("../etc", True, "tokA", "Ann", now=self.now)
        with self.assertRaises(ValueError):
            alerts.set_flag("thule-023", True, "", "Ann", now=self.now)

    def test_rate_limit(self):
        for i in range(alerts.FLAGS_PER_HOUR):
            alerts.set_flag(f"a-{i}", True, "tokA", "Ann", now=self.now + timedelta(minutes=i))
        with self.assertRaises(alerts.TooMany):
            alerts.set_flag("a-more", True, "tokA", "Ann", now=self.now + timedelta(minutes=30))
        alerts.set_flag("a-more", True, "tokB", "Bob", now=self.now + timedelta(minutes=30))             # another device may
        alerts.set_flag("a-later", True, "tokA", "Ann", now=self.now + timedelta(minutes=61))            # the hour has passed
        with patch.object(alerts, "FLAGS_PER_HOUR_ALL", 3):
            with self.assertRaises(alerts.TooMany):
                alerts.set_flag("a-all", True, "tokC", "Cy", now=self.now + timedelta(minutes=62))

    def test_reported_once_in_one_message(self):
        alerts.set_flag("a-1", True, "tokA", "Ann", "First", "artifact/a-1", "typo", self.now)
        alerts.set_flag("a-2", True, "tokB", "Bob", "Second", "artifact/a-2", "", self.now)
        alerts.set_flag("a-3", True, "tokC", "", "Third", "artifact/a-3", "", self.now)
        alerts.set_flag("a-3", False, "tokC", "", now=self.now)                                  # withdrawn before the run: unsent
        mails, tg = [], FakeTelegram()
        lines = alerts.flag_notices(tg, lambda cfg, to, subject, body: mails.append((to, subject, body)))
        self.assertEqual(len(lines), 2)
        self.assertEqual(len(mails), 1)
        self.assertEqual(mails[0][0], "keeper@example.org")
        self.assertIn("2 history artifacts flagged", mails[0][1])
        self.assertIn("Ann flagged First (a-1)", mails[0][2]); self.assertIn(": typo", mails[0][2]); self.assertIn("#history/artifact/a-2", mails[0][2])
        self.assertNotIn("Third", mails[0][2])
        self.assertEqual(tg.sent[0][0], "42"); self.assertIn("🚩", tg.sent[0][1])
        self.assertEqual(alerts.flag_notices(tg, lambda *a: mails.append(a)), [])                # not again
        self.assertEqual(len(mails), 1)
        # a channel failure leaves them for the next run
        alerts.set_flag("a-4", True, "tokA", "Ann", "Fourth", now=self.now)
        def boom(*a): raise OSError("smtp down")
        self.assertEqual(alerts.flag_notices(None, boom), [])
        self.assertEqual(len(alerts.flag_notices(None, lambda cfg, to, subject, body: mails.append((to, subject, body)))), 1)
        # and run() carries them
        alerts.set_flag("a-5", True, "tokA", "Ann", "Fifth", now=self.now)
        alerts.run(self.now, tg=FakeTelegram(), email=lambda cfg, to, subject, body: mails.append((to, subject, body)))
        self.assertIn("Fifth", mails[-1][2])
