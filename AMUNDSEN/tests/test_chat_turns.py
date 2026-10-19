"""Ordered multi-persona turns without calling a model or posting externally."""
import tempfile
import threading
import unittest
from pathlib import Path
from unittest.mock import patch

from dashboard import chatbot


class CrewTurnTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.rows = []
        self.crew = chatbot.Crew(Path(self.tmp.name), self.post, lambda channel: list(self.rows))
        self.crew.enabled = True
        self.crew.context = lambda *args: 'Test readings'
        bots = patch('dashboard.chat.bots', return_value=chatbot.PERSONAS)
        bots.start()
        self.addCleanup(bots.stop)
        for name in ('typing', 'link_entities'):
            replacement = (lambda text: text) if name == 'link_entities' else (lambda *args: None)
            patcher = patch('dashboard.chat.' + name, replacement)
            patcher.start()
            self.addCleanup(patcher.stop)

    def post(self, name, emoji, text, channel, meta=None):
        self.rows.append(dict(name=name, emoji=emoji, text=text))

    def ask(self, text, channel='deck'):
        # the chat stores a human message before the crew hears of it
        self.rows.append(dict(name='Scientist', emoji='', text=text))
        self.crew.on_message('Scientist', text, channel)

    def finish(self):
        with self.crew._turn_lock:
            worker = self.crew._turn_worker
        if worker:
            worker.join(timeout=3)
            self.assertFalse(worker.is_alive(), 'turn worker did not finish')

    def test_waits_for_posted_response_and_keeps_human_turns_fifo(self):
        entered, release = threading.Event(), threading.Event()
        prompts = []

        def complete(system, task, *args, **kwargs):
            prompts.append((system, task, kwargs.get('history') or []))
            if len(prompts) == 1:
                entered.set()
                if not release.wait(3):
                    raise RuntimeError('test first response not released')
                return 'First answer about ice. @doc please add your view.'
            # distinct wording, so the repeated-answer check does not ask for another draft
            return ['Doc on the salinity drop.', 'Ada on the 1903 wintering.', 'Doc on the mooring depth.'][len(prompts) - 2]

        with patch.object(chatbot, 'complete', side_effect=complete):
            self.ask('First question')
            self.assertTrue(entered.wait(2))
            self.ask('Second question')
            # The second persona and second human turn cannot start early.
            self.assertEqual(len(prompts), 1)
            self.assertEqual([r['name'] for r in self.rows], ['Scientist', 'Scientist'])
            release.set()
            self.finish()
        self.assertEqual(len(prompts), 4, 'generated @mention must not duplicate scheduled Doc')
        self.assertEqual([r['name'] for r in self.rows], ['Scientist', 'Scientist', 'Ada', 'Doc', 'Ada', 'Doc'])
        self.assertIn('Ada: First answer about ice. @doc please add your view.', [m['content'] for m in prompts[1][2]])
        self.assertIn('shared Deck', prompts[1][0])
        self.assertNotIn('only the two of you', prompts[1][0])
        self.assertIn('Do not repeat', prompts[1][1])
        self.assertIn('Scientist: First question', [m['content'] for m in prompts[1][2]])
        self.assertIn('Scientist: Second question', [m['content'] for m in prompts[2][2]])
        self.assertIsNone(self.crew._turn_worker)

    def test_failed_speaker_does_not_strand_following_speakers_or_turns(self):
        with patch.object(chatbot, 'complete', side_effect=[RuntimeError('failed first speaker'), 'Doc answers']):
            self.crew.on_message('Scientist', 'Question', 'deck')
            self.finish()
        self.assertEqual([r['name'] for r in self.rows], ['Doc'])
        with patch.object(chatbot, 'complete', return_value='New turn answers'):
            self.crew.on_message('Scientist', '@ada Follow-up', 'deck')
            self.finish()
        self.assertEqual([r['name'] for r in self.rows], ['Doc', 'Ada'])

    def test_explicit_single_speaker_stays_single(self):
        with patch.object(chatbot, 'complete', return_value='A focused answer') as complete:
            self.crew.on_message('Scientist', '@doc question', 'deck')
            self.finish()
        self.assertEqual(complete.call_count, 1)
        self.assertEqual([r['name'] for r in self.rows], ['Doc'])
