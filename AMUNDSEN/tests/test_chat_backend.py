import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import Mock, patch
from dashboard import chatbot

class ChatBackendTests(unittest.TestCase):
    def test_shared_server_does_not_call_ollama(self):
        with tempfile.TemporaryDirectory() as folder:
            config=Path(folder)/'chat.json'
            config.write_text(json.dumps(dict(api='openai',url='http://127.0.0.1:18043',model='gemma-camera')))
            crew=chatbot.Crew(Path(folder),lambda *a:None,lambda *a, **k: [])
            crew.context=lambda *a, **k:'Test readings'
            response=Mock();response.json.return_value={'choices':[{'message':{'content':'Ahoy'}}]}
            # the shared server answers its model list, so the crew talk to it and never look at Ollama
            up=Mock();up.ok=True
            with patch.object(chatbot,'LLM_CONFIG',config),patch.object(chatbot,'_status_cache',{'at':0.0,'value':None}),\
                 patch('requests.get',return_value=up),patch('requests.post',return_value=response) as post:
                self.assertEqual(crew._generate('capn','hello')[0],'Ahoy')
            self.assertEqual(post.call_args.args[0],'http://127.0.0.1:18043/v1/chat/completions')
            body=post.call_args.kwargs['json']
            self.assertEqual(body['model'],'gemma-camera')
            self.assertFalse(body['chat_template_kwargs']['enable_thinking'])
            self.assertNotIn('keep_alive',body)

    def test_legacy_ollama_backend(self):
        with tempfile.TemporaryDirectory() as folder:
            crew=chatbot.Crew(Path(folder),lambda *a:None,lambda *a, **k: []);crew.context=lambda *a, **k:''
            response=Mock();response.json.return_value={'message':{'content':'Ahoy'}}
            # Ollama reports the model already in memory; only then is it asked
            ps=Mock();ps.json.return_value={'models':[{'name':chatbot.LLM_MODEL+':latest'}]}
            with patch.object(chatbot,'LLM_CONFIG',Path(folder)/'missing'),patch.object(chatbot,'LLM_API','ollama'),\
                 patch.object(chatbot,'_status_cache',{'at':0.0,'value':None}),\
                 patch('requests.get',return_value=ps),patch('requests.post',return_value=response) as post:
                self.assertEqual(crew._generate('capn','hello')[0],'Ahoy')
            self.assertTrue(post.call_args.args[0].endswith('/api/chat'))
            self.assertEqual(post.call_args.kwargs['json']['keep_alive'],-1)

    def test_nothing_loaded_means_no_request_and_no_load(self):
        with tempfile.TemporaryDirectory() as folder:
            crew=chatbot.Crew(Path(folder),lambda *a:None,lambda *a, **k: []);crew.context=lambda *a, **k:''
            ps=Mock();ps.json.return_value={'models':[]}
            with patch.object(chatbot,'LLM_CONFIG',Path(folder)/'missing'),patch.object(chatbot,'LLM_API','ollama'),\
                 patch.object(chatbot,'_status_cache',{'at':0.0,'value':None}),\
                 patch('requests.get',return_value=ps),patch('requests.post') as post:
                with self.assertRaises(chatbot.ModelOffline):
                    crew._generate('capn','hello')
            post.assert_not_called()

if __name__=='__main__':unittest.main()
