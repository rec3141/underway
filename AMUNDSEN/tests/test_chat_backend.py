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
            crew=chatbot.Crew(Path(folder),lambda *a:None,lambda:dict(messages=[]))
            crew.context=lambda:'Test readings'
            response=Mock();response.json.return_value={'choices':[{'message':{'content':'Ahoy'}}]}
            with patch.object(chatbot,'LLM_CONFIG',config),patch('requests.post',return_value=response) as post:
                self.assertEqual(crew._generate('capn','hello'),'Ahoy')
            self.assertEqual(post.call_args.args[0],'http://127.0.0.1:18043/v1/chat/completions')
            body=post.call_args.kwargs['json']
            self.assertEqual(body['model'],'gemma-camera')
            self.assertFalse(body['chat_template_kwargs']['enable_thinking'])
            self.assertNotIn('keep_alive',body)

    def test_legacy_ollama_backend(self):
        with tempfile.TemporaryDirectory() as folder:
            crew=chatbot.Crew(Path(folder),lambda *a:None,lambda:dict(messages=[]));crew.context=lambda:''
            response=Mock();response.json.return_value={'message':{'content':'Ahoy'}}
            with patch.object(chatbot,'LLM_CONFIG',Path(folder)/'missing'),patch.object(chatbot,'LLM_API','ollama'),patch('requests.post',return_value=response) as post:
                self.assertEqual(crew._generate('capn','hello'),'Ahoy')
            self.assertTrue(post.call_args.args[0].endswith('/api/chat'))

if __name__=='__main__':unittest.main()
