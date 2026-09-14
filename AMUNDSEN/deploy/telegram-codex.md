# Telegram Codex sessions

In a private chat with the bot, send `/codex ice explain this code` to start
the named session `ice`. Send `/codex ice <another request>` to resume it.
Alternatively, reply to a text message or caption with `/codex ice`; the
quoted text becomes the prompt. Extra text after the session name becomes
the request alongside that quote. Ordinary messages never go to Codex.

Only the numeric user ID in `UNDERWAY_CODEX_TELEGRAM_ID` (or `TELEGRAM_ID`
when unset) can use this command, and only in that user's private chat.
Names are case-sensitive and contain up to 64 letters, numbers, hyphens
or underscores, starting with a letter or number.

Install and authenticate the Codex CLI as the Telegram service account.
The service uses that account's `.codex` directory for credentials and
session history. `deploy/install.sh` grants it write access to that directory
even though the rest of the home directory remains read-only. Set
`UNDERWAY_CODEX_HOME` in `site.env` before running the installer if the
account uses a different directory.

Optional settings in `underway.env` are `UNDERWAY_CODEX_BIN` (CLI path),
`UNDERWAY_CODEX_CWD` (workspace, default: the running repository), and
`UNDERWAY_CODEX_TIMEOUT` (seconds per request, default: 1800). Codex runs
with the `workspace-write` sandbox and no interactive approvals. Additional
workspace paths must also be writable under the systemd service's policy.

Jobs and the mapping from Telegram session names to Codex thread IDs live
in `db/telegram_codex.sqlite`. Requests run serially in the background;
subscription commands remain available. Queued requests and completed
answers waiting for delivery survive bot restarts. An interrupted request
is reported rather than replayed automatically, since it may already have
changed files. The same name resumes its saved Codex thread. Names created
here are bot aliases, not a search over sessions from other Codex clients.

After deploying, run `sudo deploy/install.sh` to update the Telegram unit.
Answers return to the same private Telegram chat. Long answers are split
into messages; an answer beyond 24,000 characters keeps its final portion.
