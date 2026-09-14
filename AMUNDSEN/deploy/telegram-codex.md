# Dedicated Codex Telegram bot

`underway-codex.service` uses `TELEGRAM_KEY_CODEX` from `underway.env`.
The schedule bot keeps its original token and no longer runs Codex.
Send ordinary text to the Codex bot: no command or session name is needed.
Every message resumes the same persistent session; replies include quoted
text or captions. `/start` and `/help` show these instructions.

Only the private user ID in `UNDERWAY_CODEX_TELEGRAM_ID`, falling back to
`TELEGRAM_ID`, can use Codex. Jobs, delivery progress, a separate Telegram
update offset, and the session ID live in `db/codex_bot.sqlite`. On first
use, the most recently used legacy Codex session for that user is retained.
Old bot update IDs and pending jobs are never imported or replayed.

The CLI runs as the service account with `workspace-write`, network access,
and no interactive approvals. Its working directory and writable workspace
are `UNDERWAY_CODEX_CWD` in `site.env`, default `/data/dev/underway`.
GitHub operations use that account's Git/SSH credentials and permissions.
The workspace's `.git` directory is explicitly writable for pulls and merges.
The deployment checkout stays read-only to this service. The queue directory
and `UNDERWAY_CODEX_HOME` (default the account's `.codex`) remain writable
for persistent state. The rest of the filesystem is read-only through systemd.

On Ubuntu, `deploy/install.sh` installs the service-specific AppArmor profile
that permits user namespaces, allowing Bubblewrap to construct its sandbox
without disabling the system-wide namespace restriction. Codex's filesystem
sandbox and systemd's write restrictions remain enabled.

Install and authenticate the Codex CLI as the service account, add the new
bot token, then run `sudo deploy/install.sh` and
`sudo systemctl enable --now underway-codex.service`.
Optional `underway.env` settings are `UNDERWAY_CODEX_BIN` and
`UNDERWAY_CODEX_TIMEOUT` (seconds, default 1800).

Jobs run serially. Queued requests and completed answers survive restarts;
interrupted requests are reported rather than replayed, since they may have
already changed files. Long answers are split into Telegram messages, retaining
the last 24,000 characters if necessary.
