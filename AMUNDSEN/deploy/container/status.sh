#!/usr/bin/env bash
# Is it running, and what has it been doing lately?
cd "$(dirname "$0")"
docker compose ps
echo; docker compose logs --tail 40
