#!/usr/bin/env bash
# Stop the underway dashboard. Nothing is lost; ./start.sh brings it back.
cd "$(dirname "$0")" && docker compose down
