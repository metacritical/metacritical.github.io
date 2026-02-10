#!/usr/bin/env bash
set -euo pipefail

BLOG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PORT="${PORT:-8080}"

if lsof -nP -iTCP:"$PORT" -sTCP:LISTEN >/dev/null 2>&1; then
  EXISTING_PID="$(lsof -tiTCP:"$PORT" -sTCP:LISTEN | head -n1 || true)"
  EXISTING_CMD="$(ps -p "${EXISTING_PID:-0}" -o command= 2>/dev/null || true)"
  if [[ "$EXISTING_CMD" == *"scripts/dev_server.py"* ]]; then
    echo "[web] Port $PORT in use by existing dev server (pid: $EXISTING_PID). Replacing it..."
    kill "$EXISTING_PID" 2>/dev/null || true
    sleep 1
    if lsof -nP -iTCP:"$PORT" -sTCP:LISTEN >/dev/null 2>&1; then
      kill -9 "$EXISTING_PID" 2>/dev/null || true
    fi
  fi
fi

if lsof -nP -iTCP:"$PORT" -sTCP:LISTEN >/dev/null 2>&1; then
  echo "[web] Port $PORT is already in use by a non-blog process."
  echo "[web] Set a different port, e.g. PORT=8081 doorman"
  exit 1
fi

cd "$BLOG_DIR/public"
echo "[web] Serving $PWD on http://localhost:$PORT"
python "$BLOG_DIR/scripts/dev_server.py" "$BLOG_DIR" "$PORT"
