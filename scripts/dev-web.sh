#!/usr/bin/env bash
set -euo pipefail

BLOG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PORT="${PORT:-8080}"

if lsof -nP -iTCP:"$PORT" -sTCP:LISTEN >/dev/null 2>&1; then
  echo "[web] Port $PORT is already in use."
  echo "[web] Reusing the existing server. Stop it first to let doorman start web."
  while true; do
    sleep 3600
  done
fi

# Use the combined dev server (static files + editor API).
if [ -f "$BLOG_DIR/scripts/dev-editor-server.py" ]; then
  echo "[web] Starting combined server (static + editor API) on port $PORT"
  python3 "$BLOG_DIR/scripts/dev-editor-server.py" "$PORT" "$BLOG_DIR"
else
  cd "$BLOG_DIR/public"
  echo "[web] Serving $PWD on http://localhost:$PORT (static only)"
  python -m http.server "$PORT"
fi
