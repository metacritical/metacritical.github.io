#!/usr/bin/env bash
set -euo pipefail

BLOG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
POLL_SECONDS="${WATCH_POLL_SECONDS:-2}"
LOCK_DIR="${TMPDIR:-/tmp}/selfdotsend-dev-watch.lock"
LOCK_PID_FILE="$LOCK_DIR/pid"

acquire_lock() {
  if mkdir "$LOCK_DIR" 2>/dev/null; then
    echo "$$" > "$LOCK_PID_FILE"
    return
  fi

  if [ -f "$LOCK_PID_FILE" ]; then
    local old_pid
    old_pid="$(cat "$LOCK_PID_FILE" 2>/dev/null || true)"
    if [ -n "$old_pid" ] && kill -0 "$old_pid" 2>/dev/null; then
      echo "[watch] Existing watcher detected (pid: $old_pid). Taking over..."
      kill "$old_pid" 2>/dev/null || true
      sleep 1
      if kill -0 "$old_pid" 2>/dev/null; then
        kill -9 "$old_pid" 2>/dev/null || true
      fi
    fi
  fi

  rm -rf "$LOCK_DIR"
  mkdir "$LOCK_DIR"
  echo "$$" > "$LOCK_PID_FILE"
}

cleanup_lock() {
  if [ -f "$LOCK_PID_FILE" ] && [ "$(cat "$LOCK_PID_FILE" 2>/dev/null)" = "$$" ]; then
    rm -rf "$LOCK_DIR"
  fi
}

acquire_lock
trap cleanup_lock EXIT

hash_state() {
  {
    find "$BLOG_DIR" \
      \( -path "$BLOG_DIR/public" -o -path "$BLOG_DIR/public-aog" -o -path "$BLOG_DIR/public-test" -o -path "$BLOG_DIR/.git" \) -prune -o \
      -type f \( \
        -name "*.org" -o \
        -name "*.el" -o \
        -name "*.mustache" -o \
        -name "*.css" -o \
        -name "*.js" -o \
        -name "publish.sh" -o \
        -name "README.md" -o \
        -path "$BLOG_DIR/media/*" -o \
        -path "$BLOG_DIR/assets/*" -o \
        -path "$BLOG_DIR/tools/diagrams/*" \
      \) -print0 | sort -z | xargs -0 stat -f "%m %N" 2>/dev/null

    # Also watch active theme source in AOG repo.
    if [ -d "/Users/pankajdoharey/Development/Projects/AOG/themes/selfdotsend" ]; then
      find "/Users/pankajdoharey/Development/Projects/AOG/themes/selfdotsend" -type f -print0 | sort -z | \
        xargs -0 stat -f "%m %N" 2>/dev/null
    fi
  } | shasum | awk '{print $1}'
}

echo "[watch] Initial publish..."
"$BLOG_DIR/publish.sh"

LAST_HASH="$(hash_state)"
echo "[watch] Watching for changes every ${POLL_SECONDS}s..."

while true; do
  sleep "$POLL_SECONDS"
  NEXT_HASH="$(hash_state)"
  if [ "$NEXT_HASH" != "$LAST_HASH" ]; then
    echo "[watch] Change detected. Rebuilding..."
    if "$BLOG_DIR/publish.sh"; then
      LAST_HASH="$NEXT_HASH"
      echo "[watch] Rebuild complete."
    else
      echo "[watch] Rebuild failed. Will retry on next change."
    fi
  fi
done
