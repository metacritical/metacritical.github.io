#!/usr/bin/env bash
set -euo pipefail

BLOG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MODE="draft"
TITLE=""

usage() {
  cat <<'USAGE'
Usage:
  ./scripts/new-article.sh "Post title"
  ./scripts/new-article.sh --publish "Post title"

Creates:
  draft mode   -> drafts/<slug>.org
  publish mode -> posts/<slug>.org
USAGE
}

slugify() {
  echo "$1" | tr '[:upper:]' '[:lower:]' | \
    sed -E 's/[^a-z0-9]+/-/g; s/^-+//; s/-+$//; s/-+/-/g'
}

if [ "${1:-}" = "--help" ] || [ "${1:-}" = "-h" ]; then
  usage
  exit 0
fi

if [ "${1:-}" = "--publish" ]; then
  MODE="publish"
  shift
fi

TITLE="${*:-}"
if [ -z "$TITLE" ]; then
  usage
  exit 1
fi

SLUG="$(slugify "$TITLE")"
DATE_NOW="$(date +%Y-%m-%d)"

if [ "$MODE" = "publish" ]; then
  TARGET_DIR="$BLOG_DIR/posts"
else
  TARGET_DIR="$BLOG_DIR/drafts"
fi
mkdir -p "$TARGET_DIR"

TARGET_FILE="$TARGET_DIR/$SLUG.org"
if [ -f "$TARGET_FILE" ]; then
  TARGET_FILE="$TARGET_DIR/$SLUG-$(date +%H%M%S).org"
fi

cat > "$TARGET_FILE" <<EOF
#+TITLE: $TITLE
#+DATE: $DATE_NOW
#+OPTIONS: toc:nil
#+DESCRIPTION: 

Write your draft here.
EOF

# Track new files immediately so AOG git-based publish can preview drafts.
if git -C "$BLOG_DIR" rev-parse --git-dir >/dev/null 2>&1; then
  git -C "$BLOG_DIR" add "$TARGET_FILE" >/dev/null 2>&1 || true
fi

echo "Created: ${TARGET_FILE#$BLOG_DIR/}"
