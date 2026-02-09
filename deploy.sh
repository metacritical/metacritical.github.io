#!/bin/bash
set -euo pipefail

BLOG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PAGES_REPO_URL="${PAGES_REPO_URL:-git@github.com:metacritical/metacritical.github.io.git}"
PAGES_DIR="${PAGES_DIR:-$HOME/Development/metacritical.github.io}"
BRANCH="${PAGES_BRANCH:-main}"
COMMIT_MSG="${COMMIT_MSG:-Deploy site $(date '+%Y-%m-%d %H:%M:%S')}"

cd "$BLOG_DIR"

# 1) Build site
RENDER_DIAGRAMS="${RENDER_DIAGRAMS:-1}" ./publish.sh

# 2) Ensure pages repo exists and is current
if [ ! -d "$PAGES_DIR/.git" ]; then
  git clone "$PAGES_REPO_URL" "$PAGES_DIR"
fi

git -C "$PAGES_DIR" fetch origin
git -C "$PAGES_DIR" checkout "$BRANCH"
git -C "$PAGES_DIR" pull --ff-only origin "$BRANCH"

# 3) Preserve CNAME if present in repo
TMP_CNAME=""
if [ -f "$PAGES_DIR/CNAME" ]; then
  TMP_CNAME="$(cat "$PAGES_DIR/CNAME")"
fi

# 4) Replace repo root contents with generated site output
find "$PAGES_DIR" -mindepth 1 -maxdepth 1 ! -name '.git' -exec rm -rf {} +
rsync -a --delete "$BLOG_DIR/public/" "$PAGES_DIR/"

# 5) Restore CNAME if it existed
if [ -n "$TMP_CNAME" ]; then
  printf '%s\n' "$TMP_CNAME" > "$PAGES_DIR/CNAME"
fi

# 6) Commit and push if there are changes
if [ -n "$(git -C "$PAGES_DIR" status --porcelain)" ]; then
  git -C "$PAGES_DIR" add -A
  git -C "$PAGES_DIR" commit -m "$COMMIT_MSG"
  git -C "$PAGES_DIR" push origin "$BRANCH"
  echo "Deployed to $PAGES_REPO_URL ($BRANCH)"
else
  echo "No changes to deploy."
fi
