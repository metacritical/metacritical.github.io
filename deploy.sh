#!/bin/bash
set -euo pipefail

BLOG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMMIT_MSG="${COMMIT_MSG:-Deploy site $(date '+%Y-%m-%d %H:%M:%S')}"

cd "$BLOG_DIR"

# 0) Must be on source branch — publish.sh needs source files.
CURRENT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
if [ "$CURRENT_BRANCH" != "source" ]; then
  echo "ERROR: deploy.sh must run from the 'source' branch (currently on '$CURRENT_BRANCH')"
  exit 1
fi

# 1) Build site
RENDER_DIAGRAMS="${RENDER_DIAGRAMS:-1}" ./publish.sh

# 2) Commit built output on source so working tree is clean before branch switch.
git add public/
if ! git diff --cached --quiet; then
  git commit -m "🔄 Sync: Regenerate Public Output (Build)"
fi
git push origin source

# 3) Remove untracked files (e.g. stale root-level files from prior deploys)
git clean -fd

# 4) Switch to master
git checkout master
git pull --ff-only origin master 2>/dev/null || true

# 4) Clear working tree using git (preserves .git).
git rm -rf . 2>/dev/null || true

# 5) Extract built files from source branch's public/ directory.
git checkout source -- public/

# 6) Flatten public/ contents to repo root.
cp -a public/. .
rm -rf public

# 7) Ensure CNAME is present.
printf 'selfdotsend.com\n' > CNAME

# 8) Commit and push if there are changes.
git add -A
if ! git diff --cached --quiet; then
  git commit -m "$COMMIT_MSG"
  git push origin master
  echo "Deployed to master."
else
  echo "No changes to deploy."
fi

# 9) Switch back to source branch.
git checkout source
