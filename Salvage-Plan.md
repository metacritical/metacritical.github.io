# Salvage Plan — Full Git History & About Page Restoration

## Problem

The local git repository has only 8 commits (all from 2026-06-30).
The upstream GitHub repo has ~324 commits with the full history, but
`git clone` / `git fetch` from GitHub keeps timing out (network issue).

## Goal

1. Pull the full 324-commit history from upstream into the local repo
   without losing current uncommitted changes.
2. Find the old longer about page (with original picture) from the history.
3. Restore it.

## Strategy

### Step 1 — Get the full history

The current repo is stuck with shallow history. Three options:

**Option A — Fix the remote fetch (try again with better settings)**
```bash
# From the repo root:
git remote set-url origin https://github.com/metacritical/metacritical.github.io.git
git fetch --unshallow
# If that times out, try:
git fetch origin 'refs/heads/*:refs/remotes/origin/*' --depth=1000
```

**Option B — Fresh clone + cherry-pick current work**
```bash
# Outside the project:
git clone https://github.com/metacritical/metacritical.github.io.git metacritical-full
cd metacritical-full
# Copy all uncommitted files from the shallow repo over:
rsync -av --exclude=.git /path/to/metacritical.github.io/ ./
# Review and commit the changes:
git add -A
git commit -m "chore: restore working tree changes"
```

**Option C — Replace local .git with upstream's .git**
```bash
# Fresh bare clone:
git clone --bare https://github.com/metacritical/metacritical.github.io.git /tmp/mc-bare
# Replace local .git:
mv .git .git-shallow-backup
cp -a /tmp/mc-bare .git
git reset HEAD .  # unstage everything
# Now full history is available, working tree preserved
```

### Step 2 — Find the old about page

Once full history is available:

```bash
# Find all commits that touched about files:
git log --all --oneline --name-only -- '*about*'

# Search all versions of about.org for content that mentions a picture or
# has significantly more lines:
git log --all -p -- about.org | grep -c 'file:\\|media\\|profile\\|picture\\|avatar' | head -20

# Or view the about.org at a specific old commit:
git show <old-commit-hash>:about.org

# Check if the picture file still exists in history:
git log --all --oneline --name-only --diff-filter=A -- '*profile*' '*pankaj*' '*avatar*'
```

### Step 3 — What to do if history can't be recovered

The about page has already been rebuilt from archive.org content and is
sitting in `about.org` at 60 lines / 4 sections. If full history is
unreachable, this version can be deployed as-is after a `git add` + commit.

## Current working tree state (preserve these before any git surgery)

- `about.org` — rewritten with combined content
- `media/css/disqus-iframe.css` — cleaned
- `media/css/style.css` — cleaned (filter removed, disqus rules removed)
- `media/css/theme-medium.css` — cleaned (filter removed)
- `media/js/disqus-theme-fix.js` — no-op
- `publish.sh` — disqus cleanup
- `public/` — various generated files
- `deep-seek-handoff.md` — handoff doc
- `drafts/` — new draft articles (untracked)
- `scripts/` — new scripts (untracked)
- `series/` — new directory (untracked)

## Handoff for next session

Start by running Option A or B from Step 1 above to get the full history.
Then use Step 2 commands to search through old commits for the about page.
The current about content from archive.org is already in `about.org` as a
fallback.
