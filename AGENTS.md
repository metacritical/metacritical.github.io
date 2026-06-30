# AGENTS.md

## Build & Deploy

**Always use the scripts. Never run manual git/rsync commands to deploy.**

### publish.sh
Builds the site from source (org files, themes, media) into `public/`.
- Renders ditaa diagrams
- Runs AOG (org-mode static site generator)
- Copies media files to `public/media/`
- Injects script tags (claps.js, disqus-theme-fix.js) and normalizes URLs
- Run: `./publish.sh`

### deploy.sh
Full deploy pipeline. Builds, deploys to master, pushes.
- Must be run from the `source` branch (it will error otherwise)
- Snapshots `public/` to temp before switching branches (public/ is tracked on source, not master)
- Commits and pushes to `master`, then switches back to `source`
- Run: `./deploy.sh`

### Workflow
1. Make changes on `source` branch
2. `git add -A && git commit -m "..." && git push origin source`
3. `./deploy.sh`

### Local preview
- `./scripts/dev-web.sh` serves `public/` on port 8080
- `./scripts/dev-watch.sh` auto-rebuilds on file changes (Doorman)

## Key gotchas
- `public/` is tracked on `source` but absent on `master` — deploy.sh snapshots it to temp before `git checkout master`
- AOG overwrites `public/media/css/` with its own CSS — site-specific CSS overrides are handled in JS, not CSS injection
- Disqus filter (`invert(1) hue-rotate(180deg)`) is applied entirely from `media/js/disqus-theme-fix.js`, which also attempts to inject a counter-filter for images inside the cross-origin iframe
