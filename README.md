# Self dot send Blog (AOG + Org + Emacs)

This repository contains blog content and site assets.
Publishing is done by **AOG** (Emacs-based Org publishing) using theme source from:

- `/Users/pankajdoharey/Development/Projects/AOG/themes/selfdotsend`

## 1) Prerequisites

Required:
- `emacs` (batch mode must work)
- `node` (used for build-time code-highlight fallback)
- `python` (for local static server)
- `aog` CLI wrapper in PATH (`/Users/pankajdoharey/.oh_my_bash/bin/aog`)

Optional (for diagrams):
- `java` (needed by `ditaa` / `plantuml` jars)

## 2) Repository Layout

- `posts/*.org`: blog posts
- `index.org`: homepage card list
- `about.org`, `archive.org`, `nano-chat.org`: static pages
- `media/`: logo/images copied to `public/media/`
- `assets/`: article assets copied to `public/assets/`
- `scripts/`: build helpers (slug normalization, diagrams, search index, syntax fallback)
- `publish.sh`: canonical site build command
- `public/`: generated output

## 3) Build (single run)

From repo root:

```bash
cd /Users/pankajdoharey/Development/selfdotsend-new
./publish.sh
```

What this does:
1. Normalizes Org heading IDs (`CUSTOM_ID`) for readable slugs
2. Renders diagrams (enabled by default)
3. Runs `aog publish`
4. Copies `media/` and `assets/` into `public/`
5. Rewrites `file:///...` links to site-relative paths
6. Applies build-time syntax highlighting fallback to exported HTML
7. Creates stable aliases (`/archive/`, `/blog/`, `/nano-chat/`)
8. Generates `public/search-index.json`
9. Cleans post-build clutter (archives legacy dirs like `public-aog/`, removes `*~` / `.DS_Store`)

Disable cleanup for a run:

```bash
CLEANUP_AFTER_BUILD=0 ./publish.sh
```

## 4) Local Preview

```bash
cd /Users/pankajdoharey/Development/selfdotsend-new/public
python -m http.server 8080
```

Open: `http://localhost:8080`

## 5) Dev Mode (auto rebuild + server)

This repo now includes `Procfile` for `doorman`:

```bash
cd /Users/pankajdoharey/Development/selfdotsend-new
doorman
```

Processes started:
- `watch`: `./scripts/dev-watch.sh` (polls source changes and rebuilds)
- `web`: `./scripts/dev-web.sh` serves `public/` on `http://localhost:8080`

Tuning:
- `WATCH_POLL_SECONDS=1 doorman` to poll faster
- `PORT=8081 doorman` if 8080 is already in use
- Stop both with `Ctrl+C`

Notes:
- `dev-watch` is single-instance guarded, so accidental duplicate `doorman`
  runs won't create multiple rebuild loops.

## 6) Diagram Behavior

Diagrams are ON by default.

- Normal: `./publish.sh`
- Disable explicitly: `RENDER_DIAGRAMS=0 ./publish.sh`

Jars expected at:
- `tools/diagrams/ditaa-0.11.0-standalone.jar`
- `tools/diagrams/plantuml-mit-1.2026.1.jar`

## 7) AOG CLI Commands

`aog` wrapper supports:

```bash
aog publish [repo_dir] [output_dir]
aog preview [repo_dir] [output_dir] [port]
```

Examples:

```bash
aog publish /Users/pankajdoharey/Development/selfdotsend-new /Users/pankajdoharey/Development/selfdotsend-new/public
aog preview /Users/pankajdoharey/Development/selfdotsend-new /Users/pankajdoharey/Development/selfdotsend-new/public 8080
```

## 8) Emacs Configuration Notes

The `aog` wrapper already sets these for batch publish:
- `aog/theme` -> `selfdotsend`
- `aog/highlight-render` -> `htmlize`
- `org-src-fontify-natively` -> `t`
- `org-html-htmlize-output-type` -> `css`

If you see htmlize warnings, ensure `htmlize` is installed in your Emacs setup.

## 9) Publishing to GitHub Pages

Generated site is in `public/`.
Deploy flow is usually:
1. `./publish.sh`
2. Commit generated output to your Pages repo branch/layout
3. Push

(Exact branch/path depends on your Pages repo settings.)

## 10) Known Warnings (safe)

During publish you may see warnings like:
- `File ... in hyper link does not exist`

Those are emitted while AOG validates links before all copied/aliased outputs exist.
Final generated files can still be valid (verify in `public/`).

## 11) Troubleshooting

No syntax highlighting:
- Run `./publish.sh` again
- Hard refresh browser (`Cmd+Shift+R`)
- Confirm generated HTML under `public/blog/...` contains `span.org-*` inside code blocks

Archive link shows directory listing:
- Use `/archive/` (header already points there)
- Rebuild to regenerate aliases: `./publish.sh`

Logo/image not showing:
- Confirm file exists under `media/images/`
- Rebuild so it copies into `public/media/images/`
