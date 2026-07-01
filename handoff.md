# Handoff — Self dot send (metacritical.github.io)

## Project Overview

A static blog powered by AOG (Emacs Org-mode static site generator), with a
LiveDrafts editor, series management, tag cloud, and image controls. Built on
GitHub Pages with a custom selfdotsend theme.

## Recent Milestones (Commits ee11aa4 + e031197)

### What Was Built / Fixed

| Area | What | Key Files |
|---|---|---|
| **Draft Editor** | Full contentEditable editor wired to draft pages. Toolbars: bubble (text selection), slash menu (/), plus button (+), image toolbar, block toolbar. Auto-save with status indicator. | `media/js/draft-editor.js`, `media/css/draft-editor.css`, `scripts/build_draft_previews.py`, `scripts/generate_editor_page.py` |
| **Metadata Row** | Consolidated author, date, tags, series, save indicator, and toolbar buttons into two clean rows below the title. | `scripts/build_draft_previews.py` |
| **Tag Management** | Inline tag pills with × to remove, + tag button opens cloud popup with suggestions from all org files + localStorage. Tags persist on save. | `scripts/build_draft_previews.py`, `scripts/inject_tags.py`, `scripts/dev-editor-server.py` |
| **Series Management** | CLI (`manage-series.sh`), API endpoints, UI picker in metadata row. Create, list, add/remove articles, reorder, update description. | `scripts/manage-series.sh`, `scripts/dev-editor-server.py`, `scripts/build_draft_previews.py` |
| **Image Toolbar** | Click any image in the editor → align (L/C/R), fit (cover/contain/fill/scale-down/none), position (9 options), filter (18 effects), replace URL, replace upload, resize handle. Fixed race condition where toolbar was immediately hidden. | `media/js/draft-editor.js`, `media/css/draft-editor.css` |
| **Code Blocks** | Syntax highlighting preserved on draft pages. Copy button + dark/light toggle via `code-block-controls.js`. Language extracted from class name on HTML→Org conversion. | `media/js/code-block-controls.js`, `media/js/draft-editor.js` |
| **CSS Restored** | `style.css` (1266 lines) and `theme-medium.css` (407 lines) restored from deployed commit dde9769. `custom.css` emptied. All toolbars modernized to white/teal. | `media/css/style.css`, `media/css/theme-medium.css`, `media/css/draft-editor.css` |
| **Git History** | Full 333-commit history recovered from bare clone (`betacritical-tatti`). Shallow backup at `.git-shallow-backup/`. | `.git/` (replaced) |
| **Anti-Regression** | AGENTS.md now includes rules: read full files, never recreate functionality, verify save paths, post-change checklists. | `AGENTS.md` |

### Bug Fixes

1. **Image toolbar hidden on click** — document click handler hid toolbar immediately after bodyEl handler showed it. Fixed with `!evt.target.closest('figure.image-block')` guard.
2. **schemeCopy◐ text in org files** — `cb-bar` toolbar elements leaked content into org files via `nodeToOrg`. Fixed with dedicated `org-src-container` handler + `contenteditable="false"` on `cb-bar`.
3. **Series nav CSS in org files** — series nav HTML was in editable body, got written to org files. Excluded via class check in DOM restructuring.
4. **Tags not persisting** — `inject_tags.py` had `if "article-tags" in content: return False` guard that prevented re-injection on rebuild.
5. **Code blocks defaulting to text** — `nodeToOrg` read `data-lang` attribute but AOG code blocks store language in class name (`src-scheme`).

## Architecture

### Technology Stack

| Layer | Language | Key Files |
|---|---|---|
| Site Generator | Emacs Lisp | AOG (external repo at `~/Development/Projects/AOG/`) |
| Orchestration | Bash | `publish.sh`, `deploy.sh`, `scripts/dev-*.sh`, `scripts/manage-series.sh`, `scripts/new-article.sh`, `scripts/publish-draft.sh` |
| Build-time Processing | Python | `scripts/generate_drafts.py`, `scripts/build_draft_previews.py`, `scripts/generate_editor_page.py`, `scripts/inject_tags.py`, `scripts/inject_series_nav.py`, `scripts/dev-editor-server.py` |
| Syntax Highlighting | Node.js | `scripts/highlight_exported_code.js` |
| Client-side Editor | JavaScript | `media/js/draft-editor.js` (3369 lines), `media/js/code-block-controls.js` |
| Client-side Styling | CSS | `media/css/draft-editor.css` (703 lines), `media/css/style.css`, `media/css/theme-medium.css` |

### Build Pipeline (`publish.sh`)

1. Render ditaa diagrams
2. AOG builds site (org → HTML, no drafts) to `public/`
3. URL normalization, JS/CSS injection on all HTML
4. Highlight exported code blocks
5. Copy media/assets, generate archive, search index
6. Build DRAFT index (`generate_drafts.py`)
7. AOG draft build (with drafts) to temp dir
8. Copy draft pages to `public/drafts/<slug>/`
9. Draft post-processing: highlighting, JS/CSS injection
10. Inject series nav (`inject_series_nav.py`)
11. Inject tags (`inject_tags.py`)
12. Generate editor page (`generate_editor_page.py`)

### Editor API (`dev-editor-server.py`)

Runs alongside static dev server. Endpoints:

| Endpoint | Method | Purpose |
|---|---|---|
| `/editor/api/load-draft?slug=X&kind=draft|post` | GET | Load org file, return JSON |
| `/editor/api/save` | POST | Write org file with headers + body + tags |
| `/editor/api/upload` | POST | Upload image as data URL, save to assets/ |
| `/editor/api/fetch-meta` | POST | Fetch OG meta from URL |
| `/editor/api/tags` | GET | List all tags from all org files |
| `/editor/api/series` | GET | List all series |
| `/editor/api/series/<slug>` | GET | Get series detail |
| `/editor/api/series` | POST | Create/update series |
| `/editor/api/series/<slug>/add` | POST | Add article to series |
| `/editor/api/series/<slug>/remove` | POST | Remove article from series |

### CLI Commands

| Command | What it does |
|---|---|
| `./publish.sh` | Full site rebuild |
| `./scripts/dev-web.sh` | Start combined dev server (static + API) on :8080 |
| `./scripts/dev-watch.sh` | Doorman — auto-rebuild on file changes |
| `./scripts/new-article.sh \"Title\"` | Create new draft |
| `./scripts/new-article.sh --publish \"Title\"` | Create new published post |
| `./scripts/publish-draft.sh <slug>` | Promote draft to published |
| `./scripts/manage-series.sh list` | List all series |
| `./scripts/manage-series.sh create \"Name\" \"Desc\"` | Create series |
| `./scripts/manage-series.sh add <series> <slug>` | Add article to series |
| `./scripts/manage-series.sh remove <series> <slug>` | Remove article from series |
| `./scripts/manage-series.sh reorder <series> <s1> <s2> ...` | Reorder articles |
| `./scripts/manage-series.sh show <series>` | Show series detail |
| `./deploy.sh` | Build + deploy to master branch |

### `aog` CLI Wrapper

`~/.oh_my_bash/bin/aog` provides shortcuts: `aog build`, `aog dev`, `aog new`, `aog ls`, `aog promote`, `aog preview`, `aog publish`, `aog draft-publish`.

## Key Design Decisions

### Hero Banner
- NOT injected by build script. Placed in org content as `[[/media/images/placeholder.png]]`.
- New drafts get it via `new-article.sh` template.
- Bootstrap converts AOG's `<div class="figure">` to `<figure class="image-block">` at runtime so the image toolbar works.
- Same image appears on both draft and published (both render from org file).

### Series Nav (teal #36c9c7, not blue)
- Defined in `series/<slug>.json` files.
- Injected into article HTML at build time by `inject_series_nav.py`.
- CSS embedded inline (survives AOG theme overwrite).
- Injected into draft preview pages too.

### Tags
- Source of truth: `#+TAGS:` header in org file.
- `inject_tags.py` reads org file → injects as styled pills into HTML on every build (no stale guard).
- Bootstrap extracts from HTML on page load.
- Tag picker in metadata row shows suggestions from all org files (via API) + localStorage.
- Adding/removing tags marks editor dirty → auto-save → org file updated → rebuild picks up changes.

### Series-assigned tags / metadata
Each article in the series JSON has: `title`, `url`, `date`, `status` (published/draft).
The JSON is the source of truth — modified by CLI or API, consumed by `inject_series_nav.py`.

## Known Issues / Edge Cases

1. **Doorman auto-rebuild race** — auto-save writes org file, Doorman triggers rebuild, which regenerates `public/`. If user refreshes during rebuild, they get stale HTML. Mitigation: auto-save has 2s debounce.
2. **Draft pages missing code-block-controls.js** — publish.sh draft post-processing should inject it, but has been intermittent. `build_draft_previews.py` now includes it directly as fallback.
3. **Code blocks inside contentEditable** — set to `contenteditable="false"`. User edits code via editor's slash menu (`/code`), which creates new `pre.code-block` with full language/theme/copy controls.
4. **Series picker URL matching** — uses `articleUrl.indexOf(seriesUrl) !== -1` which might catch unintended matches with similar slugs. Works for current use case.
5. **No Prism language files locally** — `prism-autoloader` loads languages from CDN (`cdnjs.cloudflare.com`). Requires internet for in-editor highlighting of non-core languages.

## Files Referenced But External

| Path | Purpose |
|---|---|
| `~/Development/Projects/AOG/` | Emacs site generator, theme templates, elisp source |
| `~/Development/Projects/AOG/themes/selfdotsend/templates/nav.mustache` | Site nav template |
| `~/Development/Projects/AOG/themes/selfdotsend/templates/post.mustache` | Article post template |
| `~/.oh_my_bash/bin/aog` | CLI wrapper for build/dev commands |
| `~/Development/betacritical-tatti` | Bare clone used for git history recovery |

## Quick Start

```bash
# Full rebuild (necessary after org file changes)
./publish.sh

# Dev server (static + editor API)
./scripts/dev-web.sh

# Then visit:
# http://localhost:8080/ — blog
# http://localhost:8080/editor/ — new draft
# http://localhost:8080/editor/?slug=part-3&kind=draft — edit draft
# http://localhost:8080/drafts/ — drafts listing

# Create a new test series
./scripts/manage-series.sh create "Test Series" "A test series description"
./scripts/manage-series.sh add test-series writing-a-programmers-editor-part-6

# Create new draft
./scripts/new-article.sh "My New Article"
```
