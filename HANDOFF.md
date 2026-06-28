# Handoff — Draft Editor

## Goal
A visual draft editor (inline = `/drafts/<slug>/`, standalone = `/editor`) whose output is valid Org-mode preservable through the Emacs/AOG publishing pipeline.

## Latest Commits (newest first)
| Commit | Description |
|--------|-------------|
| `b1a869d` | sync: regenerated public assets after SVG icon size change |
| `208d4e1` | fix: enlarge plus menu SVGs to 24px, stroke-width 2.2 |
| `6d12fdf` | fix: restore menu button icons to cyan #36c9c7 |
| `07707f6` | fix: bump menu icon color to #1f1f1b (reverted) |
| `0640d42` | fix: popup menu button icons black, toggle stays cyan |
| `bac6b62` | fix: reduce icon font-weight, hide inline tools on block select, prevent Backspace deleting body |
| `c12d1af` | fix: insert menu opens leftward; click body creates paragraph at end |
| `97984c8` | fix: enlarge plus menu icons (22px SVGs, larger text), color #36c9c7 |
| `e39b16e` | fix: plus menu opens leftward (right-anchored, menu-first DOM order) |
| `35ddd86` | fix: show plus next to all text blocks, hide only for components |
| `b2d9e8d` | fix: only show plus add-button next to empty text blocks |
| `4faf98e` | fix: persist code-block settings across save/load for both editors |
| `45fefe6` | chore: add console.time/timeEnd for plus-ready latency |
| `9be687d` | fix: persist code-block data-bg and syntax-theme through save/load |
| `880d635` | fix: strip shadows/outlines/backgrounds from code tokens in dark mode |
| `7485734` | fix: keep select focused after syntax theme change |
| `7526a19` | fix: remove code border causing line artifacts |
| `2a835c3` | fix: remove text-shadow halo around code tokens |
| `e303c4a` | fix: inline editor code-block ArrowDown/ArrowUp exit + Elisp escaping |

## Current State

### Plus menu (inline editor)
- Opens leftward (right-anchored, menu DOM order before button)
- Icons: 24px SVGs with stroke-width 2.2
- Colors: button icons, borders, toggle arrow all cyan #36c9c7
- Font-weight on text labels: 400
- Menu items: 44px circular buttons

### Toolbars
- Block toolbar hidden when text is selected (fix in `selectBlock`)
- Inline formatting toolbar (`#draft-tools`) hidden when block is selected
- Both editors prevent Backspace from deleting the last remaining empty block

### Code blocks
- No text-shadow halo (removed from prism-themes-scoped.css)
- No `code { border: 1px solid #eee }` in style.css
- data-bg and data-syntax-theme persist through save/load cycle via `data-code-meta`
- ArrowDown at end creates new paragraph; ArrowUp at start moves to title or prev block

### Verifications
- `node --check media/js/draft-editor.js` → pass
- extracted inline JS → pass
- `emacs --batch -f batch-byte-compile scripts/generate_draft_preview.el` → pass (one defcustom group warning)
- `LOCAL_DEV=1 ./publish.sh` → succeeds

### Known minor issue
- `defcustom` for `sds/code-block-attrs-re` warns "fails to specify containing group" — non-functional, can be suppressed or grouped later.

## Key Files
- `scripts/generate_draft_preview.el` — inline editor (Elisp format string + embedded JS)
- `media/js/draft-editor.js` — standalone editor (JS class)
- `media/css/draft-editor.css` — standalone editor styles
- `media/css/prism-themes-scoped.css` — scoped Prism syntax themes
- `media/css/style.css` — global site styles (has `code { border }` fix)
- `scripts/dev_server.py` — dev server (doorman web process)
- `scripts/dev-watch.sh` — dev watch (auto-rebuild on changes)
- `publish.sh` — site build

## Next Steps (user-driven)
User is refining the editor iteratively. No specific backlog — await user's next request.
