# Handoff: Draft Editor UX Improvements

## Goal
Make the local draft editor at `/drafts/<slug>/` behave like a visual, Medium-style block editor while keeping the underlying Org files valid for the AOG/Emacs publishing pipeline. The inline page should be editable directly without a separate "Edit" button, and the standalone `/editor?draft=<slug>` route should still load content correctly.

## Achievements

### Inline editor (`scripts/generate_draft_preview.el`)
- **Arrow-key block navigation**
  - `ArrowDown` at the end of the last block creates a new empty paragraph and focuses it.
  - `ArrowDown` on a void block (image, embed, hr) jumps over it to the next block or appends a paragraph.
  - `ArrowUp` at the start of the first body block moves focus to the title.
  - `ArrowUp` on a void block moves to the previous block or the title.
- **Plus (`+`) menu improvements**
  - Added a "Text" button to the menu so users can insert a new paragraph block after the current block.
  - Inserted blocks keep the existing image/upload/video/embed/code options.
- **Floating formatting toolbar**
  - Repositioned/hidden correctly on selection and scroll/resize.
  - Hidden when the selection is inside a code block or image figure, where formatting commands are not useful.
- **Image controls**
  - Clicking an image block selects it, adds a teal focus border, and shows a fixed toolbar.
  - Toolbar actions: align left/center/right, grayscale/sepia/no filter, move up, move down, delete.
  - `Delete`/`Backspace` with a selected image removes it and moves the caret to an adjacent block.
  - Captions remain editable and are not treated as image selection.
- **HTML → Org export**
  - `htmlToOrgNode` now handles `h1`–`h6`, unordered/ordered lists, inline emphasis (`*`, `/`, `_`, `+`), `<code>`, `<span>` foreground/background colors, `<mark>`, and `<br>` hard line breaks.
  - Code blocks are exported by walking the DOM and preserving real line breaks, even after syntax highlighting.

### Standalone editor (`media/js/draft-editor.js`, `editor/index.html`)
- Previously fixed: draft loading, Org rendering (headings, lists, blocks, inline markup, raw HTML), and a Preview/Edit toggle.
- Preserved as an optional route but the inline `/drafts/<slug>/` page is the primary editing surface.

### Build / validation
- `scripts/generate_draft_preview.el` byte-compiles without warnings.
- Previews regenerated with `LOCAL_DEV=1 RENDER_DIAGRAMS=0 ./publish.sh`.
- Extracted inline script from a generated preview page passed `node --check`.
- Standalone `media/js/draft-editor.js` passed `node --check`.

## Current State
- `/drafts/<slug>/` serves the inline visual editor directly (no redirect to `/editor`).
- Draft index cards link to `/drafts/<slug>/`.
- Generated preview pages in `public/drafts/<slug>/index.html` contain the updated editor JS/CSS.
- The inline editor auto-saves to `/editor/api/save` every 5 seconds and supports tags, fonts, colors, links, code blocks, images, videos, and embed cards.

## Known Limitations / Open Items
1. **Browser end-to-end testing is still needed.** Verify:
   - Arrow keys work at the boundaries of code blocks, lists, and image figures.
   - The plus menu inserts blocks in the expected position.
   - The floating toolbar appears for text selections and stays hidden for code/images.
   - Image toolbar alignment, filters, reorder, and delete work and persist after save.
   - Saving preserves headings, lists, code indentation, and figure markup in the Org file.
2. **Image resize** currently relies on the browser's `resize: both` on `figure.image-block`. A custom corner resize handle with aspect-ratio locking has not been implemented.
3. **Mobile layout** of the floating `+` button and image toolbar has not been verified.
4. **Image upload path conventions**: inserted images currently use the inline editor's upload endpoint. Verify that saved Org files reference paths that AOG/publish can resolve, or wire uploads to match the standalone editor's conventions.
5. **Standalone `/editor` future**: decide whether to keep it as a fallback or remove it now that the inline editor is primary.

## Critical Files
- `scripts/generate_draft_preview.el` — inline editor template, CSS, and JS.
- `scripts/dev_server.py` — dev server, draft parsing, and `/editor/api/*` routes.
- `media/js/draft-editor.js` — standalone block editor class.
- `editor/index.html` — standalone editor page.
- `drafts/mnist-with-a-twist.org` — test draft with image and code block.
- `public/drafts/<slug>/index.html` — generated inline editable pages (regenerate with `publish.sh`).

## How to Regenerate / Test
```bash
# Regenerate the site and draft previews
LOCAL_DEV=1 RENDER_DIAGRAMS=0 ./publish.sh

# Run the dev server
python scripts/dev_server.py

# Visit a draft
open http://localhost:8000/drafts/mnist-with-a-twist/
```

## Constraints to Preserve
- Org source files must remain valid AOG/Emacs publishing input.
- Do not commit generated `public/` files or secrets.
- Do not re-add a separate "Edit" button to `/drafts/<slug>/`; the page is already contenteditable.
- Keep file paths and the existing publishing workflow unchanged.
