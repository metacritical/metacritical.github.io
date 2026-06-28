# Task Plan: Fix "Saved just now" to Wait for Full Build Pipeline

## Problem

The green "Saved just now" indicator appears immediately after `POST /editor/api/save` returns, but the full pipeline (org file → HTML build via `publish.sh`) hasn't completed yet. A manual page refresh is needed to see the latest content. The indicator is misleading.

## Goal

- Green "Saved" indicator should only appear after the FULL pipeline completes: org file written + HTML build finished
- After pipeline completes, auto-refresh the page within ~1 second to show latest content
- Auto-refresh only in development mode (doorman / `LOCAL_DEV=1`)
- In production (no dev server), save indicator behavior should remain unchanged

## Files to Modify

1. **`scripts/dev_server.py`** — Add a `/editor/api/check-build` endpoint
2. **`scripts/generate_draft_preview.el`** — Modify inline editor's `saveDraft()` to poll build status
3. **`media/js/draft-editor.js`** — Modify standalone editor's `saveDoc()` to poll build status
4. **`scripts/dev_server.py`** — Add `window.__DEV__ = true` to the dev edit injection

## Detailed Changes

### 1. Add `window.__DEV__ = true` to dev injection (`scripts/dev_server.py`)

In `_dev_edit_injection()` (line ~515), add at the top of the injected `<script>`:
```javascript
window.__DEV__ = true;
```

This gives all pages served through the dev server a reliable dev-mode signal.

### 2. Add `/editor/api/check-build` endpoint (`scripts/dev_server.py`)

New GET endpoint that accepts `?path=<org-relative-path>`:

- `_check_build(queries)`: parse the `path` query param
- Resolve the org file relative to `self.blog_dir`
- If org file doesn't exist → return `{done: false}`
- Derive expected HTML output path:
  - `drafts/<slug>.org` → `public/drafts/<slug>/index.html`
  - `posts/<slug>.org` → `public/<slug>/index.html` (or check in drafts/published/)
- If HTML file doesn't exist → return `{done: false}`
- Compare mtimes: if HTML mtime >= org mtime → `{done: true}` else `{done: false}`

Add route in `do_GET` (after existing editor API routes):
```python
if path == "/editor/api/check-build":
    self._check_build(parsed.query)
    return
```

### 3. Modify inline editor `saveDraft()` (`scripts/generate_draft_preview.el`)

Current flow (lines 2332-2364):
```
POST /editor/api/save → response.ok → setSaveState('is-saved', 'Saved just now')
```

New flow:
```
POST /editor/api/save → response.ok → setSaveState('is-saving', 'Building...')
                                       ↓
                              poll /editor/api/check-build?path=<path>
                                       ↓ every 500ms
                              done=true → setSaveState('is-saved', 'Saved')
                                       ↓
                              if (window.__DEV__):
                                setTimeout(() => location.reload(), 1000)
```

Details:
- Extract `path` from `resp.path` (the org file path returned by save endpoint)
- URL-encode it and poll `/editor/api/check-build?path=<encoded-path>`
- Poll interval: 500ms
- Timeout: 30s fallback (show "Saved" even if build never completes)
- Cancel polling on error (fallback to current behavior)
- Auto-refresh only if `window.__DEV__` is truthy

### 4. Modify standalone editor `saveDoc()` (`media/js/draft-editor.js`)

Current flow (lines 2955-2985):
```
POST → success → setStatus('Draft saved: <path>')
```

New flow:
- After successful save with `data.path`:
  - `setStatus('Building...')` (yellow/orange)
  - Poll `/editor/api/check-build?path=<path>` every 500ms
  - When done: `setStatus('Saved')` then if `window.__DEV__`, `setTimeout(location.reload, 1000)`
  - Timeout: 30s
- If `data.path` is not available or server doesn't support check-build (404), fall back to original behavior

## Dev-Mode Detection

- Use `window.__DEV__` flag set by the dev server injection
- In production, this flag won't exist, so auto-refresh and polling logic will be skipped
- The check-build endpoint only exists on the dev server, so 404 handling falls back gracefully

## Sequence Diagram

```
Browser               Dev Server            Dev Watch
  │                       │                     │
  │── POST /api/save ────→│                     │
  │←──── {ok, path} ─────│                     │
  │                       │                     │
  │ [show "Building..."]  │                     │
  │                       │                     │
  │── GET /check-build ──→│                     │
  │←── {done: false} ────│                     │
  │     (poll 500ms)      │                     │
  │                       │   (watcher detects  │
  │                       │    change, runs     │
  │                       │    publish.sh)      │
  │                       │────────────────────→│
  │                       │←── build complete   │
  │                       │                     │
  │── GET /check-build ──→│                     │
  │←── {done: true} ─────│                     │
  │                       │                     │
  │ [show "Saved"]        │                     │
  │ [1s → location.reload]│                     │
```

## Edge Cases

- **Rapid successive saves**: Use the existing `saveSeq` counter — stale poll responses should be discarded if a newer save has started
- **Build never completes**: 30s timeout, fall back to "Saved"
- **Non-dev/production**: `window.__DEV__` not set, skip polling and auto-refresh, show "Saved" immediately (current behavior)
- **Check-build endpoint 404**: If running on a server without the new endpoint, the fetch fails; catch error and fall back to current behavior
- **Dirty state during build**: If user continues editing while "Building..." state is active, handle markDirty() properly — don't let stale build completion override dirty state
- **Published posts**: Same check-build logic applies, but HTML path derivation differs (posts vs drafts)
