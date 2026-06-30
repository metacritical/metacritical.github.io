# Deep Seek Handoff — Disqus

## Status: ✅ Fixed

The Disqus comment box is now visible without any hacks.

## What Was Changed

**Disqus admin dashboard** (server-side):
- Forum colour scheme toggled from "Dark" → **"Light"**
- This makes Disqus render its light theme (dark text on white backgrounds)
  for all visitors, regardless of their system `prefers-color-scheme` setting

**Code changes** (all reverse the old filter-based fix that inverted images):

| File | Change |
|---|---|
| `media/css/style.css:580-582` | Removed `filter: invert(1) hue-rotate(180deg)` rule |
| `media/css/theme-medium.css:265-267` | Removed `filter: invert(1) hue-rotate(180deg)` rule |
| `media/js/disqus-theme-fix.js` | Replaced with no-op comment |
| `media/css/disqus-iframe.css` | Replaced with no-op comment |
| `publish.sh` | Removed `color_scheme` injection + `disqus-theme-fix.js` injection |
| `public/` HTML files | Stripped stale `disqus-theme-fix.js` script tags |

## Why Previous Approaches Didn't Work

Everything in the old approach (`filter`, `color-scheme`, CSS overrides on the
iframe element) was a workaround for Disqus's forum-level `"colorScheme":"dark"`
setting.  Once that was changed to `"light"` in the Disqus admin dashboard, the
cross-origin iframe renders native light-theme content with proper contrast —
no CSS/JS hacks needed.

## Files That Still Reference Disqus (kept for compatibility)

- `media/css/style.css:576-578` — `min-height: 280px` on iframe (harmless layout)
- `themes/mediumish/_includes/disqus.html` — template (may not be active)
- `publish.sh` — still injects `claps.js` only
