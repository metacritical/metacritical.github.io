#!/usr/bin/env python3
"""Generate the LiveDrafts editor page at /editor/index.html.

Takes the site template (nav, header, footer) from a published page and
injects the editor DOM + CSS/JS includes into the content area.
"""
import re
import sys
from pathlib import Path

repo_dir = Path(sys.argv[1])
public_dir = repo_dir / "public"
output_path = public_dir / "editor" / "index.html"

# Find a published page to use as template for site chrome.
template_path = None
for p in sorted(public_dir.glob("blog/*/*/*/*/index.html")):
    template_path = p
    break

if template_path is None:
    template_path = public_dir / "index.html"

if not template_path.exists():
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text("<!DOCTYPE html><html><body>Editor unavailable</body></html>")
    sys.exit(0)

template = template_path.read_text(encoding="utf-8", errors="replace")

EDITOR_BODY = """<div id="de-editor-banner" style="display:none;background:#f59e0b;color:#1f1f1b;padding:6px 16px;font-size:12px;font-weight:600;text-align:center;letter-spacing:.04em;position:sticky;top:0;z-index:100;border-bottom:1px solid #d97706"></div>
<div id="de-app">
  <div class="de-toolbar">
    <button class="de-tb-btn" type="button" onclick="window.editor.saveDoc('draft')">Save Draft</button>
    <button class="de-tb-btn primary" type="button" onclick="window.editor.saveDoc('publish')">Publish</button>
    <button class="de-tb-btn" type="button" onclick="window.editor.copyOrg()">Copy Org</button>
    <span class="de-tb-sep"></span>
    <button class="de-tb-btn" type="button" onclick="window.editor.history.undo()" title="Undo">Undo</button>
    <button class="de-tb-btn" type="button" onclick="window.editor.history.redo()" title="Redo">Redo</button>
    <span class="de-tb-spacer"></span>
    <span class="de-tb-status" id="de-status"></span>
  </div>
  <div class="de-sheet">
    <h1 class="de-title" contenteditable="true" spellcheck="true"></h1>
    <div class="de-body" contenteditable="true" spellcheck="true"></div>
  </div>
</div>"""

EDITOR_CSS = """
<link rel="stylesheet" href="/media/css/draft-editor.css" type="text/css">
<link rel="stylesheet" href="/media/js/prism/prism-okaidia.css" type="text/css">
<style>
.de-toolbar{display:flex;align-items:center;gap:6px;padding:10px 0;margin-bottom:12px;flex-wrap:wrap}
.de-tb-btn{font-size:13px;font-weight:500;padding:6px 14px;border:1px solid #d8d3c7;border-radius:6px;background:#fffdf8;color:#3a382f;cursor:pointer;transition:background .12s,border-color .12s}
.de-tb-btn:hover{background:#f4f1e8;border-color:#c4beb1}
.de-tb-btn.primary{background:#36c9c7;color:#fff;border-color:#36c9c7}
.de-tb-btn.primary:hover{background:#2bbab8}
.de-tb-sep{width:1px;height:22px;background:#e0dbcf;margin:0 4px}
.de-tb-spacer{flex:1}
.de-tb-status{font-size:12px;color:#6d6a64;font-family:ui-monospace,monospace}
#de-app{max-width:788px;margin:0 auto;padding:20px 0 60px}
</style>
"""

EDITOR_JS = """
<script src="/media/js/prism/prism-core.min.js"></script>
<script src="/media/js/prism/prism-autoloader.min.js"></script>
<script>
// Configure Prism autoloader to use CDN for language definitions.
if (window.Prism && Prism.plugins && Prism.plugins.autoloader) {
  Prism.plugins.autoloader.languages_path = 'https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/';
  Prism.manual = true;
}
</script>
<script src="/media/js/draft-editor.js"></script>
<script>
document.addEventListener('DOMContentLoaded', function() {
  window.editor = new DraftEditor({
    titleEl: '.de-title',
    bodyEl: '.de-body',
    statusEl: '#de-status',
    initialMode: 'draft',
  });

  // Auto-load draft/post from URL query params.
  var params = new URLSearchParams(location.search);
  var slug = params.get('slug');
  var kind = params.get('kind') || 'draft';
  var banner = document.getElementById('de-editor-banner');

  if (slug) {
    window.editor.loadDraftFromQuery(slug, kind).then(function(ok) {
      if (ok && banner) {
        var mode = window.editor.activeSourceMode || kind;
        if (mode === 'publish') {
          banner.style.background = '#36c9c7';
          banner.style.borderBottom = '1px solid #2bbab8';
          banner.textContent = 'Editing Published Post: ' + slug;
        } else {
          banner.textContent = 'Editing Draft: ' + slug;
        }
        banner.style.display = 'block';
      }
    });
  } else if (banner) {
    banner.textContent = 'New Draft';
    banner.style.display = 'block';
  }

  // Keyboard shortcuts.
  document.addEventListener('keydown', function(e) {
    if ((e.metaKey || e.ctrlKey) && e.key === 's') {
      e.preventDefault();
      window.editor.saveDoc(e.shiftKey ? 'publish' : 'draft');
    }
    if ((e.metaKey || e.ctrlKey) && e.shiftKey && e.key === 'Z') {
      e.preventDefault();
      if (window.editor.history) window.editor.history.redo();
    }
    if ((e.metaKey || e.ctrlKey) && e.key === 'z') {
      e.preventDefault();
      if (window.editor.history) window.editor.history.undo();
    }
  });
});
</script>
"""

# Inject editor CSS before </head>
result = template
if '</head>' in result:
    result = result.replace('</head>', f'{EDITOR_CSS}\n</head>', 1)

# Inject editor JS before </body>
if '</body>' in result:
    result = result.replace('</body>', f'{EDITOR_JS}\n</body>', 1)

# Replace the content area with editor HTML.
# Try post-page wrapper first, then home wrapper.
replaced = False
for pattern in [
    r'(<section[^>]*class="[^"]*post-page"[^>]*>\s*<div class="post">)\s*(.*?)(\s*</div>\s*</section>)',
    r'(<section[^>]*class="[^"]*home"[^>]*>\s*<div class="post">)\s*(.*?)(\s*</div>\s*</section>)',
    r'(<section[^>]*id="wrapper"[^>]*>\s*<div class="post">)\s*(.*?)(\s*</div>\s*</section>)',
]:
    new_result, count = re.subn(
        pattern,
        lambda m: m.group(1) + '\n' + EDITOR_BODY + '\n' + m.group(3),
        result,
        flags=re.DOTALL,
        count=1,
    )
    if count > 0:
        result = new_result
        replaced = True
        break

if not replaced:
    # Fallback: inject right after the opening body/nav.
    result = result.replace(
        '<body class="container">',
        f'<body class="container">\n{EDITOR_BODY}',
        1
    )

# Fix page title.
result = re.sub(r'<title>.*?</title>', '<title>Editor — Self dot send</title>', result, count=1)

output_path.parent.mkdir(parents=True, exist_ok=True)
output_path.write_text(result, encoding="utf-8")
print(f"Editor page generated: {output_path}")
