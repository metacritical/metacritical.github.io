#!/usr/bin/env python3
"""Build HTML preview pages for draft articles.

Runs a second AOG build with drafts included (to a temp dir), then copies
each draft article page to public/drafts/<slug>/index.html with a draft
banner injected at the top.
"""
import re
import os
import sys
import html
import json
import shutil
import subprocess
import tempfile
from pathlib import Path

repo_dir = Path(sys.argv[1])
drafts_dir = repo_dir / "drafts"
public_dir = repo_dir / "public"
drafts_public = public_dir / "drafts"

if not drafts_dir.is_dir() or not any(drafts_dir.glob("*.org")):
    sys.exit(0)

print("[drafts] Building preview pages...")

draft_tmp = Path(tempfile.mkdtemp(prefix="draft-build-"))

# Save current branch so we can restore it if AOG switches.
result = subprocess.run(
    ["git", "-C", str(repo_dir), "rev-parse", "--abbrev-ref", "HEAD"],
    capture_output=True, text=True
)
start_branch = result.stdout.strip()

# Run AOG with drafts included.
env = os.environ.copy()
env["AOG_INCLUDE_DRAFTS"] = "1"
print("[drafts] Running AOG draft build...")
result = subprocess.run(
    ["aog", "publish", str(repo_dir), str(draft_tmp)],
    capture_output=True, text=True, env=env, timeout=180
)

# Restore branch if AOG changed it.
result = subprocess.run(
    ["git", "-C", str(repo_dir), "rev-parse", "--abbrev-ref", "HEAD"],
    capture_output=True, text=True
)
current_branch = result.stdout.strip()
if start_branch and current_branch and current_branch != start_branch:
    subprocess.run(
        ["git", "-C", str(repo_dir), "checkout", start_branch],
        capture_output=True
    )

# Normalize file:// URLs in all generated HTML.
for html_file in draft_tmp.rglob("*.html"):
    content = html_file.read_text(encoding="utf-8", errors="replace")
    content = content.replace("file:///blog/", "/blog/")
    content = content.replace("file:///assets/", "/assets/")
    content = content.replace("file:///media/", "/media/")
    html_file.write_text(content, encoding="utf-8")

DRAFT_BANNER = (
    '<div style="background:#f59e0b;color:#1f1f1b;padding:6px 16px;'
    'font-size:12px;font-weight:600;text-align:center;letter-spacing:.04em;'
    'position:sticky;top:0;z-index:100;border-bottom:1px solid #d97706;'
    'display:flex;align-items:center;justify-content:center;gap:14px">'
    '<span>DRAFT — Not yet published</span>'
    '<a href="/editor/?slug={slug}&kind=draft" '
    'style="background:#1f1f1b;color:#fff;padding:3px 12px;border-radius:4px;'
    'font-size:11px;text-decoration:none;letter-spacing:0">Edit</a>'
    '</div>'
)

count = 0
for org_file in sorted(drafts_dir.glob("*.org")):
    slug = org_file.stem
    content = org_file.read_text(encoding="utf-8", errors="replace")

    # Parse image settings from #+ATTR_HTML lines so they survive rebuild.
    image_settings = {}
    for attr_match in re.finditer(r'^#\+ATTR_HTML:\s+(.*?)\s*$', content, re.MULTILINE):
        attrs_str = attr_match.group(1)
        img_match = re.search(r'\[\[([^\]]+)\]\]', content[attr_match.end():])
        if not img_match:
            continue
        img_url = img_match.group(1)
        settings = {}
        for m in re.finditer(r':(width|height)\s+(\d+)', attrs_str):
            settings[m.group(1)] = m.group(2)
        for m in re.finditer(r':data-(\S+)\s+(\S+)', attrs_str):
            settings[m.group(1)] = m.group(2)
        if settings:
            image_settings[img_url] = settings

    m_uri = re.search(r'^#\+URI:\s+(.*)', content, re.MULTILINE)
    m_date = re.search(r'^#\+DATE:\s+\[?(\d{4})-(\d{2})-(\d{2})', content, re.MULTILINE)

    src_page = None
    if m_uri and m_date:
        year = m_date.group(1)
        month = m_date.group(2)
        day = m_date.group(3)
        uri = m_uri.group(1).strip()
        uri = uri.replace("%y", year).replace("%m", month).replace("%d", day)
        uri = uri.rstrip("/")
        candidate = draft_tmp / uri.lstrip("/") / "index.html"
        if candidate.exists():
            src_page = candidate

    if src_page is None:
        # Fallback: search for a matching slug directory.
        for index_file in draft_tmp.rglob("index.html"):
            if slug in str(index_file) and "blog/" in str(index_file):
                src_page = index_file
                break

    if src_page is None:
        print(f"  [drafts] SKIP: No HTML generated for {slug}")
        continue

    dst_dir = drafts_public / slug
    dst_dir.mkdir(parents=True, exist_ok=True)

    page_html = src_page.read_text(encoding="utf-8", errors="replace")

    # Inject draft banner right after the opening <body> tag.
    page_html = page_html.replace(
        '<body class="container">',
        f'<body class="container">\n{DRAFT_BANNER.format(slug=slug)}',
        1
    )

    # Append "(Draft)" to the <title> tag.
    page_html = re.sub(
        r'(<title>)(.*?)(</title>)',
        lambda m: f'{m.group(1)}{m.group(2)} (Draft){m.group(3)}',
        page_html,
        count=1
    )

    # Inject editor CSS and Prism CSS before </head>.
    editor_css = (
        '<link rel="stylesheet" href="/media/css/draft-editor.css" type="text/css">\n'
        '  <link rel="stylesheet" href="/media/js/prism/prism-okaidia.css" type="text/css">'
    )
    page_html = page_html.replace('</head>', f'  {editor_css}\n</head>', 1)

    # Inject saved image settings so the bootstrap script can restore them.
    settings_json = json.dumps(image_settings)
    page_html = page_html.replace('</head>', f'  <script>window.__imageSettings = {settings_json};</script>\n</head>', 1)

    # Inject editor JS and bootstrap script before </body>.
    editor_js = f"""
<script src="/media/js/prism/prism-core.min.js"></script>
<script src="/media/js/prism/prism-autoloader.min.js"></script>
<script>
if (window.Prism && Prism.plugins && Prism.plugins.autoloader) {{
  Prism.plugins.autoloader.languages_path = 'https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/';
  Prism.manual = true;
}}
</script>
 <script src="/media/js/draft-editor.js"></script>
<script>
document.addEventListener('DOMContentLoaded', function() {{
  var postDiv = document.querySelector('.post');
  if (!postDiv) return;

  var titleEl = postDiv.querySelector('h1.title');
  if (!titleEl) return;

  // --- Extract existing metadata before removing it ---
  var authorRow = postDiv.querySelector('.post-author-row');
  var authorName = 'Pankaj Doharey';
  var authorDate = '';
  if (authorRow) {{
    var nameEl = authorRow.querySelector('.post-author-name');
    if (nameEl) authorName = nameEl.textContent.trim();
    var dateEl = authorRow.querySelector('.post-author-date');
    if (dateEl) authorDate = dateEl.textContent.trim();
    authorRow.remove();
  }}

  var oldTags = postDiv.querySelector('.article-tags');
  var existingTags = [];
  if (oldTags) {{
    oldTags.querySelectorAll('.article-tag').forEach(function(t) {{
      existingTags.push(t.textContent.trim());
    }});
    oldTags.remove();
  }}

  // Also remove the aside (story rail) — not needed in draft editor.
  var aside = postDiv.querySelector('aside.story-rail');
  if (aside) aside.remove();

  // --- Build compact metadata row ---
  var meta = document.createElement('div');
  meta.className = 'de-meta-row';
  meta.setAttribute('data-slug', '{slug}');
  meta.setAttribute('data-url', '/drafts/{slug}/');
  meta.innerHTML =
    '<img class="de-meta-avatar" src="/media/images/avatar.jpg" alt="' + authorName + '">' +
    '<div class="de-meta-info">' +
      '<span class="de-meta-author">' + authorName + '</span>' +
      '<span class="de-meta-sep">&middot;</span>' +
      '<span class="de-meta-date">' + (authorDate || new Date().toISOString().slice(0,10)) + '</span>' +
    '</div>' +
    '<button class="de-series-btn" type="button" id="de-series-btn">+ series</button>' +
    '<span class="de-meta-tags" id="de-tag-list"></span>' +
    '<button class="de-tag-add-btn" type="button" id="de-tag-input">+ tag</button>';

  // Second row: toolbar buttons.
  var toolbar = document.createElement('div');
  toolbar.className = 'de-tool-row';
  toolbar.innerHTML =
    '<span class="de-save-indicator" id="de-save-dot" title="Save status"></span>' +
    '<span class="de-tb-status-text" id="de-status-text"></span>' +
    '<span class="de-tool-spacer"></span>' +
    '<button class="de-tb-icon" type="button" id="de-save" title="Save Draft"><svg viewBox="0 0 24 24" width="16" height="16" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M19 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11l5 5v11a2 2 0 0 1-2 2z"/><polyline points="17 21 17 13 7 13 7 21"/><polyline points="7 3 7 8 15 8"/></svg></button>' +
    '<button class="de-tb-icon" type="button" id="de-publish" title="Publish"><svg viewBox="0 0 24 24" width="16" height="16" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><polyline points="17 8 12 3 7 8"/><line x1="12" y1="3" x2="12" y2="15"/></svg></button>' +
    '<span class="de-tb-sep"></span>' +
    '<button class="de-tb-icon" type="button" id="de-undo" title="Undo"><svg viewBox="0 0 24 24" width="16" height="16" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><polyline points="1 4 1 10 7 10"/><path d="M3.51 15a9 9 0 1 0 2.13-9.36L1 10"/></svg></button>' +
    '<button class="de-tb-icon" type="button" id="de-redo" title="Redo"><svg viewBox="0 0 24 24" width="16" height="16" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><polyline points="23 4 23 10 17 10"/><path d="M20.49 15a9 9 0 1 1-2.12-9.36L23 10"/></svg></button>';

  // Insert metadata row right after title, then toolbar below it.
  if (titleEl.nextSibling) {{
    postDiv.insertBefore(meta, titleEl.nextSibling);
  }} else {{
    postDiv.appendChild(meta);
  }}
  postDiv.insertBefore(toolbar, meta.nextSibling);

  // Inject styles.
  var mstyle = document.createElement('style');
  mstyle.textContent = [
    '.de-meta-row{{display:flex;align-items:center;gap:8px;padding:10px 0 6px;margin-bottom:0;flex-wrap:wrap}}',
    '.de-meta-avatar{{width:28px;height:28px;border-radius:50%;object-fit:cover;flex:0 0 auto}}',
    '.de-meta-info{{display:flex;align-items:center;gap:5px;flex:0 0 auto}}',
    '.de-meta-author{{font-size:13px;font-weight:600;color:#1f1f1b}}',
    '.de-meta-sep{{color:#c4beb1;font-size:12px}}',
    '.de-meta-date{{font-size:12px;color:#9c968a}}',
    '.de-meta-tags{{display:flex;gap:4px;flex-wrap:wrap;align-items:center}}',
    '.de-meta-tag{{display:inline-flex;align-items:center;gap:3px;font-size:12px;font-weight:500;font-family:"Helvetica Neue",Helvetica,Arial,sans-serif;color:#6f6a5e;background:#f1ece0;padding:3px 4px 3px 11px;border-radius:12px;transition:background .12s}}',
    '.de-meta-tag:hover{{background:#e8e3d4}}',
    '.de-meta-tag .de-tag-remove{{cursor:pointer;color:#b0aaa0;font-size:13px;line-height:1;padding:0 0 0 2px;border:none;background:none}}',
    '.de-meta-tag .de-tag-remove:hover{{color:#dc2626}}',
    '.de-tag-add-btn{{font-size:11px;font-weight:500;color:#9c968a;background:#f1ece0;border:none;padding:2px 9px;border-radius:11px;cursor:pointer;transition:background .12s,color .12s}}',
    '.de-tag-add-btn:hover{{background:#e8e3d4;color:#6f6a5e}}',
    '.de-series-btn{{font-size:11px;font-weight:600;color:#36c9c7;background:#e0f5f4;border:none;padding:2px 9px;border-radius:11px;cursor:pointer;transition:background .12s,color .12s;white-space:nowrap}}',
    '.de-series-btn:hover{{background:#bceae8;color:#2baa9f}}',
    '.de-series-btn.assigned{{background:#e0f5f4;color:#2baa9f}}',
    '.de-tool-row{{display:flex;align-items:center;gap:4px;padding:4px 0 12px;margin-bottom:4px;border-bottom:1px solid #f0ede5}}',
    '.de-tool-spacer{{flex:1}}',
    '.de-tb-status-text{{font-size:11px;color:#9c968a}}',
    '.de-tb-icon{{width:28px;height:28px;display:inline-flex;align-items:center;justify-content:center;border:1px solid #e2ddd3;border-radius:6px;background:#fffdf8;color:#5c5750;font-size:13px;cursor:pointer;transition:background .12s,color .12s}}',
    '.de-tb-icon:hover{{background:#f4f1e8;color:#1f1f1b}}',
    '.de-tb-sep{{width:1px;height:18px;background:#e2ddd3;margin:0 2px}}',
    '.de-save-indicator{{width:7px;height:7px;border-radius:50%;background:#36c9c7;display:inline-block;transition:background .25s,box-shadow .25s;flex:0 0 auto}}',
    '.de-save-indicator.saving{{background:#f59e0b;box-shadow:0 0 5px rgba(245,158,11,.5);animation:de-pulse 1s infinite}}',
    '.de-save-indicator.dirty{{background:#e8a035}}',
    '.de-save-indicator.error{{background:#dc2626}}',
    '@keyframes de-pulse{{0%,100%{{opacity:1}}50%{{opacity:.4}}}}'
  ].join('');
  document.head.appendChild(mstyle);

  // Make title editable.
  titleEl.setAttribute('contenteditable', 'true');
  titleEl.setAttribute('spellcheck', 'true');

  // Wrap remaining content into de-body.
  var bodyDiv = document.createElement('div');
  bodyDiv.className = 'de-body';
  bodyDiv.setAttribute('contenteditable', 'true');
  bodyDiv.setAttribute('spellcheck', 'true');

  var moveNodes = [];
  for (var i = 0; i < postDiv.childNodes.length; i++) {{
    var node = postDiv.childNodes[i];
    if (node === bodyDiv || node === titleEl || node === meta) continue;
    if (node.nodeType === 3) {{
      if (node.textContent && node.textContent.trim()) moveNodes.push(node);
      continue;
    }}
    if (node.nodeType !== 1) continue;
    var tag = node.tagName;
    if (tag === 'STYLE' || tag === 'SCRIPT') continue;
    if (node.classList && (node.classList.contains('de-meta-row') || node.classList.contains('de-tool-row') || node.classList.contains('de-toolbar'))) continue;
    if (node.classList && node.classList.contains('series-nav-wrap')) continue;
    if (node.classList && node.classList.contains('post-meta-wrap')) continue;
    moveNodes.push(node);
  }}
  for (var j = 0; j < moveNodes.length; j++) {{
    bodyDiv.appendChild(moveNodes[j]);
  }}

  // Make code blocks non-editable and convert AOG div.figure to image-block figures.
  bodyDiv.querySelectorAll('.org-src-container, .org-src-container pre, .org-src-container code, .cb-bar').forEach(function(el) {{
    el.setAttribute('contenteditable', 'false');
  }});

  // Convert AOG-rendered figures to editable image-block figures so the image toolbar works.
  bodyDiv.querySelectorAll('div.figure').forEach(function(fig) {{
    var img = fig.querySelector('img');
    if (!img) return;
    var src = img.getAttribute('src') || '';
    var saved = (window.__imageSettings || {{}})[src] || {{}};
    var fit = saved.fit || 'cover';
    var position = saved.position || 'center';
    var filter = saved.filter || 'none';
    var align = saved.align || 'center';
    var imgW = (img.getAttribute('width') || '').replace('px', '');
    var imgH = (img.getAttribute('height') || '').replace('px', '');
    var w = saved.width || imgW || '760';
    var h = saved.height || imgH || '420';
    var newFig = document.createElement('figure');
    var cls = 'image-block fit-' + fit + ' pos-' + position + ' align-' + align;
    if (filter && filter !== 'none') cls += ' filter-' + filter;
    newFig.className = cls;
    newFig.setAttribute('contenteditable', 'false');
    newFig.setAttribute('data-src', src);
    newFig.dataset.fit = fit;
    newFig.dataset.position = position;
    newFig.dataset.filter = filter;
    newFig.dataset.align = align;
    newFig.style.cssText = 'width:' + w + 'px;max-width:100%;height:' + h + 'px';
    newFig.innerHTML = '<img src="' + src + '" alt="">' + (img.alt ? '<figcaption>' + img.alt + '</figcaption>' : '');
    fig.parentNode.replaceChild(newFig, fig);
  }});

  if (toolbar.nextSibling) {{
    postDiv.insertBefore(bodyDiv, toolbar.nextSibling);
  }} else {{
    postDiv.appendChild(bodyDiv);
  }}

  // --- Initialize editor ---
  window.editor = new DraftEditor({{
    titleEl: titleEl,
    bodyEl: bodyDiv,
    statusEl: null,
    initialMode: 'draft',
    initialTargetPath: 'drafts/{slug}.org',
  }});

  // --- Tag management with picker popup ---
  var tagList = document.getElementById('de-tag-list');
  var tagAddBtn = document.getElementById('de-tag-input'); // repurposed as button

  // Tag popup element.
  var tagPopup = document.createElement('div');
  tagPopup.className = 'de-tag-popup';
  tagPopup.style.cssText = 'display:none;position:absolute;background:#fff;border:1px solid #e2ddd3;border-radius:8px;box-shadow:0 8px 24px rgba(0,0,0,0.12);padding:8px;z-index:10000;min-width:200px;max-width:300px';
  document.body.appendChild(tagPopup);

  function getAllStoredTags() {{
    try {{ return JSON.parse(localStorage.getItem('selfdotsend_all_tags') || '[]'); }} catch(_) {{ return []; }}
  }}
  function saveAllStoredTags(tags) {{
    try {{ localStorage.setItem('selfdotsend_all_tags', JSON.stringify(tags)); }} catch(_) {{}}
  }}

  function addTag(val) {{
    val = val.trim().replace(/,/g, '');
    if (!val) return;
    var tags = window.editor.getTags();
    var lower = val.toLowerCase();
    if (tags.indexOf(lower) === -1) {{
      tags.push(lower);
      window.editor.setTags(tags);
      renderTags();
      window.editor._markDirty();
    }}
    // Persist to global store.
    var all = getAllStoredTags();
    if (all.indexOf(lower) === -1) {{ all.push(lower); saveAllStoredTags(all); }}
  }}

  function removeTag(t) {{
    var tags = window.editor.getTags().filter(function(x) {{ return x !== t; }});
    window.editor.setTags(tags);
    renderTags();
    window.editor._markDirty();
  }}

  function renderTags() {{
    tagList.innerHTML = '';
    window.editor.getTags().forEach(function(t) {{
      var pill = document.createElement('span');
      pill.className = 'de-meta-tag';
      pill.textContent = t;
      var rm = document.createElement('button');
      rm.className = 'de-tag-remove';
      rm.textContent = '\u00d7';
      rm.addEventListener('click', function() {{ removeTag(t); }});
      pill.appendChild(rm);
      tagList.appendChild(pill);
    }});
  }}

  function openTagPopup() {{
    var rect = tagAddBtn.getBoundingClientRect();
    tagPopup.style.top = (rect.bottom + 4) + 'px';
    tagPopup.style.left = rect.left + 'px';
    tagPopup.style.display = 'block';
    tagPopup.innerHTML = '<div style="font-size:12px;color:#9c968a;padding:8px">Loading tags...</div>';

    var current = window.editor.getTags();

    // Fetch all tags from API + localStorage, merge, then render.
    fetch('/editor/api/tags').then(function(r) {{ return r.json(); }}).then(function(data) {{
      var apiTags = (data.ok && data.tags) ? data.tags : [];
      var localTags = getAllStoredTags();
      // Merge all sources.
      var merged = {{}};
      apiTags.concat(localTags).forEach(function(t) {{
        merged[t.toLowerCase()] = t;
      }});
      var allTags = Object.keys(merged).sort();

      var available = allTags.filter(function(t) {{ return current.indexOf(t) === -1; }});

      var html = '<div style="margin-bottom:8px">';
      html += '<input type="text" id="de-tag-popup-input" placeholder="Type new tag + Enter..." style="width:100%;box-sizing:border-box;border:1px solid #e2ddd3;border-radius:6px;padding:6px 10px;font-size:12px;outline:none">';
      html += '</div>';

      if (available.length) {{
        html += '<div style="font-size:10px;color:#9c968a;margin-bottom:6px;text-transform:uppercase;letter-spacing:.04em">All Tags (' + available.length + ')</div>';
        html += '<div style="display:flex;flex-wrap:wrap;gap:4px;max-height:200px;overflow-y:auto">';
        available.forEach(function(t) {{
          var display = merged[t] || t;
          html += '<span class="de-tag-suggest" data-tag="' + t.replace(/"/g, '&quot;') + '" style="font-size:11px;color:#6f6a5e;background:#f1ece0;padding:3px 9px;border-radius:11px;cursor:pointer;transition:background .1s">' + display + '</span>';
        }});
        html += '</div>';
      }} else {{
        html += '<div style="font-size:11px;color:#c4beb1;padding:4px 0">All tags are already added.</div>';
      }}

      tagPopup.innerHTML = html;

      var popupInput = document.getElementById('de-tag-popup-input');
      popupInput.focus();
      popupInput.addEventListener('keydown', function(e) {{
        if (e.key === 'Enter' || e.key === ',') {{
          e.preventDefault();
          addTag(popupInput.value);
          popupInput.value = '';
          closeTagPopup();
          setTimeout(openTagPopup, 50);
        }}
        if (e.key === 'Escape') closeTagPopup();
      }});

      tagPopup.querySelectorAll('.de-tag-suggest').forEach(function(el) {{
        el.addEventListener('click', function() {{
          addTag(el.getAttribute('data-tag'));
          closeTagPopup();
          setTimeout(openTagPopup, 50);
        }});
        el.addEventListener('mouseenter', function() {{ el.style.background = '#e8e3d4'; }});
        el.addEventListener('mouseleave', function() {{ el.style.background = '#f1ece0'; }});
      }});
    }}).catch(function() {{
      tagPopup.innerHTML = '<div style="font-size:12px;color:#dc2626;padding:8px">Unable to load tags.</div>';
    }});
  }}

  function closeTagPopup() {{
    tagPopup.style.display = 'none';
  }}

  tagAddBtn.addEventListener('click', function(e) {{
    e.preventDefault();
    if (tagPopup.style.display === 'block') closeTagPopup();
    else openTagPopup();
  }});

  document.addEventListener('click', function(e) {{
    if (tagPopup.style.display !== 'block') return;
    if (!tagPopup.contains(e.target) && e.target !== tagAddBtn) closeTagPopup();
  }});

  // Persist existing tags to global store.
  existingTags.forEach(function(t) {{
    var all = getAllStoredTags();
    var lower = t.toLowerCase();
    if (all.indexOf(lower) === -1) {{ all.push(lower); saveAllStoredTags(all); }}
  }});

  window.editor.setTags(existingTags.map(function(t) {{ return t.toLowerCase(); }}));
  renderTags();

  // --- Series picker ---
  var seriesBtn = document.getElementById('de-series-btn');
  var seriesPopup = document.createElement('div');
  seriesPopup.className = 'de-popup';
  seriesPopup.style.cssText = 'display:none;position:absolute;background:#fff;border:1px solid #e2ddd3;border-radius:8px;box-shadow:0 8px 24px rgba(0,0,0,0.12);padding:8px;z-index:10000;min-width:240px;max-width:300px';
  document.body.appendChild(seriesPopup);
  var currentSeriesSlug = '';
  var currentSeriesPos = 0;

  function loadSeries() {{
    fetch('/editor/api/series').then(function(r){{return r.json()}}).then(function(data) {{
      if (!data.ok) return;
      var all = data.series || [];
      var articleUrl = meta.getAttribute('data-url') || '/drafts/{slug}/';
      currentSeriesSlug = '';
      currentSeriesPos = 0;
      // Find which series this article belongs to.
      all.forEach(function(s) {{
        fetch('/editor/api/series/' + s.slug).then(function(r){{return r.json()}}).then(function(d) {{
          if (!d.ok) return;
          var arts = d.series.articles || [];
          for (var k = 0; k < arts.length; k++) {{
            if (arts[k].url && articleUrl.indexOf(arts[k].url) !== -1) {{
              currentSeriesSlug = s.slug;
              currentSeriesPos = k + 1;
              seriesBtn.textContent = s.name;
              seriesBtn.classList.add('assigned');
              seriesBtn.title = 'Position ' + currentSeriesPos + ' of ' + arts.length;
              return;
            }}
          }}
          // Also check if the slug is in the url
          for (var k = 0; k < arts.length; k++) {{
            if (arts[k].url && arts[k].url.indexOf('{slug}') !== -1) {{
              currentSeriesSlug = s.slug;
              currentSeriesPos = k + 1;
              seriesBtn.textContent = s.name;
              seriesBtn.classList.add('assigned');
              return;
            }}
          }}
        }});
      }});
    }});
  }}

  function openSeriesPopup() {{
    var rect = seriesBtn.getBoundingClientRect();
    seriesPopup.style.top = (rect.bottom + 4) + 'px';
    seriesPopup.style.left = rect.left + 'px';
    seriesPopup.style.display = 'block';
    seriesPopup.innerHTML = '<div style="font-size:12px;color:#9c968a;padding:8px">Loading...</div>';

    fetch('/editor/api/series').then(function(r){{return r.json()}}).then(function(data) {{
      var all = data.series || [];
      var articleUrl = meta.getAttribute('data-url') || '/drafts/{slug}/';
      var html = '<div style="font-size:11px;font-weight:600;color:#6f6a5e;margin-bottom:6px;padding:0 4px">Assign to Series</div>';
      if (all.length === 0) {{
        html += '<div style="font-size:11px;color:#c4beb1;padding:4px">No series found. Create one on the CLI.</div>';
      }} else {{
        html += '<div style="max-height:220px;overflow-y:auto">';
        all.forEach(function(s) {{
          var active = (s.slug === currentSeriesSlug) ? ' style="background:#e0f5f4;border-color:#36c9c7"' : '';
          html += '<div class="de-series-opt" data-slug="' + s.slug + '"' + active + ' style="padding:6px 8px;font-size:12px;cursor:pointer;border-radius:6px;margin-bottom:2px;border:1px solid transparent">';
          html += '<div style="font-weight:600">' + s.name + '</div>';
          html += '<div style="font-size:10px;color:#9c968a;margin-top:1px">' + s.count + ' articles</div>';
          html += '</div>';
        }});
        html += '</div>';
      }}
      if (currentSeriesSlug) {{
        html += '<div class="de-series-remove" style="margin-top:6px;padding:6px 8px;font-size:11px;color:#dc2626;cursor:pointer;border-radius:6px;text-align:center">Remove from series</div>';
      }}
      html += '<div style="margin-top:6px;padding:4px 0"><input type="text" id="de-series-create-input" placeholder="Create new series..." style="width:100%;box-sizing:border-box;border:1px solid #e2ddd3;border-radius:6px;padding:6px 8px;font-size:12px;outline:none"></div>';
      seriesPopup.innerHTML = html;

      // Wire clicks
      seriesPopup.querySelectorAll('.de-series-opt').forEach(function(el) {{
        el.addEventListener('click', function() {{
          var slug = el.getAttribute('data-slug');
          fetch('/editor/api/series/' + slug).then(function(r){{return r.json()}}).then(function(d) {{
            if (!d.ok) return;
            var arts = d.series.articles || [];
            // Check if already in this series
            var found = false;
            for (var k = 0; k < arts.length; k++) {{
              if (arts[k].url && arts[k].url.indexOf('{slug}') !== -1) found = true;
            }}
            if (!found) {{
              fetch('/editor/api/series/' + slug + '/add', {{
                method: 'POST',
                headers: {{'Content-Type':'application/json'}},
                body: JSON.stringify({{article:{{title:titleEl.innerText.trim(),url:articleUrl,date:(new Date().toISOString().slice(0,10)),status:'draft'}}}})
              }});
            }}
            seriesBtn.textContent = d.series.name;
            seriesBtn.classList.add('assigned');
            currentSeriesSlug = slug;
            closeSeriesPopup();
            loadSeries();
          }});
        }});
      }});

      var removeEl = seriesPopup.querySelector('.de-series-remove');
      if (removeEl) {{
        removeEl.addEventListener('click', function() {{
          fetch('/editor/api/series/' + currentSeriesSlug + '/remove', {{
            method: 'POST',
            headers: {{'Content-Type':'application/json'}},
            body: JSON.stringify({{url: articleUrl, slug: '{slug}'}})
          }}).then(function() {{
            seriesBtn.textContent = '+ series';
            seriesBtn.classList.remove('assigned');
            currentSeriesSlug = '';
            closeSeriesPopup();
          }});
        }});
      }}

      var createInput = document.getElementById('de-series-create-input');
      if (createInput) {{
        createInput.focus();
        createInput.addEventListener('keydown', function(e) {{
          if (e.key === 'Enter') {{
            e.preventDefault();
            var name = createInput.value.trim();
            if (!name) return;
            var newSlug = name.toLowerCase().replace(/[^a-z0-9]+/g,'-').replace(/^-|-$/g,'');
            fetch('/editor/api/series', {{
              method: 'POST',
              headers: {{'Content-Type':'application/json'}},
              body: JSON.stringify({{slug:newSlug,name:name,description:'',articles:[{{title:titleEl.innerText.trim(),url:articleUrl,date:(new Date().toISOString().slice(0,10)),status:'draft'}}]}})
            }}).then(function() {{
              seriesBtn.textContent = name;
              seriesBtn.classList.add('assigned');
              currentSeriesSlug = newSlug;
              closeSeriesPopup();
              loadSeries();
            }});
          }}
          if (e.key === 'Escape') closeSeriesPopup();
        }});
      }}
    }});
  }}

  function closeSeriesPopup() {{
    seriesPopup.style.display = 'none';
  }}

  seriesBtn.addEventListener('click', function(e) {{
    e.preventDefault();
    if (seriesPopup.style.display === 'block') closeSeriesPopup();
    else openSeriesPopup();
  }});

  document.addEventListener('click', function(e) {{
    if (seriesPopup.style.display !== 'block') return;
    if (!seriesPopup.contains(e.target) && e.target !== seriesBtn) closeSeriesPopup();
  }});

  loadSeries();

  // --- Save indicator + auto-save ---
  var saveDot = document.getElementById('de-save-dot');
  var statusText = document.getElementById('de-status-text');
  var autoSaveTimer = null;
  var AUTO_SAVE_DELAY = 2000;

  function setSaveState(state) {{
    saveDot.className = 'de-save-indicator ' + state;
    var labels = {{'': 'Saved', saving: 'Saving...', dirty: 'Unsaved', error: 'Save failed'}};
    if (statusText) statusText.textContent = labels[state] || '';
  }}

  function doSave(mode) {{
    setSaveState('saving');
    window.editor.saveDoc(mode).then(function() {{
      setSaveState('');
      window.editor.dirty = false;
    }}).catch(function() {{
      setSaveState('error');
    }});
  }}

  window.editor.opts.onChange = function() {{
    setSaveState('dirty');
    clearTimeout(autoSaveTimer);
    autoSaveTimer = setTimeout(function() {{ doSave('draft'); }}, AUTO_SAVE_DELAY);
  }};

  window.addEventListener('beforeunload', function() {{
    if (window.editor.dirty) doSave('draft');
  }});

  // --- Wire buttons ---
  document.getElementById('de-save').addEventListener('click', function() {{ clearTimeout(autoSaveTimer); doSave('draft'); }});
  document.getElementById('de-publish').addEventListener('click', function() {{ clearTimeout(autoSaveTimer); doSave('publish'); }});
  document.getElementById('de-undo').addEventListener('click', function() {{ if (window.editor.history) window.editor.history.undo(); }});
  document.getElementById('de-redo').addEventListener('click', function() {{ if (window.editor.history) window.editor.history.redo(); }});

  document.addEventListener('keydown', function(e) {{
    if ((e.metaKey || e.ctrlKey) && e.key === 's') {{
      e.preventDefault();
      clearTimeout(autoSaveTimer);
      doSave(e.shiftKey ? 'publish' : 'draft');
    }}
    if ((e.metaKey || e.ctrlKey) && e.shiftKey && e.key === 'Z') {{
      e.preventDefault();
      if (window.editor.history) window.editor.history.redo();
    }}
    if ((e.metaKey || e.ctrlKey) && e.key === 'z') {{
      e.preventDefault();
      if (window.editor.history) window.editor.history.undo();
    }}
  }});

  setSaveState('');
}});
</script>"""

    page_html = page_html.replace('</body>', f'{editor_js}\n</body>', 1)

    (dst_dir / "index.html").write_text(page_html, encoding="utf-8")
    print(f"  [drafts] Preview ready (editable): /drafts/{slug}/")
    count += 1

shutil.rmtree(draft_tmp, ignore_errors=True)
print(f"[drafts] Built {count} preview page(s).")
