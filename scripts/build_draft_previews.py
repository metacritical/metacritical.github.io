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

    # Inject hero banner image after the author row, before article content.
    hero_img = (
        '<img src="/media/images/placeholder.png" alt="" '
        'style="width:100%;border-radius:8px;margin:0 0 24px;display:block">'
    )
    page_html = re.sub(
        r'(</div>\s*<p>)',
        f'</div>\n  {hero_img}\n  <p>',
        page_html,
        count=1
    )

    # Append "(Draft)" to the <title> tag.
    page_html = re.sub(
        r'(<title>)(.*?)(</title>)',
        lambda m: f'{m.group(1)}{m.group(2)} (Draft){m.group(3)}',
        page_html,
        count=1
    )

    (dst_dir / "index.html").write_text(page_html, encoding="utf-8")
    print(f"  [drafts] Preview ready: /drafts/{slug}/")
    count += 1

shutil.rmtree(draft_tmp, ignore_errors=True)
print(f"[drafts] Built {count} preview page(s).")
