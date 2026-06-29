#!/usr/bin/env python3
"""Generate the archive page from published blog posts."""
import re, os, sys, html
from pathlib import Path
from collections import defaultdict

blog_dir = Path(sys.argv[1])
public_dir = blog_dir / "public"
template_path = public_dir / "archiveorg" / "index.html"
output_paths = [public_dir / "archive" / "index.html", public_dir / "blog" / "index.html"]

MONTHS = ["January","February","March","April","May","June",
          "July","August","September","October","November","December"]

posts = []
for idx_file in sorted(public_dir.glob("blog/*/*/*/*/index.html")):
    parts = idx_file.parts
    rel = "/" + "/".join(parts[parts.index("blog"):])
    slug = idx_file.parent.name
    if slug in ("archive", "all-posts"):
        continue
    year, month, day = parts[-5], parts[-4], parts[-3]
    try:
        content = idx_file.read_text(encoding="utf-8", errors="replace")
    except Exception:
        continue
    m = re.search(r"<title>(.*?)</title>", content)
    raw_title = m.group(1).strip() if m else slug
    title = html.escape(raw_title.split(" - Self")[0].strip()) if " - Self" in raw_title else html.escape(raw_title)
    rel = "/" + "/".join(parts[parts.index("blog"):])
    url = rel.replace("/index.html", "/").rstrip("/") + "/"
    posts.append((int(year), int(month), int(day), title, url, slug))

posts.sort(key=lambda p: (p[0], p[1], p[2]), reverse=True)

grouped = defaultdict(list)
for year, month, day, title, url, slug in posts:
    grouped[(year, month)].append((day, title, url))

body_html = '<h1>Archive</h1>\n'
prev = None
for (year, month) in sorted(grouped.keys(), reverse=True):
    if (year, month) != prev:
        body_html += f'<h2>{MONTHS[month-1]} {year}</h2>\n<ul class="org-ul">\n'
        prev = (year, month)
    for day, title, url in sorted(grouped[(year, month)], reverse=True):
        body_html += f'  <li>{year:04d}-{month:02d}-{day:02d} :: <a href="{url}">{title}</a></li>\n'
    body_html += '</ul>\n'

if not template_path.exists():
    sys.exit(0)

template = template_path.read_text(encoding="utf-8", errors="replace")
result = re.sub(
    r'(<section[^>]*>\s*<div class="post">\s*)(.*?)(\s*</div>\s*</section>)',
    lambda m: m.group(1) + body_html.rstrip() + m.group(3),
    template,
    flags=re.DOTALL,
)

for out in output_paths:
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(result, encoding="utf-8")
    print(f"Archive written to {out}")
