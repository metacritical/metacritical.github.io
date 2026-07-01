#!/usr/bin/env python3
"""Generate the blog homepage card list from published posts.

Replaces the static <section class="medium-home"> block in public/index.html
with a card for every published post, mirroring the archive listing produced
by generate_archive.py. Run after AOG has emitted public/index.html.
"""
import re
import sys
import html
from pathlib import Path

blog_dir = Path(sys.argv[1])
public_dir = blog_dir / "public"
index_path = public_dir / "index.html"

MONTHS = ["", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

IMAGE_EXTS = ("*.png", "*.jpg", "*.jpeg", "*.gif", "*.webp")


def find_hero(year, month, day, slug):
    """Return a served image URL for a post, falling back to the placeholder."""
    assetdir = blog_dir / "assets" / "blog" / year / month / day / slug
    if assetdir.is_dir():
        for ext in IMAGE_EXTS:
            for p in sorted(assetdir.glob(ext)):
                return "/assets/blog/%s/%s/%s/%s/%s" % (year, month, day, slug, p.name)
    return "/media/images/placeholder.png"


def extract_description(content):
    """Pull the post description from its <meta name="description"> tag."""
    for pattern in (
        r'<meta\s+name="description"\s+content="([^"]*)"',
        r'<meta\s+content="([^"]*)"\s+name="description"',
        r"<meta\s+name='description'\s+content='([^']*)'",
    ):
        m = re.search(pattern, content, re.IGNORECASE)
        if m and m.group(1).strip():
            return m.group(1).strip()
    return ""


def source_backed_slugs():
    """Slugs that have a backing source file in posts/.

    A published page is only a real article if some posts/*.org produces its
    slug (via #+URI, falling back to the filename stem). Stale orphan pages
    left over from old builds have no source and are excluded.
    """
    valid = set()
    posts_dir = blog_dir / "posts"
    if not posts_dir.is_dir():
        return valid
    for org in posts_dir.glob("*.org"):
        try:
            text = org.read_text(encoding="utf-8", errors="replace")
        except Exception:
            continue
        m_date = re.search(r'^#\+DATE:\s+\[?(\d{4})-(\d{2})-(\d{2})', text, re.MULTILINE)
        y, mo, d = m_date.groups() if m_date else ("", "", "")
        m_uri = re.search(r'^#\+URI:\s+(.+)', text, re.MULTILINE)
        if m_uri:
            uri = m_uri.group(1).strip().replace("%y", y).replace("%m", mo).replace("%d", d)
            slug = uri.rstrip("/").split("/")[-1]
        else:
            slug = org.stem
        valid.add(slug)
    return valid


valid_slugs = source_backed_slugs()

posts = []
for idx_file in sorted(public_dir.glob("blog/*/*/*/*/index.html")):
    parts = idx_file.parts
    slug = idx_file.parent.name
    if slug in ("archive", "all-posts"):
        continue
    if valid_slugs and slug not in valid_slugs:
        continue
    year, month, day = parts[-5], parts[-4], parts[-3]
    try:
        content = idx_file.read_text(encoding="utf-8", errors="replace")
    except Exception:
        continue

    m = re.search(r"<title>(.*?)</title>", content)
    raw_title = m.group(1).strip() if m else slug
    raw_clean = (raw_title.split(" - Self")[0].strip()
                 if " - Self" in raw_title else raw_title)
    title = html.escape(html.unescape(raw_clean))

    desc = html.escape(html.unescape(extract_description(content)))

    rel = "/" + "/".join(parts[parts.index("blog"):])
    url = rel.replace("/index.html", "/").rstrip("/") + "/"
    img = find_hero(year, month, day, slug)

    posts.append((int(year), int(month), int(day), title, url, desc, img))

posts.sort(key=lambda p: (p[0], p[1], p[2]), reverse=True)

cards = []
for year, month, day, title, url, desc, img in posts:
    date_str = "%s %02d, %d" % (MONTHS[month], day, year)
    desc_html = "      <p>%s</p>\n" % desc if desc else ""
    cards.append(
        '  <article class="medium-post-card">\n'
        '    <a class="medium-post-main" href="%s">\n'
        '      <h2>%s</h2>\n'
        '%s'
        '      <div class="medium-post-meta">%s</div>\n'
        '    </a>\n'
        '    <a class="medium-post-thumb" href="%s">\n'
        '      <img src="%s" alt="%s">\n'
        '    </a>\n'
        '  </article>'
        % (url, title, desc_html, date_str, url, img, title)
    )

body_html = '<section class="medium-home">\n' + "\n".join(cards) + "\n</section>"

if not index_path.exists():
    sys.exit(0)

page = index_path.read_text(encoding="utf-8", errors="replace")
new_page, count = re.subn(
    r'<section class="medium-home">.*?</section>',
    lambda _: body_html,
    page,
    count=1,
    flags=re.DOTALL,
)

if count:
    index_path.write_text(new_page, encoding="utf-8")
    print("Homepage regenerated with %d posts." % len(posts))
else:
    print("Homepage: no medium-home section found; left unchanged.")
