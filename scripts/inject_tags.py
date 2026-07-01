#!/usr/bin/env python3
"""Inject tag pills and series badge into article and draft pages.

Reads #+TAGS from org source files and series JSON definitions, then
injects styled tag pills below the author row and a series badge
into the author row, next to the author name.
"""
import json
import re
import html
import sys
from pathlib import Path

repo_dir = Path(sys.argv[1])
public_dir = repo_dir / "public"
posts_dir = repo_dir / "posts"
drafts_dir = repo_dir / "drafts"
series_dir = repo_dir / "series"

TAG_FONT = '"Helvetica Neue", Helvetica, Arial, sans-serif'

TAG_CSS = """<style>
.article-tags{display:flex;flex-wrap:wrap;gap:6px;margin:0 0 20px;padding:0;list-style:none}
.article-tags li{list-style:none;margin:0;padding:0}
.article-tag{display:inline-block;font-size:12px;font-weight:500;font-family:""" + TAG_FONT + """;color:#6f6a5e;background:#f1ece0;padding:3px 11px;border-radius:12px;text-decoration:none;transition:background .12s,color .12s}
.article-tag:hover{background:#e4dfd0;color:#1f1f1b}
.series-badge{display:inline-flex;align-items:center;font-size:11px;font-weight:500;font-family:""" + TAG_FONT + """;color:#2baa9f;background:#e0f5f4;border:none;padding:2px 9px;border-radius:11px;white-space:nowrap}
</style>"""


def read_org_content(org_path):
    if not org_path.exists():
        return ""
    return org_path.read_text(encoding="utf-8", errors="replace")


def get_tags_from_org(org_path):
    """Extract tags from #+TAGS header line."""
    content = read_org_content(org_path)
    if not content:
        return []
    m = re.search(r'^#\+TAGS:\s+(.*)', content, re.MULTILINE)
    if not m:
        return []
    raw = m.group(1).strip()
    return [t.strip() for t in raw.split(",") if t.strip()]


def get_uri_from_org(org_path):
    """Extract the resolved URL path from #+URI: header."""
    content = read_org_content(org_path)
    if not content:
        return None
    m = re.search(r'^#\+URI:\s+(.*)', content, re.MULTILINE)
    if not m:
        return None
    uri = m.group(1).strip()
    date_m = re.search(r'^#\+DATE:\s*\[?(\d{4})-(\d{2})-(\d{2})', content, re.MULTILINE)
    if date_m:
        uri = uri.replace('%y', date_m.group(1)).replace('%m', date_m.group(2)).replace('%d', date_m.group(3))
    return uri.rstrip("/")


def slugify_tag(tag):
    """Convert tag name to URL slug."""
    slug = tag.lower().strip()
    slug = re.sub(r"[^a-z0-9]+", "-", slug)
    return slug.strip("-")


def build_tag_html(tags):
    """Build HTML for tag pills."""
    if not tags:
        return ""
    items = []
    for tag in tags:
        slug = slugify_tag(tag)
        url = f"/tags/{slug}/"
        items.append(
            f'<li><a class="article-tag" href="{url}">{html.escape(tag)}</a></li>'
        )
    return f'<ul class="article-tags">{"".join(items)}</ul>'


def get_series_info_for_url(article_url):
    """Find series info for a given article URL."""
    if not series_dir.exists():
        return None
    for sf in sorted(series_dir.glob("*.json")):
        try:
            series = json.loads(sf.read_text(encoding="utf-8"))
        except (json.JSONDecodeError, OSError):
            continue
        articles = series.get("articles", [])
        for art in articles:
            if art["url"].rstrip("/") == article_url.rstrip("/"):
                return {
                    "name": series["name"],
                    "slug": sf.stem,
                }
    return None


def build_series_badge_html(info):
    """Build series badge HTML — just the series name in a teal pill."""
    if not info:
        return ""
    slug = info["slug"]
    name = html.escape(info["name"])
    return (
        f'<span class="series-badge">'
        f'<a href="/series/{slug}/" style="color:inherit;text-decoration:none">{name}</a>'
        f'</span>'
    )


def inject_tags(html_path, tags_html, css_block, is_draft=False):
    """Inject tag pills after the author row, before content."""
    if not tags_html:
        return False

    content = html_path.read_text(encoding="utf-8", errors="replace")

    # Remove any existing tag block first (for re-injection on rebuild).
    content = re.sub(
        r'\s*<style>\.article-tags\{.*?</style>\s*',
        '',
        content,
        flags=re.DOTALL,
    )
    content = re.sub(
        r'\s*<ul class="article-tags">.*?</ul>',
        '',
        content,
        flags=re.DOTALL,
    )

    tag_block = css_block + "\n  " + tags_html

    pattern = r'(</div>\s*<p>)'
    replacement = rf'</div>\n  {tag_block}\n  <p>'

    if is_draft:
        pattern = r'(hero_img.*?>\s*<p>|</div>\s*<p>)'

    new_content, count = re.subn(pattern, replacement, content, count=1)

    if count == 0:
        pattern2 = r'(<div class="post-author-row">.*?</div>\s*</div>\s*)'
        new_content, count = re.subn(
            pattern2,
            lambda m: m.group(1) + "\n  " + tag_block + "\n  ",
            content,
            count=1,
            flags=re.DOTALL,
        )

    if count > 0:
        html_path.write_text(new_content, encoding="utf-8")
        return True
    return False


def inject_series_badge(html_path, badge_html):
    """Inject series badge into the author row, on the same line as name/date."""
    if not badge_html:
        return False

    content = html_path.read_text(encoding="utf-8", errors="replace")

    # Remove any existing series badge first.
    content = re.sub(
        r'\s*<span class="series-badge">.*?</span>',
        '',
        content,
        flags=re.DOTALL,
    )

    # Insert inside .post-author-meta, after the date span.
    pattern = r'(<span class="post-author-date">[^<]*</span>)\s*</div>\s*</div>'
    replacement = rf'\1\n    {badge_html}\n  </div>\n</div>'

    new_content, count = re.subn(pattern, replacement, content, count=1)

    if count > 0:
        html_path.write_text(new_content, encoding="utf-8")
        return True
    return False


def main():
    total = 0

    # Process published articles.
    for org_file in sorted(posts_dir.glob("*.org")):
        slug = org_file.stem
        tags = get_tags_from_org(org_file)

        uri_path = get_uri_from_org(org_file)
        article_url = uri_path + "/" if uri_path else None

        tags_html = build_tag_html(tags) if tags else ""
        series_info = get_series_info_for_url(article_url) if article_url else None
        series_badge_html = build_series_badge_html(series_info)

        # Match by #+URI: first, fall back to filename stem.
        html_file = None
        for candidate in public_dir.rglob("index.html"):
            path_str = str(candidate)
            match = False
            if uri_path and uri_path in path_str:
                match = True
            elif f"/{slug}/" in path_str:
                match = True
            if match and "blog/" in path_str:
                html_file = candidate
                break

        if html_file is None:
            continue

        if series_badge_html:
            inject_series_badge(html_file, series_badge_html)

        if tags_html:
            if inject_tags(html_file, tags_html, TAG_CSS):
                print(f"  [tags] Injected into: {html_file.relative_to(public_dir)}")
                total += 1
        elif series_badge_html:
            # Inject CSS separately when only badge was added (no tags).
            content = html_file.read_text(encoding="utf-8", errors="replace")
            if ".series-badge{" not in content:
                content = content.replace(
                    "</head>",
                    f"  {TAG_CSS}\n</head>",
                    1,
                )
                html_file.write_text(content, encoding="utf-8")

    # Process draft preview pages.
    if (public_dir / "drafts").exists():
        for org_file in sorted(drafts_dir.glob("*.org")):
            slug = org_file.stem
            tags = get_tags_from_org(org_file)

            uri_path = get_uri_from_org(org_file)
            article_url = uri_path + "/" if uri_path else None

            tags_html = build_tag_html(tags) if tags else ""
            series_info = get_series_info_for_url(article_url) if article_url else None
            series_badge_html = build_series_badge_html(series_info)

            html_path = public_dir / "drafts" / slug / "index.html"
            if not html_path.exists():
                continue

            if series_badge_html:
                inject_series_badge(html_path, series_badge_html)

            if tags_html:
                if inject_tags(html_path, tags_html, TAG_CSS, is_draft=True):
                    print(f"  [tags] Injected into: drafts/{slug}/")
                    total += 1

    print(f"[tags] Done. Injected tags into {total} page(s).")


if __name__ == "__main__":
    main()
