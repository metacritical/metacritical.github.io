#!/usr/bin/env python3
"""Inject clickable tag links into article and draft pages.

Reads #+TAGS from org source files and injects styled tag pills
into the generated HTML, positioned after the author row, before content.
"""
import re
import html
import sys
from pathlib import Path

repo_dir = Path(sys.argv[1])
public_dir = repo_dir / "public"
posts_dir = repo_dir / "posts"
drafts_dir = repo_dir / "drafts"

TAG_CSS = """<style>
.article-tags{display:flex;flex-wrap:wrap;gap:6px;margin:0 0 20px;padding:0;list-style:none}
.article-tag{display:inline-block;font-size:12px;font-weight:500;color:#6f6a5e;background:#f1ece0;padding:3px 11px;border-radius:12px;text-decoration:none;transition:background .12s,color .12s}
.article-tag:hover{background:#e4dfd0;color:#1f1f1b}
</style>"""


def get_tags_from_org(org_path):
    """Extract tags from #+TAGS header line."""
    if not org_path.exists():
        return []
    content = org_path.read_text(encoding="utf-8", errors="replace")
    m = re.search(r'^#\+TAGS:\s+(.*)', content, re.MULTILINE)
    if not m:
        return []
    raw = m.group(1).strip()
    return [t.strip() for t in raw.split(",") if t.strip()]


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


def inject_tags(html_path, tags_html, is_draft=False):
    """Inject tag HTML into an article page."""
    if not tags_html:
        return False

    content = html_path.read_text(encoding="utf-8", errors="replace")

    # Remove any existing tag block first so we can re-inject on rebuild.
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

    tag_block = TAG_CSS + "\n  " + tags_html

    pattern = r'(</div>\s*<p>)'
    replacement = f'</div>\n  {tag_block}\n  <p>'

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


def main():
    total = 0

    # Process published articles.
    for org_file in sorted(posts_dir.glob("*.org")):
        slug = org_file.stem
        tags = get_tags_from_org(org_file)
        if not tags:
            continue

        tags_html = build_tag_html(tags)

        for html_file in public_dir.rglob("index.html"):
            path_str = str(html_file)
            if f"/{slug}/" in path_str and "blog/" in path_str:
                if inject_tags(html_file, tags_html):
                    print(f"  [tags] Injected into: {html_file.relative_to(public_dir)}")
                    total += 1
                break

    # Process draft preview pages.
    if (public_dir / "drafts").exists():
        for org_file in sorted(drafts_dir.glob("*.org")):
            slug = org_file.stem
            tags = get_tags_from_org(org_file)
            if not tags:
                continue

            tags_html = build_tag_html(tags)
            html_path = public_dir / "drafts" / slug / "index.html"

            if html_path.exists():
                if inject_tags(html_path, tags_html, is_draft=True):
                    print(f"  [tags] Injected into: drafts/{slug}/")
                    total += 1

    print(f"[tags] Done. Injected tags into {total} page(s).")


if __name__ == "__main__":
    main()
