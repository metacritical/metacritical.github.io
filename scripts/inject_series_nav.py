#!/usr/bin/env python3
"""Inject series navigation blocks into published article HTML.

Reads a series definition JSON file and injects a navigation block
(prev/next + full series listing) into each article's generated HTML
that exists in the public/ directory.

The nav is injected INSIDE the .post div so it inherits the article's
centered layout. CSS is embedded inline to survive AOG's theme overwrite.
"""
import html
import json
import re
import sys
from pathlib import Path


SERIES_CSS = """
.series-nav-wrap{margin:36px 0 28px;font-family:"ui-sans-serif",-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif}
.series-nav-wrap *{box-sizing:border-box}
.series-nav-card{border:1px solid #e2ddd0;border-radius:12px;overflow:hidden;background:#faf8f2}
.series-nav-top{padding:16px 20px 14px;border-bottom:1px solid #ebe6d9}
.sn-eyebrow{display:inline-block;font-size:10px;font-weight:700;letter-spacing:.09em;text-transform:uppercase;color:#36c9c7;margin-bottom:4px}
.sn-name{font-family:"Charter","Iowan Old Style","Palatino Linotype",Palatino,serif;font-size:18px;font-weight:600;color:#1f1f1b;margin:0;line-height:1.3}
.sn-desc{font-size:12.5px;line-height:1.55;color:#6f6a5e;margin:3px 0 0}
.sn-pn{display:flex;border-bottom:1px solid #ebe6d9}
.sn-card{flex:1;padding:11px 16px;text-decoration:none;display:flex;flex-direction:column;gap:1px;min-width:0;transition:background .12s}
a.sn-card:hover{background:rgba(54,201,199,.06)}
.sn-card-l{border-right:1px solid #ebe6d9;text-align:left}
.sn-card-r{text-align:right}
.sn-card-lbl{font-size:10px;font-weight:600;letter-spacing:.03em;color:#a8a294}
.sn-card-tt{font-size:12px;font-weight:500;color:#36c9c7;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
.sn-card.off .sn-card-tt{color:#bfb9ac}
a.sn-card.off:hover{background:transparent;cursor:default}
.sn-pos{display:flex;align-items:center;font-size:11px;font-weight:600;color:#6f6a5e;padding:0 14px;white-space:nowrap}
.sn-list{list-style:none;margin:0;padding:4px 0}
.sn-row{display:flex;align-items:center;gap:10px;padding:5px 20px;font-size:13px;line-height:1.4;transition:background .1s}
.sn-row:hover{background:#f4f1e8}
.sn-dot{flex:0 0 22px;height:22px;border-radius:50%;display:inline-flex;align-items:center;justify-content:center;background:#e8e3d4;color:#8a8578;font-size:10px;font-weight:700}
.sn-row.here .sn-dot{background:#36c9c7;color:#fff}
.sn-tt{flex:1;min-width:0;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
.sn-tt a{color:#33312b;text-decoration:none;font-weight:500}
.sn-tt a:hover{color:#36c9c7}
.sn-row.here .sn-tt{color:#1f1f1b;font-weight:700}
.sn-tt.off{color:#bfb9ac}
.sn-tag{font-size:9px;font-weight:600;padding:1px 6px;border-radius:3px;letter-spacing:.04em;flex:0 0 auto}
.sn-tag.here{background:#36c9c7;color:#fff}
.sn-tag.draft{background:#e8e3d4;color:#a8a294}
.sn-date{font-size:10.5px;color:#c4beb1;font-variant-numeric:tabular-nums;flex:0 0 auto;margin-left:auto;padding-left:6px}
.sn-foot{padding:7px 20px;font-size:10px;color:#c4beb1;text-align:center;border-top:1px solid #ebe6d9;background:#f6f3eb}
@media(max-width:540px){
 .series-nav-wrap{margin:24px 0}
 .series-nav-top{padding:12px 14px 10px}
 .sn-name{font-size:16px}
 .sn-card{padding:8px 10px}
 .sn-card-tt{font-size:11px}
 .sn-row{padding:4px 14px;gap:8px}
 .sn-date{display:none}
}
"""


def load_series(series_file):
    with open(series_file, encoding="utf-8") as f:
        return json.load(f)


def build_nav_html(series, current_url, public_dir):
    """Build the complete series navigation HTML (CSS + markup)."""
    articles = series["articles"]
    current_idx = None

    for i, art in enumerate(articles):
        if art["url"].rstrip("/") == current_url.rstrip("/"):
            current_idx = i
            break

    if current_idx is None:
        return None

    prev_art = articles[current_idx - 1] if current_idx > 0 else None
    next_art = articles[current_idx + 1] if current_idx < len(articles) - 1 else None
    total = len(articles)
    published_count = sum(
        1 for a in articles
        if (public_dir / a["url"].lstrip("/") / "index.html").exists()
    )

    p = []
    p.append('<div class="series-nav-wrap">')
    p.append(f'<style>{SERIES_CSS}</style>')
    p.append('<div class="series-nav-card">')

    # Header
    p.append('<div class="series-nav-top">')
    p.append('<span class="sn-eyebrow">Article Series</span>')
    p.append(f'<h3 class="sn-name">{html.escape(series["name"])}</h3>')
    if series.get("description"):
        p.append(f'<p class="sn-desc">{html.escape(series["description"])}</p>')
    p.append('</div>')

    # Prev / Next
    p.append('<div class="sn-pn">')

    if prev_art:
        exists = (public_dir / prev_art["url"].lstrip("/") / "index.html").exists()
        off = "" if exists else " off"
        href = f' href="{prev_art["url"]}"' if exists else ""
        p.append(f'<a class="sn-card sn-card-l{off}"{href}>')
        p.append(f'<span class="sn-card-lbl">&larr; Previous</span>')
        p.append(f'<span class="sn-card-tt">{html.escape(prev_art["title"])}</span>')
        p.append('</a>')
    else:
        p.append('<span class="sn-card sn-card-l off">')
        p.append('<span class="sn-card-lbl">&larr;</span>')
        p.append('<span class="sn-card-tt">Start of series</span>')
        p.append('</span>')

    p.append(f'<span class="sn-pos">{current_idx + 1} / {total}</span>')

    if next_art:
        exists = (public_dir / next_art["url"].lstrip("/") / "index.html").exists()
        off = "" if exists else " off"
        href = f' href="{next_art["url"]}"' if exists else ""
        p.append(f'<a class="sn-card sn-card-r{off}"{href}>')
        p.append(f'<span class="sn-card-lbl">Next &rarr;</span>')
        p.append(f'<span class="sn-card-tt">{html.escape(next_art["title"])}</span>')
        p.append('</a>')
    else:
        p.append('<span class="sn-card sn-card-r off">')
        p.append('<span class="sn-card-lbl">&rarr;</span>')
        p.append('<span class="sn-card-tt">End of series</span>')
        p.append('</span>')

    p.append('</div>')

    # Article list
    p.append('<ol class="sn-list">')
    for i, art in enumerate(articles):
        art_path = public_dir / art["url"].lstrip("/") / "index.html"
        is_here = i == current_idx
        cls = "sn-row" + (" here" if is_here else "")

        p.append(f'<li class="{cls}">')
        p.append(f'<span class="sn-dot">{i+1}</span>')

        tt = html.escape(art["title"])
        if is_here:
            p.append(f'<span class="sn-tt">{tt}</span>')
            p.append('<span class="sn-tag here">Here</span>')
        elif art_path.exists():
            p.append(f'<span class="sn-tt"><a href="{art["url"]}">{tt}</a></span>')
        else:
            p.append(f'<span class="sn-tt off">{tt}</span>')
            p.append('<span class="sn-tag draft">Draft</span>')

        p.append(f'<span class="sn-date">{art["date"]}</span>')
        p.append('</li>')
    p.append('</ol>')

    # Footer
    p.append(f'<div class="sn-foot">{published_count} of {total} articles published</div>')

    p.append('</div>')  # card
    p.append('</div>')  # wrap
    return "\n".join(p)


def inject_into_file(html_path, nav_html):
    """Inject nav_html INSIDE the .post div, before it closes."""
    content = html_path.read_text(encoding="utf-8", errors="replace")

    # Remove any previously injected series nav.
    content = re.sub(
        r'<div class="series-nav-wrap">.*?</div>\s*</div>\s*<!-- sn-end -->?',
        "",
        content,
        flags=re.DOTALL,
    )
    # Old format cleanup.
    content = re.sub(r'<div class="series-nav-wrap">.*?</div>\s*(?=</div>|<div>\s*<section)',
                     "", content, flags=re.DOTALL)
    content = re.sub(r'<nav class="series-nav">.*?</nav>\s*', "", content, flags=re.DOTALL)

    # Inject INSIDE .post, before its closing </div>.
    # Pattern: [ws]</div>[ws]</section>[ws]<div>[ws]<section...post-meta-wrap
    pattern = r'(\s*</div>)(\s*</section>\s*<div>\s*<section[^>]*class="post-meta-wrap")'
    replacement = rf'\n{nav_html}\1\2'

    new_content, count = re.subn(pattern, replacement, content, count=1)

    if count == 0:
        # Fallback: try matching just before post-meta-wrap div.
        pattern2 = r'(\s*</div>\s*</section>)(\s*<div>)'
        replacement2 = rf'\n{nav_html}\1\2'
        new_content, count = re.subn(pattern2, replacement2, content, count=1)

    if count > 0:
        html_path.write_text(new_content, encoding="utf-8")
        return True
    return False


def main():
    repo_dir = Path(sys.argv[1])
    public_dir = repo_dir / "public"
    series_dir = repo_dir / "series"

    if not series_dir.is_dir():
        print("[series] No series directory found, skipping.")
        return

    series_files = sorted(series_dir.glob("*.json"))
    if not series_files:
        print("[series] No series definition files found, skipping.")
        return

    total_injected = 0
    for sf in series_files:
        series = load_series(sf)
        print(f'[series] Processing: {series["name"]}')

        for art in series["articles"]:
            art_url = art["url"].lstrip("/")
            html_path = public_dir / art_url / "index.html"

            if not html_path.exists():
                # Check for draft preview page.
                slug = art_url.rstrip("/").split("/")[-1]
                draft_path = public_dir / "drafts" / slug / "index.html"
                if draft_path.exists():
                    nav_html = build_nav_html(series, art["url"], public_dir)
                    if nav_html and inject_into_file(draft_path, nav_html):
                        total_injected += 1
                        print(f"  [series] Injected nav into draft: /drafts/{slug}/")
                    continue
                continue

            nav_html = build_nav_html(series, art["url"], public_dir)
            if nav_html is None:
                continue

            if inject_into_file(html_path, nav_html):
                total_injected += 1
                print(f"  [series] Injected nav into: {art['url']}")
            else:
                print(f"  [series] WARN: Could not find injection point in {art['url']}")

    print(f"[series] Done. Injected navigation into {total_injected} article(s).")


if __name__ == "__main__":
    main()
