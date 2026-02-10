#!/usr/bin/env python3
from __future__ import annotations

import html
import re
import sys
from pathlib import Path


def slugify(value: str) -> str:
    s = value.strip().lower()
    s = re.sub(r"[^a-z0-9]+", "-", s)
    s = re.sub(r"-{2,}", "-", s).strip("-")
    return s or "draft"


def parse_org(path: Path) -> tuple[str, str]:
    raw = path.read_text(encoding="utf-8", errors="replace")
    title = path.stem
    body_lines: list[str] = []
    for line in raw.splitlines():
        if line.startswith("#+TITLE:"):
            title = line.split(":", 1)[1].strip() or title
            continue
        if line.startswith("#+"):
            continue
        body_lines.append(line)
    body = "\n".join(body_lines).strip()
    return title, body


def page_html(title: str, body: str) -> str:
    safe_title = html.escape(title)
    safe_body = html.escape(body)
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width,initial-scale=1">
  <title>{safe_title}</title>
  <style>
    body {{ font-family: Georgia, serif; margin: 0; background: #f7f6f2; color: #1c1b19; }}
    main {{ max-width: 880px; margin: 0 auto; padding: 36px 24px 64px; }}
    h1 {{ font-size: clamp(34px, 6vw, 56px); line-height: 1.1; margin: 0 0 22px; }}
    pre {{ white-space: pre-wrap; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: 16px; line-height: 1.7; background: #fff; border: 1px solid #e3ddd1; padding: 18px; border-radius: 12px; }}
    a {{ color: #0d6a57; text-decoration: none; }}
  </style>
</head>
<body>
<main>
  <p><a href="/drafts/">Back to drafts</a></p>
  <h1>{safe_title}</h1>
  <pre>{safe_body}</pre>
</main>
</body>
</html>
"""


def index_html(items: list[tuple[str, str]]) -> str:
    links = "\n".join(
        f'<li><a href="/drafts/{slug}/">{html.escape(title)}</a></li>'
        for slug, title in items
    )
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width,initial-scale=1">
  <title>Draft Preview</title>
  <style>
    body {{ font-family: Georgia, serif; margin: 0; background: #f7f6f2; color: #1c1b19; }}
    main {{ max-width: 880px; margin: 0 auto; padding: 36px 24px 64px; }}
    h1 {{ font-size: clamp(34px, 6vw, 56px); line-height: 1.1; margin: 0 0 20px; }}
    li {{ margin: 10px 0; }}
    a {{ color: #0d6a57; text-decoration: none; font-size: 20px; }}
  </style>
</head>
<body>
<main>
  <h1>Draft Preview</h1>
  <p>Visible only when built with <code>DRAFTS=1</code>.</p>
  <ul>{links}</ul>
</main>
</body>
</html>
"""


def main() -> int:
    if len(sys.argv) != 2:
        print("Usage: generate_draft_preview.py <BLOG_DIR>")
        return 2

    blog_dir = Path(sys.argv[1]).resolve()
    drafts_dir = blog_dir / "drafts"
    public_drafts_dir = blog_dir / "public" / "drafts"
    if public_drafts_dir.exists():
        for child in public_drafts_dir.glob("*"):
            if child.is_dir():
                for p in child.rglob("*"):
                    if p.is_file():
                        p.unlink()
                for p in sorted(child.rglob("*"), reverse=True):
                    if p.is_dir():
                        p.rmdir()
                child.rmdir()
            elif child.is_file():
                child.unlink()
    public_drafts_dir.mkdir(parents=True, exist_ok=True)

    items: list[tuple[str, str]] = []
    if drafts_dir.exists():
        for draft in sorted(drafts_dir.glob("*.org")):
            title, body = parse_org(draft)
            slug = slugify(draft.stem)
            out_dir = public_drafts_dir / slug
            out_dir.mkdir(parents=True, exist_ok=True)
            (out_dir / "index.html").write_text(page_html(title, body), encoding="utf-8")
            items.append((slug, title))

    (public_drafts_dir / "index.html").write_text(index_html(items), encoding="utf-8")
    print(f"Generated draft preview pages: {len(items)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
