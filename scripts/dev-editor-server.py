#!/usr/bin/env python3
"""Combined dev server: static files + editor API.

Serves static files from public/ and handles /editor/api/* endpoints
for the LiveDrafts editor (load, save, upload, fetch-meta).

Usage:
  python3 scripts/dev-editor-server.py [port] [blog_dir]
"""
import base64
import datetime
import html
import json
import os
import re
import sys
import urllib.request
import urllib.error
from http.server import HTTPServer, SimpleHTTPRequestHandler
from pathlib import Path
from urllib.parse import urlparse, parse_qs

BLOG_DIR = Path(sys.argv[2] if len(sys.argv) > 2 else ".").resolve()
PUBLIC_DIR = BLOG_DIR / "public"
DRAFTS_DIR = BLOG_DIR / "drafts"
POSTS_DIR = BLOG_DIR / "posts"
ASSETS_DIR = BLOG_DIR / "assets"
PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 8080


def slugify(text):
    """Convert title to URL-safe slug."""
    slug = text.lower().strip()
    slug = re.sub(r"[^a-z0-9]+", "-", slug)
    slug = re.sub(r"-{2,}", "-", slug)
    slug = slug.strip("-")
    return slug or "untitled"


def parse_org_file(content):
    """Parse an org file into headers dict + body string."""
    lines = content.split("\n")
    headers = {}
    body_start = 0

    for i, line in enumerate(lines):
        m = re.match(r"^#\+(\w+):\s*(.*)", line)
        if m:
            headers[m.group(1).upper()] = m.group(2).strip()
            body_start = i + 1
        elif line.strip() == "" and body_start > 0:
            body_start = i + 1
            break
        elif body_start == 0 and not line.startswith("#+"):
            break

    body = "\n".join(lines[body_start:]).strip()
    return headers, body


def parse_tags(headers):
    """Extract tags from #+TAGS header."""
    raw = headers.get("TAGS", "")
    return [t.strip() for t in raw.split(",") if t.strip()]


def build_org_headers(title, date_str, tags, description="Draft post", existing_headers=None):
    """Build org file header block preserving existing headers."""
    existing = existing_headers or {}
    slug = slugify(title)
    day_name = datetime.datetime.strptime(date_str, "%Y-%m-%d").strftime("%a")

    lines = [
        f"#+TITLE:       {title}",
        f"#+AUTHOR:      {existing.get('AUTHOR', 'Pankaj Doharey')}",
        f"#+EMAIL:       {existing.get('EMAIL', 'pankajdoharey@gmail.com')}",
        f"#+DATE:        [{date_str} {day_name}]",
        f"#+URI:         {existing.get('URI', '/blog/%y/%m/%d/' + slug)}",
        f"#+KEYWORDS:    {existing.get('KEYWORDS', '')}",
        f"#+TAGS:        {', '.join(tags)}",
        f"#+LANGUAGE:    {existing.get('LANGUAGE', 'en')}",
        f"#+OPTIONS:     {existing.get('OPTIONS', 'H:3 num:nil toc:nil \\\\n:nil ::t |:t ^:nil -:nil f:t *:t <:t')}",
        f"#+DESCRIPTION: {description}",
    ]
    return "\n".join(lines)


def handle_load_draft(query_params):
    """GET /editor/api/load-draft?slug=X&kind=draft"""
    slug = query_params.get("slug", [""])[0]
    kind = query_params.get("kind", ["draft"])[0]

    if not slug:
        return 400, {"ok": False, "error": "Missing slug parameter"}

    if kind == "post":
        search_dir = POSTS_DIR
        kind_label = "published post"
    else:
        search_dir = DRAFTS_DIR
        kind_label = "draft"

    org_file = search_dir / f"{slug}.org"
    if not org_file.exists():
        return 404, {"ok": False, "error": f"{kind_label.capitalize()} not found: {slug}.org"}

    content = org_file.read_text(encoding="utf-8", errors="replace")
    headers, body = parse_org_file(content)
    title = headers.get("TITLE", slug.replace("-", " ").title())
    tags = parse_tags(headers)

    target_path = f"{'drafts' if kind == 'draft' else 'posts'}/{slug}.org"

    return 200, {
        "ok": True,
        "title": title,
        "body": body,
        "tags": tags,
        "targetPath": target_path,
        "mode": "publish" if kind == "post" else "draft",
    }


def handle_save(body_data):
    """POST /editor/api/save"""
    title = body_data.get("title", "").strip()
    org_body = body_data.get("body", "")
    mode = body_data.get("mode", "draft")
    tags = body_data.get("tags", [])
    target_path = body_data.get("targetPath", "")

    if not title:
        return 400, {"ok": False, "error": "Title is required"}

    slug = slugify(title)
    date_str = datetime.date.today().isoformat()

    if not target_path or not target_path.endswith(".org"):
        if mode == "publish":
            target_path = f"posts/{slug}.org"
        else:
            target_path = f"drafts/{slug}.org"

    target_file = BLOG_DIR / target_path
    target_file.parent.mkdir(parents=True, exist_ok=True)

    existing_headers = {}
    if target_file.exists():
        old_content = target_file.read_text(encoding="utf-8", errors="replace")
        existing_headers, _ = parse_org_file(old_content)
        old_date = existing_headers.get("DATE", "")
        date_match = re.search(r"\[?(\d{4}-\d{2}-\d{2})", old_date)
        if date_match:
            date_str = date_match.group(1)

    header_block = build_org_headers(
        title, date_str, tags,
        description=existing_headers.get("DESCRIPTION", "Draft post"),
        existing_headers=existing_headers,
    )

    full_content = f"{header_block}\n\n{org_body}\n"
    target_file.write_text(full_content, encoding="utf-8")

    saved_mode = "publish" if "posts/" in target_path else "draft"

    return 200, {
        "ok": True,
        "path": target_path,
        "mode": saved_mode,
    }


def handle_upload(body_data):
    """POST /editor/api/upload"""
    filename = body_data.get("filename", "upload.png")
    data_url = body_data.get("dataUrl", "")

    if not data_url:
        return 400, {"ok": False, "error": "Missing dataUrl"}

    header_match = re.match(r"data:image/(\w+);base64,(.+)", data_url)
    if not header_match:
        return 400, {"ok": False, "error": "Invalid data URL format"}

    ext = header_match.group(1)
    b64_data = header_match.group(2)

    try:
        img_bytes = base64.b64decode(b64_data)
    except Exception:
        return 400, {"ok": False, "error": "Invalid base64 data"}

    date_str = datetime.date.today().isoformat()
    slug_dir = slugify(Path(filename).stem)
    asset_dir = ASSETS_DIR / "blog" / date_str / slug_dir
    asset_dir.mkdir(parents=True, exist_ok=True)

    safe_name = f"image-{datetime.datetime.now().strftime('%H%M%S')}.{ext}"
    asset_path = asset_dir / safe_name
    asset_path.write_bytes(img_bytes)

    url = f"/assets/blog/{date_str}/{slug_dir}/{safe_name}"

    return 200, {"ok": True, "url": url}


def handle_fetch_meta(body_data):
    """POST /editor/api/fetch-meta"""
    url = body_data.get("url", "").strip()
    if not url:
        return 400, {"ok": False, "error": "Missing url"}

    if not url.startswith("http"):
        url = "https://" + url

    try:
        req = urllib.request.Request(url, headers={
            "User-Agent": "Mozilla/5.0 (compatible; SelfDotSendEditor/1.0)"
        })
        with urllib.request.urlopen(req, timeout=10) as resp:
            content_type = resp.headers.get("Content-Type", "")
            if "text" not in content_type and "html" not in content_type:
                return 200, {"ok": True, "title": url, "description": "", "image": "", "site": urlparse(url).hostname}
            raw = resp.read(65536).decode("utf-8", errors="replace")
    except urllib.error.HTTPError as e:
        return e.code, {"ok": False, "error": f"HTTP {e.code}: {e.reason}"}
    except Exception as e:
        return 500, {"ok": False, "error": str(e)}

    def extract_meta(pattern, html_text, group=1):
        m = re.search(pattern, html_text, re.IGNORECASE | re.DOTALL)
        return m.group(group).strip() if m else ""

    og_title = extract_meta(r'<meta[^>]+property=["\']og:title["\'][^>]+content=["\'](.*?)["\']', raw)
    if not og_title:
        og_title = extract_meta(r'<title[^>]*>(.*?)</title>', raw)
    og_desc = extract_meta(r'<meta[^>]+property=["\']og:description["\'][^>]+content=["\'](.*?)["\']', raw)
    if not og_desc:
        og_desc = extract_meta(r'<meta[^>]+name=["\']description["\'][^>]+content=["\'](.*?)["\']', raw)
    og_image = extract_meta(r'<meta[^>]+property=["\']og:image["\'][^>]+content=["\'](.*?)["\']', raw)
    og_site = extract_meta(r'<meta[^>]+property=["\']og:site_name["\'][^>]+content=["\'](.*?)["\']', raw)
    if not og_site:
        og_site = urlparse(url).hostname or ""

    return 200, {
        "ok": True,
        "title": html.unescape(og_title)[:300],
        "description": html.unescape(og_desc)[:500],
        "image": og_image,
        "site": html.unescape(og_site)[:100],
        "url": url,
    }


def handle_get_all_tags():
    """GET /editor/api/tags — collect all tags from posts/ and drafts/ org files."""
    all_tags = set()
    for search_dir in [POSTS_DIR, DRAFTS_DIR]:
        if not search_dir.is_dir():
            continue
        for org_file in search_dir.glob("*.org"):
            content = org_file.read_text(encoding="utf-8", errors="replace")
            headers, _ = parse_org_file(content)
            for tag in parse_tags(headers):
                all_tags.add(tag.strip().lower())
    # Also scan public/tags/ directories for tags that exist on the site.
    tags_public = PUBLIC_DIR / "tags"
    if tags_public.is_dir():
        for d in tags_public.iterdir():
            if d.is_dir() and d.name != "index.html":
                all_tags.add(d.name.replace("-", " "))
    return 200, {"ok": True, "tags": sorted(all_tags)}


SERIES_DIR = BLOG_DIR / "series"


def handle_list_series():
    """GET /editor/api/series — list all series."""
    if not SERIES_DIR.is_dir():
        return 200, {"ok": True, "series": []}
    result = []
    for f in sorted(SERIES_DIR.glob("*.json")):
        try:
            data = json.loads(f.read_text(encoding="utf-8"))
            result.append({
                "slug": f.stem,
                "name": data.get("name", f.stem),
                "description": data.get("description", ""),
                "count": len(data.get("articles", [])),
            })
        except Exception:
            pass
    return 200, {"ok": True, "series": result}


def handle_get_series(slug):
    """GET /editor/api/series/<slug> — get series detail."""
    f = SERIES_DIR / f"{slug}.json"
    if not f.exists():
        return 404, {"ok": False, "error": "Series not found"}
    data = json.loads(f.read_text(encoding="utf-8"))
    return 200, {"ok": True, "series": data}


def handle_save_series(body_data):
    """POST /editor/api/series — create or update a series."""
    slug = body_data.get("slug", "").strip()
    name = body_data.get("name", "").strip()
    description = body_data.get("description", "")
    articles = body_data.get("articles", [])
    if not slug or not name:
        return 400, {"ok": False, "error": "slug and name required"}
    SERIES_DIR.mkdir(parents=True, exist_ok=True)
    f = SERIES_DIR / f"{slug}.json"
    data = {"name": name, "description": description, "articles": articles}
    f.write_text(json.dumps(data, indent=2), encoding="utf-8")
    return 200, {"ok": True, "slug": slug}


def handle_series_add_article(series_slug, body_data):
    """POST /editor/api/series/<slug>/add — add article to series."""
    f = SERIES_DIR / f"{series_slug}.json"
    if not f.exists():
        return 404, {"ok": False, "error": "Series not found"}
    data = json.loads(f.read_text(encoding="utf-8"))
    article = body_data.get("article", {})
    title = article.get("title", "")
    url = article.get("url", "")
    date = article.get("date", "")
    status = article.get("status", "draft")
    # Check if already exists.
    for a in data.get("articles", []):
        if url and url in a.get("url", ""):
            return 200, {"ok": True, "message": "Already in series"}
    data.setdefault("articles", []).append({
        "title": title, "url": url, "date": date, "status": status
    })
    f.write_text(json.dumps(data, indent=2), encoding="utf-8")
    return 200, {"ok": True, "position": len(data["articles"])}


def handle_series_remove_article(series_slug, body_data):
    """POST /editor/api/series/<slug>/remove — remove article from series."""
    f = SERIES_DIR / f"{series_slug}.json"
    if not f.exists():
        return 404, {"ok": False, "error": "Series not found"}
    data = json.loads(f.read_text(encoding="utf-8"))
    article_url = body_data.get("url", "")
    article_slug = body_data.get("slug", "")
    before = len(data.get("articles", []))
    data["articles"] = [
        a for a in data.get("articles", [])
        if article_url not in a.get("url", "") and article_slug not in a.get("url", "")
    ]
    f.write_text(json.dumps(data, indent=2), encoding="utf-8")
    return 200, {"ok": True, "removed": before - len(data["articles"])}


class DevEditorHandler(SimpleHTTPRequestHandler):
    """Serves static files from public/ and handles /editor/api/* routes."""

    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=str(PUBLIC_DIR), **kwargs)

    def _send_json(self, code, data):
        body = json.dumps(data).encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")
        self.end_headers()
        self.wfile.write(body)

    def _read_json_body(self):
        length = int(self.headers.get("Content-Length", 0))
        if length == 0:
            return {}
        raw = self.rfile.read(length)
        return json.loads(raw)

    def do_OPTIONS(self):
        self._send_json(200, {"ok": True})

    def do_GET(self):
        parsed = urlparse(self.path)
        path = parsed.path

        if path.startswith("/editor/api/"):
            query = parse_qs(parsed.query)
            if path == "/editor/api/load-draft":
                code, data = handle_load_draft(query)
                self._send_json(code, data)
                return
            if path == "/editor/api/tags":
                code, data = handle_get_all_tags()
                self._send_json(code, data)
                return
            if path == "/editor/api/series":
                code, data = handle_list_series()
                self._send_json(code, data)
                return
            # /editor/api/series/<slug>
            parts = path.strip("/").split("/")
            if len(parts) == 4 and parts[0] == "editor" and parts[1] == "api" and parts[2] == "series":
                code, data = handle_get_series(parts[3])
                self._send_json(code, data)
                return
            self._send_json(404, {"ok": False, "error": "Unknown API endpoint"})
            return

        super().do_GET()

    def do_POST(self):
        parsed = urlparse(self.path)
        path = parsed.path

        if path.startswith("/editor/api/"):
            try:
                body_data = self._read_json_body()
            except (json.JSONDecodeError, Exception) as e:
                self._send_json(400, {"ok": False, "error": f"Invalid JSON: {e}"})
                return

            if path == "/editor/api/save":
                code, data = handle_save(body_data)
            elif path == "/editor/api/upload":
                code, data = handle_upload(body_data)
            elif path == "/editor/api/fetch-meta":
                code, data = handle_fetch_meta(body_data)
            elif path == "/editor/api/series":
                code, data = handle_save_series(body_data)
            elif path.startswith("/editor/api/series/") and path.endswith("/add"):
                series_slug = path.split("/")[4]
                code, data = handle_series_add_article(series_slug, body_data)
            elif path.startswith("/editor/api/series/") and path.endswith("/remove"):
                series_slug = path.split("/")[4]
                code, data = handle_series_remove_article(series_slug, body_data)
            else:
                code, data = 404, {"ok": False, "error": "Unknown API endpoint"}

            self._send_json(code, data)
            return

        self._send_json(404, {"ok": False, "error": "Not an API endpoint"})

    def log_message(self, format, *args):
        msg = format % args
        if "/editor/api/" in msg:
            sys.stderr.write(f"[api] {msg}\n")
        elif "404" in msg:
            pass
        else:
            sys.stderr.write(f"[web] {msg}\n")


def main():
    os.chdir(str(PUBLIC_DIR))
    server = HTTPServer(("0.0.0.0", PORT), DevEditorHandler)
    print(f"[dev-editor-server] Serving {PUBLIC_DIR}")
    print(f"[dev-editor-server] Editor API: http://localhost:{PORT}/editor/api/")
    print(f"[dev-editor-server] Editor page: http://localhost:{PORT}/editor/")
    print(f"[dev-editor-server] Press Ctrl+C to stop.")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\n[dev-editor-server] Shutting down.")
        server.server_close()


if __name__ == "__main__":
    main()
