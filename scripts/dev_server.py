#!/usr/bin/env python3
"""Local dev server for blog preview + editor.

Serves:
- /... from BLOG_DIR/public
- /editor from BLOG_DIR/editor/index.html
- POST /editor/api/save to persist a draft .org file in BLOG_DIR/drafts
"""

from __future__ import annotations

import base64
import datetime as dt
import html
import http.server
import json
import mimetypes
import os
import subprocess
import re
import socketserver
import sys
from pathlib import Path
from urllib.parse import unquote, urljoin, urlparse
from urllib.request import Request, urlopen


def slugify(value: str) -> str:
    lowered = value.strip().lower()
    lowered = re.sub(r"[^a-z0-9]+", "-", lowered)
    lowered = re.sub(r"-{2,}", "-", lowered).strip("-")
    return lowered or f"draft-{dt.datetime.now().strftime('%Y%m%d-%H%M%S')}"


def org_draft_text(title: str, body: str) -> str:
    now = dt.datetime.now().strftime("%Y-%m-%d")
    return (
        f"#+TITLE: {title}\n"
        f"#+DATE: {now}\n"
        "#+OPTIONS: toc:nil\n"
        "#+DESCRIPTION: Draft post\n"
        "\n"
        f"{body.strip()}\n"
    )


def safe_ext_from_mime(mime: str) -> str:
    ext = mimetypes.guess_extension(mime) or ".bin"
    if ext == ".jpe":
        return ".jpg"
    if len(ext) > 8:
        return ".bin"
    return ext


def abs_http_url(raw: str) -> str | None:
    parsed = urlparse(raw)
    if parsed.scheme not in {"http", "https"}:
        return None
    if not parsed.netloc:
        return None
    return raw


def extract_meta_value(doc: str, keys: list[str]) -> str:
    for key in keys:
        pattern = re.compile(
            rf'<meta[^>]+(?:property|name)=["\']{re.escape(key)}["\'][^>]*content=["\']([^"\']+)["\']',
            re.IGNORECASE,
        )
        match = pattern.search(doc)
        if match:
            return html.unescape(match.group(1).strip())
    return ""


def extract_title(doc: str) -> str:
    match = re.search(r"<title[^>]*>(.*?)</title>", doc, re.IGNORECASE | re.DOTALL)
    if not match:
        return ""
    return html.unescape(re.sub(r"\s+", " ", match.group(1)).strip())


class DevHandler(http.server.SimpleHTTPRequestHandler):
    server_version = "SelfDotSendDev/1.0"

    def __init__(self, *args, public_dir: Path, blog_dir: Path, **kwargs):
        self.public_dir = public_dir
        self.blog_dir = blog_dir
        self.editor_dir = blog_dir / "editor"
        super().__init__(*args, directory=str(public_dir), **kwargs)

    def _json(self, code: int, payload: dict) -> None:
        body = json.dumps(payload).encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def _read_json(self) -> dict | None:
        try:
            content_len = int(self.headers.get("Content-Length", "0"))
            payload = self.rfile.read(content_len)
            return json.loads(payload.decode("utf-8"))
        except Exception:
            return None

    def _git_add(self, path: Path) -> None:
        try:
            subprocess.run(
                ["git", "-C", str(self.blog_dir), "add", str(path)],
                check=False,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
        except Exception:
            pass

    def _save_post(self, data: dict) -> None:
        title = str(data.get("title", "")).strip()
        body = str(data.get("body", "")).strip()
        mode = str(data.get("mode", "draft")).strip().lower()
        if not title:
            self._json(400, {"ok": False, "error": "Title is required"})
            return
        if mode not in {"draft", "publish"}:
            self._json(400, {"ok": False, "error": "Mode must be draft or publish"})
            return
        target_dir = self.blog_dir / ("posts" if mode == "publish" else "drafts")
        target_dir.mkdir(parents=True, exist_ok=True)
        slug = slugify(title)
        filename = target_dir / f"{slug}.org"
        if filename.exists():
            stamp = dt.datetime.now().strftime("%H%M%S")
            filename = target_dir / f"{slug}-{stamp}.org"
        filename.write_text(org_draft_text(title, body), encoding="utf-8")
        self._git_add(filename)
        self._json(
            200,
            {
                "ok": True,
                "mode": mode,
                "path": str(filename.relative_to(self.blog_dir)),
                "message": "Draft saved.",
            },
        )

    def _upload_image(self, data: dict) -> None:
        filename = str(data.get("filename", "upload")).strip() or "upload"
        data_url = str(data.get("dataUrl", "")).strip()
        match = re.match(r"^data:(image/[a-zA-Z0-9.+-]+);base64,(.+)$", data_url)
        if not match:
            self._json(400, {"ok": False, "error": "Invalid image payload"})
            return
        mime = match.group(1).lower()
        encoded = match.group(2)
        try:
            raw = base64.b64decode(encoded, validate=True)
        except Exception:
            self._json(400, {"ok": False, "error": "Corrupt image data"})
            return
        if len(raw) > 20 * 1024 * 1024:
            self._json(400, {"ok": False, "error": "Image too large (20MB max)"})
            return
        stem = slugify(Path(filename).stem) or "image"
        ext = safe_ext_from_mime(mime)
        now = dt.datetime.now()
        rel_dir = Path("assets") / "uploads" / now.strftime("%Y") / now.strftime("%m")
        out_dir = self.blog_dir / rel_dir
        out_dir.mkdir(parents=True, exist_ok=True)
        out_file = out_dir / f"{stem}-{now.strftime('%H%M%S')}{ext}"
        out_file.write_bytes(raw)
        self._git_add(out_file)
        self._json(
            200,
            {
                "ok": True,
                "path": str(out_file.relative_to(self.blog_dir)),
                "url": "/" + str((rel_dir / out_file.name).as_posix()),
            },
        )

    def _fetch_meta(self, data: dict) -> None:
        raw_url = str(data.get("url", "")).strip()
        safe_url = abs_http_url(raw_url)
        if not safe_url:
            self._json(400, {"ok": False, "error": "URL must start with http/https"})
            return
        try:
            req = Request(
                safe_url,
                headers={
                    "User-Agent": "SelfDotSendEditor/1.0 (+https://selfdotsend.com)"
                },
            )
            with urlopen(req, timeout=8) as resp:
                payload = resp.read(1024 * 1024)
            doc = payload.decode("utf-8", errors="ignore")
        except Exception as err:
            self._json(502, {"ok": False, "error": f"Failed to fetch metadata: {err}"})
            return
        title = extract_meta_value(doc, ["og:title", "twitter:title"]) or extract_title(doc)
        description = extract_meta_value(
            doc, ["og:description", "twitter:description", "description"]
        )
        image = extract_meta_value(doc, ["og:image", "twitter:image"])
        site_name = extract_meta_value(doc, ["og:site_name"]) or (urlparse(safe_url).hostname or "")
        if image:
            image = urljoin(safe_url, image)
        self._json(
            200,
            {
                "ok": True,
                "url": safe_url,
                "title": title or safe_url,
                "description": description,
                "image": image,
                "site": site_name,
            },
        )

    def _dev_edit_injection(self) -> str:
        return """
<script>
(function () {
  if (location.pathname === "/editor" || location.pathname === "/editor/") return;
  if (document.getElementById("local-dev-edit-link")) return;
  var a = document.createElement("a");
  a.id = "local-dev-edit-link";
  a.href = "/editor";
  a.setAttribute("aria-label", "Search");
  a.title = "Search";
  a.innerHTML = `<svg width="22" height="22" viewBox="0 0 24 24" aria-hidden="true"><circle cx="11" cy="11" r="7" fill="none" stroke="#666" stroke-width="2"></circle><line x1="16.65" y1="16.65" x2="21" y2="21" stroke="#666" stroke-width="2" stroke-linecap="round"></line></svg><span style="margin-left:8px;font:500 14px/1.2 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;color:#666;white-space:nowrap;">Search '/'</span>`;
  a.style.cssText = "margin-left:12px;display:inline-flex;align-items:center;justify-content:center;text-decoration:none;padding:6px;background:transparent;";
  var host = document.querySelector("nav.main-nav .nav-right, .site-nav, .site-nav-left ul.nav, .main-nav");
  if (host) {
    host.appendChild(a);
  } else {
    a.style.position = "fixed";
    a.style.right = "16px";
    a.style.top = "16px";
    a.style.zIndex = "9999";
    document.body.appendChild(a);
  }
})();
</script>
"""

    def _try_serve_injected_html(self, path: str) -> bool:
        rel = path.lstrip("/")
        if not rel:
            candidate = self.public_dir / "index.html"
        else:
            candidate = self.public_dir / rel
            if candidate.is_dir():
                candidate = candidate / "index.html"
        if not candidate.exists() or not candidate.is_file() or candidate.suffix != ".html":
            return False
        try:
            html = candidate.read_text(encoding="utf-8")
        except Exception:
            return False
        injection = self._dev_edit_injection()
        if "</body>" in html:
            html = html.replace("</body>", injection + "\n</body>", 1)
        else:
            html += injection
        body = html.encode("utf-8")
        self.send_response(200)
        self.send_header("Content-Type", "text/html; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)
        return True

    def do_GET(self) -> None:
        parsed = urlparse(self.path)
        path = unquote(parsed.path)

        if path in {"/editor", "/editor/", "/__editor", "/__editor/"}:
            editor_html = self.editor_dir / "index.html"
            if not editor_html.exists():
                self.send_error(404, "Editor not found")
                return
            body = editor_html.read_bytes()
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
            return

        if self._try_serve_injected_html(path):
            return

        super().do_GET()

    def do_POST(self) -> None:
        parsed = urlparse(self.path)
        path = unquote(parsed.path)
        if path not in {
            "/editor/api/save",
            "/__editor/api/save",
            "/editor/api/upload",
            "/editor/api/fetch-meta",
        }:
            self.send_error(404, "Not Found")
            return

        data = self._read_json()
        if data is None:
            self._json(400, {"ok": False, "error": "Invalid JSON"})
            return

        if path in {"/editor/api/save", "/__editor/api/save"}:
            self._save_post(data)
            return

        if path == "/editor/api/upload":
            self._upload_image(data)
            return

        if path == "/editor/api/fetch-meta":
            self._fetch_meta(data)
            return


def main() -> int:
    if len(sys.argv) != 3:
        print("Usage: dev_server.py <BLOG_DIR> <PORT>")
        return 2

    blog_dir = Path(sys.argv[1]).resolve()
    public_dir = blog_dir / "public"
    port = int(sys.argv[2])

    class ReusableTCPServer(socketserver.TCPServer):
        allow_reuse_address = True

    with ReusableTCPServer(
        ("", port),
        lambda *args, **kwargs: DevHandler(
            *args, public_dir=public_dir, blog_dir=blog_dir, **kwargs
        ),
    ) as httpd:
        print(f"[web] Serving {public_dir} on http://localhost:{port}")
        print(f"[web] Editor available at http://localhost:{port}/editor")
        httpd.serve_forever()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
