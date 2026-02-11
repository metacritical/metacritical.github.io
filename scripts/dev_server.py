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
from urllib.parse import parse_qs, unquote, urljoin, urlparse
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
        target_path = str(data.get("targetPath", "")).strip()
        if not title:
            self._json(400, {"ok": False, "error": "Title is required"})
            return
        if mode not in {"draft", "publish"}:
            self._json(400, {"ok": False, "error": "Mode must be draft or publish"})
            return
        overwrite_existing = False

        if mode == "draft" and target_path:
            rel = Path(target_path)
            if rel.is_absolute() or ".." in rel.parts or rel.suffix.lower() != ".org":
                self._json(400, {"ok": False, "error": "Invalid draft target path"})
                return
            filename = (self.blog_dir / rel).resolve()
            drafts_root = (self.blog_dir / "drafts").resolve()
            if not str(filename).startswith(str(drafts_root) + os.sep):
                self._json(400, {"ok": False, "error": "Draft target must be under drafts/"})
                return
            filename.parent.mkdir(parents=True, exist_ok=True)
            overwrite_existing = filename.exists()
        else:
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
                "message": "Draft updated." if overwrite_existing else "Draft saved.",
            },
        )

    def _parse_org_draft(self, draft_file: Path) -> tuple[str, str]:
        title = draft_file.stem.replace("-", " ").strip().title()
        body_lines: list[str] = []
        for line in draft_file.read_text(encoding="utf-8").splitlines():
            if line.startswith("#+TITLE:"):
                title = line[len("#+TITLE:") :].strip() or title
                continue
            if line.startswith("#+"):
                continue
            body_lines.append(line)
        body = "\n".join(body_lines).strip()
        return title, body

    def _load_draft(self, query: str) -> None:
        params = parse_qs(query, keep_blank_values=False)
        slug = (params.get("slug", [""])[0]).strip().lower()
        if not re.fullmatch(r"[a-z0-9][a-z0-9-]*", slug):
            self._json(400, {"ok": False, "error": "Invalid draft slug"})
            return
        draft_file = self.blog_dir / "drafts" / f"{slug}.org"
        if not draft_file.exists():
            self._json(404, {"ok": False, "error": "Draft not found"})
            return
        title, body = self._parse_org_draft(draft_file)
        self._json(
            200,
            {
                "ok": True,
                "slug": slug,
                "title": title,
                "body": body,
                "targetPath": f"drafts/{slug}.org",
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
  var host = document.querySelector("nav.main-nav .nav-right, .site-nav, .site-nav-left ul.nav, .main-nav");
  var draftMatch = location.pathname.match(/^\\/drafts\\/([^\\/]+)\\/?$/);
  var isDraftDetail = !!draftMatch && location.pathname !== "/drafts/";
  var draftSlug = isDraftDetail ? draftMatch[1] : "";

  function buildActionLink(id, label, href) {
    var el = document.createElement("a");
    el.id = id;
    el.href = href;
    el.setAttribute("aria-label", label);
    el.title = label;
    el.innerHTML = `<span style="display:inline-flex;align-items:center;justify-content:center;width:20px;height:20px;border:1.5px solid #777;border-radius:3px;"><svg width="13" height="13" viewBox="0 0 24 24" aria-hidden="true"><path d="M3 17.25V21h3.75L19.81 7.94l-3.75-3.75L3 17.25zm18-11.5a1 1 0 0 0 0-1.41L19.66 3a1 1 0 0 0-1.41 0l-1.59 1.59 3.75 3.75L21 5.75z" fill="#666"></path></svg></span><span style="font:500 14px/1.2 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;color:#666;">${label}</span>`;
    el.style.cssText = "margin-left:12px;display:inline-flex;align-items:center;gap:8px;text-decoration:none;padding:6px 8px;background:transparent;border-radius:8px;";
    return el;
  }

  if (!document.getElementById("local-dev-drafts-link")) {
    var drafts = document.createElement("a");
    drafts.id = "local-dev-drafts-link";
    drafts.href = "/drafts/";
    drafts.textContent = "Drafts";
    drafts.style.cssText = "margin-left:8px;padding:8px 10px;color:#666;text-decoration:none;font:500 14px/1.2 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;";
    if (host) host.appendChild(drafts);
  }

  if (!document.getElementById("local-dev-write-link")) {
    var actionLabel = isDraftDetail ? "Edit" : "Write";
    var actionHref = isDraftDetail ? ("/editor?draft=" + encodeURIComponent(draftSlug)) : "/editor";
    var write = buildActionLink("local-dev-write-link", actionLabel, actionHref);
    if (host) {
      host.appendChild(write);
    } else {
      write.style.position = "fixed";
      write.style.right = "16px";
      write.style.top = "16px";
      write.style.zIndex = "9999";
      document.body.appendChild(write);
    }
  } else {
    var existing = document.getElementById("local-dev-write-link");
    if (existing && isDraftDetail) {
      existing.href = "/editor?draft=" + encodeURIComponent(draftSlug);
      existing.title = "Edit";
      existing.setAttribute("aria-label", "Edit");
      var textSpan = existing.querySelector("span:last-child");
      if (textSpan) textSpan.textContent = "Edit";
    }
  }

  var searchBtn = document.getElementById("site-search-open");
  if (searchBtn && !searchBtn.dataset.devEnhanced) {
    searchBtn.dataset.devEnhanced = "1";
    searchBtn.innerHTML = `<svg width="16" height="16" viewBox="0 0 24 24" aria-hidden="true" style="margin-right:6px;vertical-align:-2px;"><circle cx="11" cy="11" r="7" fill="none" stroke="currentColor" stroke-width="2"></circle><line x1="16.65" y1="16.65" x2="21" y2="21" stroke="currentColor" stroke-width="2" stroke-linecap="round"></line></svg><span>Search</span><kbd style="margin-left:8px;padding:1px 6px;border:1px solid rgba(0,0,0,.28);border-radius:5px;font:600 11px/1.4 ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;background:rgba(255,255,255,.65);">/</kbd>`;
    searchBtn.style.display = "inline-flex";
    searchBtn.style.alignItems = "center";
    searchBtn.style.gap = "2px";
    searchBtn.style.whiteSpace = "nowrap";
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

        if path in {"/editor/api/load-draft", "/__editor/api/load-draft"}:
            self._load_draft(parsed.query)
            return

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
            "/editor/api/load-draft",
            "/__editor/api/load-draft",
        }:
            self.send_error(404, "Not Found")
            return

        if path in {"/editor/api/load-draft", "/__editor/api/load-draft"}:
            self.send_error(405, "Use GET for draft loading")
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
