#!/usr/bin/env python3
"""Local dev server for blog preview + editor.

Serves:
- /... from BLOG_DIR/public
- /__editor from BLOG_DIR/editor/index.html
- POST /__editor/api/save to persist a draft .org file in BLOG_DIR/drafts
"""

from __future__ import annotations

import datetime as dt
import http.server
import json
import os
import re
import socketserver
import sys
from pathlib import Path
from urllib.parse import unquote, urlparse


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

    def do_GET(self) -> None:
        parsed = urlparse(self.path)
        path = unquote(parsed.path)

        if path == "/__editor" or path == "/__editor/":
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

        super().do_GET()

    def do_POST(self) -> None:
        parsed = urlparse(self.path)
        path = unquote(parsed.path)
        if path != "/__editor/api/save":
            self.send_error(404, "Not Found")
            return

        try:
            content_len = int(self.headers.get("Content-Length", "0"))
            payload = self.rfile.read(content_len)
            data = json.loads(payload.decode("utf-8"))
        except Exception:
            self._json(400, {"ok": False, "error": "Invalid JSON"})
            return

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
        # Avoid accidental overwrite of an existing draft.
        if filename.exists():
            stamp = dt.datetime.now().strftime("%H%M%S")
            filename = target_dir / f"{slug}-{stamp}.org"

        filename.write_text(org_draft_text(title, body), encoding="utf-8")
        self._json(
            200,
            {
                "ok": True,
                "mode": mode,
                "path": str(filename.relative_to(self.blog_dir)),
                "message": "Draft saved.",
            },
        )


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
        print(f"[web] Editor available at http://localhost:{port}/__editor")
        httpd.serve_forever()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
