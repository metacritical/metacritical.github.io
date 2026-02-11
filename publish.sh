#!/bin/bash

# Publish via AOG theme engine.

# Allow invocation via `sh publish.sh` by re-execing in bash.
[ -n "${BASH_VERSION:-}" ] || exec bash "$0" "$@"

set -euo pipefail

BLOG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DITAA_JAR="${DITAA_JAR:-$BLOG_DIR/tools/diagrams/ditaa-0.11.0-standalone.jar}"
PLANTUML_JAR="${PLANTUML_JAR:-$BLOG_DIR/tools/diagrams/plantuml-mit-1.2026.1.jar}"
EMACS_BIN="${EMACS_BIN:-emacs}"
START_BRANCH="$(git -C "$BLOG_DIR" rev-parse --abbrev-ref HEAD 2>/dev/null || true)"
TMP_PUBLISHED_DIR=""

cleanup_post_build_artifacts() {
  # Keep the repo root tidy for commits by archiving known legacy folders.
  local stamp archive_dir moved_any
  stamp="$(date +%Y-%m-%d)"
  archive_dir="$BLOG_DIR/ancient/${stamp}-post-build-cleanup"
  moved_any=0

  for legacy_dir in public-aog public-test templates_legacy output; do
    if [ -e "$BLOG_DIR/$legacy_dir" ]; then
      mkdir -p "$archive_dir"
      mv "$BLOG_DIR/$legacy_dir" "$archive_dir/"
      moved_any=1
    fi
  done

  if [ "$moved_any" -eq 1 ]; then
    echo "Archived legacy directories to: $archive_dir"
  fi

  # Remove editor backups/OS metadata only when they are untracked.
  while IFS= read -r -d '' junk_file; do
    rel_path="${junk_file#$BLOG_DIR/}"
    if ! git -C "$BLOG_DIR" ls-files --error-unmatch "$rel_path" >/dev/null 2>&1; then
      rm -f "$junk_file"
    fi
  done < <(find "$BLOG_DIR" \
    \( -path "$BLOG_DIR/.git" -o -path "$BLOG_DIR/public" -o -path "$BLOG_DIR/ancient" \) -prune -o \
    -type f \( -name "*~" -o -name ".DS_Store" \) -print0)
}

# Ensure exported section anchors are readable and stable (CUSTOM_ID slugs).
"$EMACS_BIN" --batch -l "$BLOG_DIR/scripts/normalize_org_custom_ids.el" "$BLOG_DIR"

# Render Org Babel diagrams by default.
# Disable explicitly with: RENDER_DIAGRAMS=0 ./publish.sh
if [ "${RENDER_DIAGRAMS:-1}" = "1" ]; then
  echo "Rendering Org diagrams for posts..."
  mkdir -p "$BLOG_DIR/media/images"
  while IFS= read -r org_file; do
    # Only process files that actually contain diagram source blocks.
    if rg -qi '^[[:space:]]*#\+BEGIN_SRC[[:space:]]+(ditaa|plantuml|dot)\b|^[[:space:]]*#\+BEGIN_SRC[[:space:]]+(ditaa|plantuml|dot)[[:space:]]' "$org_file"; then
      /opt/homebrew/bin/timeout 90s \
        "$EMACS_BIN" --batch -l "$BLOG_DIR/scripts/render_org_artifacts.el" \
        "$BLOG_DIR" "$org_file" "$DITAA_JAR" "$PLANTUML_JAR" || \
        echo "[WARN] Diagram render timed out or failed for: $org_file"
    fi
  done < <(find "$BLOG_DIR/posts" -name "*.org" -type f | sort)
fi

# Publish with AOG using native selfdotsend theme.
echo "Publishing blog with AOG..."
if [ -n "$START_BRANCH" ]; then
  export AOG_REPOSITORY_ORG_BRANCH="$START_BRANCH"
  export AOG_REPOSITORY_HTML_BRANCH="$START_BRANCH"
fi
# Drafts are previewed via /public/drafts pages; keep AOG's main publish feed
# limited to published posts only.
export AOG_INCLUDE_DRAFTS=0
rm -rf "$BLOG_DIR/public"
# AOG emits a large volume of benign link-validation warnings during
# generation; filter them so real failures are visible in dev output.
aog publish "$BLOG_DIR" "$BLOG_DIR/public" 2>&1 | \
  rg -v '^(Mark set|Fontifing nil\.\.\.|Htmlizing nil\.\.\.|Setting up indent for shell type bash|Indentation variables are now local\.|Indentation setup for shell type bash|\[WARN\] File .* in hyper link does not exist|\(:title )'

# AOG may leave the repository on its org source branch; switch back so
# post-processing steps run against the expected working tree.
if [ -n "$START_BRANCH" ]; then
  CURRENT_BRANCH="$(git -C "$BLOG_DIR" rev-parse --abbrev-ref HEAD 2>/dev/null || true)"
  if [ -n "$CURRENT_BRANCH" ] && [ "$CURRENT_BRANCH" != "$START_BRANCH" ]; then
    # Preserve generated output before changing branches.
    TMP_PUBLISHED_DIR="$(mktemp -d)"
    if [ -d "$BLOG_DIR/public" ]; then
      cp -R "$BLOG_DIR/public/." "$TMP_PUBLISHED_DIR/"
    fi

    # Clear source-branch output to avoid checkout conflicts.
    rm -rf "$BLOG_DIR/public"
    git -C "$BLOG_DIR" checkout "$START_BRANCH" >/dev/null

    # Restore generated output in the caller branch.
    rm -rf "$BLOG_DIR/public"
    mkdir -p "$BLOG_DIR/public"
    if [ -n "$(ls -A "$TMP_PUBLISHED_DIR" 2>/dev/null || true)" ]; then
      cp -R "$TMP_PUBLISHED_DIR/." "$BLOG_DIR/public/"
    fi
    rm -rf "$TMP_PUBLISHED_DIR"
  fi
fi

# Keep only site-specific media assets (logos, generated diagrams).
if [ -d "$BLOG_DIR/media" ]; then
  mkdir -p "$BLOG_DIR/public/media"
  if [ -d "$BLOG_DIR/media/img" ]; then
    mkdir -p "$BLOG_DIR/public/media/img"
    cp -r "$BLOG_DIR/media/img/"* "$BLOG_DIR/public/media/img/" 2>/dev/null || true
  fi
  if [ -d "$BLOG_DIR/media/images" ]; then
    mkdir -p "$BLOG_DIR/public/media/images"
    cp -r "$BLOG_DIR/media/images/"* "$BLOG_DIR/public/media/images/" 2>/dev/null || true
  fi
fi
if [ -d "$BLOG_DIR/assets" ]; then
  mkdir -p "$BLOG_DIR/public/assets"
  cp -r "$BLOG_DIR/assets/"* "$BLOG_DIR/public/assets/" 2>/dev/null || true
fi

# Keep custom domain mapping in generated output.
printf 'www.selfdotsend.com\n' > "$BLOG_DIR/public/CNAME"
if [ -f "$BLOG_DIR/llms.txt" ]; then
  cp "$BLOG_DIR/llms.txt" "$BLOG_DIR/public/llms.txt"
fi
if [ -f "$BLOG_DIR/404.html" ]; then
  cp "$BLOG_DIR/404.html" "$BLOG_DIR/public/404.html"
fi

# Normalize Org-exported file:// URLs to site-root absolute URLs.
find "$BLOG_DIR/public" -type f -name "*.html" -print0 | \
  xargs -0 sed -i '' \
    -e 's|file:///blog/|/blog/|g' \
    -e 's|file:///assets/|/assets/|g' \
    -e 's|file:///media/|/media/|g'

# Ensure nav/footer consistency across all generated pages.
while IFS= read -r -d '' html_file; do
  perl -0777 -i -pe '
    # Add Drafts nav link on main site header when missing.
    s@(<a href="/nano-chat/">Nano Chat</a>)(?!\s*<a href="/drafts/">Drafts</a>)@$1\n    <a href="/drafts/">Drafts</a>@g;

    # Normalize author mailto link.
    s@<a href="mailto:[^"]*">Pankaj Doharey</a>@<a href="mailto:pankajdoharey%40gmail.com">Pankaj Doharey</a>@g;

    # Add LinkedIn profile icon link next to author in footer when missing.
    s@(<a href="mailto:pankajdoharey(?:%40gmail\.com|\.com)">Pankaj Doharey</a>)(?!\s*<a[^>]*footer-linkedin)@$1 <a class="footer-linkedin" href="https://www.linkedin.com/in/pankajdoharey/" target="_blank" rel="noopener" aria-label="LinkedIn profile" title="LinkedIn"><svg width="14" height="14" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M6.94 8.5H3.56V20h3.38V8.5zM5.25 3A2.25 2.25 0 1 0 5.3 7.5 2.25 2.25 0 0 0 5.25 3zM20 13.22c0-3.49-1.86-5.12-4.34-5.12-2 0-2.9 1.1-3.4 1.88V8.5H8.88V20h3.38v-5.7c0-1.5.28-2.95 2.14-2.95 1.83 0 1.85 1.72 1.85 3.04V20h3.38v-6.78z"/></svg></a>@g;

    # Normalize powered-by footer copy.
    s@Powered by\s*<a href="https://github\.com/metacritical/AOG" target="_blank">AOG</a>@Powered by <a href="https://github.com/metacritical/AOG" target="_blank">AOG</a> + Modalert 💊@g;
  ' "$html_file"
done < <(find "$BLOG_DIR/public" -type f -name "*.html" -print0)

# Remove synthetic homepage title injected by generic post template.
if [ -f "$BLOG_DIR/public/index.html" ]; then
  sed -i '' -e 's|<h1 class="title">Home</h1>||g' "$BLOG_DIR/public/index.html"
fi

# About page uses a dedicated author portrait.
if [ -f "$BLOG_DIR/public/about/index.html" ]; then
  sed -i '' -e 's|/media/images/avatar.jpg|/media/images/pankaj.png|g' "$BLOG_DIR/public/about/index.html"
  perl -0777 -i -pe '
    s@<aside class="story-rail" aria-label="Story actions">.*?</aside>@<aside class="story-rail" aria-label="Profile links">\n    <button class="story-clap-btn" type="button" aria-label="Clap for this page">\n      <img src="/media/images/clap.png" alt="" aria-hidden="true">\n    </button>\n    <span class="story-clap-count">0</span>\n    <a class="story-share-link story-share-x" href="https://x.com/pankajdoharey" target="_blank" rel="noopener" aria-label="X profile" title="X">\n      <svg width="20" height="20" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M18.9 2H22l-6.77 7.74L23.2 22h-6.27l-4.9-6.41L6.44 22H3.3l7.24-8.28L.8 2h6.43l4.43 5.85L18.9 2zm-2.2 18h1.74L6.27 3.9H4.4L16.7 20z"/></svg>\n    </a>\n    <a class="story-share-link story-share-ln" href="https://www.linkedin.com/in/pankaj-doharey/" target="_blank" rel="noopener" aria-label="LinkedIn profile" title="LinkedIn">\n      <svg width="20" height="20" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M6.94 8.5H3.56V20h3.38V8.5zM5.25 3A2.25 2.25 0 1 0 5.3 7.5 2.25 2.25 0 0 0 5.25 3zM20 13.22c0-3.49-1.86-5.12-4.34-5.12-2 0-2.9 1.1-3.4 1.88V8.5H8.88V20h3.38v-5.7c0-1.5.28-2.95 2.14-2.95 1.83 0 1.85 1.72 1.85 3.04V20h3.38v-6.78z"/></svg>\n    </a>\n    <a class="story-share-link story-share-yc" href="https://news.ycombinator.com/threads?id=pankajdoharey" target="_blank" rel="noopener" aria-label="Hacker News profile" title="Hacker News">\n      <svg width="20" height="20" viewBox="0 0 24 24" aria-hidden="true"><rect x="2" y="2" width="20" height="20" rx="2" fill="#ff6600"></rect><path fill="#fff" d="M7.4 6h2.2l2.5 4.3L14.6 6h2.1l-3.6 6.2V18h-2v-5.8z"/></svg>\n    </a>\n    <a class="story-share-link story-share-devto" href="https://dev.to/metacritical" target="_blank" rel="noopener" aria-label="dev.to profile" title="dev.to">\n      <svg width="20" height="20" viewBox="0 0 24 24" aria-hidden="true"><rect x="2" y="2" width="20" height="20" rx="2" fill="#0a0a0a"></rect><path fill="#fff" d="M6.2 8.3h2.1c1.8 0 3.1 1.2 3.1 3.7 0 2.5-1.3 3.7-3.1 3.7H6.2V8.3zm2 .9v5.6h.2c1.2 0 2-1 2-2.8s-.8-2.8-2-2.8h-.2zm4.4-.9h5.1v1h-3.9v2h3.3v1h-3.3v2.4h4.1v1h-5.3V8.3z"/></svg>\n    </a>\n  </aside>@s;
  ' "$BLOG_DIR/public/about/index.html"
fi

# Build-time syntax highlighting fallback for exported src blocks.
# This guarantees visible highlighting even when editor-side htmlize
# output is unavailable for some languages/modes.
node "$BLOG_DIR/scripts/highlight_exported_code.js" "$BLOG_DIR/public"

# Enforce a dedicated monokai-pro code stylesheet that is loaded after
# all page/theme CSS so code colors stay consistent across classic/medium.
DOOM_CSS="$BLOG_DIR/media/css/doom-monokai-pro.css"
TARGET_THEME_CSS="$BLOG_DIR/public/media/css/theme-medium.css"
TARGET_CODE_CSS="$BLOG_DIR/public/media/css/code-theme-monokai-pro.css"
if [ -f "$DOOM_CSS" ] && [ -f "$TARGET_THEME_CSS" ]; then
  cp "$DOOM_CSS" "$TARGET_CODE_CSS"

  THEME_VER="$(stat -f '%m' "$TARGET_THEME_CSS" 2>/dev/null || date +%s)"
  CODE_VER="$(stat -f '%m' "$TARGET_CODE_CSS" 2>/dev/null || date +%s)"

  find "$BLOG_DIR/public" -type f -name "*.html" -print0 | \
    xargs -0 sed -i '' -E \
      -e "s|/media/css/theme-medium\\.css(\\?v=[0-9]+)?\"|/media/css/theme-medium.css?v=${THEME_VER}\"|g" \
      -e "s|/media/css/code-theme-monokai-pro\\.css(\\?v=[0-9]+)?\"|/media/css/code-theme-monokai-pro.css?v=${CODE_VER}\"|g"

  while IFS= read -r -d '' html_file; do
    if ! rg -q '/media/css/code-theme-monokai-pro.css' "$html_file"; then
      tmp_file="$(mktemp)"
      awk -v code_ver="$CODE_VER" '
        {
          print
          if (!inserted && $0 ~ /\/media\/css\/theme-medium\.css\?v=[0-9]+/) {
            print "  <link rel=\"stylesheet\" href=\"/media/css/code-theme-monokai-pro.css?v=" code_ver "\" type=\"text/css\">"
            inserted=1
          }
        }
      ' "$html_file" > "$tmp_file"
      mv "$tmp_file" "$html_file"
    fi
  done < <(find "$BLOG_DIR/public" -type f -name "*.html" -print0)
fi

# Stable route alias for nano chat page.
NANO_SOURCE="$(find "$BLOG_DIR/public/blog" -type f -path "*/nano-chat/index.html" 2>/dev/null | head -n 1 || true)"
if [ -n "${NANO_SOURCE}" ]; then
  mkdir -p "$BLOG_DIR/public/nano-chat"
  cp "$NANO_SOURCE" "$BLOG_DIR/public/nano-chat/index.html"
fi

# Stable route aliases for archive page.
ARCHIVE_SOURCE=""
if [ -f "$BLOG_DIR/public/archiveorg/index.html" ]; then
  ARCHIVE_SOURCE="$BLOG_DIR/public/archiveorg/index.html"
else
  ARCHIVE_SOURCE="$(find "$BLOG_DIR/public/blog" -type f -path "*/archive/index.html" 2>/dev/null | head -n 1 || true)"
fi
if [ -n "${ARCHIVE_SOURCE}" ]; then
  mkdir -p "$BLOG_DIR/public/archive"
  cp "$ARCHIVE_SOURCE" "$BLOG_DIR/public/archive/index.html"
  # Ensure /blog/ resolves to styled archive page instead of raw directory listing.
  mkdir -p "$BLOG_DIR/public/blog"
  cp "$ARCHIVE_SOURCE" "$BLOG_DIR/public/blog/index.html"
fi

# Build static local search index from generated post pages.
"$EMACS_BIN" --batch -l "$BLOG_DIR/scripts/generate_search_index.el" "$BLOG_DIR"

# Always generate draft preview pages from /drafts.
"$EMACS_BIN" --batch -l "$BLOG_DIR/scripts/generate_draft_preview.el" "$BLOG_DIR"

# Optional cleanup step to keep commit diffs clean after each build.
if [ "${CLEANUP_AFTER_BUILD:-1}" = "1" ]; then
  cleanup_post_build_artifacts
fi

echo "Blog published successfully at: $BLOG_DIR/public"
echo "To view the blog, run: cd $BLOG_DIR/public && python -m http.server 8080"
echo "Then visit: http://localhost:8080"
