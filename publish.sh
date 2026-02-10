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
DRAFTS="${DRAFTS:-0}"
DRAFT_PREVIEW_DIR="$BLOG_DIR/posts/draft-preview-temp"

prepare_draft_preview_files() {
  rm -rf "$DRAFT_PREVIEW_DIR"
  if [ "$DRAFTS" != "1" ]; then
    return
  fi
  if [ ! -d "$BLOG_DIR/drafts" ]; then
    return
  fi
  mkdir -p "$DRAFT_PREVIEW_DIR"
  find "$BLOG_DIR/drafts" -type f -name "*.org" -print0 | while IFS= read -r -d '' draft_file; do
    cp "$draft_file" "$DRAFT_PREVIEW_DIR/$(basename "$draft_file")"
  done
  if git -C "$BLOG_DIR" rev-parse --git-dir >/dev/null 2>&1; then
    git -C "$BLOG_DIR" add "$DRAFT_PREVIEW_DIR"/*.org >/dev/null 2>&1 || true
  fi
}

cleanup_draft_preview_files() {
  if git -C "$BLOG_DIR" rev-parse --git-dir >/dev/null 2>&1; then
    git -C "$BLOG_DIR" reset -q HEAD -- "$DRAFT_PREVIEW_DIR" >/dev/null 2>&1 || true
  fi
  rm -rf "$DRAFT_PREVIEW_DIR"
}

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

prepare_draft_preview_files
trap cleanup_draft_preview_files EXIT

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
if [ "$DRAFTS" = "1" ]; then
  echo "Including drafts in this build (DRAFTS=1)."
  export AOG_INCLUDE_DRAFTS=1
else
  export AOG_INCLUDE_DRAFTS=0
fi
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

# Normalize Org-exported file:// URLs to site-root absolute URLs.
find "$BLOG_DIR/public" -type f -name "*.html" -print0 | \
  xargs -0 sed -i '' \
    -e 's|file:///blog/|/blog/|g' \
    -e 's|file:///assets/|/assets/|g' \
    -e 's|file:///media/|/media/|g'

# Remove synthetic homepage title injected by generic post template.
if [ -f "$BLOG_DIR/public/index.html" ]; then
  sed -i '' -e 's|<h1 class="title">Home</h1>||g' "$BLOG_DIR/public/index.html"
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

# Local preview helper for drafts: generates /drafts/ pages only when DRAFTS=1.
if [ "$DRAFTS" = "1" ]; then
  python "$BLOG_DIR/scripts/generate_draft_preview.py" "$BLOG_DIR"
else
  rm -rf "$BLOG_DIR/public/drafts"
fi

# Optional cleanup step to keep commit diffs clean after each build.
if [ "${CLEANUP_AFTER_BUILD:-1}" = "1" ]; then
  cleanup_post_build_artifacts
fi

echo "Blog published successfully at: $BLOG_DIR/public"
echo "To view the blog, run: cd $BLOG_DIR/public && python -m http.server 8080"
echo "Then visit: http://localhost:8080"
