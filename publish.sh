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

# Generate labeled gapbuffer diagrams with L-shaped arrows and 1x scale.
FONT_HELV="${FONT_HELV:-/System/Library/Fonts/Helvetica.ttc}"
ASSET_DIR="$BLOG_DIR/assets/blog/2018/08/11/writing-a-programmers-editor-(datastructure)---part-2"
EXTRA_HPAD=60

write_ditaa_source() {
  local name="$1"
  case "$name" in
    insertion)
      cat > "/tmp/gapbuffer-insertion.ditaa" << 'DITAA'
+----+----+---+---+---+---+
|cBF0|cBF0| A | B | C | D |
+----+----+---+---+---+---+
   |
   +----+
        |
        v
+---+----+---+---+---+---+
| X |cBF0| A | B | C | D |
+---+----+---+---+---+---+
   |
   +----+
        |
        v
+---+---+----+---+---+---+
| X | Y |cBF0| A | B | C |
+---+---+----+---+---+---+
DITAA
      ;;
    move)
      cat > "/tmp/gapbuffer-move.ditaa" << 'DITAA'
+----+----+---+---+---+---+---+
|cBF0|cBF0| H | E | L | L | O |
+----+----+---+---+---+---+---+
   |
   +----+
        |
        v
+---+---+---+----+----+---+---+
| H | E | L |cBF0|cBF0| L | O |
+---+---+---+----+----+---+---+
DITAA
      ;;
    resize)
      cat > "/tmp/gapbuffer-resize.ditaa" << 'DITAA'
+---+---+---+---+---+---+---+---+
| A | B | C | D | E | F | G | H |
+---+---+---+---+---+---+---+---+
   |
   +----+
        |
        v
+---+---+---+---+----+----+----+----+----+----+----+----+---+---+---+---+
| A | B | C | D |cBF0|cBF0|cBF0|cBF0|cBF0|cBF0|cBF0|cBF0| E | F | G | H |
+---+---+---+---+----+----+----+----+----+----+----+----+---+---+---+---+
DITAA
      ;;
  esac
}

generate_labeled_diagram() {
  local name="$1" top="$2" bot="$3"
  local out="$ASSET_DIR/gapbuffer-$name.png"
  local hash_file="$ASSET_DIR/.gapbuffer-$name.ditaa.hash"
  local src_hash

  # Only rerender if the source template changed or the output is missing.
  write_ditaa_source "$name"
  src_hash="$(shasum "/tmp/gapbuffer-${name}.ditaa" | awk '{print $1}')"
  if [ -f "$out" ] && [ -f "$hash_file" ] && [ "$src_hash" = "$(cat "$hash_file" 2>/dev/null)" ]; then
    return
  fi
  echo "$src_hash" > "$hash_file"

  # Render at native 1x to match gapbuffer.png's crisp pixel-perfect style.
  java -jar "$DITAA_JAR" "/tmp/gapbuffer-${name}.ditaa" "/tmp/${name}-native.png" -E -S 2>/dev/null

  core_w=$(identify "/tmp/${name}-native.png" | awk '{print $3}' | cut -dx -f1)
  text_w=$(magick -font "$FONT_HELV" -pointsize 10 label:"$bot" -format "%w" info:)
  text_pad=$(( (text_w - core_w) / 2 + 10 ))
  [ $text_pad -lt 0 ] && text_pad=0
  hpad=$(( text_pad + EXTRA_HPAD ))

  magick "/tmp/${name}-native.png" \
    -gravity west -splice "${hpad}x0" -gravity east -splice "${hpad}x0" \
    -gravity north -splice 0x40 \
    -font "$FONT_HELV" -pointsize 16 -fill black -annotate +0+15 "$top" \
    -gravity south -splice 0x30 \
    -font "$FONT_HELV" -pointsize 10 -fill black -annotate +0+15 "$bot" \
    -alpha off -fuzz 10% -fill '#BBFF00' -opaque '#99DD99' \
    "$out"
  echo "Generated gapbuffer-$name.png ($(identify "$out" | awk '{print $3}'))"
}

generate_labeled_diagram "insertion" "Insertion — typing fills the gap" "Gap shrinks by one for each character typed (O(1) per insertion)"
generate_labeled_diagram "move"      "Cursor Movement — gap shifts right" "Moving cursor N positions requires shifting N chars (O(N))"
generate_labeled_diagram "resize"    "Buffer Resize — gap exhausted" "Buffer doubles, new gap opens (amortized O(1) per insertion)"

# Convert ditaa-rendered gap buffer diagrams from default green to #BBFF00.
find "$BLOG_DIR/assets" -path "*/gapbuffer*.png" -not -name "gapbuffer1.png" \
  -exec magick {} -fuzz 10% -fill '#BBFF00' -opaque '#99DD99' {} \; 2>/dev/null || true

# Publish with AOG using native selfdotsend theme.
echo "Publishing blog with AOG..."
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
  if [ -d "$BLOG_DIR/media/js" ]; then
    mkdir -p "$BLOG_DIR/public/media/js"
    cp -r "$BLOG_DIR/media/js/"* "$BLOG_DIR/public/media/js/" 2>/dev/null || true
  fi
  if [ -d "$BLOG_DIR/media/css" ]; then
    mkdir -p "$BLOG_DIR/public/media/css"
    cp -r "$BLOG_DIR/media/css/"* "$BLOG_DIR/public/media/css/" 2>/dev/null || true
  fi
fi
if [ -d "$BLOG_DIR/assets" ]; then
  mkdir -p "$BLOG_DIR/public/assets"
  cp -r "$BLOG_DIR/assets/"* "$BLOG_DIR/public/assets/" 2>/dev/null || true
fi

# Keep custom domain mapping in generated output.
printf 'selfdotsend.com\n' > "$BLOG_DIR/public/CNAME"

# Normalize Org-exported file:// URLs to site-root absolute URLs.
find "$BLOG_DIR/public" -type f -name "*.html" -print0 | \
  xargs -0 sed -i '' \
    -e 's|file:///blog/|/blog/|g' \
    -e 's|file:///assets/|/assets/|g' \
    -e 's|file:///media/|/media/|g'




# Inject claps.js and code-block-controls.js runtime helpers.
find "$BLOG_DIR/public" -type f -name "*.html" -print0 | \
  xargs -0 sed -i '' \
    -e 's|</body>|<script src="/media/js/claps.js"></script>\n<script src="/media/js/code-block-controls.js"></script>\n</body>|g'

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

# Generate archive page from published posts (overrides AOG's auto-generated listing).
python3 "$BLOG_DIR/scripts/generate_archive.py" "$BLOG_DIR"

# Generate drafts index page (lists articles in drafts/ directory).
python3 "$BLOG_DIR/scripts/generate_drafts.py" "$BLOG_DIR"

# Build HTML preview pages for each draft article.
python3 "$BLOG_DIR/scripts/build_draft_previews.py" "$BLOG_DIR"

# Apply post-processing to draft preview pages (they were built after the
# main post-processing pipeline ran, so they missed CSS/JS/highlight injection).
if [ -d "$BLOG_DIR/public/drafts" ]; then
  # Syntax highlighting for exported code blocks.
  node "$BLOG_DIR/scripts/highlight_exported_code.js" "$BLOG_DIR/public/drafts"

  # Inject claps.js and code-block-controls.js into draft pages.
  find "$BLOG_DIR/public/drafts" -type f -name "*.html" -print0 | \
    xargs -0 sed -i '' \
      -e 's|</body>|<script src="/media/js/claps.js"></script>\n<script src="/media/js/code-block-controls.js"></script>\n</body>|g' 2>/dev/null || true

  # Inject code-theme CSS into draft pages.
  CODE_CSS="$BLOG_DIR/media/css/doom-monokai-pro.css"
  if [ -f "$CODE_CSS" ]; then
    CODE_VER="$(stat -f '%m' "$BLOG_DIR/public/media/css/code-theme-monokai-pro.css" 2>/dev/null || date +%s)"
    find "$BLOG_DIR/public/drafts" -type f -name "*.html" -print0 | \
      xargs -0 sed -i '' \
        -e "s|</head>|  <link rel=\"stylesheet\" href=\"/media/css/code-theme-monokai-pro.css?v=${CODE_VER}\" type=\"text/css\">\n</head>|g" 2>/dev/null || true
  fi
fi

# Inject series navigation into published articles AND draft preview pages.
python3 "$BLOG_DIR/scripts/inject_series_nav.py" "$BLOG_DIR"

# Inject clickable tag links into article and draft pages.
python3 "$BLOG_DIR/scripts/inject_tags.py" "$BLOG_DIR"

# Generate the LiveDrafts editor page (/editor/).
python3 "$BLOG_DIR/scripts/generate_editor_page.py" "$BLOG_DIR"

# Build static local search index from generated post pages.
"$EMACS_BIN" --batch -l "$BLOG_DIR/scripts/generate_search_index.el" "$BLOG_DIR"

# Dev-only: inject live-reload script and write build ID for auto-refresh.
if [ "${DEV_MODE:-0}" = "1" ]; then
  cp "$BLOG_DIR/media/js/dev-reload.js" "$BLOG_DIR/public/media/js/dev-reload.js"
  find "$BLOG_DIR/public" -type f -name "*.html" -print0 | \
    xargs -0 sed -i '' 's|</body>|<script src="/media/js/dev-reload.js"></script>\n</body>|g' 2>/dev/null || true
  date +%s > "$BLOG_DIR/public/dev-build-id.txt"
  echo "[dev] Live reload enabled."
fi

# Optional cleanup step to keep commit diffs clean after each build.
if [ "${CLEANUP_AFTER_BUILD:-1}" = "1" ]; then
  cleanup_post_build_artifacts
fi

echo "Blog published successfully at: $BLOG_DIR/public"
echo "To view the blog, run: cd $BLOG_DIR/public && python -m http.server 8080"
echo "Then visit: http://localhost:8080"
