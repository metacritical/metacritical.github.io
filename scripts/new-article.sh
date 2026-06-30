#!/bin/bash
set -euo pipefail

# Create a new article as a draft (default) or published post (--publish).
#
# Usage:
#   ./scripts/new-article.sh "My Article Title"
#   ./scripts/new-article.sh --publish "My Article Title"
#
# Files are created in drafts/ by default, posts/ with --publish.

PUBLISH=0
if [ "${1:-}" = "--publish" ]; then
  PUBLISH=1
  shift
fi

TITLE="${1:?Usage: new-article.sh [--publish] \"Title\"}"

BLOG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Slugify: lowercase, replace non-alphanumeric with hyphens, collapse doubles.
slugify() {
  local raw="$1"
  echo "$raw" | tr '[:upper:]' '[:lower:]' \
    | sed 's/[^a-z0-9]/-/g' \
    | sed 's/--*/-/g' \
    | sed 's/^-//;s/-$//'
}

SLUG=$(slugify "$TITLE")
TODAY=$(date +%Y-%m-%d)
DAY_NAME=$(date +%a)

if [ "$PUBLISH" -eq 1 ]; then
  TARGET_DIR="$BLOG_DIR/posts"
  STATUS="published"
else
  TARGET_DIR="$BLOG_DIR/drafts"
  STATUS="draft"
fi

mkdir -p "$TARGET_DIR"
FILENAME="$TARGET_DIR/$SLUG.org"

if [ -f "$FILENAME" ]; then
  echo "Error: $FILENAME already exists." >&2
  exit 1
fi

cat > "$FILENAME" <<EOF
#+TITLE:       $TITLE
#+AUTHOR:      Pankaj Doharey
#+EMAIL:       pankajdoharey@gmail.com
#+DATE:        [$TODAY $DAY_NAME]
#+URI:         /blog/%y/%m/%d/$SLUG
#+KEYWORDS:
#+TAGS:
#+LANGUAGE:    en
#+OPTIONS:     H:3 num:nil toc:nil \n:nil ::t |:t ^:nil -:nil f:t *:t <:t
#+DESCRIPTION:
EOF

echo "Created $STATUS article:"
echo "  $FILENAME"
if [ "$PUBLISH" -eq 0 ]; then
  echo ""
  echo "To publish later:  aog promote $SLUG"
fi
