#!/bin/bash
set -euo pipefail

# Promote draft article(s) from drafts/ to posts/.
#
# Usage:
#   ./scripts/publish-draft.sh                    Smart promote (see below)
#   ./scripts/publish-draft.sh <draft-name>       Promote a specific draft
#   ./scripts/publish-draft.sh name1 name2        Promote multiple drafts
#   ./scripts/publish-draft.sh --all              Promote every draft
#   ./scripts/publish-draft.sh --rebuild [...]    Rebuild site after promoting
#
# Smart promote (no draft names given):
#   0 drafts  -> "No drafts found"
#   1 draft   -> Promote it immediately, no prompt
#   2+ drafts -> Interactive numbered picker (enter numbers, names, or 'all')
#
# Selection accepts:
#   Numbers:  1 3 5        (promote items 1, 3, 5)
#   Ranges:   3-5          (promote items 3 through 5)
#   Names:    part-3       (partial match against filename)
#   All:      all          (promote everything)

BLOG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DRAFTS_DIR="$BLOG_DIR/drafts"
POSTS_DIR="$BLOG_DIR/posts"
REBUILD=0
PROMOTE_ALL=0
SELECTED=()

# Parse arguments.
while [ $# -gt 0 ]; do
  case "$1" in
    --rebuild|-r) REBUILD=1; shift ;;
    --all|-a)     PROMOTE_ALL=1; shift ;;
    -h|--help)
      sed -n '4,/^$/p' "$0" | sed 's/^# //; s/^#$//'
      exit 0
      ;;
    -*) echo "Unknown option: $1" >&2; exit 1 ;;
    *)  SELECTED+=("${1%.org}"); shift ;;
  esac
done

# Collect all draft slugs (sorted).
mapfile -t ALL_DRAFTS < <(
  ls "$DRAFTS_DIR"/*.org 2>/dev/null \
    | xargs -n1 basename 2>/dev/null \
    | sed 's/\.org$//' \
    | sort \
  || true
)

if [ ${#ALL_DRAFTS[@]} -eq 0 ]; then
  echo "No drafts found in drafts/"
  exit 0
fi

# Resolve what to promote.
if [ "$PROMOTE_ALL" -eq 1 ]; then
  SELECTED=("${ALL_DRAFTS[@]}")
fi

if [ ${#SELECTED[@]} -eq 0 ]; then
  if [ ${#ALL_DRAFTS[@]} -eq 1 ]; then
    # Only one draft — promote it directly.
    SELECTED=("${ALL_DRAFTS[0]}")
    echo "Only one draft found — promoting: ${ALL_DRAFTS[0]}"
    echo ""
  else
    # Multiple drafts — interactive picker.
    echo "Drafts available:"
    echo ""
    for i in "${!ALL_DRAFTS[@]}"; do
      printf "  [%d] %s\n" "$((i+1))" "${ALL_DRAFTS[$i]}"
    done
    echo ""
    echo "Select: numbers (1 3 5), ranges (3-5), names (part-3), or 'all':"
    read -r CHOICE

    if [ "$CHOICE" = "all" ] || [ "$CHOICE" = "a" ]; then
      SELECTED=("${ALL_DRAFTS[@]}")
    else
      for token in $(echo "$CHOICE" | tr ',' ' '); do
        if [[ "$token" =~ ^([0-9]+)-([0-9]+)$ ]]; then
          # Range: expand N-M
          lo=${BASH_REMATCH[1]}
          hi=${BASH_REMATCH[2]}
          for ((n=lo; n<=hi; n++)); do
            if [ "$n" -ge 1 ] && [ "$n" -le "${#ALL_DRAFTS[@]}" ]; then
              SELECTED+=("${ALL_DRAFTS[$((n-1))]}")
            else
              echo "  Out of range: $n" >&2
            fi
          done
        elif [[ "$token" =~ ^[0-9]+$ ]]; then
          # Single number.
          if [ "$token" -ge 1 ] && [ "$token" -le "${#ALL_DRAFTS[@]}" ]; then
            SELECTED+=("${ALL_DRAFTS[$((token-1))]}")
          else
            echo "  Out of range: $token" >&2
          fi
        else
          # Name: try exact, then partial match.
          if printf '%s\n' "${ALL_DRAFTS[@]}" | grep -qx "$token"; then
            SELECTED+=("$token")
          else
            PARTIAL_MATCHES=()
            for d in "${ALL_DRAFTS[@]}"; do
              [[ "$d" == *"$token"* ]] && PARTIAL_MATCHES+=("$d")
            done
            if [ ${#PARTIAL_MATCHES[@]} -eq 1 ]; then
              SELECTED+=("${PARTIAL_MATCHES[0]}")
            elif [ ${#PARTIAL_MATCHES[@]} -gt 1 ]; then
              echo "  Ambiguous name '$token' matches: ${PARTIAL_MATCHES[*]}" >&2
            else
              echo "  No match: $token" >&2
            fi
          fi
        fi
      done
    fi
  fi
fi

# Deduplicate selection.
mapfile -t SELECTED < <(printf '%s\n' "${SELECTED[@]}" | awk '!seen[$0]++')

if [ ${#SELECTED[@]} -eq 0 ]; then
  echo "Nothing selected."
  exit 0
fi

# Promote each selected draft.
mkdir -p "$POSTS_DIR"

PROMOTED=0
SKIPPED=0

for DRAFT in "${SELECTED[@]}"; do
  SOURCE="$DRAFTS_DIR/$DRAFT.org"
  TARGET="$POSTS_DIR/$DRAFT.org"

  # If not found, try partial match.
  if [ ! -f "$SOURCE" ]; then
    MATCH=$(ls "$DRAFTS_DIR"/*"$DRAFT"*.org 2>/dev/null | head -1 || true)
    if [ -n "$MATCH" ]; then
      SOURCE="$MATCH"
      TARGET="$POSTS_DIR/$(basename "$MATCH")"
      DRAFT="$(basename "$MATCH" .org)"
    else
      echo "  SKIP: Not found: drafts/$DRAFT.org"
      SKIPPED=$((SKIPPED + 1))
      continue
    fi
  fi

  if [ -f "$TARGET" ]; then
    echo "  SKIP: Already published: posts/$DRAFT.org"
    SKIPPED=$((SKIPPED + 1))
    continue
  fi

  mv "$SOURCE" "$TARGET"
  echo "  PROMOTED: drafts/$DRAFT.org -> posts/$DRAFT.org"
  PROMOTED=$((PROMOTED + 1))

  # Update series JSON status if applicable.
  SERIES_FILE="$BLOG_DIR/series/writing-a-programmers-editor.json"
  if [ -f "$SERIES_FILE" ]; then
    URI=$(grep '^#+URI:' "$TARGET" | sed 's/#+URI:\s*//' || true)
    DATE_LINE=$(grep '^#+DATE:' "$TARGET" || true)
    if [ -n "$URI" ] && [ -n "$DATE_LINE" ]; then
      YEAR=$(echo "$DATE_LINE" | sed 's/.*\[\([0-9]*\)-.*/\1/')
      MONTH=$(echo "$DATE_LINE" | sed 's/.*\[[0-9]*-\([0-9]*\)-.*/\1/')
      DAY=$(echo "$DATE_LINE" | sed 's/.*\[[0-9]*-[0-9]*-\([0-9]*\).*/\1/')
      EXPANDED_URI=$(echo "$URI" | sed "s/%y/$YEAR/;s/%m/$MONTH/;s/%d/$DAY/")
      FULL_URL="${EXPANDED_URI}/"
      python3 -c "
import json
with open('$SERIES_FILE', 'r') as f:
    data = json.load(f)
for art in data.get('articles', []):
    if art.get('url') == '$FULL_URL':
        art['status'] = 'published'
        with open('$SERIES_FILE', 'w') as f:
            json.dump(data, f, indent=2)
        print('    Series status updated: draft -> published')
        break
" 2>/dev/null || true
    fi
  fi
done

echo ""
echo "Promoted: $PROMOTED, Skipped: $SKIPPED"

if [ "$PROMOTED" -gt 0 ]; then
  if [ "$REBUILD" -eq 1 ]; then
    echo ""
    echo "Rebuilding site..."
    (cd "$BLOG_DIR" && ./publish.sh)
  else
    echo ""
    echo "Run ./publish.sh to rebuild the site."
  fi
fi
