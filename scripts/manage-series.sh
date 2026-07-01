#!/usr/bin/env bash
set -euo pipefail

# Manage article series from the command line.
#
# Usage:
#   ./scripts/manage-series.sh list
#   ./scripts/manage-series.sh show <series-slug>
#   ./scripts/manage-series.sh create "Series Name" "Description text"
#   ./scripts/manage-series.sh add <series-slug> <article-slug> [--title "Title"] [--date YYYY-MM-DD] [--url /blog/...]
#   ./scripts/manage-series.sh remove <series-slug> <article-slug>
#   ./scripts/manage-series.sh reorder <series-slug> <slug1> <slug2> <slug3> ...
#   ./scripts/manage-series.sh set-desc <series-slug> "New description"

BLOG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SERIES_DIR="$BLOG_DIR/series"
DRAFTS_DIR="$BLOG_DIR/drafts"
POSTS_DIR="$BLOG_DIR/posts"

slugify() {
  echo "$1" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/-/g' | sed 's/--*/-/g' | sed 's/^-//;s/-$//'
}

cmd_list() {
  if [ ! -d "$SERIES_DIR" ] || [ -z "$(ls "$SERIES_DIR"/*.json 2>/dev/null)" ]; then
    echo "No series found in $SERIES_DIR"
    exit 0
  fi
  echo "Series:"
  for f in "$SERIES_DIR"/*.json; do
    name=$(python3 -c "import json; d=json.load(open('$f')); print(d.get('name','?'))")
    count=$(python3 -c "import json; d=json.load(open('$f')); print(len(d.get('articles',[])))")
    slug=$(basename "$f" .json)
    printf "  %s — %s (%s articles)\n" "$slug" "$name" "$count"
  done
}

cmd_show() {
  local slug="$1"
  local file="$SERIES_DIR/$slug.json"
  if [ ! -f "$file" ]; then
    echo "Series not found: $slug"
    exit 1
  fi
  python3 -c "
import json
with open('$file') as f:
    d = json.load(f)
print('Name:', d.get('name',''))
print('Description:', d.get('description',''))
print()
arts = d.get('articles', [])
for i, a in enumerate(arts):
    status = a.get('status', '?')
    mark = '\u2713' if status == 'published' else '\u270e'
    print(f'  {i+1}. [{mark}] {a[\"title\"]}')
    print(f'     URL: {a[\"url\"]}  Date: {a.get(\"date\",\"\")}  Status: {status}')
print(f'\n{sum(1 for a in arts if a.get(\"status\")==\"published\")} of {len(arts)} published')
"
}

cmd_create() {
  local name="$1"
  local desc="${2:-}"
  local slug=$(slugify "$name")
  local file="$SERIES_DIR/$slug.json"
  mkdir -p "$SERIES_DIR"
  if [ -f "$file" ]; then
    echo "Series already exists: $slug"
    exit 1
  fi
  python3 -c "
import json
d = {'name': '$name', 'description': '''$desc''', 'articles': []}
with open('$file', 'w') as f:
    json.dump(d, f, indent=2)
print(f'Created series: $slug')
print(f'  Name: $name')
print(f'  File: $file')
"
}

cmd_add() {
  local series_slug="$1"
  local article_slug="$2"
  shift 2
  local title=""
  local date=""
  local url=""
  while [ $# -gt 0 ]; do
    case "$1" in
      --title) title="$2"; shift 2 ;;
      --date) date="$2"; shift 2 ;;
      --url) url="$2"; shift 2 ;;
      *) shift ;;
    esac
  done
  local file="$SERIES_DIR/$series_slug.json"
  if [ ! -f "$file" ]; then
    echo "Series not found: $series_slug"
    exit 1
  fi
  # Auto-detect title, date, url from org file if not provided.
  local org_file=""
  for candidate in "$POSTS_DIR/$article_slug.org" "$DRAFTS_DIR/$article_slug.org"; do
    if [ -f "$candidate" ]; then org_file="$candidate"; break; fi
  done
  if [ -z "$title" ] && [ -n "$org_file" ]; then
    title=$(grep '^#+TITLE:' "$org_file" | sed 's/^#+TITLE:\s*//' || echo "$article_slug")
  fi
  if [ -z "$date" ] && [ -n "$org_file" ]; then
    date=$(grep '^#+DATE:' "$org_file" | sed 's/^#+DATE:\s*\[\([0-9-]*\).*/\1/' || echo "")
  fi
  if [ -z "$url" ] && [ -n "$org_file" ] && [ -n "$date" ]; then
    local uri=$(grep '^#+URI:' "$org_file" | sed 's/^#+URI:\s*//' || echo "")
    if [ -n "$uri" ]; then
      url=$(echo "$uri" | sed "s/%y/$(echo $date | cut -d- -f1)/;s/%m/$(echo $date | cut -d- -f2)/;s/%d/$(echo $date | cut -d- -f3)/")
      url="${url}/"
    fi
  fi
  [ -z "$title" ] && title="$article_slug"
  [ -z "$date" ] && date=$(date +%Y-%m-%d)
  [ -z "$url" ] && url="/drafts/$article_slug/"
  local status="draft"
  [ -f "$POSTS_DIR/$article_slug.org" ] && status="published"
  python3 -c "
import json
with open('$file', 'r') as f:
    d = json.load(f)
# Check if already exists.
for a in d.get('articles', []):
    if '$article_slug' in a.get('url', ''):
        print('Article already in series')
        exit(0)
d.setdefault('articles', []).append({
    'title': '''$title''',
    'url': '$url',
    'date': '$date',
    'status': '$status'
})
with open('$file', 'w') as f:
    json.dump(d, f, indent=2)
print(f'Added: $title -> $series_slug (position {len(d[\"articles\"])})')
"
}

cmd_remove() {
  local series_slug="$1"
  local article_slug="$2"
  local file="$SERIES_DIR/$series_slug.json"
  if [ ! -f "$file" ]; then
    echo "Series not found: $series_slug"
    exit 1
  fi
  python3 -c "
import json
with open('$file', 'r') as f:
    d = json.load(f)
before = len(d.get('articles', []))
d['articles'] = [a for a in d.get('articles', []) if '$article_slug' not in a.get('url', '')]
after = len(d['articles'])
with open('$file', 'w') as f:
    json.dump(d, f, indent=2)
if before > after:
    print(f'Removed: $article_slug from $series_slug')
else:
    print(f'Not found in series: $article_slug')
"
}

cmd_reorder() {
  local series_slug="$1"
  shift
  local slugs=("$@")
  local file="$SERIES_DIR/$series_slug.json"
  if [ ! -f "$file" ]; then
    echo "Series not found: $series_slug"
    exit 1
  fi
  local slug_args=""
  for s in "${slugs[@]}"; do slug_args="$slug_args \"$s\""; done
  python3 -c "
import json, sys
with open('$file', 'r') as f:
    d = json.load(f)
old = d.get('articles', [])
order = [$slug_args]
def find_article(slug):
    for a in old:
        if slug in a.get('url', ''):
            return a
    return None
new_order = []
for slug in order:
    a = find_article(slug)
    if a: new_order.append(a)
# Append any not mentioned.
for a in old:
    if a not in new_order: new_order.append(a)
d['articles'] = new_order
with open('$file', 'w') as f:
    json.dump(d, f, indent=2)
print(f'Reordered: $series_slug')
"
}

cmd_set_desc() {
  local series_slug="$1"
  local desc="$2"
  local file="$SERIES_DIR/$series_slug.json"
  if [ ! -f "$file" ]; then
    echo "Series not found: $series_slug"
    exit 1
  fi
  python3 -c "
import json
with open('$file', 'r') as f:
    d = json.load(f)
d['description'] = '''$desc'''
with open('$file', 'w') as f:
    json.dump(d, f, indent=2)
print(f'Updated description for: $series_slug')
"
}

# Parse command.
cmd="${1:-}"; shift || true
case "$cmd" in
  list)     cmd_list ;;
  show)     cmd_show "${1:?Usage: show <series-slug>}" ;;
  create)   cmd_create "${1:?Usage: create \"Name\" \"desc\"}" "${2:-}" ;;
  add)      cmd_add "${1:?Usage: add <series> <slug> [options]}" "${2:?Usage: add <series> <slug>}" "${@:3}" ;;
  remove)   cmd_remove "${1:?Usage: remove <series> <slug>}" "${2:?Usage: remove <series> <slug>}" ;;
  reorder)  cmd_reorder "${1:?Usage: reorder <series> <slug1> <slug2>...}" "${@:2}" ;;
  set-desc) cmd_set_desc "${1:?Usage: set-desc <series> \"desc\"}" "${2:?Usage: set-desc <series> \"desc\"}" ;;
  -h|--help|help|"")
    echo "Series Management CLI"
    echo ""
    echo "Usage:"
    echo "  ./scripts/manage-series.sh list"
    echo "  ./scripts/manage-series.sh show <series-slug>"
    echo "  ./scripts/manage-series.sh create \"Series Name\" \"Description\""
    echo "  ./scripts/manage-series.sh add <series-slug> <article-slug> [--title \"T\"] [--date D] [--url U]"
    echo "  ./scripts/manage-series.sh remove <series-slug> <article-slug>"
    echo "  ./scripts/manage-series.sh reorder <series-slug> <slug1> <slug2> ..."
    echo "  ./scripts/manage-series.sh set-desc <series-slug> \"New description\""
    ;;
  *) echo "Unknown command: $cmd"; exit 1 ;;
esac
