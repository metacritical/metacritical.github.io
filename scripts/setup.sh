#!/usr/bin/env bash
set -euo pipefail

# Bootstrap a clean macOS machine for the Self dot send AOG blog.
#
# Usage:
#   ./scripts/setup.sh
#   ./scripts/setup.sh --skip-build
#   AOG_DIR="$HOME/Development/Projects/AOG" ./scripts/setup.sh

BLOG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
AOG_DIR="${AOG_DIR:-$BLOG_DIR/../AOG}"
AOG_REPO_URL="${AOG_REPO_URL:-git@github.com:metacritical/AOG.git}"
SKIP_BUILD=0

usage() {
  sed -n '3,9p' "$0"
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --skip-build) SKIP_BUILD=1; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage >&2; exit 1 ;;
  esac
done

if [ "$(uname -s)" != "Darwin" ]; then
  echo "This bootstrap currently supports macOS only." >&2
  echo "The build uses macOS sed behavior and Homebrew's GNU timeout." >&2
  exit 1
fi

if ! command -v brew >/dev/null 2>&1; then
  echo "Homebrew is required. Install it from https://brew.sh, then rerun this script." >&2
  exit 1
fi

echo "Installing build dependencies..."
brew install emacs node python imagemagick coreutils ripgrep openjdk

if [ ! -d "$AOG_DIR/.git" ]; then
  echo "Cloning AOG into $AOG_DIR..."
  mkdir -p "$(dirname "$AOG_DIR")"
  git clone "$AOG_REPO_URL" "$AOG_DIR"
else
  echo "Using existing AOG checkout at $AOG_DIR"
fi

EMACS_BIN="${EMACS_BIN:-$(command -v emacs)}"
echo "Installing required Emacs packages..."
"$EMACS_BIN" --batch --eval '
(require '\''package\'')
(setq package-archives '\''(("gnu" . "https://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (package '\''(mustache htmlize dash ht))
  (unless (package-installed-p package) (package-install package)))'

if [ ! -x "$AOG_DIR/bin/aog" ]; then
  echo "AOG CLI not found at $AOG_DIR/bin/aog" >&2
  exit 1
fi

mkdir -p "$HOME/.local/bin"
ln -sf "$AOG_DIR/bin/aog" "$HOME/.local/bin/aog"
export PATH="$AOG_DIR/bin:$HOME/.local/bin:$PATH"

if [ ! -x "$BLOG_DIR/tools/diagrams/ditaa-server.jar" ] && [ ! -f "$BLOG_DIR/tools/diagrams/ditaa-server.jar" ]; then
  echo "Ditaa server JAR is missing from the blog checkout." >&2
  exit 1
fi

echo "AOG: $(command -v aog)"
echo "Emacs: $EMACS_BIN"
echo "Java: $(command -v java || true)"
echo "Ditaa server: $BLOG_DIR/tools/diagrams/ditaa-server.jar"

if [ "$SKIP_BUILD" -eq 0 ]; then
  echo "Running the first site build..."
  (cd "$BLOG_DIR" && ./publish.sh)
fi

echo
echo "Setup complete. Add this to your shell profile if needed:"
echo "  export PATH=\"$AOG_DIR/bin:\$HOME/.local/bin:\$PATH\""
echo "Build with: (cd \"$BLOG_DIR\" && ./publish.sh)"
