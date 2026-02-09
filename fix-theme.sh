#!/bin/bash
set -euo pipefail

BLOG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PUBLIC_DIR="$BLOG_DIR/public"

mkdir -p "$BLOG_DIR/media/js" "$BLOG_DIR/media/css" "$BLOG_DIR/media/images"

# Ensure site-style JS path exists (selfdotsend.com style expects /media/js/main.js)
if [ -f "$BLOG_DIR/themes/metacritical/assets/js/main.js" ]; then
  cp "$BLOG_DIR/themes/metacritical/assets/js/main.js" "$BLOG_DIR/media/js/main.js"
fi
if [ -f "$BLOG_DIR/themes/metacritical/assets/css/style.css" ]; then
  cp "$BLOG_DIR/themes/metacritical/assets/css/style.css" "$BLOG_DIR/media/css/style.css"
fi
if [ -f "$BLOG_DIR/themes/metacritical/assets/css/highlight.css" ]; then
  cp "$BLOG_DIR/themes/metacritical/assets/css/highlight.css" "$BLOG_DIR/media/css/highlight.css"
fi
if [ -f "$BLOG_DIR/themes/metacritical/assets/img/logo.png" ]; then
  cp "$BLOG_DIR/themes/metacritical/assets/img/logo.png" "$BLOG_DIR/media/images/logo.png"
fi

build_header() {
  cat <<'HTML'
<!DOCTYPE html>
<html lang="en-us">
  <head>
    <title>Index - Self dot send (Computing Blog)</title>
    <meta charset="utf-8" />
    <meta name="author" content="Pankaj Doharey" />
    <link rel="stylesheet" href="/media/css/style.css">
    <link rel="stylesheet" href="/media/css/highlight.css">
    <link rel="stylesheet" href="/media/css/theme-medium.css">
    <style type="text/css"></style>
  </head>
  <body class="container">
    <nav class="main-nav">
      <a href="/">Blog</a>
      <a href="/archive.html">Archive</a>
      <a href="/tags/index.html">Tag</a>
      <a href="/about.html">About</a>
      <button id="theme-switch" class="theme-switch" type="button">Theme: Classic</button>
      <a class="cta" href="https://github.com/metacritical">
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 499.36" focusable="false"><title>GitHub</title><path d="M256 0C114.64 0 0 114.61 0 256c0 113.09 73.34 209 175.08 242.9 12.8 2.35 17.47-5.56 17.47-12.34 0-6.08-.22-22.18-.35-43.54-71.2 15.49-86.2-34.34-86.2-34.34-11.64-29.57-28.42-37.45-28.42-37.45-23.27-15.84 1.73-15.55 1.73-15.55 25.69 1.81 39.21 26.38 39.21 26.38 22.84 39.12 59.92 27.82 74.5 21.27 2.33-16.54 8.94-27.82 16.25-34.22-56.84-6.43-116.6-28.43-116.6-126.49 0-27.95 10-50.8 26.35-68.69-2.63-6.48-11.42-32.5 2.51-67.75 0 0 21.49-6.88 70.4 26.24a242.65 242.65 0 0 1 128.18 0c48.87-33.13 70.33-26.24 70.33-26.24 14 35.25 5.18 61.27 2.55 67.75 16.41 17.9 26.31 40.75 26.31 68.69 0 98.35-59.85 120-116.88 126.32 9.19 7.9 17.38 23.53 17.38 47.41 0 34.22-.31 61.83-.31 70.23 0 6.85 4.61 14.81 17.6 12.31C438.72 464.97 512 369.08 512 256.02 512 114.62 397.37 0 256 0z" fill="currentColor" fill-rule="evenodd"></path></svg>
      </a>
    </nav>
HTML
}

build_profile() {
  cat <<'HTML'
    <div class="profile">
      <section id="wrapper">
        <head id="header">
          <a href="/">
            <img id="avatar" class="2x" src="/media/images/logo.png">
          </a>
          <h1>Self dot send (Computing Blog)</h1>
          <h2>Message passing is just a procedure call.<h2>
        </head>
      </section>
    </div>
    <br />
    <br />
HTML
}

build_footer() {
  cat <<'HTML'
    <script async="" src="https://www.google-analytics.com/analytics.js"></script>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"></script>
    <script src="/media/js/main.js"></script>
    <script src="/media/js/theme-switcher.js"></script>
  </body>
  <div>
    <section id="wrapper">
      <br />
      <br />
      <script src="https://code.jquery.com/jquery-latest.min.js"></script>
      <script src="https://cdnjs.cloudflare.com/ajax/libs/prettify/r298/prettify.js"></script>
      <script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js"></script>
      <script src="/media/js/main.js"></script>
      <div align="center">
        <footer id="footer">
          <p class="small">
            Copyright &copy; 2012 - <span id="footerYear"></span> <a href="mailto:pankajdoharey@gmail.com">Pankaj Doharey</a>
            &nbsp;&nbsp;-&nbsp;&nbsp;
            Powered by <a href="https://github.com/metacritical/AOG" target="_blank">AOG</a>
            <script type="text/javascript">document.getElementById("footerYear").innerHTML = (new Date()).getFullYear();</script>
          </p>
          <br />
        </footer>
      </div>
    </section>
  </div>
  </body>
</html>
HTML
}

post_list_html() {
  local posts
  posts=$(find "$PUBLIC_DIR/posts" -name '*.html' -type f ! -name 'index.html' | sort)
  while IFS= read -r post; do
    [ -z "$post" ] && continue
    local title
    title=$(grep -m1 -o '<h2 id="[^"]*">[^<]*</h2>' "$post" 2>/dev/null | sed -E 's#<h2 id="[^"]*">##; s#</h2>##' || true)
    if [ -z "$title" ]; then
      local org_file="$BLOG_DIR/posts/$(basename "$post" .html).org"
      if [ -f "$org_file" ]; then
        title=$(sed -n 's/^#\\+TITLE:[[:space:]]*//p' "$org_file" | head -n 1)
      fi
    fi
    [ -z "$title" ] && title=$(basename "$post" .html | tr '-' ' ')
    local rel="${post#"$PUBLIC_DIR"}"
    echo "    <section id=\"wrapper\" class=\"home\">"
    echo "      <ul class=\"post-list\">"
    echo "        <li>"
    echo "          <a href=\"$rel\"><aside class=\"dates\"></aside></a>"
    echo "          <a href=\"$rel\">$title</a>"
    echo "        </li>"
    echo "      </ul>"
    echo "    </section>"
  done <<< "$posts"
}

# Rewrite every generated HTML page with the legacy shell.
find "$PUBLIC_DIR" -name '*.html' -type f | while IFS= read -r file; do
  tmp=$(mktemp)
  content=$(sed -n '/<main id="content">/,/<\/main>/p' "$file" | sed '1d;$d' || true)
  if [ -z "${content:-}" ]; then
    content=$(cat "$file")
  fi

  build_header > "$tmp"
  build_profile >> "$tmp"

  if [ "$file" = "$PUBLIC_DIR/index.html" ]; then
    post_list_html >> "$tmp"
  else
    echo '    <section id="wrapper" class="home">' >> "$tmp"
    echo "$content" >> "$tmp"
    echo '    </section>' >> "$tmp"
  fi

  build_footer >> "$tmp"
  mv "$tmp" "$file"
done

echo "Applied selfdotsend.com style shell to generated HTML."
