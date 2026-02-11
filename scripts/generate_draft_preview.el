;;; generate_draft_preview.el --- Build draft preview pages -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'seq)
(require 'json)
(require 'ox-html)

(defun sds/slugify (s)
  (let* ((lower (downcase (string-trim s)))
         (step1 (replace-regexp-in-string "[^a-z0-9]+" "-" lower))
         (step2 (replace-regexp-in-string "-+" "-" step1))
         (trimmed (replace-regexp-in-string "\\`-\\|-\\'" "" step2)))
    (if (string-empty-p trimmed) "draft" trimmed)))

(defun sds/html-escape (s)
  (let ((v (or s "")))
    (setq v (string-replace "&" "&amp;" v))
    (setq v (string-replace "<" "&lt;" v))
    (setq v (string-replace ">" "&gt;" v))
    (setq v (string-replace "\"" "&quot;" v))
    v))

(defun sds/parse-tag-list (raw)
  (let* ((trimmed (string-trim (or raw "")))
         (colon-style (and (string-prefix-p ":" trimmed)
                           (string-suffix-p ":" trimmed)))
         (pieces (if colon-style
                     (split-string trimmed ":" t "[[:space:]\n\r\t]+")
                   (split-string trimmed "[, ]+" t "[[:space:]\n\r\t]+"))))
    (delete-dups
     (seq-filter (lambda (it) (not (string-empty-p it)))
                 (mapcar (lambda (it) (string-trim (downcase it))) pieces)))))

(defun sds/extract-first-image (body)
  (let ((case-fold-search t))
    (when (and body
               (string-match "\\[\\[\\([^]\n]+\\(?:\\.png\\|\\.jpg\\|\\.jpeg\\|\\.gif\\|\\.webp\\|\\.svg\\)\\(?:[?#][^]\n]+\\)?\\)\\(?:\\]\\[[^]]*\\)?\\]\\]" body))
      (match-string 1 body))))

(defun sds/extract-first-image-from-html (html)
  (let ((case-fold-search t))
    (when (and html
               (string-match "<img[^>]+src=[\"']\\([^\"']+\\)[\"']" html))
      (match-string 1 html))))

(defun sds/estimate-read-mins (text)
  (let* ((clean (replace-regexp-in-string "\\[\\[[^]]+\\]\\[[^]]+\\]\\]" " " (or text "")))
         (words (split-string clean "[^[:alnum:]]+" t))
         (count (length words)))
    (max 1 (/ (+ count 199) 200))))

(defun sds/placeholder-description-p (desc)
  (let ((clean (downcase (string-trim (or desc "")))))
    (member clean '("" "draft post" "draft" "untitled draft" "start a new draft."))))

(defun sds/body-preview-text (body)
  (let* ((s (or body ""))
         (s (replace-regexp-in-string "^#\\+.*$" "" s))
         (s (replace-regexp-in-string "\\[\\[[^]]+\\]\\[\\([^]]+\\)\\]\\]" "\\1" s))
         (s (replace-regexp-in-string "\\[\\[[^]]+\\]\\]" "" s))
         (s (replace-regexp-in-string "\\`[[:space:]\n\r\t]+\\|[[:space:]\n\r\t]+\\'" "" s)))
    s))

(defun sds/excerpt (desc body)
  (let* ((source (if (sds/placeholder-description-p desc)
                     (sds/body-preview-text body)
                   desc))
         (flat (replace-regexp-in-string "[\n\r\t]+" " " (or source "")))
         (trimmed (string-trim flat)))
    (if (string-empty-p trimmed)
        "No draft content yet."
      (if (> (length trimmed) 180)
        (concat (substring trimmed 0 177) "...")
      trimmed))))

(defun sds/sort-items-by-date-desc (items)
  (sort (copy-sequence items)
        (lambda (a b)
          (let ((da (or (plist-get a :date) "0000-00-00"))
                (db (or (plist-get b :date) "0000-00-00")))
            (string> da db)))))

(defun sds/group-by-tags (items)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (item items)
      (let* ((tags (or (plist-get item :tags) '()))
             (primary (if (and tags (not (string-empty-p (car tags))))
                          (car tags)
                        "untagged")))
        (puthash primary (cons item (gethash primary table)) table)))
    (let (groups)
      (maphash
       (lambda (k v)
         (push (cons k (sds/sort-items-by-date-desc v)) groups))
       table)
      (sort groups
            (lambda (a b)
              (cond
               ((string= (car a) "untagged") nil)
               ((string= (car b) "untagged") t)
               (t (string< (car a) (car b)))))))))

(defun sds/collect-known-tags (blog-dir)
  (let ((acc '()))
    (dolist (dir-name '("posts" "drafts"))
      (let ((dir (expand-file-name dir-name blog-dir)))
        (when (file-directory-p dir)
          (dolist (f (directory-files dir t "\\.org\\'"))
            (let* ((parsed (sds/parse-org f))
                   (tags (plist-get parsed :tags)))
              (setq acc (append acc tags)))))))
    (delete-dups
     (seq-filter
      (lambda (tag)
        (and (not (string-empty-p (or tag "")))
             (not (string= tag "untagged"))))
      acc))))

(defun sds/org-file-to-editable-html (path)
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path)
        (let ((org-export-with-toc nil)
              (org-export-with-section-numbers nil)
              (org-export-with-author nil)
              (org-export-with-date nil)
              (org-export-with-title nil)
              (org-export-with-tags nil)
              (org-html-htmlize-output-type 'css))
          (org-export-string-as (buffer-string) 'html t
                                '(:with-toc nil
                                  :section-numbers nil
                                  :with-author nil
                                  :with-date nil
                                  :with-title nil
                                  :with-tags nil))))
    (error "")))

(defun sds/parse-org (path)
  (with-temp-buffer
    (insert-file-contents path)
    (let ((title (file-name-base path))
          (date "")
          (description "")
          (tags '())
          (body-lines '()))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ((string-match-p "\\`#\\+TITLE:" line)
            (setq title (string-trim (substring line (length "#+TITLE:")))))
           ((string-match-p "\\`#\\+DATE:" line)
            (setq date (string-trim (substring line (length "#+DATE:")))))
           ((string-match-p "\\`#\\+DESCRIPTION:" line)
            (setq description (string-trim (substring line (length "#+DESCRIPTION:")))))
           ((string-match-p "\\`#\\+FILETAGS:" line)
            (setq tags (append tags (sds/parse-tag-list (substring line (length "#+FILETAGS:"))))))
           ((string-match-p "\\`#\\+TAGS:" line)
            (setq tags (append tags (sds/parse-tag-list (substring line (length "#+TAGS:"))))))
           ((string-match-p "\\`#\\+KEYWORDS:" line)
            (setq tags (append tags (sds/parse-tag-list (substring line (length "#+KEYWORDS:"))))))
           ((string-match-p "\\`#\\+" line)
            nil)
           (t
            (push line body-lines))))
        (forward-line 1))
      (let* ((body (string-trim (string-join (nreverse body-lines) "\n")))
             (clean-tags (delete-dups (seq-filter (lambda (it) (not (string-empty-p it))) tags)))
             (html-body (sds/org-file-to-editable-html path))
             (image (or (sds/extract-first-image body)
                        (sds/extract-first-image-from-html html-body))))
        (list :title title
              :date date
              :description description
              :tags clean-tags
              :body body
              :html-body html-body
              :image image)))))

(defun sds/detail-page (item known-tags)
  (let* ((title (sds/html-escape (plist-get item :title)))
         (body-html (or (plist-get item :html-body) ""))
         (draft-path (sds/html-escape (plist-get item :draft-path)))
         (date (sds/html-escape (or (plist-get item :date) "")))
         (tags (or (plist-get item :tags) '()))
         (tags-csv (sds/html-escape (string-join tags ",")))
         (known-tags-json (json-encode (vconcat (delete-dups (append known-tags tags)))))
         (mins (number-to-string (or (plist-get item :read-mins) 1))))
    (format "<!doctype html>
<html lang=\"en\" data-theme=\"medium\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">
  <title>%s</title>
  <link rel=\"icon\" href=\"/media/images/logo.png\" type=\"image/png\">
  <link rel=\"stylesheet\" href=\"/media/css/style.css\" type=\"text/css\">
  <link rel=\"stylesheet\" href=\"/media/css/theme-medium.css\" type=\"text/css\">
  <style>
    body { background:#faf9f5; }
    #wrapper.post-page { max-width: 900px; }
    .draft-badge { font-size:17px; vertical-align:super; margin-left:3px; color:#37c9c7; font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; font-weight:400; letter-spacing:0; font-style:italic; }
    .nav-right { display:flex; align-items:center; gap:8px; flex-wrap:wrap; }
    .nav-right > a { color:#2d2b27; font-size:14px; font-weight:500; font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; padding:8px 10px; text-decoration:none; }
    .draft-write-action { margin-left:12px; display:inline-flex; align-items:center; gap:8px; text-decoration:none; padding:6px 8px; background:transparent; border-radius:8px; }
    .draft-write-action .icon { display:inline-flex; align-items:center; justify-content:center; width:20px; height:20px; border:1.5px solid #777; border-radius:3px; }
    .draft-write-action .label { font:500 14px/1.2 -apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; color:#666; }
    .draft-editable { min-height: 40vh; }
    .draft-editable[contenteditable=\"true\"]:focus { outline: none; }
    #draft-title[contenteditable=\"true\"]:focus { outline: none; box-shadow: none; border: 0; }
    .draft-editable p { margin-bottom: 20px; }
    .draft-inline-tags { display:inline-flex; align-items:center; gap:8px; margin-left:8px; flex-wrap:wrap; }
    .draft-tag-pill { background:#ece7dc; color:#4f4a3f; border-radius:999px; padding:3px 10px; font:600 12px/1.2 -apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; }
    .draft-tag-pill.untagged { opacity:.9; }
    .draft-tag-add { border:1px solid #d8d0c3; background:#fff; color:#4f4a3f; border-radius:999px; padding:3px 10px; font:600 12px/1.2 -apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; cursor:pointer; }
    .draft-tag-pop { position:fixed; z-index:10005; width:min(360px, calc(100vw - 24px)); background:#fff; border:1px solid #ddd7ca; border-radius:12px; box-shadow:0 16px 36px rgba(0,0,0,.18); padding:10px; display:none; }
    .draft-tag-pop.open { display:block; }
    .draft-tag-grid { display:flex; flex-wrap:wrap; gap:8px; max-height:180px; overflow-y:auto; overflow-x:hidden; margin-bottom:10px; padding:2px 2px 8px; align-content:flex-start; }
    .draft-tag-choice { border:1px solid #d8d0c3; background:#fff; color:#4f4a3f; border-radius:999px; padding:6px 14px; font:600 12px/1.35 -apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; cursor:pointer; }
    .draft-tag-choice.active { border-color:#36c9c7; color:#0f5f4f; background:#ebfbfa; }
    .draft-tag-actions { display:flex; align-items:center; gap:8px; }
    .draft-tag-new-btn, .draft-tag-save-btn { border:1px solid #d8d0c3; background:#fff; color:#4f4a3f; border-radius:8px; padding:8px 12px; font:600 12px/1.3 -apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; cursor:pointer; }
    .draft-tag-save-btn { border-color:#36c9c7; color:#0f5f4f; }
    .draft-tag-new-input { flex:1; min-width:0; border:1px solid #d8d0c3; border-radius:8px; padding:9px 11px; font:500 13px/1.3 -apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; display:none; }
    .draft-tag-new-input.open { display:block; }
    .post-author-name-row { display:flex; align-items:center; gap:0; flex-wrap:wrap; }
    .draft-tools { position: fixed; z-index: 9999; display:none; gap:6px; align-items:center; background:#1f1f1f; border-radius:12px; padding:8px; box-shadow: 0 12px 22px rgba(0,0,0,.2); }
    .draft-tools button { border:none; background:transparent; color:#fff; border-radius:8px; padding:8px 10px; font-weight:700; font-size:13px; min-width:34px; }
    .draft-tools button:hover { background: rgba(255,255,255,.14); }
    .draft-plus { position: fixed; z-index: 9998; display:none; align-items:center; gap:8px; pointer-events:auto; }
    .draft-plus-toggle { width:25px; height:25px; border-radius:50%%; border:1px solid #36c9c7; background:#fff; color:#36c9c7; display:inline-flex; align-items:center; justify-content:center; padding:0; box-shadow:0 2px 6px rgba(0,0,0,.08); }
    .draft-plus-toggle:hover { background:#f5fffe; }
    .draft-plus-toggle svg { width:14px; height:14px; fill: currentColor; }
    .draft-plus-menu { display:none; align-items:center; gap:8px; }
    .draft-plus.open .draft-plus-menu { display:inline-flex; }
    .draft-plus-menu button { width:36px; height:36px; border-radius:50%%; border:1px solid #0f5f4f; background:#fff; color:#0f5f4f; display:inline-flex; align-items:center; justify-content:center; padding:0; }
    .draft-plus-menu button svg { width:18px; height:18px; stroke:currentColor; fill:none; stroke-width:1.8; stroke-linecap:round; stroke-linejoin:round; }
  </style>
</head>
<body class=\"container\">
<nav class=\"main-nav medium-nav\">
  <div class=\"nav-left site-brand\">
    <a class=\"brand-logo-link\" href=\"/\"><img class=\"brand-logo\" src=\"/media/images/logo.png\" alt=\"Self dot send\"></a>
    <div class=\"brand-copy\"><a class=\"brand-title\" href=\"/\">Selfd<span class=\"brand-o\">o</span>tsend<sup class=\"draft-badge\">Draft</sup></a><div class=\"brand-tagline\">Message passing is just a procedure call.</div></div>
  </div>
  <div class=\"nav-right\">
    <a href=\"/\">Blog</a>
    <a href=\"/drafts/\">Drafts</a>
    <a href=\"/archive/\">Archive</a>
    <a href=\"/tags/\">Tag</a>
    <a href=\"/about/\">About</a>
    <a href=\"/nano-chat/\">Nano Chat</a>
    <button id=\"site-search-open\" class=\"search-btn\" type=\"button\" aria-haspopup=\"dialog\" aria-controls=\"site-search-overlay\">
      <svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" aria-hidden=\"true\">
        <circle cx=\"11\" cy=\"11\" r=\"7\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"></circle>
        <line x1=\"16.65\" y1=\"16.65\" x2=\"21\" y2=\"21\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\"></line>
      </svg>
      <span>Search</span>
      <kbd>/</kbd>
    </button>
    <button id=\"theme-switch\" class=\"theme-switch\" type=\"button\">Theme: Medium</button>
    <a class=\"cta\" href=\"https://github.com/metacritical\">
      <svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 512 499.36\" focusable=\"false\"><title>GitHub</title><path d=\"M256 0C114.64 0 0 114.61 0 256c0 113.09 73.34 209 175.08 242.9 12.8 2.35 17.47-5.56 17.47-12.34 0-6.08-.22-22.18-.35-43.54-71.2 15.49-86.2-34.34-86.2-34.34-11.64-29.57-28.42-37.45-28.42-37.45-23.27-15.84 1.73-15.55 1.73-15.55 25.69 1.81 39.21 26.38 39.21 26.38 22.84 39.12 59.92 27.82 74.5 21.27 2.33-16.54 8.94-27.82 16.25-34.22-56.84-6.43-116.6-28.43-116.6-126.49 0-27.95 10-50.8 26.35-68.69-2.63-6.48-11.42-32.5 2.51-67.75 0 0 21.49-6.88 70.4 26.24a242.65 242.65 0 0 1 128.18 0c48.87-33.13 70.33-26.24 70.33-26.24 14 35.25 5.18 61.27 2.55 67.75 16.41 17.9 26.31 40.75 26.31 68.69 0 98.35-59.85 120-116.88 126.32 9.19 7.9 17.38 23.53 17.38 47.41 0 34.22-.31 61.83-.31 70.23 0 6.85 4.61 14.81 17.6 12.31C438.72 464.97 512 369.08 512 256.02 512 114.62 397.37 0 256 0z\" fill=\"currentColor\" fill-rule=\"evenodd\"></path></svg>
    </a>
    <a class=\"draft-write-action\" href=\"/editor\" aria-label=\"Write\" title=\"Write\"><span class=\"icon\"><svg width=\"13\" height=\"13\" viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path d=\"M3 17.25V21h3.75L19.81 7.94l-3.75-3.75L3 17.25zm18-11.5a1 1 0 0 0 0-1.41L19.66 3a1 1 0 0 0-1.41 0l-1.59 1.59 3.75 3.75L21 5.75z\" fill=\"#666\"></path></svg></span><span class=\"label\">Write</span></a>
  </div>
</nav>
<div id=\"site-search-overlay\" class=\"site-search-overlay\" hidden>
  <div class=\"site-search-panel\" role=\"dialog\" aria-modal=\"true\" aria-labelledby=\"site-search-title\">
    <div class=\"site-search-top\">
      <h2 id=\"site-search-title\">Search posts</h2>
      <button id=\"site-search-close\" class=\"site-search-close\" type=\"button\" aria-label=\"Close search\">Close</button>
    </div>
    <input id=\"site-search-input\" class=\"site-search-input\" type=\"search\" placeholder=\"Type to search posts...\" autocomplete=\"off\" />
    <p class=\"site-search-hint\">Use <kbd>/</kbd> to open, <kbd>↑</kbd><kbd>↓</kbd> to navigate, <kbd>Enter</kbd> to open.</p>
    <ul id=\"site-search-results\" class=\"site-search-results\"></ul>
  </div>
</div>
<section id=\"wrapper\" class=\"post-page\">
  <div class=\"post\">
    <aside class=\"story-rail\" aria-label=\"Story actions\">
      <button class=\"story-clap-btn\" type=\"button\" aria-label=\"Clap for this story\"><img src=\"/media/images/clap.png\" alt=\"\" aria-hidden=\"true\"></button>
      <span class=\"story-clap-count\">0</span>
    </aside>
    <h1 class=\"title\" id=\"draft-title\" contenteditable=\"true\">%s</h1>
    <div class=\"post-author-row\"><img class=\"post-author-avatar\" src=\"/media/images/avatar.jpg\" alt=\"Pankaj Doharey\"><div class=\"post-author-meta\"><span class=\"post-author-name-row\"><span class=\"post-author-name\">Pankaj Doharey</span><span id=\"draft-tags-inline\" class=\"draft-inline-tags\"></span><button id=\"draft-add-tag\" class=\"draft-tag-add\" type=\"button\">+ Tag</button></span><span class=\"post-author-date\">%s · %s min read</span></div></div>
    <div id=\"draft-body\" class=\"draft-editable\" contenteditable=\"true\" data-target-path=\"%s\" data-tags=\"%s\">%s</div>
  </div>
</section>

<div class=\"draft-tools\" id=\"draft-tools\">
  <button type=\"button\" data-cmd=\"bold\"><b>B</b></button>
  <button type=\"button\" data-cmd=\"italic\"><span style=\"font-style:italic; font-family:Georgia,serif;\">i</span></button>
  <button type=\"button\" data-action=\"link\">Link</button>
  <button type=\"button\" data-action=\"highlight\">HL</button>
  <button type=\"button\" data-action=\"clear\">Clr</button>
</div>
<div class=\"draft-plus\" id=\"draft-plus\">
  <button class=\"draft-plus-toggle\" id=\"draft-plus-toggle\" type=\"button\" title=\"Add an image, video, embed, or new part\" aria-label=\"Add an image, video, embed, or new part\"><svg viewBox=\"0 0 25 25\" aria-hidden=\"true\"><path d=\"M20 12h-7V5h-1v7H5v1h7v7h1v-7h7\" fill-rule=\"evenodd\"></path></svg></button>
  <div class=\"draft-plus-menu\" id=\"draft-plus-menu\">
    <button type=\"button\" data-action=\"image\" title=\"Image\" aria-label=\"Image\">
      <svg viewBox=\"0 0 24 24\" aria-hidden=\"true\"><rect x=\"3.5\" y=\"5\" width=\"17\" height=\"14\" rx=\"2\"></rect><circle cx=\"9\" cy=\"10\" r=\"1.6\"></circle><path d=\"M4.5 17l5.2-4.8 3.7 3.1 2.6-2.2 3.5 3.9\"></path></svg>
    </button>
    <button type=\"button\" data-action=\"upload\" title=\"Upload\" aria-label=\"Upload\">
      <svg viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path d=\"M12 16V5\"></path><path d=\"M8.4 8.6 12 5l3.6 3.6\"></path><path d=\"M5 17.5v1a1.5 1.5 0 0 0 1.5 1.5h11A1.5 1.5 0 0 0 19 18.5v-1\"></path></svg>
    </button>
    <button type=\"button\" data-action=\"video\" title=\"Video\" aria-label=\"Video\">
      <svg viewBox=\"0 0 24 24\" aria-hidden=\"true\"><rect x=\"3.5\" y=\"6\" width=\"12\" height=\"12\" rx=\"2\"></rect><path d=\"M15.5 10.2 20 7.8v8.4l-4.5-2.4z\"></path></svg>
    </button>
    <button type=\"button\" data-action=\"embed\" title=\"Embed\" aria-label=\"Embed\">
      <svg viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path d=\"M9 8 5 12l4 4\"></path><path d=\"M15 8l4 4-4 4\"></path></svg>
    </button>
    <button type=\"button\" data-action=\"code\" title=\"Code\" aria-label=\"Code\">
      <svg viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path d=\"M9 7 6 12l3 5\"></path><path d=\"M15 7l3 5-3 5\"></path><path d=\"M13.5 5.5 10.5 18.5\"></path></svg>
    </button>
  </div>
  <input id=\"draft-upload\" type=\"file\" accept=\"image/*\" hidden>
</div>
<div class=\"draft-tag-pop\" id=\"draft-tag-pop\">
  <div class=\"draft-tag-grid\" id=\"draft-tag-grid\"></div>
  <div class=\"draft-tag-actions\">
    <button type=\"button\" class=\"draft-tag-new-btn\" id=\"draft-tag-new-btn\">+ New</button>
    <input type=\"text\" class=\"draft-tag-new-input\" id=\"draft-tag-new-input\" placeholder=\"new-tag\">
    <button type=\"button\" class=\"draft-tag-save-btn\" id=\"draft-tag-save-btn\">Save</button>
  </div>
</div>

<script>
(function(){
  const body = document.getElementById('draft-body');
  const title = document.getElementById('draft-title');
  const tools = document.getElementById('draft-tools');
  const plus = document.getElementById('draft-plus');
  const plusToggle = document.getElementById('draft-plus-toggle');
  const plusMenu = document.getElementById('draft-plus-menu');
  const tagsInline = document.getElementById('draft-tags-inline');
  const addTagBtn = document.getElementById('draft-add-tag');
  const tagPop = document.getElementById('draft-tag-pop');
  const tagGrid = document.getElementById('draft-tag-grid');
  const tagNewBtn = document.getElementById('draft-tag-new-btn');
  const tagNewInput = document.getElementById('draft-tag-new-input');
  const tagSaveBtn = document.getElementById('draft-tag-save-btn');
  const upload = document.getElementById('draft-upload');
  const availableTags = %s;
  let targetPath = body.dataset.targetPath || '';
  let draftTags = (body.dataset.tags || '').split(',').map(t => t.trim().toLowerCase()).filter(Boolean);
  let saveTimer = null;

  function normalizeTag(tag){
    return (tag || '').trim().toLowerCase().replace(/[^a-z0-9-_ ]+/g,'').replace(/\\s+/g,'-').replace(/-+/g,'-').replace(/^-+|-+$/g,'');
  }

  function renderInlineTags(){
    tagsInline.innerHTML = '';
    if (!draftTags.length) {
      const pill = document.createElement('span');
      pill.className = 'draft-tag-pill untagged';
      pill.textContent = 'untagged';
      tagsInline.appendChild(pill);
      return;
    }
    draftTags.forEach((tag) => {
      const pill = document.createElement('span');
      pill.className = 'draft-tag-pill';
      pill.textContent = tag;
      tagsInline.appendChild(pill);
    });
  }

  function openTagPopover(){
    const rect = addTagBtn.getBoundingClientRect();
    tagPop.style.left = Math.max(8, Math.min(window.innerWidth - tagPop.offsetWidth - 8, rect.left)) + 'px';
    tagPop.style.top = Math.min(window.innerHeight - 12, rect.bottom + 8) + 'px';
    tagPop.classList.add('open');
    renderTagChoices();
  }

  function closeTagPopover(){
    tagPop.classList.remove('open');
    tagNewInput.classList.remove('open');
    tagNewInput.value = '';
  }

  function renderTagChoices(){
    const sorted = [...new Set(availableTags.map(normalizeTag).filter(Boolean))].sort();
    tagGrid.innerHTML = '';
    if (!sorted.length) {
      const empty = document.createElement('span');
      empty.style.cssText = 'font:500 12px/1.3 -apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif;color:#8a8478;';
      empty.textContent = 'No tags yet';
      tagGrid.appendChild(empty);
      return;
    }
    sorted.forEach((tag) => {
      const b = document.createElement('button');
      b.type = 'button';
      b.className = 'draft-tag-choice' + (draftTags.includes(tag) ? ' active' : '');
      b.textContent = tag;
      b.addEventListener('click', () => {
        if (draftTags.includes(tag)) {
          draftTags = draftTags.filter(t => t !== tag);
        } else {
          draftTags.push(tag);
        }
        draftTags = draftTags.filter(t => t !== 'untagged');
        renderInlineTags();
        renderTagChoices();
        scheduleSave();
      });
      tagGrid.appendChild(b);
    });
  }
  function inBodySelection(range){
    if (!range) return false;
    return body.contains(range.commonAncestorContainer);
  }

  function caretRectForRange(range){
    if (!range) return null;
    const clone = range.cloneRange();
    let rect = clone.getBoundingClientRect();
    if (rect && (rect.width || rect.height)) return rect;

    const marker = document.createElement('span');
    marker.textContent = '\\u200b';
    marker.style.display = 'inline-block';
    marker.style.width = '1px';
    marker.style.height = '1em';
    clone.insertNode(marker);
    rect = marker.getBoundingClientRect();
    clone.setStartAfter(marker);
    clone.collapse(true);
    const sel = window.getSelection();
    if (sel) {
      sel.removeAllRanges();
      sel.addRange(clone);
    }
    marker.remove();
    return rect;
  }

  function nearestBlock(node){
    let cur = node && (node.nodeType === Node.TEXT_NODE ? node.parentElement : node);
    while (cur && cur !== body) {
      if (cur.matches && cur.matches('p,div,h1,h2,h3,blockquote,pre,figure,li')) return cur;
      cur = cur.parentElement;
    }
    return null;
  }

  function placeToolsForSelection(range){
    const rect = range ? range.getBoundingClientRect() : null;
    if (!rect || (!rect.width && !rect.height)) { tools.style.display='none'; return; }
    tools.style.display='inline-flex';
    tools.style.left = Math.max(8, rect.left + rect.width/2 - tools.offsetWidth/2) + 'px';
    tools.style.top = Math.max(8, rect.top - tools.offsetHeight - 10) + 'px';
  }

  function placePlusAtCaret(range){
    const caretRect = caretRectForRange(range);
    if (!caretRect) { plus.style.display='none'; plus.classList.remove('open'); return; }
    const block = nearestBlock(range.startContainer);
    const blockRect = block ? block.getBoundingClientRect() : caretRect;
    const rail = document.querySelector('.story-rail');
    const toggleSize = plusToggle ? plusToggle.offsetWidth || 25 : 25;
    let left = blockRect.left - toggleSize - 14;
    if (rail) {
      const railRect = rail.getBoundingClientRect();
      left = Math.max(left, railRect.right + 10);
    }
    left = Math.max(8, Math.min(left, window.innerWidth - toggleSize - 8));
    const top = Math.max(8, blockRect.top + Math.max(0, (blockRect.height - toggleSize) / 2));
    plus.style.display='inline-flex';
    plus.style.left = Math.round(left) + 'px';
    plus.style.top = Math.round(top) + 'px';
  }

  function updateFloatingUi(){
    const sel = window.getSelection();
    if (!sel || sel.rangeCount === 0) {
      tools.style.display='none';
      plus.style.display='none';
      plus.classList.remove('open');
      return;
    }
    const range = sel.getRangeAt(0);
    if (!inBodySelection(range)) {
      tools.style.display='none';
      plus.style.display='none';
      plus.classList.remove('open');
      return;
    }
    if (sel.isCollapsed) {
      tools.style.display='none';
      placePlusAtCaret(range);
      return;
    }
    plus.style.display='none';
    plus.classList.remove('open');
    placeToolsForSelection(range);
  }

  function htmlToOrgNode(node){
    if (!node) return '';
    if (node.nodeType === Node.TEXT_NODE) return node.textContent;
    if (node.nodeType !== Node.ELEMENT_NODE) return '';
    const tag = node.tagName.toLowerCase();
    if (tag === 'h2') return '\\n** ' + node.textContent.trim() + '\\n';
    if (tag === 'h3') return '\\n*** ' + node.textContent.trim() + '\\n';
    if (tag === 'blockquote') return '\\n#+BEGIN_QUOTE\\n' + node.textContent.trim() + '\\n#+END_QUOTE\\n';
    if (tag === 'pre') {
      const lang = (node.dataset.lang || '').trim() || 'text';
      return '\\n#+BEGIN_SRC ' + lang + '\\n' + node.innerText.replace(/\\n+$/,'') + '\\n#+END_SRC\\n';
    }
    if (tag === 'figure' && node.classList.contains('image-block')) {
      const img = node.querySelector('img');
      const cap = node.querySelector('figcaption');
      if (!img) return '';
      const alt = (cap ? cap.textContent.trim() : (img.getAttribute('alt') || '')).trim();
      return '\\n[[' + (img.getAttribute('src') || '') + '][' + alt + ']]\\n';
    }
    if (tag === 'figure' && (node.classList.contains('video-embed') || node.classList.contains('embed-card'))) {
      return '\\n#+BEGIN_EXPORT html\\n' + node.outerHTML + '\\n#+END_EXPORT\\n';
    }
    if (tag === 'div' || tag === 'p') return '\\n' + node.textContent.trim() + '\\n';
    let out = '';
    node.childNodes.forEach(ch => { out += htmlToOrgNode(ch); });
    return out;
  }

  function htmlToOrg(){
    let bodyOrg = '';
    body.childNodes.forEach(n => { bodyOrg += htmlToOrgNode(n); });
    bodyOrg = bodyOrg.replace(/\\n{3,}/g, '\\n\\n').trim();
    const t = (title.innerText || '').trim() || 'Untitled Draft';
    return { title: t, body: bodyOrg };
  }

  function saveDraft(){
    const payload = htmlToOrg();
    fetch('/editor/api/save', {
      method: 'POST',
      headers: {'Content-Type':'application/json'},
      body: JSON.stringify({ title: payload.title, body: payload.body, mode: 'draft', targetPath, tags: draftTags })
    }).then(r => r.json()).then((resp)=>{
      if (resp && resp.ok && resp.path) {
        targetPath = resp.path;
        body.dataset.targetPath = resp.path;
      }
    }).catch(()=>{});
  }

  function scheduleSave(){
    clearTimeout(saveTimer);
    saveTimer = setTimeout(saveDraft, 1200);
  }

  function insertHtmlAtCursor(html){
    const sel = window.getSelection();
    if (!sel || sel.rangeCount === 0) return;
    const range = sel.getRangeAt(0);
    range.deleteContents();
    const wrap = document.createElement('div');
    wrap.innerHTML = html;
    const frag = document.createDocumentFragment();
    let node; let last = null;
    while ((node = wrap.firstChild)) { last = frag.appendChild(node); }
    range.insertNode(frag);
    if (last) {
      range.setStartAfter(last); range.collapse(true);
      sel.removeAllRanges(); sel.addRange(range);
    }
    scheduleSave();
  }

  tools.addEventListener('click', (e)=>{
    const btn = e.target.closest('button');
    if (!btn) return;
    const cmd = btn.dataset.cmd;
    const action = btn.dataset.action;
    if (cmd) { document.execCommand(cmd, false, null); scheduleSave(); updateFloatingUi(); return; }
    if (action === 'link') {
      const url = prompt('URL');
      if (url) { document.execCommand('createLink', false, url); scheduleSave(); }
    }
    if (action === 'highlight') { document.execCommand('hiliteColor', false, '#ffea55'); scheduleSave(); }
    if (action === 'clear') { document.execCommand('removeFormat', false, null); scheduleSave(); }
  });

  plusToggle.addEventListener('click', ()=>{
    plus.classList.toggle('open');
    updateFloatingUi();
  });

  addTagBtn.addEventListener('click', (e)=>{
    e.stopPropagation();
    if (tagPop.classList.contains('open')) closeTagPopover();
    else openTagPopover();
  });

  tagNewBtn.addEventListener('click', ()=>{
    tagNewInput.classList.add('open');
    tagNewInput.focus();
  });

  tagSaveBtn.addEventListener('click', ()=>{
    const tag = normalizeTag(tagNewInput.value);
    if (!tag) return;
    if (!availableTags.includes(tag)) availableTags.push(tag);
    if (!draftTags.includes(tag)) draftTags.push(tag);
    draftTags = draftTags.filter(t => t !== 'untagged');
    tagNewInput.value = '';
    tagNewInput.classList.remove('open');
    renderInlineTags();
    renderTagChoices();
    scheduleSave();
  });

  tagNewInput.addEventListener('keydown', (e)=>{
    if (e.key === 'Enter') {
      e.preventDefault();
      tagSaveBtn.click();
    }
  });

  document.addEventListener('click', (e)=>{
    if (!tagPop.classList.contains('open')) return;
    if (tagPop.contains(e.target) || addTagBtn.contains(e.target)) return;
    closeTagPopover();
  });

  plusMenu.addEventListener('click', async (e)=>{
    const btn = e.target.closest('button');
    if (!btn) return;
    const action = btn.dataset.action;
    if (action === 'image') {
      const url = prompt('Image URL');
      if (url) insertHtmlAtCursor('<figure class=\"image-block\"><img src=\"' + url + '\" alt=\"\"><figcaption></figcaption></figure><p><br></p>');
      return;
    }
    if (action === 'upload') { upload.click(); return; }
    if (action === 'video') {
      const url = prompt('Video embed URL');
      if (url) insertHtmlAtCursor('<figure class=\"video-embed\"><iframe src=\"' + url + '\" loading=\"lazy\" allowfullscreen></iframe></figure><p><br></p>');
      return;
    }
    if (action === 'embed') {
      const url = prompt('Link URL');
      if (!url) return;
      try {
        const r = await fetch('/editor/api/fetch-meta', { method:'POST', headers:{'Content-Type':'application/json'}, body: JSON.stringify({url}) });
        const j = await r.json();
        const t = j.title || url, d = j.description || '', s = j.site || '', img = j.image || '';
        insertHtmlAtCursor('<figure class=\"embed-card\"><a class=\"embed-card-link\" href=\"'+url+'\"><div class=\"embed-card-copy\"><h4>'+t+'</h4><p>'+d+'</p><small>'+s+'</small></div>' + (img ? '<img src=\"'+img+'\" alt=\"'+t+'\">' : '') + '</a></figure><p><br></p>');
      } catch (_) {}
      return;
    }
    if (action === 'code') {
      const lang = (prompt('Language (e.g. python, sh, js)') || 'text').trim();
      insertHtmlAtCursor('<pre class=\"code-block\" data-lang=\"'+lang+'\">\\n</pre><p><br></p>');
      return;
    }
  });

  upload.addEventListener('change', async ()=>{
    const file = upload.files && upload.files[0];
    if (!file) return;
    try {
      const reader = new FileReader();
      const dataUrl = await new Promise((resolve, reject)=>{ reader.onload = ()=>resolve(reader.result); reader.onerror = reject; reader.readAsDataURL(file); });
      const r = await fetch('/editor/api/upload', { method:'POST', headers:{'Content-Type':'application/json'}, body: JSON.stringify({ filename: file.name, dataUrl }) });
      const j = await r.json();
      if (j && j.ok && j.url) insertHtmlAtCursor('<figure class=\"image-block\"><img src=\"'+j.url+'\" alt=\"'+file.name+'\"><figcaption>'+file.name+'</figcaption></figure><p><br></p>');
    } catch (_) {}
    upload.value = '';
  });

  document.addEventListener('selectionchange', ()=>{ setTimeout(updateFloatingUi, 0); });
  body.addEventListener('keyup', ()=>{ updateFloatingUi(); scheduleSave(); });
  body.addEventListener('mouseup', updateFloatingUi);
  body.addEventListener('click', updateFloatingUi);
  body.addEventListener('focus', updateFloatingUi);
  body.addEventListener('input', scheduleSave);
  title.addEventListener('input', scheduleSave);
  renderInlineTags();

  setInterval(saveDraft, 5000);
})();
</script>
<script src=\"/media/js/main.js\"></script>
<script src=\"/media/js/theme-switcher.js\"></script>
<script src=\"/media/js/search.js\"></script>
</body>
</html>\n"
            title title date mins draft-path tags-csv body-html known-tags-json)))

(defun sds/card-html (item)
  (let* ((slug (sds/html-escape (plist-get item :slug)))
         (title (sds/html-escape (plist-get item :title)))
         (desc (sds/html-escape (or (plist-get item :excerpt) "")))
         (date (sds/html-escape (or (plist-get item :date) "")))
         (mins (number-to-string (or (plist-get item :read-mins) 1)))
         (image (plist-get item :image))
         (image-html
          (if (and image (not (string-empty-p image)))
              (format "<img src=\"%s\" alt=\"%s\">" (sds/html-escape image) title)
            "<img src=\"/media/images/placeholder.png\" alt=\"Self dot send placeholder\">")))
    (format "<article class=\"draft-card medium-post-card\">
  <a class=\"draft-link medium-post-main\" href=\"/drafts/%s/\">
    <h2>%s</h2>
    <p>%s</p>
    <div class=\"draft-meta-row\">
      <div class=\"medium-post-meta\">%s · %s min read</div>
      <span class=\"draft-bookmark-icon\" aria-hidden=\"true\"><svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" fill=\"none\" viewBox=\"0 0 24 24\"><path fill=\"#000\" d=\"M17.5 1.25a.5.5 0 0 1 1 0v2.5H21a.5.5 0 0 1 0 1h-2.5v2.5a.5.5 0 0 1-1 0v-2.5H15a.5.5 0 0 1 0-1h2.5zm-11 4.5a1 1 0 0 1 1-1H11a.5.5 0 0 0 0-1H7.5a2 2 0 0 0-2 2v14a.5.5 0 0 0 .8.4l5.7-4.4 5.7 4.4a.5.5 0 0 0 .8-.4v-8.5a.5.5 0 0 0-1 0v7.48l-5.2-4a.5.5 0 0 0-.6 0l-5.2 4z\"></path></svg></span>
    </div>
  </a>
  <a class=\"draft-thumb medium-post-thumb\" href=\"/drafts/%s/\">%s</a>
</article>"
            slug title desc date mins slug image-html)))

(defun sds/index-page (items)
  (let* ((groups (sds/group-by-tags items))
         (body
          (mapconcat
           (lambda (group)
             (let ((tag (car group))
                   (tag-items (cdr group)))
               (format "<section class=\"draft-group\">
  <div class=\"group-head\"><h2>%s</h2></div>
  <div class=\"draft-grid\">%s</div>
</section>"
                       (capitalize (sds/html-escape tag))
                       (mapconcat #'sds/card-html tag-items "\n"))))
           groups
           "\n")))
    (format "<!doctype html>
<html lang=\"en\" data-theme=\"medium\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">
  <title>Draft Preview</title>
  <link rel=\"icon\" href=\"/media/images/logo.png\" type=\"image/png\">
  <link rel=\"stylesheet\" href=\"/media/css/style.css\" type=\"text/css\">
  <link rel=\"stylesheet\" href=\"/media/css/theme-medium.css\" type=\"text/css\">
  <style>
    body { margin: 0; background: #f7f6f2; color: #1d1b18; }
    nav.main-nav { padding:12px 22px; background:#faf9f5; margin:0 auto; display:flex; justify-content:space-between; align-items:center; gap:10px; border-bottom:1px solid #e6e1d5; }
    .nav-left.site-brand { display:flex; align-items:center; gap:12px; min-width:0; }
    .brand-logo { width:50px; height:50px; border-radius:50%%; flex:0 0 50px; }
    .brand-copy { display:flex; flex-direction:column; justify-content:center; min-width:0; }
    .brand-title { color:#1f1f1b; font-family:\"Charter\",\"Iowan Old Style\",\"Palatino Linotype\",Palatino,\"Times New Roman\",serif; font-size:3.1em; font-weight:500; line-height:1; letter-spacing:-0.02em; white-space:nowrap; text-decoration:none; }
    .brand-o { color:#36c9c7; }
    .draft-badge { font-size:17px; vertical-align:super; margin-left:3px; color:#37c9c7; font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; font-weight:400; letter-spacing:0; font-style:italic; }
    .brand-tagline { font-size:12px; line-height:1.2; color:#7b7569; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
    .nav-right { display:flex; align-items:center; gap:8px; flex-wrap:wrap; }
    .nav-right > a { color:#2d2b27; font-size:14px; font-weight:500; padding:8px 10px; text-decoration:none; }
    .draft-write-action { margin-left:12px; display:inline-flex; align-items:center; gap:8px; text-decoration:none; padding:6px 8px; background:transparent; border-radius:8px; }
    .draft-write-action .icon { display:inline-flex; align-items:center; justify-content:center; width:20px; height:20px; border:1.5px solid #777; border-radius:3px; }
    .draft-write-action .label { font:500 14px/1.2 -apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; color:#666; }
    #wrapper.post-page { max-width: 980px; padding: 18px 30px 64px; }
    #wrapper.post-page .post { margin-top: 34px; }
    .medium-home { max-width: none !important; margin: 0 !important; padding: 0 !important; }
    .draft-preview-head { margin-bottom: 22px; }
    .draft-preview-head h1.title { margin-bottom:8px; }
    .lead { margin: 0 0 10px; color:#6a655a; font-size: 24px; line-height:1.35; font-family: \"Iowan Old Style\", \"Palatino Linotype\", Palatino, \"Times New Roman\", serif; }
    .group-head { display:flex; align-items:center; justify-content:space-between; border-bottom:1px solid #dfd8cc; margin-bottom:12px; }
    .group-head h2 { margin: 0 0 -1px; display:inline-block; font-size: 24px; line-height: 1.1; border-bottom:2px solid #b6b0a4; padding-bottom: 6px; font-family: ui-sans-serif,-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Helvetica,Arial,sans-serif; }
    .draft-group { margin: 0 0 28px; }
    .draft-grid { display:block; }
    .draft-card { border:0; border-radius:0; background:transparent; padding:0; }
    .draft-card.medium-post-card { display:grid; grid-template-columns: 1fr 220px; gap: 26px; align-items: center; padding: 34px 0; border-bottom: 1px solid #ece8dd; }
    .draft-link.medium-post-main { color:inherit; text-decoration:none; display:block; min-width:0; }
    .draft-link.medium-post-main h2 { margin: 0 0 10px; font-size: 38px; line-height: 1.12; letter-spacing: -0.02em; font-family: ui-sans-serif,-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Helvetica,Arial,sans-serif; color:#1f1f1b; display:-webkit-box; -webkit-line-clamp:2; -webkit-box-orient:vertical; overflow:hidden; }
    .draft-link.medium-post-main p { margin: 0 0 12px; font-size: 25px; line-height: 1.28; color:#5c5a52; display:-webkit-box; -webkit-line-clamp:2; -webkit-box-orient:vertical; overflow:hidden; }
    .draft-meta-row { display:flex; align-items:center; justify-content:space-between; gap:12px; }
    .draft-link .medium-post-meta { font-size: 18px; color:#4b4740; font-weight: 600; letter-spacing: 0.01em; }
    .draft-bookmark-icon { display:inline-flex; align-items:center; justify-content:center; width:24px; height:24px; opacity:.85; flex:0 0 24px; }
    .draft-bookmark-icon svg { width:24px; height:24px; display:block; }
    .draft-thumb.medium-post-thumb { display:block; min-width:0; }
    .draft-thumb.medium-post-thumb img { width:100%%; aspect-ratio:16/10; object-fit:cover; border-radius:8px; border:1px solid #ddd7ca; display:block; }
    @media (max-width: 960px) {
      #wrapper.post-page { padding: 14px 20px 48px; }
      .draft-card.medium-post-card { grid-template-columns: 1fr; gap: 14px; }
      .draft-link.medium-post-main h2 { font-size: 32px; }
      .draft-link.medium-post-main p { font-size: 21px; }
      .lead { font-size:20px; }
    }
  </style>
</head>
<body>
<nav class=\"main-nav medium-nav\">
  <div class=\"nav-left site-brand\">
    <a class=\"brand-logo-link\" href=\"/\"><img class=\"brand-logo\" src=\"/media/images/logo.png\" alt=\"Self dot send\"></a>
    <div class=\"brand-copy\"><a class=\"brand-title\" href=\"/\">Selfd<span class=\"brand-o\">o</span>tsend<sup class=\"draft-badge\">Draft</sup></a><div class=\"brand-tagline\">Message passing is just a procedure call.</div></div>
  </div>
  <div class=\"nav-right\">
    <a href=\"/\">Blog</a>
    <a href=\"/drafts/\">Drafts</a>
    <a href=\"/archive/\">Archive</a>
    <a href=\"/tags/\">Tag</a>
    <a href=\"/about/\">About</a>
    <a href=\"/nano-chat/\">Nano Chat</a>
    <button id=\"site-search-open\" class=\"search-btn\" type=\"button\" aria-haspopup=\"dialog\" aria-controls=\"site-search-overlay\">
      <svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" aria-hidden=\"true\">
        <circle cx=\"11\" cy=\"11\" r=\"7\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"></circle>
        <line x1=\"16.65\" y1=\"16.65\" x2=\"21\" y2=\"21\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\"></line>
      </svg>
      <span>Search</span>
      <kbd>/</kbd>
    </button>
    <button id=\"theme-switch\" class=\"theme-switch\" type=\"button\">Theme: Medium</button>
    <a class=\"cta\" href=\"https://github.com/metacritical\">
      <svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 512 499.36\" focusable=\"false\"><title>GitHub</title><path d=\"M256 0C114.64 0 0 114.61 0 256c0 113.09 73.34 209 175.08 242.9 12.8 2.35 17.47-5.56 17.47-12.34 0-6.08-.22-22.18-.35-43.54-71.2 15.49-86.2-34.34-86.2-34.34-11.64-29.57-28.42-37.45-28.42-37.45-23.27-15.84 1.73-15.55 1.73-15.55 25.69 1.81 39.21 26.38 39.21 26.38 22.84 39.12 59.92 27.82 74.5 21.27 2.33-16.54 8.94-27.82 16.25-34.22-56.84-6.43-116.6-28.43-116.6-126.49 0-27.95 10-50.8 26.35-68.69-2.63-6.48-11.42-32.5 2.51-67.75 0 0 21.49-6.88 70.4 26.24a242.65 242.65 0 0 1 128.18 0c48.87-33.13 70.33-26.24 70.33-26.24 14 35.25 5.18 61.27 2.55 67.75 16.41 17.9 26.31 40.75 26.31 68.69 0 98.35-59.85 120-116.88 126.32 9.19 7.9 17.38 23.53 17.38 47.41 0 34.22-.31 61.83-.31 70.23 0 6.85 4.61 14.81 17.6 12.31C438.72 464.97 512 369.08 512 256.02 512 114.62 397.37 0 256 0z\" fill=\"currentColor\" fill-rule=\"evenodd\"></path></svg>
    </a>
    <a class=\"draft-write-action\" href=\"/editor\" aria-label=\"Write\" title=\"Write\"><span class=\"icon\"><svg width=\"13\" height=\"13\" viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path d=\"M3 17.25V21h3.75L19.81 7.94l-3.75-3.75L3 17.25zm18-11.5a1 1 0 0 0 0-1.41L19.66 3a1 1 0 0 0-1.41 0l-1.59 1.59 3.75 3.75L21 5.75z\" fill=\"#666\"></path></svg></span><span class=\"label\">Write</span></a>
  </div>
</nav>
<div id=\"site-search-overlay\" class=\"site-search-overlay\" hidden>
  <div class=\"site-search-panel\" role=\"dialog\" aria-modal=\"true\" aria-labelledby=\"site-search-title\">
    <div class=\"site-search-top\">
      <h2 id=\"site-search-title\">Search posts</h2>
      <button id=\"site-search-close\" class=\"site-search-close\" type=\"button\" aria-label=\"Close search\">Close</button>
    </div>
    <input id=\"site-search-input\" class=\"site-search-input\" type=\"search\" placeholder=\"Type to search posts...\" autocomplete=\"off\" />
    <p class=\"site-search-hint\">Use <kbd>/</kbd> to open, <kbd>↑</kbd><kbd>↓</kbd> to navigate, <kbd>Enter</kbd> to open.</p>
    <ul id=\"site-search-results\" class=\"site-search-results\"></ul>
  </div>
</div>
<section id=\"wrapper\" class=\"post-page\">
  <div class=\"post\">
    <div class=\"draft-preview-head\">
      <h1 class=\"title\">Draft Preview</h1>
      <p class=\"lead\">Browse and edit drafts before publishing.</p>
    </div>
    <section class=\"medium-home\">%s</section>
  </div>
</section>
<script src=\"/media/js/main.js\"></script>
<script src=\"/media/js/theme-switcher.js\"></script>
<script src=\"/media/js/search.js\"></script>
</body>
</html>\n" body)))

(defun sds/main ()
  (let* ((blog-dir (file-name-as-directory (expand-file-name (car command-line-args-left))))
         (drafts-dir (expand-file-name "drafts" blog-dir))
         (public-drafts-dir (expand-file-name "public/drafts" blog-dir))
         (known-tags (sds/collect-known-tags blog-dir))
         (items '()))
    (unless blog-dir
      (error "Usage: emacs --batch -l scripts/generate_draft_preview.el <BLOG_DIR>"))
    (when (file-directory-p public-drafts-dir)
      (delete-directory public-drafts-dir t))
    (make-directory public-drafts-dir t)
    (when (file-directory-p drafts-dir)
      (dolist (draft (sort (directory-files drafts-dir t "\\.org\\'") #'string<))
        (let* ((parsed (sds/parse-org draft))
               (title (plist-get parsed :title))
               (slug (sds/slugify (file-name-base draft)))
               (date (plist-get parsed :date))
               (tags (or (plist-get parsed :tags) '("untagged")))
               (body (plist-get parsed :body))
               (description (plist-get parsed :description))
               (excerpt (sds/excerpt description body))
               (image (plist-get parsed :image))
               (html-body (plist-get parsed :html-body))
               (read-mins (sds/estimate-read-mins body))
               (item (list :slug slug
                           :title title
                           :date date
                           :tags tags
                           :body body
                           :html-body html-body
                           :draft-path (concat "drafts/" (file-name-nondirectory draft))
                           :excerpt excerpt
                           :read-mins read-mins
                           :image image))
               (out-dir (expand-file-name slug public-drafts-dir))
               (out-file (expand-file-name "index.html" out-dir)))
          (make-directory out-dir t)
          (with-temp-file out-file
            (insert (sds/detail-page item known-tags)))
          (push item items))))
    (let* ((new-item (list :slug "new"
                           :title "Untitled Draft"
                           :date (format-time-string "%Y-%m-%d")
                           :tags '()
                           :body ""
                           :html-body "<p>Write your draft here.</p>"
                           :draft-path ""
                           :excerpt "Start a new draft."
                           :read-mins 1
                           :image nil))
           (new-dir (expand-file-name "new" public-drafts-dir)))
      (make-directory new-dir t)
      (with-temp-file (expand-file-name "index.html" new-dir)
        (insert (sds/detail-page new-item known-tags))))
    (with-temp-file (expand-file-name "index.html" public-drafts-dir)
      (insert (sds/index-page (sds/sort-items-by-date-desc items))))
    (princ (format "Generated draft preview pages: %d\n" (length items)))))

(sds/main)
