;;; generate_draft_preview.el --- Build draft preview pages -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'seq)

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
               (string-match "\\[\\[\\([^]\n]+\\(?:\\.png\\|\\.jpg\\|\\.jpeg\\|\\.gif\\|\\.webp\\|\\.svg\\)\\)\\(?:\\]\\[[^]]*\\)?\\]\\]" body))
      (match-string 1 body))))

(defun sds/estimate-read-mins (text)
  (let* ((clean (replace-regexp-in-string "\\[\\[[^]]+\\]\\[[^]]+\\]\\]" " " (or text "")))
         (words (split-string clean "[^[:alnum:]]+" t))
         (count (length words)))
    (max 1 (/ (+ count 199) 200))))

(defun sds/excerpt (desc body)
  (let* ((source (if (string-empty-p (string-trim (or desc "")))
                     (or body "")
                   desc))
         (flat (replace-regexp-in-string "[\n\r\t]+" " " source))
         (trimmed (string-trim flat)))
    (if (> (length trimmed) 180)
        (concat (substring trimmed 0 177) "...")
      trimmed)))

(defun sds/sort-items-by-date-desc (items)
  (sort (copy-sequence items)
        (lambda (a b)
          (let ((da (or (plist-get a :date) "0000-00-00"))
                (db (or (plist-get b :date) "0000-00-00")))
            (string> da db)))))

(defun sds/group-by-tags (items)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (item items)
      (let ((tags (or (plist-get item :tags) '("untagged"))))
        (dolist (tag tags)
          (puthash tag (cons item (gethash tag table)) table))))
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
             (image (sds/extract-first-image body)))
        (list :title title
              :date date
              :description description
              :tags clean-tags
              :body body
              :image image)))))

(defun sds/detail-page (item)
  (let* ((title (sds/html-escape (plist-get item :title)))
         (body (sds/html-escape (plist-get item :body)))
         (slug (sds/html-escape (plist-get item :slug)))
         (date (sds/html-escape (or (plist-get item :date) "")))
         (tags (or (plist-get item :tags) '("untagged")))
         (tag-html (mapconcat (lambda (tag) (format "<span class=\"tag\">%s</span>" (sds/html-escape tag))) tags ""))
         (mins (number-to-string (or (plist-get item :read-mins) 1))))
    (format "<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">
  <title>%s</title>
  <link rel=\"icon\" href=\"/media/images/logo.png\" type=\"image/png\">
  <style>
    body { margin: 0; background: #f7f6f2; color: #1c1b19; font-family: ui-serif, Georgia, Cambria, serif; }
    main { max-width: 920px; margin: 0 auto; padding: 30px 22px 64px; }
    .top { display:flex; justify-content:space-between; align-items:center; gap:12px; margin-bottom: 14px; }
    .top a { color: #0d6a57; text-decoration:none; font:600 14px/1.2 ui-sans-serif,-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; }
    .edit { display:inline-flex; align-items:center; gap:8px; border:1px solid #d2ccc0; border-radius:10px; padding:8px 10px; color:#444; }
    .edit svg { width:14px; height:14px; }
    h1 { margin: 8px 0 8px; font-size: clamp(34px, 5.2vw, 58px); line-height: 1.06; }
    .meta { color:#6b665c; font:500 14px/1.35 ui-sans-serif,-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; margin-bottom: 8px; }
    .tags { display:flex; flex-wrap:wrap; gap:8px; margin: 12px 0 22px; }
    .tag { background:#ece7dc; color:#4f4a3f; border-radius:999px; padding:4px 10px; font:600 12px/1.2 ui-sans-serif,-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; }
    pre { white-space: pre-wrap; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: 15px; line-height: 1.7; background: #fff; border: 1px solid #e3ddd1; padding: 16px; border-radius: 12px; }
  </style>
</head>
<body>
<main>
  <div class=\"top\">
    <a href=\"/drafts/\">Back to drafts</a>
    <a class=\"edit\" href=\"/editor?draft=%s\" aria-label=\"Edit this draft\">
      <svg viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path d=\"M3 17.25V21h3.75L19.81 7.94l-3.75-3.75L3 17.25zm18-11.5a1 1 0 0 0 0-1.41L19.66 3a1 1 0 0 0-1.41 0l-1.59 1.59 3.75 3.75L21 5.75z\" fill=\"currentColor\"></path></svg>
      <span>Edit</span>
    </a>
  </div>
  <h1>%s</h1>
  <div class=\"meta\">%s · %s min read</div>
  <div class=\"tags\">%s</div>
  <pre>%s</pre>
</main>
</body>
</html>\n"
            title slug title date mins tag-html body)))

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
            "<div class=\"ph\"></div>")))
    (format "<article class=\"draft-card\">
  <a class=\"draft-link\" href=\"/drafts/%s/\">
    <div class=\"draft-thumb\">%s</div>
    <div class=\"draft-copy\">
      <h3>%s</h3>
      <p>%s</p>
      <div class=\"draft-meta\">%s · %s min read</div>
    </div>
  </a>
</article>"
            slug image-html title desc date mins)))

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
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">
  <title>Draft Preview</title>
  <link rel=\"icon\" href=\"/media/images/logo.png\" type=\"image/png\">
  <style>
    body { margin: 0; background: #f7f6f2; color: #1d1b18; font-family: ui-sans-serif,-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; }
    main { max-width: 1320px; margin: 0 auto; padding: 28px 28px 64px; }
    h1 { margin: 0 0 8px; font-size: clamp(30px, 4.2vw, 56px); line-height: 1.04; font-family: ui-serif, Georgia, Cambria, serif; }
    .lead { margin: 0 0 24px; color:#6a655a; font-size: 15px; }
    .group-head { display:flex; align-items:center; justify-content:space-between; border-bottom:1px solid #dfd8cc; margin-bottom:12px; }
    .group-head h2 { margin: 0 0 -1px; display:inline-block; font-size: 24px; line-height: 1.1; border-bottom:2px solid #b6b0a4; padding-bottom: 6px; }
    .draft-group { margin: 0 0 28px; }
    .draft-grid { display:grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 16px; }
    .draft-card { border:1px solid #ddd7cb; border-radius: 6px; background:#fff; overflow:hidden; }
    .draft-link { color:inherit; text-decoration:none; display:grid; grid-template-columns: 38%% 62%%; min-height: 230px; }
    .draft-thumb { min-height: 100%%; background: linear-gradient(135deg,#ece8df,#ded7ca); }
    .draft-thumb img { width:100%%; height:100%%; object-fit: cover; display:block; }
    .draft-thumb .ph { width:100%%; height:100%%; min-height: 230px; background: radial-gradient(circle at 30%% 20%%,#ece6da,#d9d2c4 58%%); }
    .draft-copy { padding: 18px 18px 16px; display:flex; flex-direction:column; }
    .draft-copy h3 { margin: 0 0 8px; font-size: clamp(30px, 2vw, 46px); line-height: 1.06; letter-spacing: -0.02em; }
    .draft-copy p { margin: 0; font-size: 14px; line-height: 1.45; color:#5f5b53; }
    .draft-meta { margin-top:auto; padding-top: 14px; font-size:13px; color:#716c61; font-weight:600; }
    @media (max-width: 960px) {
      .draft-grid { grid-template-columns: 1fr; }
      .draft-link { grid-template-columns: 1fr; }
      .draft-thumb .ph { min-height: 180px; }
    }
  </style>
</head>
<body>
<main>
  <h1>Draft Preview</h1>
  <p class=\"lead\">Visible only in local dev builds (<code>DRAFTS=1</code>).</p>
  %s
</main>
</body>
</html>\n" body)))

(defun sds/main ()
  (let* ((blog-dir (file-name-as-directory (expand-file-name (car command-line-args-left))))
         (drafts-dir (expand-file-name "drafts" blog-dir))
         (public-drafts-dir (expand-file-name "public/drafts" blog-dir))
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
               (read-mins (sds/estimate-read-mins body))
               (item (list :slug slug
                           :title title
                           :date date
                           :tags tags
                           :body body
                           :excerpt excerpt
                           :read-mins read-mins
                           :image image))
               (out-dir (expand-file-name slug public-drafts-dir))
               (out-file (expand-file-name "index.html" out-dir)))
          (make-directory out-dir t)
          (with-temp-file out-file
            (insert (sds/detail-page item)))
          (push item items))))
    (with-temp-file (expand-file-name "index.html" public-drafts-dir)
      (insert (sds/index-page (sds/sort-items-by-date-desc items))))
    (princ (format "Generated draft preview pages: %d\n" (length items)))))

(sds/main)
