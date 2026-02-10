;;; generate_draft_preview.el --- Build draft preview pages -*- lexical-binding: t; -*-

(require 'subr-x)

(defun sds/slugify (s)
  (let* ((lower (downcase (string-trim s)))
         (step1 (replace-regexp-in-string "[^a-z0-9]+" "-" lower))
         (step2 (replace-regexp-in-string "-+" "-" step1))
         (trimmed (replace-regexp-in-string "\\`-\\|-\\'" "" step2)))
    (if (string-empty-p trimmed) "draft" trimmed)))

(defun sds/parse-org (path)
  (with-temp-buffer
    (insert-file-contents path)
    (let ((title (file-name-base path))
          (body-lines '()))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ((string-match-p "\\`#\\+TITLE:" line)
            (setq title (string-trim (substring line (length "#+TITLE:")))))
           ((string-match-p "\\`#\\+" line)
            nil)
           (t
            (push line body-lines))))
        (forward-line 1))
      (list title (string-trim (string-join (nreverse body-lines) "\n"))))))

(defun sds/html-page (title body)
  (format "<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">
  <title>%s</title>
  <style>
    body { font-family: Georgia, serif; margin: 0; background: #f7f6f2; color: #1c1b19; }
    main { max-width: 880px; margin: 0 auto; padding: 36px 24px 64px; }
    h1 { font-size: clamp(34px, 6vw, 56px); line-height: 1.1; margin: 0 0 22px; }
    pre { white-space: pre-wrap; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: 16px; line-height: 1.7; background: #fff; border: 1px solid #e3ddd1; padding: 18px; border-radius: 12px; }
    a { color: #0d6a57; text-decoration: none; }
  </style>
</head>
<body>
<main>
  <p><a href=\"/drafts/\">Back to drafts</a></p>
  <h1>%s</h1>
  <pre>%s</pre>
</main>
</body>
</html>\n"
          (string-replace "<" "&lt;" (string-replace ">" "&gt;" title))
          (string-replace "<" "&lt;" (string-replace ">" "&gt;" title))
          (string-replace "<" "&lt;" (string-replace ">" "&gt;" body))))

(defun sds/html-index (items)
  (let ((links
         (mapconcat
          (lambda (it)
            (format "<li><a href=\"/drafts/%s/\">%s</a></li>" (car it) (cdr it)))
          items
          "")))
    (format "<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">
  <title>Draft Preview</title>
  <style>
    body { font-family: Georgia, serif; margin: 0; background: #f7f6f2; color: #1c1b19; }
    main { max-width: 880px; margin: 0 auto; padding: 36px 24px 64px; }
    h1 { font-size: clamp(34px, 6vw, 56px); line-height: 1.1; margin: 0 0 20px; }
    li { margin: 10px 0; }
    a { color: #0d6a57; text-decoration: none; font-size: 20px; }
  </style>
</head>
<body>
<main>
  <h1>Draft Preview</h1>
  <p>Visible only when built with <code>DRAFTS=1</code>.</p>
  <ul>%s</ul>
</main>
</body>
</html>\n" links)))

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
               (title (car parsed))
               (body (cadr parsed))
               (slug (sds/slugify (file-name-base draft)))
               (out-dir (expand-file-name slug public-drafts-dir))
               (out-file (expand-file-name "index.html" out-dir)))
          (make-directory out-dir t)
          (with-temp-file out-file
            (insert (sds/html-page title body)))
          (push (cons slug title) items))))
    (with-temp-file (expand-file-name "index.html" public-drafts-dir)
      (insert (sds/html-index (nreverse items))))
    (princ (format "Generated draft preview pages: %d\n" (length items)))))

(sds/main)
