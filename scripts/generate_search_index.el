;;; generate_search_index.el --- Build search-index.json from generated pages -*- lexical-binding: t; -*-

(require 'json)
(require 'cl-lib)
(require 'subr-x)

(defun sds-read-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun sds-re-group (regexp text)
  (when (string-match regexp text)
    (match-string 1 text)))

(defun sds-html-to-text (html)
  (let ((s (or html "")))
    (setq s (replace-regexp-in-string "<script\\(?:.\\|\n\\)*?</script>" " " s t t))
    (setq s (replace-regexp-in-string "<style\\(?:.\\|\n\\)*?</style>" " " s t t))
    (setq s (replace-regexp-in-string "<[^>]+>" " " s t t))
    (setq s (replace-regexp-in-string "&nbsp;" " " s t t))
    (setq s (replace-regexp-in-string "&amp;" "&" s t t))
    (setq s (replace-regexp-in-string "&quot;" "\"" s t t))
    (setq s (replace-regexp-in-string "&#39;" "'" s t t))
    (setq s (replace-regexp-in-string "[ \t\n\r]+" " " s t t))
    (string-trim s)))

(defun sds-extract-title (html)
  (let ((title (or (sds-re-group "<h1 class=\"title\">\\([^<]+\\)</h1>" html)
                   (sds-re-group "<h1[^>]*>\\([^<]+\\)</h1>" html)
                   (sds-re-group "<title>\\([^<]+\\)</title>" html))))
    (when title
      (string-trim (replace-regexp-in-string " - Self dot send (Computing Blog)$" "" title)))))

(defun sds-extract-excerpt (html)
  (let* ((body (or (sds-re-group "<div id=\"post-body\"[^>]*>\\(\\(?:.\\|\n\\)*?\\)</div>" html) html))
         (paragraph (or (sds-re-group "<p>\\(\\(?:.\\|\n\\)*?\\)</p>" body)
                        (sds-re-group "<p class=\"subtitle\">\\(\\(?:.\\|\n\\)*?\\)</p>" body)))
         (plain (sds-html-to-text paragraph)))
    (if (> (length plain) 220)
        (concat (substring plain 0 217) "...")
      plain)))

(defun sds-post-url (public-dir html-file)
  (let* ((rel (file-relative-name html-file public-dir))
         (dir (file-name-directory rel)))
    (concat "/" dir)))

(defun sds-date-from-url (url)
  (if (string-match "/blog/\\([0-9]\\{4\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\)/" url)
      (format "%s-%s-%s" (match-string 1 url) (match-string 2 url) (match-string 3 url))
    "1970-01-01"))

(defun sds-searchable-post-p (url)
  (and (string-prefix-p "/blog/" url)
       (not (string-match-p "/posts-tagged-" url))
       (not (string-match-p "/archive/" url))
       (not (string-match-p "/tags/" url))
       (not (string-match-p "/all-posts/" url))))

(defun sds-generate-search-index (blog-dir)
  (let* ((public-dir (expand-file-name "public" blog-dir))
         (output-file (expand-file-name "search-index.json" public-dir))
         (entries '()))
    (dolist (html-file (directory-files-recursively public-dir "index\\.html$"))
      (let* ((url (sds-post-url public-dir html-file))
             (html (sds-read-file html-file))
             (title (sds-extract-title html)))
        (when (and (sds-searchable-post-p url)
                   title
                   (not (string-empty-p title)))
          (push `((title . ,title)
                  (url . ,url)
                  (date . ,(sds-date-from-url url))
                  (excerpt . ,(sds-extract-excerpt html)))
                entries))))
    (setq entries
          (sort entries
                (lambda (a b)
                  (string> (alist-get 'date a)
                           (alist-get 'date b)))))
    (let ((json-encoding-pretty-print t))
      (with-temp-file output-file
        (insert (json-encode entries))))
    (princ (format "Generated search index: %s\n" output-file))))

(let ((blog-dir (or (car command-line-args-left) default-directory)))
  (sds-generate-search-index blog-dir))

