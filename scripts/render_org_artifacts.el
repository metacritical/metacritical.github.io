(require 'org)
(require 'ob)
(require 'ob-core)
(require 'ob-ditaa nil t)
(require 'ob-plantuml nil t)
(require 'ob-dot nil t)

(setq org-confirm-babel-evaluate nil)
(setq large-file-warning-threshold nil)
(fset 'yes-or-no-p (lambda (&rest _) t))
(fset 'y-or-n-p (lambda (&rest _) t))

(defun sds/run-java-renderer (jar body outfile &optional extra-args pipe-mode)
  (make-directory (file-name-directory outfile) t)
  (if pipe-mode
      (with-temp-buffer
        (insert body)
        (let ((status (apply #'call-process-region
                             (point-min) (point-max)
                             "java" nil nil nil
                             "-jar" jar
                             (append extra-args (list "-pipe")))))
          (when (eq status 0)
            (write-region (point-min) (point-max) outfile nil 'silent))
          status))
    (let ((tmp (make-temp-file "sds-diagram-" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert body))
            (apply #'call-process "java" nil nil nil
                   (append (list "-jar" jar)
                           extra-args
                           (list tmp outfile))))
        (ignore-errors (delete-file tmp))))))

(let ((blog-root (or (nth 0 command-line-args-left) default-directory))
      (org-file (nth 1 command-line-args-left))
      (ditaa-jar (nth 2 command-line-args-left))
      (plantuml-jar (nth 3 command-line-args-left)))
  (unless (and org-file (file-exists-p org-file))
    (error "Org file missing: %s" org-file))
  (setq default-directory (file-name-as-directory blog-root))
  (when (and ditaa-jar (file-exists-p ditaa-jar))
    (setq org-ditaa-jar-path ditaa-jar))
  (when (and plantuml-jar (file-exists-p plantuml-jar))
    (setq org-plantuml-jar-path plantuml-jar))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t) (plantuml . t) (dot . t)))

  (with-current-buffer (find-file-noselect org-file)
    (org-mode)
    (let ((expected-files '()))
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-babel-src-block-regexp nil t)
         (goto-char (match-beginning 0))
         (let* ((info (org-babel-get-src-block-info 'light))
                (lang (downcase (or (car info) "")))
               (params (nth 2 info))
               (body (nth 1 info))
               (outfile (cdr (assoc :file params))))
           (when outfile
             (let ((abs-out (expand-file-name outfile blog-root)))
               (push abs-out expected-files)
             (when (member lang '("ditaa" "plantuml" "dot" "graphviz-dot"))
               (org-babel-execute-src-block)
               (unless (file-exists-p abs-out)
                 (cond
                  ((and (string= lang "ditaa")
                        ditaa-jar (file-exists-p ditaa-jar))
                   (let ((status (sds/run-java-renderer
                                  ditaa-jar body abs-out
                                  (ignore-errors (split-string-and-unquote (or (cdr (assoc :cmdline params)) "")))
                                  nil)))
                     (unless (eq status 0)
                       (message "[WARN] ditaa fallback failed (%s) for %s" status abs-out))))
                  ((and (string= lang "plantuml")
                        plantuml-jar (file-exists-p plantuml-jar))
                   (let ((status (sds/run-java-renderer plantuml-jar body abs-out '("-tpng") t)))
                     (unless (eq status 0)
                       (message "[WARN] plantuml fallback failed (%s) for %s" status abs-out))))))))))
         (goto-char (match-end 0))))

      (dolist (f expected-files)
        (unless (file-exists-p f)
          (message "[WARN] Artifact not generated from %s: %s" org-file f))))
    (save-buffer)
    (kill-buffer (current-buffer))))
