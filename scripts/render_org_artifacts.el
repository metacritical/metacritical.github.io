(require 'org)
(require 'ob)
(require 'ob-core)
(require 'ob-ditaa nil t)
(require 'ob-dot nil t)

(setq org-confirm-babel-evaluate nil)
(setq large-file-warning-threshold nil)
(setq create-lockfiles nil)
(fset 'yes-or-no-p (lambda (&rest _) t))
(fset 'y-or-n-p (lambda (&rest _) t))

(defun sds/base64 (text)
  (base64-encode-string text t))

(defun sds/run-ditaa-server (jar jobs)
  "Render JOBS through one headless Ditaa JVM.

Each JOB is a cons cell containing an output path and diagram body.  The
server exits naturally after EOF, so one publish invocation owns one server
lifetime and no JVM is started per diagram."
  (when jobs
    (let ((request-buffer (generate-new-buffer " *sds-ditaa-requests*"))
          (response-buffer (generate-new-buffer " *sds-ditaa-responses*")))
      (unwind-protect
          (progn
            (with-current-buffer request-buffer
              (dolist (job (reverse jobs))
                (insert "RENDER\t"
                        (sds/base64 (car job))
                        "\t"
                        (sds/base64 (cdr job))
                        "\n")))
            (let ((status (with-current-buffer request-buffer
                            (call-process-region
                             (point-min) (point-max)
                             "java" nil response-buffer nil
                             "-Djava.awt.headless=true"
                             "-cp" jar
                             "org.stathissideris.ascii2image.core.DitaaServer"
                             "-E" "-S"))))
              (unless (eq status 0)
                (error "Ditaa server exited with status %s" status)))
            (with-current-buffer response-buffer
              (dolist (response (split-string (buffer-string) "\n" t))
                (unless (string= response "OK")
                  (error "Ditaa server request failed: %s" response)))))
        (kill-buffer request-buffer)
        (kill-buffer response-buffer)))))

(let ((blog-root (or (nth 0 command-line-args-left) default-directory))
      (ditaa-jar (nth 1 command-line-args-left))
      (org-files (cddr command-line-args-left))
      (expected-files '())
      (ditaa-jobs '()))
  (unless (and ditaa-jar (file-exists-p ditaa-jar))
    (error "Ditaa server JAR missing: %s" ditaa-jar))
  (unless org-files
    (error "No Org files supplied to diagram renderer"))
  (setq default-directory (file-name-as-directory blog-root))
  (setq org-ditaa-jar-path ditaa-jar)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t) (dot . t)))

  (dolist (org-file org-files)
    (with-current-buffer (find-file-noselect org-file)
      (org-mode)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-babel-src-block-regexp nil t)
         (let* ((block-beg (match-beginning 0))
                (block-end (match-end 0)))
           (goto-char block-beg)
           (let* ((info (org-babel-get-src-block-info 'light))
                  (lang (downcase (or (car info) "")))
                  (params (nth 2 info))
                  (body (nth 1 info))
                  (outfile (cdr (assoc :file params))))
             (when (and outfile
                        (member lang '("ditaa" "dot" "graphviz-dot")))
               (let ((abs-out (expand-file-name outfile blog-root)))
                 (push abs-out expected-files)
                 (cond
                  ((string= lang "ditaa")
                   (if (or (not (file-exists-p abs-out))
                           (file-newer-than-file-p org-file abs-out))
                       (push (cons abs-out body) ditaa-jobs)
                     (message "[INFO] Skipping unchanged ditaa artifact: %s" abs-out)))
                  ((or (string= lang "dot") (string= lang "graphviz-dot"))
                   (when (or (not (file-exists-p abs-out))
                             (file-newer-than-file-p org-file abs-out))
                     (org-babel-execute-src-block))))))
           (goto-char block-end))))
      (save-buffer)
      (kill-buffer (current-buffer))))

  (sds/run-ditaa-server ditaa-jar ditaa-jobs)

  (dolist (file expected-files)
    (unless (file-exists-p file)
      (message "[WARN] Artifact not generated: %s" file)))))
