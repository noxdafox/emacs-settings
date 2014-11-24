;; Code Navigation

;; Etags
;; on find-tag miss, refresh tags table and retry
(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
If buffer is modified, ask about save before running etags."
  (condition-case err
      ad-do-it
    (error (and (buffer-modified-p)
                (not (ding))
                (y-or-n-p "Buffer is modified, save it? ")
                (save-buffer))
           (er-refresh-etags)
           ad-do-it)))
;; refresh projects TAGS
(defun er-refresh-etags ()
  "Run ctags-exuberant over all the projects."
  (let ((tags-revert-without-query t))
    (shell-command
     (format "ctags-exuberant -e -R -f %s --exclude='__init__.py'"
             tags-file-name))
    (visit-tags-table tags-file-name nil)))

(provide 'navigation)
