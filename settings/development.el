;; Development specific configuration

;; auto completion through ycmd-company
(require 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)
(set-variable 'ycmd-python-binary-path "/usr/bin/python3")
(set-variable 'ycmd-server-command
              '("python3" "/home/noxdafox/development/ycmd/ycmd"))
(set-variable 'ycmd-global-config
              "/home/noxdafox/.emacs.d/ycmd_global_config.py")

(require 'company-ycmd)
(company-ycmd-setup)

(require 'company)
(global-company-mode t)
(set 'company-idle-delay 0.1)

;; temporary fix for Ycmd + CompAny
;; https://github.com/abingham/emacs-ycmd/pull/431
(defun company-ycmd--construct-candidate-python (candidate)
  "Construct completion string from a CANDIDATE for python file-types."
  (company-ycmd--with-destructured-candidate candidate
    (let* ((kind (s-replace "\n" " " .extra_menu_info))
           (params (and (s-prefix-p "function" kind)
                        (company-ycmd--extract-params-python
                         .detailed_info .insertion_text)))
           (meta (company-ycmd--extract-meta-python .detailed_info))
           (filepath .extra_data.location.filepath)
           (line-num .extra_data.location.line_num))
      (propertize .insertion_text 'meta meta 'doc .detailed_info 'kind kind
                  'params params 'filepath filepath 'line_num line-num))))

;; flycheck syntax checker
(require 'flycheck)
(require 'flycheck-ycmd)
(flycheck-ycmd-setup)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; code documentation
(require 'ycmd-eldoc)
(add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)

;; code navigation
(require 'ggtags)
(add-hook 'prog-mode-hook 'ggtags-mode)
(add-hook 'after-save-hook #'gtags-update-hook)

(defun gtags-root-dir ()
  "Return GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update ()
  "Make GTAGS incremental update."
  (call-process "global" nil nil nil "-u"))

(defun gtags-update-hook ()
  (when (gtags-root-dir)
    (gtags-update)))

(provide 'development)
;;; development.el ends here
