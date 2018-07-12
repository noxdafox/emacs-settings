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

;; Enable line-number-mode when developing
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

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
