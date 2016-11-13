;; global auto completion through ycmd-company
(require 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)

(require 'company-ycmd)
(company-ycmd-setup)

(require 'company)
(global-company-mode t)

;; ycmd company backend settings
(set-variable 'ycmd-server-command '("python" "/usr/lib/ycmd/ycmd"))
(define-key company-active-map (kbd "<tab>") #'company-complete)

;; disable fill-column-indicator on completion
(defun on-off-fci-before-company(command)
  (when (string= "show" command)
    (turn-off-fci-mode))
  (when (string= "hide" command)
    (turn-on-fci-mode)))

(advice-add 'company-call-frontends :before #'on-off-fci-before-company)

(provide 'completion)
