;; global auto completion through ycmd-company
(require 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)

(require 'company-ycmd)
(company-ycmd-setup)

(require 'company)
(global-company-mode t)

(set 'company-idle-delay 0.1)

;; ycmd company backend settings
(set-variable 'ycmd-server-command '("python" "/usr/lib/ycmd/ycmd"))
(define-key company-active-map (kbd "<tab>") #'company-complete)

(provide 'completion)
