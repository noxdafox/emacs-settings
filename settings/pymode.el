;; Python Mode

(require 'ycmd)
(require 'company)
(require 'flycheck)
(require 'column-marker)

(add-hook 'python-mode-hook 'ycmd-mode)
(add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook (lambda () (column-marker-1 80)))
(add-hook 'python-mode-hook (lambda () (column-marker-2 120)))
(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map (kbd "C-<") 'python-indent-shift-left)))
(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map (kbd "C->") 'python-indent-shift-right)))

(provide 'pymode)
