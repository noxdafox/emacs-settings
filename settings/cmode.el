;; C/C++ Mode

(require 'ycmd)
(require 'flycheck)
(require 'column-marker)

(add-hook 'c-mode-hook 'ycmd-mode)
(add-hook 'c++-mode-hook 'ycmd-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook (lambda () (column-marker-1 80)))
(add-hook 'c++-mode-hook (lambda () (column-marker-1 80)))

(provide 'cmode)
