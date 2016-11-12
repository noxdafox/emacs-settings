;; Python Mode

(require 'ycmd)
(require 'ggtags)
(require 'flycheck)
(require 'column-marker)

(add-hook 'python-mode-hook 'ycmd-mode)
;; disable ycmd-flycheck for python
(add-hook 'python-mode-hook
          (lambda () (add-to-list 'flycheck-disabled-checkers 'ycmd)))

(add-hook 'python-mode-hook 'ggtags-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(setq flycheck-python-pylint-executable "pylint3")
(add-hook 'python-mode-hook (lambda () (column-marker-1 80)))
(add-hook 'python-mode-hook (lambda () (column-marker-2 120)))

(provide 'pymode)
