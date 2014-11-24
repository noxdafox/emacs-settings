;; Python Mode

(add-hook 'python-mode-hook 'ycmd-mode)
(add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'flycheck-mode)

(provide 'pymode)
