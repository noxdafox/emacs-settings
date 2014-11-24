(add-hook 'c-mode-hook 'ycmd-mode)
(add-hook 'c++-mode-hook 'ycmd-mode)
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)

(provide 'cmode)
