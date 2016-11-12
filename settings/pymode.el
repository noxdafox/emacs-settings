;; Python Mode

(require 'ggtags)
(require 'flycheck)

;; disable ycmd-flycheck for python
(add-hook 'python-mode-hook
          (lambda () (add-to-list 'flycheck-disabled-checkers 'ycmd)))

(add-hook 'python-mode-hook 'ggtags-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(setq flycheck-python-pylint-executable "pylint3")

(setq python-shell-interpreter "ipython3"
    python-shell-interpreter-args "-i")

(provide 'pymode)
