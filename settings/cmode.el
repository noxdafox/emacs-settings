;; C/C++ Mode

(require 'ycmd)
(require 'ggtags)
(require 'flycheck)
(require 'column-marker)

(custom-set-variables
 '(c-basic-offset 4)
 '(c-indent-level 4)
 '(c-file-style "K&R"))

(add-hook 'c-mode-hook 'ycmd-mode)
(add-hook 'c++-mode-hook 'ycmd-mode)
(add-hook 'c-mode-hook 'ggtags-mode)
(add-hook 'c++-mode-hook 'ggtags-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook (lambda () (column-marker-1 80)))
(add-hook 'c++-mode-hook (lambda () (column-marker-1 80)))
;; disable electric comma
(eval-after-load "cc-mode"
  '(define-key c-mode-base-map "," nil))

(provide 'cmode)
