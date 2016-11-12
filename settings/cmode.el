;; C/C++ Mode

(require 'ggtags)

(custom-set-variables
 '(c-basic-offset 4)
 '(c-indent-level 4)
 '(c-file-style "K&R"))

(add-hook 'c-mode-hook 'ggtags-mode)
(add-hook 'c++-mode-hook 'ggtags-mode)

;; disable electric comma
(eval-after-load "cc-mode"
  '(define-key c-mode-base-map "," nil))

(provide 'cmode)
