;; C/C++ Mode

(custom-set-variables
 '(c-basic-offset 4)
 '(c-indent-level 4)
 '(c-file-style "K&R"))

;; disable electric comma
(eval-after-load "cc-mode"
  '(define-key c-mode-base-map "," nil))

(provide 'cmode)
