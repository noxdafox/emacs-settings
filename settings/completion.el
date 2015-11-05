;; global auto completion through company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; ycmd company backend
(require 'ycmd)
(set-variable 'ycmd-server-command '("python" "/usr/lib/ycmd/ycmd"))
(require 'company-ycmd)
(company-ycmd-setup)
(define-key company-active-map (kbd "<tab>") #'company-complete)
;; CompAny color face setting
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (when bg (not "unspecified-bg")
          (custom-set-faces
           `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
           `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
           `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
           `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
           `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

(provide 'completion)
