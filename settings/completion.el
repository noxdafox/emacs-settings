;; global auto completion through ycmd-company
(require 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)

(require 'company-ycmd)
(company-ycmd-setup)

;; ycmd company backend settings
(set-variable 'ycmd-server-command '("python" "/usr/lib/ycmd/ycmd"))
(define-key company-active-map (kbd "<tab>") #'company-complete)

;; CompAny color face setting
(require 'color)
(defun background-color ()
  "Return background color."
  (let ((background (face-attribute 'default :background)))
  (if (equal background "unspecified-bg")
      "#000000"
    background)))

(let ((bg (background-color)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

(provide 'completion)
