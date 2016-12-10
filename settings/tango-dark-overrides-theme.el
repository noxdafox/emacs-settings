(deftheme tango-dark-overrides
  "Overrides Tango Dark Theme settings.")

(require 'color)

(let ((class '((class color) (min-colors 89)))
      (alum "#eeeeec") (deep-blue "#181a26")
      (cyan "#2aa198") (blue-variable "#268bd2")
      (bg (face-attribute 'default :background)))

  (custom-theme-set-faces
   'tango-dark-overrides
   ;; Ensure sufficient contrast on low-color terminals.
   `(default ((((class color) (min-colors 4096))
               (:foreground ,alum :background ,deep-blue))
              (((class color) (min-colors 256))
               (:foreground ,alum :background "#222"))
              (,class
               (:foreground ,alum :background "black"))))
   `(mode-line ((,class
                 (:box (:line-width -1 :style released-button)
                       :background ,deep-blue :foreground ,alum))))
   `(mode-line-inactive ((,class
                          (:box (:line-width -1 :style released-button)
                                :background ,deep-blue :foreground ,alum))))
   `(fringe ((,class (:background ,deep-blue))))
   `(font-lock-string-face ((,class (:foreground ,cyan))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue-variable))))

   ;; Company color adjustments
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-annotation ((t (:inherit font-lock-constant-face))))))

(provide-theme 'tango-dark-overrides)
