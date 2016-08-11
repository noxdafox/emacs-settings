;; Automatic set variables, do not touch
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(custom-enabled-themes (quote (tango-dark-custom)))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; set transparency
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;; load .emacs.d files
(add-to-list 'load-path "~/.emacs.d/settings/")

;; custom settings
(require 'ide)
(require 'tango)
(require 'editor)
(require 'completion)
(require 'navigation)

;; mode specifics
(require 'cmode)
(require 'jmode)
(require 'pymode)
(require 'orgmode)
(require 'texmode)
