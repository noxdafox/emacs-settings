;; Automatic set variables, do not touch
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (tango-dark)))
 '(inhibit-startup-screen t)
 '(tabbar-separator (quote (0.5)))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; load .emacs.d files
(add-to-list 'load-path "~/.emacs.d/settings/")

;; custom settings
(require 'ide)
(require 'editor)
(require 'completion)
(require 'navigation)

;; mode specifics
(require 'cmode)
(require 'jmode)
(require 'pymode)
(require 'texmode)
