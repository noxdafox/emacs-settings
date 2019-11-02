;; Automatic set variables, do not touch
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; load .emacs.d files
(add-to-list 'load-path "~/.emacs.d/settings/")

;; install dependencies if missing
(require 'packages)

;; custom settings
(require 'editor)
(require 'development)
(require 'keybindings)
(require 'modes)
