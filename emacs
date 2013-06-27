;; Automatic set variables, do not touch
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(font-use-system-font t)
 '(inhibit-startup-screen t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "unknown" :slant normal :weight normal :height 90 :width normal)))))

;; load .emacs.d files
(add-to-list 'load-path "~/.emacs.d/")

;; load custom settings
(load "~/.emacs.d/settings/coding.el")
(load "~/.emacs.d/settings/ide.el")
(load "~/.emacs.d/settings/editing.el")

;; set global variables
(defvar projects-home-dir "~/development/")
