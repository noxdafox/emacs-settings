;; Automatic set variables, do not touch
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; load theme
(load-theme 'tango-dark t)
(load-theme 'tango-dark-overrides t)

;; set transparency
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;; load .emacs.d files
(add-to-list 'load-path "~/.emacs.d/settings/")

;; install dependencies if missing
(package-install 'jdee)
(package-install 'magit)
(package-install 'ggtags)
(package-install 'elixir-mode)
(package-install 'use-package)
(package-install 'column-marker)
(package-install 'bash-completion)
(package-install 'smart-mode-line)
(package-install 'ycmd)
(package-install 'ycmd-eldoc)
(package-install 'company)
(package-install 'company-ycmd)
(package-install 'flycheck)
(package-install 'flycheck-ycmd)

;; custom settings
(require 'editor)
(require 'development)
(require 'keybindings)
(require 'modes)
