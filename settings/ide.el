;; MELPA repositories
(require 'package)
(package-initialize)
(custom-set-variables
  '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
		       ("melpa" . "https://melpa.org/packages/"))))

;; TLS for package repositories
(custom-set-variables
 '(tls-checktrust t))

;; semantic mode
(require 'semantic)
(custom-set-variables
 '(semantic-mode t))

;; speed bar
(require 'sr-speedbar)
(custom-set-variables
 '(speedbar-use-images nil)
 '(sr-speedbar-right-side nil)
 '(speedbar-show-unknown-files t))
(sr-speedbar-open)

;; minimal ide
(custom-set-variables
 '(tooltip-mode nil)
 '(tool-bar-mode nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil))

;; uniquify buffer names
(require 'uniquify)
(custom-set-variables
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; Interactively Do Things mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Linum
(require 'linum)
(global-linum-mode t)

;; flycheck ycmd
(require 'flycheck-ycmd)
(flycheck-ycmd-setup)

(provide 'ide)
