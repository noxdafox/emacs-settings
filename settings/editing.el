;; Typing helpers
(electric-pair-mode t) ; add parenthesis in pair
(global-set-key (kbd ",") (lambda() (interactive) (insert ", ")))

;; Auto Complete Mode
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start 3) ; start AC at third character
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;; Auto delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; TRAMP edit remote files
(require 'tramp)
(setq tramp-default-method "ssh")
