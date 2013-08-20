;; Major modes
;;;; Python Mode
(add-hook 'python-mode-hook
	  '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
;;;; C/C++ Mode
(add-hook 'c-mode-hook
	  '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'c++-mode-hook
	  '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
;;;; Javascript Mode
(add-hook 'javascript-mode-hook
	  '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

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

;; auto-save and backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; TRAMP
;; TRAMP edit remote files
(require 'tramp)
(setq tramp-default-method "ssh")
;; set TRAMP autosave directory to local temp
(setq tramp-auto-save-directory temporary-file-directory)
;; disable backup for TRAMP buffers
(setq tramp-backup-directory-alist backup-directory-alist)
