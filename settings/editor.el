;; Font
(set-default-font "Inconsolata-12")

;; Modern scrolling
(setq scroll-step 1)
(setq auto-window-vscroll nil)
(setq scroll-conservatively 10000)

;; typing helpers
(show-paren-mode t)
(electric-pair-mode t)
(delete-selection-mode 1)
(setq-default indent-tabs-mode nil)
(global-set-key (kbd ",") (lambda() (interactive) (insert ", ")))

;; column marker
(require 'column-marker)
(add-hook 'prog-mode-hook (lambda () (column-marker-1 80)))
(add-hook 'prog-mode-hook (lambda () (column-marker-2 120)))

;; auto delete trailing whitespaces
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

(provide 'editor)
