;; Emacs Editing configurations

;; MELPA repositories
(require 'package)
(package-initialize)
(custom-set-variables
  '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
		       ("melpa" . "https://melpa.org/packages/"))))

;; TLS for package repositories
(custom-set-variables
 '(tls-checktrust t))

;; Font
(set-frame-font "Inconsolata-12")

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
;; editor.el ends here
