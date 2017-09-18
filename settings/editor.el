;; Emacs Editing configurations

;; Font
(set-frame-font "Inconsolata-12" t t)

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

;; smart mode line
(sml/setup)

;; Linum
(require 'linum)
(setq linum-disabled-modes-list '(shell-mode org-mode compilation-mode))
(defun linum-on ()
  (unless (or (minibufferp)
              (member major-mode linum-disabled-modes-list))
    (linum-mode 1)))
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

;; auto-focus on help windows
(setq help-window-select t)

;; TRAMP
;; TRAMP edit remote files
(require 'tramp)
(setq tramp-default-method "ssh")
;; set TRAMP autosave directory to local temp
(setq tramp-auto-save-directory temporary-file-directory)
;; disable backup for TRAMP buffers
(setq tramp-backup-directory-alist backup-directory-alist)

;; Speedbar
(require 'sr-speedbar)
(setq speedbar-show-unknown-files t)

(provide 'editor)
;; editor.el ends here
