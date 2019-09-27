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
(use-package uniquify
  :custom
  (uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; Interactively Do Things mode
(use-package ido
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t))

;; smart mode line
(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup))

;; pretty icons
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t)

;; modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

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
(use-package column-marker
  :ensure t
  :hook ((prog-mode-hook . (lambda () (column-marker-1 80)))
         (prog-mode-hook . (lambda () (column-marker-1 120)))
         (elixir-mode . (lambda () (column-marker-1 98)))))

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
(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-auto-save-directory temporary-file-directory)
  (setq tramp-backup-directory-alist backup-directory-alist))

;; Speedbar
(use-package sr-speedbar
  :ensure t
  :config
  (setq speedbar-show-unknown-files t))

;; Open the shell in the same window
(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*shell") display-buffer-same-window))

;; Enable *case-region functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Rename file in place
(defun rename-current-buffer-file ()
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: "
                                      (file-name-directory filename)
                                      basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(provide 'editor)
;; editor.el ends here
