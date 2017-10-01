;; Modes specific customisations

;; Lisp
(require 'flycheck)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; C/C++ Mode
(custom-set-variables
 '(c-basic-offset 4)
 '(c-indent-level 4)
 '(c-file-style "K&R"))

(defun custom-c-indentation ()
  (c-set-offset 'arglist-intro '+))

(add-hook 'c-mode-hook 'custom-c-indentation)
(add-hook 'c++-mode-hook 'custom-c-indentation)

;; disable electric comma
(eval-after-load "cc-mode"
  '(define-key c-mode-base-map "," nil))

;; Python Mode
; override syntax checker, use pylint
(require 'flycheck)
(add-hook 'python-mode-hook
          (lambda () (add-to-list 'flycheck-disabled-checkers 'ycmd)))
(add-hook 'python-mode-hook 'flycheck-mode)
(setq flycheck-python-pylint-executable "pylint3")

; ipython interpreter
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt -i")

;; Comint mode
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; file completion space/slash insertion
 '(comint-buffer-maximum-size 8192)     ; maximum buffer size in lines
 '(comint-prompt-read-only t))          ; set comint prompt read-only

(defun comint-preoutput-turn-buffer-read-only (text)
  (propertize text 'read-only t))
(add-hook 'comint-preoutput-filter-functions
          'comint-preoutput-turn-buffer-read-only)

; do not override prompt colors
(set-face-attribute 'comint-highlight-prompt nil :inherit nil)

(require 'bash-completion)
(bash-completion-setup)

; automatically truncate buffer size
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)

; scroll to bottom when searching history
(defadvice comint-previous-input
    (before before-comint-previous-input activate)
  (goto-char (point-max)))

(defadvice comint-next-input
    (before before-comint-next-input activate)
  (goto-char (point-max)))

(defadvice comint-history-isearch-backward-regexp
    (before before-comint-history-isearch-backward-regexp activate)
  (goto-char (point-max)))

;; TeX Mode
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; Lisp Mode
(setq ycmd-file-type-map (remove '(emacs-lisp-mode "elisp") ycmd-file-type-map))

;; Elixir mode
(add-hook 'elixir-mode-hook 'alchemist-mode)
(eval-after-load 'flycheck '(flycheck-credo-setup))

;; Org mode
(require 'org)
(setq org-log-done 'time)
(setq org-directory "~/documents/org/")
(setq org-agenda-files (list org-directory))
(setq org-default-todo-file (concat org-directory "todo.org"))
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-todo-keywords '((sequence "TODO" "|" "DONE" "TRASHED")))
(setq org-capture-templates
      '(("t" "Todo" entry (file org-default-todo-file)
         "* TODO %?\n  %i\n  %a")))
; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Rust mode
(setq rust_src_path "/usr/lib/rustlib/src/")
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook (lambda () (column-marker-1 90)))

;; Icons mode for Dired
(require 'all-the-icons)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(provide 'modes)
;;; modes.el ends here
