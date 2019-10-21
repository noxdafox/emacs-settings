;; Modes specific customisations

;; Lisp
(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

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
(use-package flycheck
  :hook (python-mode . flycheck-mode)
  :config
  (setq flycheck-python-pylint-executable "pylint"))

; ipython interpreter
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args
      (string-join '("-i " "--TerminalIPythonApp.interactive_shell_class"
                     "=" "rlipython.TerminalInteractiveShell") ""))

;; Comint mode
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; scroll to bottom on input
 '(comint-move-point-for-output nil)    ; disable autoscrolling
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; file completion space/slash insertion
 '(comint-buffer-maximum-size 8192)     ; maximum buffer size in lines
 '(comint-prompt-read-only t))          ; set comint prompt read-only

(add-hook 'comint-preoutput-filter-functions
          'comint-preoutput-turn-buffer-read-only)

(defun comint-preoutput-turn-buffer-read-only (text)
  (propertize text 'read-only t))

; Fix progress bar for programs such as apt
(advice-add
 'ansi-color-apply-on-region
 :before 'ora-ansi-color-apply-on-region)

(defun ora-ansi-color-apply-on-region (begin end)
  "Display progress bar in the mode line."
  (let ((end-marker (copy-marker end)) mb)
    (save-excursion
      (goto-char (copy-marker begin))
      (while (re-search-forward "\0337" end-marker t)
        (setq mb (match-beginning 0))
        (when (re-search-forward "\0338" end-marker t)
          (ora-apt-progress-message
           (substring-no-properties
            (delete-and-extract-region mb (point))
            2 -2)))))))

(defun ora-apt-progress-message (progress)
  (message
   (replace-regexp-in-string
    "%" "%%"
    (ansi-color-apply progress))))

; do not override prompt colors
(set-face-attribute 'comint-highlight-prompt nil :inherit nil)

(use-package bash-completion
  :ensure t
  :config
  (bash-completion-setup))

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
(use-package tex
  :ensure auctex
  :defer t
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . LaTeX-math-mode))
  :config
  (setq TeX-PDF-mode t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-engine 'xetex)
  (setq-default TeX-master nil))

;; Org mode
(use-package org
  :config
  (make-directory "~/documents/org/" t)
  (setq org-log-done 'time)
  (setq org-directory "~/documents/org/")
  (setq org-agenda-files (list org-directory))
  (setq org-default-todo-file (concat org-directory "todo.org"))
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-todo-keywords '((sequence "TODO" "|" "DONE" "TRASHED")))
  (setq org-capture-templates
        '(("t" "Todo" entry (file org-default-todo-file)
           "* TODO %?\n  %i\n  %a")))
  ;; Org Beamer for presentations
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode %f"
          "xelatex -shell-escape -interaction nonstopmode %f")))

(use-package ox-latex
  :config
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options
        '(("frame" "none")
	  ("fontsize" "\\scriptsize")
	  ("linenos" "")))
  (setq org-latex-minted-langs
        (append org-latex-minted-langs '((elixir "elixir")
                                         (erlang "erlang")
                                         (python "python")))))

; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

; Org Journal mode
(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir "~/documents/journal/"))

(use-package flyspell
  :hook ((rst-mode . flyspell-mode)
         (org-mode . flyspell-mode)
         (text-mode . flyspell-mode))
  :config
  (setq ispell-dictionary "en_GB"))

;; Icons mode for Dired
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; nXML mode
(use-package nxml-mode
  :defer t
  :config
  (defun format-xml ()
    (interactive)
    (execute-kbd-macro (kbd "M-% > < RET > C-q C-j < RET ! C-M-\\"))))

;; Guile Mode
(use-package geiser
  :ensure t
  :defer t
  :hook (scheme-mode . geiser-mode)
  :config
  (setq geiser-repl-use-other-window nil))

;; Elixir-ls
(use-package lsp-elixir
  :ensure t
  :defer t
  :hook (elixir-mode . lsp))

;; Racket mode
(use-package racket-mode
  :ensure t
  :mode "\\.rkt\\'")

;; Docker mode
(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode "\\Dockerfile")

(use-package json-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(provide 'modes)
;;; modes.el ends here
