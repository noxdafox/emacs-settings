;; Emacs custom key bindings

;; navigate through frames and windows
(windmove-default-keybindings)
(bind-key* "<backtab>" 'other-frame)

;; F11 to toggle full screen
(bind-key* "<f11>" 'toggle-frame-fullscreen)

;; FlySpell correction
(bind-key* "C-l" 'flyspell-correct-word-before-point)

;; make cursor movement keys under right hand's home-row.
(bind-key* "M-i" 'previous-line)
(bind-key* "M-j" 'backward-char)
(bind-key* "M-k" 'next-line)
(bind-key* "M-l" 'forward-char)
(bind-key* "M-u" 'backward-word)
(bind-key* "M-o" 'forward-word)
(bind-key* "M-n" 'forward-paragraph)
(bind-key* "M-p" 'backward-paragraph)
(bind-key* "M-a" 'beginning-of-line)
(bind-key* "M-e" 'end-of-line)

;; move lines and paragraphs
(bind-key* "C-S-i" 'move-text-up)
(bind-key* "C-S-k" 'move-text-down)
(bind-key* "C-S-j" 'move-text-left)
(bind-key* "C-S-l" 'move-text-right)

;; advanced kill functions
(bind-key* "C-w" 'kill-command)
(bind-key* "M-w" 'kill-ring-save-command)

;; grep at point
(bind-key* "C-f" 'grep-symbol)

;; cycle through buffers
(bind-key* "C-<tab>" 'next-buffer)

;; region code manipulation
(bind-key* "M-m" 'mark-paragraph)
(bind-key* "M-r" 'replace-string)
(bind-key* "M-c" 'comment-or-uncomment)

;; text scaling
(bind-key* "C-+" 'text-scale-increase)
(bind-key* "C--" 'text-scale-decrease)

;; org mode
(bind-key* "C-c a" 'org-agenda)
(bind-key* "C-c c" 'org-capture)
(bind-key* "C-c l" 'org-store-link)

;; toggle speedbar
(bind-key* "<f8>" 'sr-speedbar-toggle)

;; force the reload of a buffer
(bind-key* "<f5>" 'force-revert-buffer)

;; special characters
(bind-key* (kbd "C-a") 'special-character)

;; shell mode history search
(use-package shell
  :bind (:map shell-mode-map
              ("<down>" . comint-next-input)
              ("<up>" . comint-previous-input)
              ("C-r" . comint-history-isearch-backward-regexp)))

(use-package python
  :bind (:map inferior-python-mode-map
              ("<down>" . comint-next-input)
              ("<up>" . comint-previous-input)
              ("C-r" . comint-history-isearch-backward-regexp)))

;; lisp mode documentation
(define-key lisp-mode-map (kbd "M-h") 'describe-symbol)
(define-key emacs-lisp-mode-map (kbd "M-h") 'describe-symbol)

;; racket mode documentation
(use-package racket-mode
  :ensure t
  :bind (:map racket-mode-map
              ("M-h" . racket-describe)
         :map racket-repl-mode-map
              ("M-h" . racket-describe)))

;; elixir mode alchemist documentation
(use-package alchemist
  :ensure t
  :bind (:map elixir-mode-map
              ("M-h" . alchemist-help-search-at-point)))

;; geiser start REPL and compile
(use-package geiser
  :ensure t
  :bind (:map geiser-mode-map
              ("C-c C-k" . geiser-run-and-compile)))

;; utility functions
(defun toggle-fullscreen ()
  "Toggle full screen on X11."
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(defun move-text-up ()
  "Moves the line or the region up."
  (interactive)
  (if (use-region-p)
      (move-region (region-beginning) (region-end) -1)
    (move-line-up)))

(defun move-text-down ()
  "Moves the line or the region down."
  (interactive)
  (if (use-region-p)
      (move-region (region-beginning) (region-end) 1)
    (move-line-down)))

(defun move-text-right ()
  "Move the current line right."
  (interactive)
  (shift-region 1))

(defun move-text-left ()
  "Move the current line left."
  (interactive)
  (shift-region -1))

(defun move-line-up ()
  "Move the current line up."
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move the current line down."
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (setq deactivate-mark nil))))

(defun comment-or-uncomment ()
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region
     (line-beginning-position) (line-end-position))))

(defun grep-symbol ()
  (interactive)
  (grep-compute-defaults)
  (rgrep (thing-at-point 'symbol t)
         (string-join '("*.[ch]" "*.cc" "*.[ch]xx" "*.[ch]pp" "*.[CHh]" "*.CC"
                        "*.HH" "*.[ch]++" "[Mm]akefile*" "*.py" "*.erl" "*.ex"
                        "*.exc" "*.tex" "*.sty" "*.js" "*.el") " ")
         (ido-read-directory-name "Root folder:")))

(defun kill-command ()
  "Kill region if active, symbol otherwise."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (beginning-of-thing 'symbol) (end-of-thing 'symbol))))

(defun kill-ring-save-command ()
  "Save region if active, symbol otherwise."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (beginning-of-thing 'symbol) (end-of-thing 'symbol))))

(defun special-character (s)
  "Insert a special character according to its type"
  (interactive "sInput special character: ")
  (cond ((string= s "l") (insert "λ"))
        ((string= s "t") (insert "™"))))

(defun geiser-run-and-compile ()
  "Start the REPL if not running before compiling."
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (geiser-repl--connection*)
      (call-interactively 'run-geiser))
    (set-buffer buffer)
    (geiser-compile-current-buffer)))

(defun force-revert-buffer ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(provide 'keybindings)
