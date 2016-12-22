;; Emacs custom key bindings

(require 'use-package)

;; navigate through windows with shift + arrow keys
(windmove-default-keybindings)

;; F11 to toggle full screen
(bind-key* "<f11>" 'toggle-fullscreen)

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

;; cycle through buffers
(bind-key* "C-<tab>" 'next-buffer)

;; show documentation at point
(bind-key* "M-h" 'ycmd-show-documentation)

;; comment region
(bind-key* "M-c" 'comment-or-uncomment-region)

;; company completion key
(define-key company-active-map (kbd "<tab>") #'company-complete)

;; utility functions
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
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
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(provide 'keybindings)
