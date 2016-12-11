;; navigate through windows with shift + arrow keys
(windmove-default-keybindings)

;; F11 to toggle full screen
(global-set-key [f11] 'toggle-fullscreen)

;; make cursor movement keys under right hand's home-row.
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-u") 'backward-word)
(global-set-key (kbd "M-o") 'forward-word)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-a") 'beginning-of-line)
(global-set-key (kbd "M-e") 'end-of-line)

;; move lines and paragraphs
(global-set-key (kbd "C-S-i") 'move-text-up)
(global-set-key (kbd "C-S-k") 'move-text-down)
(global-set-key (kbd "C-S-j") 'move-text-left)
(global-set-key (kbd "C-S-l") 'move-text-right)

;; tabbar tabs switching
(global-set-key [C-tab] 'tabbar-forward-tab)
(global-set-key [C-M-tab] 'tabbar-forward-group)

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
