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

;; move lines and paragraphs with right hand
(global-set-key (kbd "C-S-i") 'move-line-up)
(global-set-key (kbd "C-S-k") 'move-line-down)
(global-set-key (kbd "C-S-j") 'move-region-up)
(global-set-key (kbd "C-S-l") 'move-region-down)
(global-set-key (kbd "C-S-o") 'move-region-right)
(global-set-key (kbd "C-S-u") 'move-region-left)

;; move lines and regions
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(defun move-region-right ()
  "Move the current line right."
  (interactive)
  (shift-region 1))

(defun move-region-left ()
  "Move the current line left."
  (interactive)
  (shift-region -1))

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(provide 'editor)
