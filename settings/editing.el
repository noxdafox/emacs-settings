;; Major modes
;;;; Python Mode
(add-hook 'python-mode-hook
	  '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
;;;; C/C++ Mode
(add-hook 'c-mode-hook
	  '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'c++-mode-hook
	  '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
;;;; Javascript Mode
(add-hook 'javascript-mode-hook
	  '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
;;;; AUCTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook
	  '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

;; typing helpers
(show-paren-mode t)
(electric-pair-mode t)
(setq mouse-yank-at-point t)
(setq-default indent-tabs-mode nil)
(global-set-key (kbd ",") (lambda() (interactive) (insert ", ")))
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-<left>") 'move-region-up)
(global-set-key (kbd "M-<right>") 'move-region-down)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; auto Complete Mode
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start 3) ; start AC at third character
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

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
