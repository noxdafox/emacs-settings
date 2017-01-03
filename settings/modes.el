;; Modes specific customisations

;; Lisp
(require 'flycheck)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; C/C++ Mode
;; ----------
(custom-set-variables
 '(c-basic-offset 4)
 '(c-indent-level 4)
 '(c-file-style "K&R"))

;; disable electric comma
(eval-after-load "cc-mode"
  '(define-key c-mode-base-map "," nil))

;; Python Mode
;; -----------
;; override syntax checker, use pylint
(require 'flycheck)
(add-hook 'python-mode-hook
          (lambda () (add-to-list 'flycheck-disabled-checkers 'ycmd)))
(add-hook 'python-mode-hook 'flycheck-mode)
(setq flycheck-python-pylint-executable "pylint3")

;; ipython interpreter
(setq python-shell-interpreter "ipython3"
    python-shell-interpreter-args "-i")

;; Shell mode
;; ----------
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; file completion space/slash insertion
 '(comint-buffer-maximum-size 8192))    ; maximum buffer size in lines

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
;; --------
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(provide 'modes)
;;; modes.el ends here
