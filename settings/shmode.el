;; Shell mode

(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; file completion space/slash insertion
 )

(require 'bash-completion)
(bash-completion-setup)

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

(provide 'shmode)
