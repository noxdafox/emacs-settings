;; Java Mode

(require 'jdee)
(require 'ggtags)
(require 'flycheck)
(require 'column-marker)

(add-hook 'java-mode-hook 'ggtags-mode)
(add-hook 'java-mode-hook 'flycheck-mode)
(add-hook 'java-mode-hook (lambda () (column-marker-1 80)))

(provide 'jmode)
