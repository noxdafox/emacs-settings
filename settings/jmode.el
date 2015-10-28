;; C/C++ Mode

(require 'jdee)
(require 'column-marker)

(add-hook 'java-mode-hook 'jdee-mode)
(add-hook 'java-mode-hook (lambda () (column-marker-1 80)))

(provide 'jmode)
