;; Java Mode

(require 'jdee)
(require 'ggtags)

(add-hook 'java-mode-hook 'ggtags-mode)

(provide 'jmode)
