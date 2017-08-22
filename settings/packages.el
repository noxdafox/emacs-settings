;; add repositories
(require 'package)

(custom-set-variables
 '(tls-checktrust t)
 '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))))

(package-initialize)

;; dependencies
(package-install 'jdee)
(package-install 'magit)
(package-install 'erlang)
(package-install 'ggtags)
(package-install 'alchemist)
(package-install 'use-package)
(package-install 'column-marker)
(package-install 'bash-completion)
(package-install 'smart-mode-line)
(package-install 'ycmd)
(package-install 'company)
(package-install 'company-ycmd)
(package-install 'flycheck)
(package-install 'flycheck-ycmd)
(package-install 'flycheck-credo)

(provide 'packages)
