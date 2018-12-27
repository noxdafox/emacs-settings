;; add repositories
(require 'package)

(custom-set-variables
 '(tls-checktrust t)
 '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))))

(when (not package-archive-contents)
    (package-list-packages))

;; dependencies
(package-install 'jdee)
(package-install 'magit)
(package-install 'geiser)
(package-install 'erlang)
(package-install 'ggtags)
(package-install 'rust-mode)
(package-install 'yaml-mode)
(package-install 'alchemist)
(package-install 'racket-mode)
(package-install 'sr-speedbar)
(package-install 'use-package)
(package-install 'column-marker)
(package-install 'bash-completion)
(package-install 'smart-mode-line)
(package-install 'lsp-mode)
(package-install 'lsp-ui)
(package-install 'company)
(package-install 'company-lsp)
(package-install 'flycheck)
(package-install 'flycheck-rust)
(package-install 'flycheck-credo)
(package-install 'all-the-icons)
(package-install 'all-the-icons-dired)
(package-install 'dockerfile-mode)

(provide 'packages)
