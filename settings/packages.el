;; add repositories
(require 'package)

(custom-set-variables
 '(tls-checktrust t)
 '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))))

(package-initialize)

(unless package-archive-contents
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(package-install 'magit)
(package-install 'yaml-mode)

(provide 'packages)
