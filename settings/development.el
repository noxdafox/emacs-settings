;; Development specific configuration

(use-package company
  :ensure t
  :bind (:map company-active-map
              ("<tab>" . company-complete))
  :config
  (global-company-mode t)
  (setq company-idle-delay 0.1))

(use-package magit
  :ensure t
  :config
  (setq auto-revert-check-vc-info t))

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-python-pylint-executable "pylint")
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook (after-init . global-flycheck-mode))

;; Tree Sitter
(use-package tree-sitter
  :ensure t
  :defer t
  :hook (((c-mode c++-mode elixir-mode json-mode emacs-lisp-mode python-mode yaml-mode) . tree-sitter-mode)
         ((c-mode c++-mode elixir-mode json-mode emacs-lisp-mode python-mode yaml-mode) . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; Language Server Protocol
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (elixir-mode . lsp)
         (python-mode . lsp))
  :config
  (setq lsp-lens-enable nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-snippet nil)
  (setq-default lsp-pylsp-plugins-pylint-enabled t)
  (setq-default lsp-pylsp-plugins-flake8-enabled nil)
  (setq-default lsp-pylsp-plugins-pydocstyle-enabled nil))

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-delay 2)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  :commands lsp-ui-mode)

;; Enable line-number-mode when developing
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Code navigation
(use-package ggtags
  :ensure t
  :hook ((prog-mode . ggtags-mode)
         (after-save . gtags-update-hook)))

(defun gtags-root-dir ()
  "Return GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update ()
  "Make GTAGS incremental update."
  (call-process "global" nil nil nil "-u"))

(defun gtags-update-hook ()
  (when (gtags-root-dir)
    (gtags-update)))

(provide 'development)
;;; development.el ends here
