;; MELPA repositories
(require 'package)
(package-initialize)
(custom-set-variables
  '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
		       ("melpa" . "https://melpa.org/packages/"))))

;; TLS for package repositories
(custom-set-variables
 '(tls-checktrust t))

;; semantic mode
(require 'semantic)
(custom-set-variables
 '(semantic-mode t))

;; speed bar
(require 'sr-speedbar)
(custom-set-variables
 '(speedbar-use-images nil)
 '(sr-speedbar-right-side nil)
 '(speedbar-show-unknown-files t))
(sr-speedbar-open)

;; minimal ide
(custom-set-variables
 '(tooltip-mode nil)
 '(tool-bar-mode nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(mode-line-format nil))

;; F11 to toggle full screen
(global-set-key [f11] 'toggle-fullscreen)

;; navigate through windows with shift + arrow keys
(windmove-default-keybindings)

;; uniquify buffer names
(require 'uniquify)
(custom-set-variables
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; IDO mode
(ido-mode t)

;; Linum
(require 'linum)
(global-linum-mode t)

;; Tabbar
(require 'tabbar)
;; Tabbar settings
(set-face-attribute
 'tabbar-default nil
 :background "gray20"
 :foreground "gray20"
 :underline nil
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :underline nil
 :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "gray75"
 :foreground "black"
 :underline nil
 :box '(:line-width 5 :color "gray75" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 5 :color "white" :style nil))
(set-face-attribute
 'tabbar-button nil
 :underline nil
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :background "gray20"
 :underline nil
 :height 0.6)
;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 '(tabbar-separator (quote (0.5))))

(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label (if tabbar--buffer-show-groups
		   (format "[%s] " (tabbar-tab-tabset tab))
		 (format "%s " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
	label
      (tabbar-shorten
       label (max 1 (/ (window-width)
		       (length (tabbar-view
				(tabbar-current-tabset)))))))))
(tabbar-mode t)
(global-set-key [C-tab] 'tabbar-forward-tab)
(global-set-key [C-M-tab] 'tabbar-forward-group)

(defun byte-compile-current-buffer ()
  "byte-compile current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(provide 'ide)
