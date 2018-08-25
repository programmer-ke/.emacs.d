
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Add melpa package list
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-list '(fill-column-indicator js2-mode elpy))

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; configure line length indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode
 global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)

;; disable toolbar
(tool-bar-mode -1)

;; easily start a new named terminal emulator
(defun start-term (buffer-name)
  "Start a bash terminal and rename buffer."
  (interactive "Terminal emulator name: ")
  (ansi-term "/bin/bash")
  (rename-buffer buffer-name t))

;; js2-mode - javascript editing as a major mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; manually installed local packages
(add-to-list 'load-path "~/.emacs.d/local-packages/")

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(put 'downcase-region 'disabled nil)

;; enable elpy. Do not forget to install the external dependencies
(elpy-enable)

;; configure emacs auto-saves
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
