
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (elpy fill-column-indicator js2-mode)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Andale Mono" :foundry "unknown" :slant normal :weight normal :height 140 :width normal)))))

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install packages
;; ----------------
;; The following would work for emacs versions 25.1+
;; (package-install-selected-packages)
;;
;; This works for emacs < 25.1
;; TODO: dynamically check version
(dolist (package package-selected-packages)
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

;; Add melpa package list
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; enable elpy. Do not forget to install the external dependencies
(elpy-enable)

;; configure emacs auto-saves
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
