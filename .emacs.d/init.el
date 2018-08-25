
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
 '(package-selected-packages (quote (elpy fill-column-indicator auto-complete js2-mode)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Andale Mono" :foundry "unknown" :slant normal :weight normal :height 140 :width normal)))))

;; configure line length indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode
 global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)

;; load sbcl
(setq inferior-lisp-program "/usr/local/bin/sbcl")
;; quicklisp helper
(load (expand-file-name "~/quicklisp/slime-helper.el"))

;; disable toolbar
(tool-bar-mode -1)

;; easily start a new named terminal emulator
(defun start-term (buffer-name)
  "Start a bash terminal and rename buffer."
  (interactive "Terminal emulator name: ")
  (ansi-term "/bin/bash")
  (rename-buffer buffer-name t))

;; Start terminals and load saved buffers
(defun bootstrap-me ()
  (interactive)
  (start-term "cws")
  (start-term "other")
  (start-term "pesa")
  (start-term "ussd")
  (start-term "ussd-simulator")
  (start-term "sms")
  (start-term "status")
  (start-term "consul")
  (start-term "partner-portal")
  (start-term "ping")
  (desktop-read))

;; js2-mode - javascript editing as a major mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; manually installed local packages
(add-to-list 'load-path "~/.emacs.d/local-packages/")

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(put 'downcase-region 'disabled nil)

;; Add melpa and elpy package lists
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)
(elpy-enable)

;; configure auto-complete
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)


;; configure emacs auto-saves
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)


;; mac - use cmd key as ctrl key
(setq mac-command-modifier 'control)
