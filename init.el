
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Add melpa package list
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-list
      '(fill-column-indicator js2-mode csv-mode company yasnippet yasnippet-snippets markdown-mode ess terraform-mode groovy-mode solidity-mode typescript-mode))

;; fetch the list of packages available
;; Remove the elpa/archives directory to force a refresh
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
  (interactive "sTerminal emulator name: ")
  (ansi-term "/bin/bash")
  (rename-buffer buffer-name t))

;; install js2 as a minor mode just for JS linting
(add-hook 'js-mode-hook 'js2-minor-mode)

;; manually installed local packages
(add-to-list 'load-path "~/.emacs.d/local-packages/")

(put 'downcase-region 'disabled nil)

;; configure emacs auto-saves
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; long live python 3
(setq python-shell-interpreter "python3")

;; mark variables to be set in .dir-locals.el as safe
(put 'eglot-server-programs 'safe-local-variable 'listp)
(put 'python-shell-interpreter 'safe-local-variable 'stringp)

;; start emacs server (for use with emacsclient)
(server-start)

;; Setup solidity
(require 'solidity-mode)

;; enable org-mode source languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)))

;; Set list of agenda files
(setq org-agenda-files '("~/digital-garden"))

;; enable company mode globally
(global-company-mode)

;; setup yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; configure automatic modes based on file extension
(require 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . js-jsx-mode))
