
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Add melpa-stable and melpa package lists
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq package-archive-priorities
      '(("gnu" . 8)
	("nongnu" . 9)
	("melpa-stable" . 10)
	("melpa" . 7)))

(setq package-list
      '(fill-column-indicator
	js2-mode csv-mode company yasnippet yasnippet-snippets
	markdown-mode ess terraform-mode groovy-mode solidity-mode
	typescript-mode elfeed org-trello python-black pyvenv gptel
	dot-env htmlize go-mode protobuf-mode vterm))

;; Fetch the list of packages available
;; Remove the elpa/archives directory to force a refresh
(unless package-archive-contents
  (package-refresh-contents))

;; install packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Read .env variables
(require 'dot-env)
(setq dot-env-filepath "~/.env")
(dot-env-config)

;; Activate protobuf-mode
(require 'protobuf-mode)

;; Activate vterm
(require 'vterm)

;; short answers y/n
(setq use-short-answers t)

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
(setq backup-directory-alist `(("." . "~/.autosaves-emacs")))
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

;; Setup js-comint for interactive JS dev
(require 'js-comint)
;; Copying python-mode bindings
(define-key js-mode-map (kbd "C-c C-e") 'js-comint-send-last-sexp)
(define-key js-mode-map (kbd "C-c C-c") 'js-comint-send-buffer)
(define-key js-mode-map (kbd "C-c C-r") 'js-comint-send-region)
(define-key js-mode-map (kbd "C-c C-z") 'js-comint-start-or-switch-to-repl)

;; enable org-mode source languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)))

;; Set list of agenda files
(setq org-agenda-files '("~/digital-garden"))

;; disable spaces added in source code blocks
(setq org-edit-src-content-indentation 0)


(defun blog/get-draft-filename ()
  "Return the name for a draft blog post composition"
  (interactive)
  (let ((name (read-string "Filename: ")))
    (format "%s.draft.org" name)))

;; setup org capture templates
(setq org-capture-templates
      '(("d" "Templates for the digital garden")
	("dp" "post" plain
	 (file (lambda ()
		 (concat "~/digital-garden/" (blog/get-draft-filename))))
	 (file  "~/projects/digital-garden-blog/org-templates/draft.org"))))

;; enable company mode globally
(global-company-mode)

;; setup yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; configure automatic modes based on file extension
(require 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . js-jsx-mode))

;; Easily start/stop the scaffold eth commands in separate terminals
;; Ensure you're in the scaffold-eth root directory
(defun scaffold-eth/stop ()
  (interactive)
  (ignore-errors (kill-buffer "yarn-deploy"))
  (ignore-errors (kill-buffer "yarn-start"))
  (ignore-errors (kill-buffer "yarn-chain")))

(defun scaffold-eth/start ()
  (interactive)

  (if (or
       (get-buffer "*yarn-chain*")
       (get-buffer "*yarn-start*")
       (get-buffer "*yarn-deploy*"))
      (message "Already running, use the stop command first")
    (progn
      (vterm "yarn-chain")
      (comint-send-string "yarn-chain" "yarn chain\n")
      (sleep-for 3) ; give the chain a few seconds to go up
      (vterm "yarn-start")
      (comint-send-string "yarn-start" "yarn start\n")
      (vterm "yarn-deploy")
      (comint-send-string "yarn-deploy" "yarn deploy\n"))))

;; setup org-trello
(require 'org-trello)

;; activate org-trello by file extenstion .trello.org
;; https://github.com/org-trello/org-trello/issues/249#issuecomment-303229131
(defun my/org-mode-hook-org-trello-mode ()
  (when (and (buffer-file-name)
             (string-match "\\.trello.org$" (buffer-file-name)))
      (message "Turning on org-trello in %s" (buffer-file-name))
      (org-trello-mode)))
(add-hook 'org-mode-hook #'my/org-mode-hook-org-trello-mode)

;; RSS subscriptions via elfeed
(setq elfeed-feeds
      '(("https://programmer.ke/index.xml" mine)
	("https://osanseviero.github.io/hackerllama/blog/index.xml" ml)
	("https://nullprogram.com/feed/" skeeto emacs)
	("https://micronews.debian.org/feeds/feed.rss" debian-news)
	("https://world.hey.com/dhh/feed.atom" dhh)
	("https://www.answer.ai/index.xml" answer-ai)
	("https://third-bit.com/atom.xml" third-bit)
	("https://www.openmymind.net/atom.xml" openmymind)
	("https://archive.casouri.cc/note/atom.xml" casouri emacs)
	("https://lukesmith.xyz/index.xml" lukesmith)))

;; load gptel config
(load-file "~/.emacs.d/gptel_init.el")

;; activate emacs-copilot
(require 'copilot)

;; custom key bindings
(keymap-global-set "C-c g" 'gptel-menu)
