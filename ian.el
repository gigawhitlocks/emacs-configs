;;; ian.el --- my custom emacs config with no one else considered because fuck you
;;;            naw but really I just don't have the time for that
;;;
;;; Commentary:
;;;
;;; After throwing away an old Emacs config, built when I had no idea what I was doing
;;; and abandoning the "wisdom of the crowds"-configured Spacemacs for better control
;;; here we are for better or worse
;;;
;;; Code:

(defun bootstrap ()
  "Install use-package and melpa to prepare for installation of other packages."

  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)

  ;; Now install use-package to enable us to use it
  ;; to manage the rest of our packages
  
  (unless (package-installed-p 'use-package)
    (progn
      (unless package-archive-contents
	(package-refresh-contents))
      (package-install 'use-package)))

  ;; use-package and package.el don't know how to install
  ;; an up-to-date version of org-mode
  ;; so part of this bootstrap process, since org-mode
  ;; is a built-in and we want changes from other
  ;; layers to apply to our updated Org as things are
  ;; installed, is to manually update Org before
  ;; even use-package is set up
  ;; credit https://github.com/jwiegley/use-package/issues/319

  ;; TODO this only seems to work after some other initialization step has run
  ;; one time. I'm not sure what that step is, just that this next step only
  ;; succeeds if it's not the first time we've run emacs with this configuration.
  ;; For now I'm going to leave this commented out, until I figure out
  ;; what is causing this behavior

  ;;(unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))
  ;;  (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))

  ;; set ensure to be the default
  (require 'use-package-ensure)
  (setq use-package-always-ensure t)

  ;; allow use-package to install system tools via apt, brew
  (use-package use-package-ensure-system-package)

  ;; sane keybindings from the start
  (use-package general)

  ;; these go in bootstrap because packages installed
  ;; with use-package use :diminish and :delight
  (use-package diminish)
  (use-package delight))

(defun global-packages ()
  "Install and configure packages used with many modes and standalone modes and applications."
  
  (defun setup-projectile ()
    (use-package projectile
      :delight)
    (use-package helm-projectile)
    (projectile-mode +1))

  (defun setup-evil ()
    "Install and configure evil-mode and related bindings."
    (use-package evil
      :init
      (setq evil-want-keybinding nil)
      (setq evil-want-integration t)
      :config
      (evil-mode 1))

    (use-package evil-collection
      :after evil
      :config
      (evil-collection-init))

    ;; bindings for org-mode
    (use-package evil-org)

    ;; add fd as a remap for esc
    (use-package evil-escape
      :delight)

    (evil-escape-mode 1)
    (setq-default evil-escape-key-sequence "fd"))

  (defun setup-magit ()
    (use-package magit)
    ;; disable the default emacs vc because git is all I use,
    ;; for I am a simple man
    (setq vc-handled-backends nil)
    (use-package evil-magit))

  (use-package which-key
    :delight
    :init
    (which-key-mode)
    (which-key-setup-minibuffer))

  ;; anything so trivial that there is no config necessary goes here
  (defun extra-packages ()
    (use-package restart-emacs)
    ;; themes
    ;;(use-package color-theme-sanityinc-tomorrow)
    (use-package leuven-theme)

    ;; other stuff
    (use-package origami
      :config
      (global-origami-mode))

    (use-package treemacs))

  ;; auto-completion
  (use-package company
    :delight
    :config
    ;; enable it everywhere
    (add-hook 'after-init-hook 'global-company-mode))

  ;; linter
  (use-package flycheck
    :delight
    ;; enable it everywhere
    :init (global-flycheck-mode))

  ;; helm
  (defun setup-helm ()
    "Install and configure helm, the most important command and control center"
    (use-package helm
      :delight
      :config
      (global-set-key (kbd "M-x") #'helm-M-x)
      (add-to-list 'evil-collection-mode-list 'helm-mode)
      (define-key helm-find-files-map "\t" 'helm-execute-persistent-action)
      (helm-mode 1)))
  
  (setup-evil)
  (setup-projectile)
  (setup-magit)
  (setup-helm)
  (extra-packages))

(defun languages ()
  "Setup for specific programming languages."

  (defun setup-lsp ()
    "Enable nice rendering of diagnostics like compile errors."
    (setq lsp-scala-server-command "/usr/local/bin/metals-emacs")
    (use-package lsp-mode
      :init (setq lsp-prefer-flymake nil))

    (use-package lsp-ui
      :init (setq lsp-ui-doc-position 'point))

    ;; Add lsp backend for other tools
    (use-package company-lsp)
    (use-package lsp-origami))

  (defun scala ()
    "Enable scala-mode and sbt-mode."
    ;; this was taken from the install instructions 4/24/2019
    (use-package scala-mode
      :mode "\\.s\\(cala\\|bt\\)$")

    (use-package sbt-mode
      :commands sbt-start sbt-command
      :config
      ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
      ;; allows using SPACE when in the minibuffer
      (substitute-key-definition
       'minibuffer-complete-word
       'self-insert-command
       minibuffer-local-completion-map))

    (use-package lsp-scala
      :after scala-mode
      :demand t
      ;; Enable lsp-scala automatically in scala files
      :hook (scala-mode . lsp)))

  (defun docker ()
    (use-package dockerfile-mode)
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
    (put 'dockerfile-image-name 'safe-local-variable #'stringp))

  (defun python ()
    (use-package anaconda-mode
      :config
      (add-hook 'python-mode-hook 'anaconda-mode)
      (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

  (setup-lsp)
  (python)
  (docker)
  (scala))

(defun config ()
  "Global configuration variables and such."

  ;; a helper function that flips between our main theme and a dark alternative
  ;; (defun toggle-theme ()
  ;;   (interactive)
  ;;   (if (string= "leuven" (car custom-enabled-themes))
  ;; 	(color-theme-sanityinc-tomorrow-eighties)
  ;;     (load-theme 'leuven t)))

  (general-create-definer my-leader-def
    ;; :prefix my-leader
    :prefix "SPC")

  (general-create-definer my-local-leader-def
    ;; :prefix my-local-leader
    :prefix "SPC m")

  ;; global keybindings
  (my-leader-def
    :keymaps	'normal
    ;; buffer control
    "bb"	'switch-to-buffer
    "TAB"	'switch-to-prev-buffer
    "bd"	'kill-buffer-ask

    ;; errors
    "ec"        'flycheck-clear
    "el"	'flycheck-list-errors
    "en"        'flycheck-next-error
    "ep"        'flycheck-previous-error

    ;; hmm
    "ff"	'helm-find-files
    "fed"       '(lambda () (interactive)
		   (find-file "~/.emacs.d/ian.el"))

    ;; git
    "gb"	'magit-blame
    "gs"	'magit-status
    "gg"	'magit
    "gd"	'magit-diff

    ;; bookmarks (j for jump)
    "jj"        'bookmark-jump
    "js"        'bookmark-set

    ;; org

    ;; projectile
    "p"		'projectile-command-map
    "pf"	'helm-projectile-find-file

    ;; quitting
    "qq"        'exit-emacs
    "qr"        'restart-emacs
    
    ;; simple toggles
    "tn"	'linum-mode
    ;; "tt"	'toggle-theme

    ;; window control
    "w-"	'split-window-below
    "w/"	'split-window-right
    "wj"        (lambda () (interactive)
		  (select-window (window-in-direction 'below)))
    "wk"        (lambda () (interactive)
		  (select-window (window-in-direction 'above)))
    "wh"        (lambda () (interactive)
		  (select-window (window-in-direction 'left)))
    "wl"        (lambda () (interactive)
		  (select-window (window-in-direction 'right)))
    "wd"	'delete-window
    "wD"	'delete-other-windows
    "wo"	'other-window

    ";"         'comment-line

    "SPC"	'helm-M-x)

  (my-local-leader-def 'normal emacs-lisp-mode-map
    "e" 'eval-last-sexp)

  ;; Fontify the whole line for headings (with a background color).
  (setq org-fontify-whole-heading-line t)

  ;; backups to /tmp
  (setq backup-directory-alist `(("." . "/tmp/.emacs-saves")))
  (setq backup-by-copying t)

  ;; load the best theme, leuven
  (load-theme 'leuven t)

  (diminish 'eldoc-mode)
  (diminish 'undo-tree-mode)
  (diminish 'auto-revert-mode)

  ;; less annoying bell (from emacs wiki)
  ;; flashes the modeline foreground
  (setq ring-bell-function
	(lambda ()
	  (let ((orig-fg (face-foreground 'mode-line)))
	    ;; change the flash color here
	    ;; overrides themes :P
	    ;; guess that's one way to do it
	    (set-face-foreground 'mode-line "#F2804F")
	    (run-with-idle-timer 0.1 nil
				 (lambda (fg) (set-face-foreground 'mode-line fg))
				 orig-fg))))

  ;; easily take gifs (if byzanz-record is available.. might only work in Linux? not tested)
  (defun gif-this-frame (duration)
    (interactive "sDuration: ")
    (start-process "emacs-to-gif" nil
		   "byzanz-record"
		   "-d" duration
		   "-w" (number-to-string (+ 5 (frame-pixel-width)))
		   "-h" (number-to-string (+ 50 (frame-pixel-height)))
		   "-x" (number-to-string (frame-parameter nil 'left))
		   "-y" (number-to-string (+ (frame-parameter nil 'top) 10))
		   (concat "~/emacs_gifs/" (format-time-string "%Y-%m-%dT%T") ".gif")))

  ;; remove extraneous window chrome
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (scroll-bar-mode -1))

(defun main()
  "Initialize everything!"
  (bootstrap)
  (global-packages)
  (languages)
  (config))

(provide '~/.emacs.d/ian.el)
;;; ian.el ends here
