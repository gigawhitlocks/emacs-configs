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

    (unless (package-installed-p 'use-package)
      (progn
	(unless package-archive-contents
	  (package-refresh-contents))
	(package-install 'use-package)))

    (require 'use-package-ensure)
    (setq use-package-always-ensure t))

(defun global-packages ()
  "Install and configure packages used with many modes and standalone modes and applications."
  (use-package use-package-ensure-system-package)
  
  (defun setup-projectile ()
    (use-package projectile
      :delight)
    (use-package helm-projectile)
    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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
    (use-package color-theme-sanityinc-tomorrow)
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

  ;; sane keybindings
  (use-package general)

  ;; take control of the modeline
  (use-package diminish)
  (use-package delight)
  
  ;; helm
  (defun setup-helm ()
    "Install and configure helm, the most important command and control center"
    (use-package helm
      :delight
      :config
      (global-set-key (kbd "M-x") #'helm-M-x)
      (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
      (global-set-key (kbd "C-x C-f") #'helm-find-files)
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
    (use-package lsp-mode
      :init (setq lsp-prefer-flymake nil))

    (use-package lsp-ui
      :init (setq lsp-ui-doc-position 'bottom))

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
  (defun toggle-theme ()
    (interactive)
    (if (string= "leuven" (car custom-enabled-themes))
	(color-theme-sanityinc-tomorrow-eighties)
      (load-theme 'leuven t)))

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
    "bk"	'kill-buffer

    ;; errors
    "el"	'flycheck-list-errors
    "en"        'flycheck-next-error
    "ep"        'flycheck-previous-error

    ;; hmm
    "ff"	'helm-find-files

    ;; git
    "gb"	'magit-blame
    "gs"	'magit-status
    "gg"	'magit
    "gd"	'magit-diff

    ;; projectile
    "p"		'projectile-command-map
    "pf"	'helm-projectile-find-file

    ;; simple toggles
    "tn"	'linum-mode
    "tt"	'toggle-theme

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

    ","		'helm-M-x)

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
