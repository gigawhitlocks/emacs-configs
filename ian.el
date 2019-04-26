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

  (defun setup-projectile ()
    (use-package projectile)
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
    (use-package evil-escape)
    (evil-escape-mode 1)
    (setq-default evil-escape-key-sequence "fd")
    (use-package evil-leader
       :init
       (global-evil-leader-mode)
       (evil-leader/set-leader ",")

       ;; very important global keybindings
       (evil-leader/set-key
	 "bb" 'switch-to-buffer
	 "bk" 'kill-buffer
	 "ff" 'find-file
	 "tn" 'linum-mode
	 "w-" 'split-window-below
	 "w/" 'split-window-right
	 "wk" 'ace-delete-window
	)))

  (defun setup-magit ()
    (use-package magit)
    ;; disable the default emacs vc because git is all I use,
    ;; for I am a simple man
    (setq vc-handled-backends nil))

  (defun setup-ivy ()
    "Installs Ivy with a suggested config."

    ;; I've lifted this sample config from the github readme for Ivy
    (use-package ivy)
    (use-package counsel)
    (use-package swiper)
    (ivy-mode 1)


    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (setq search-default-mode #'char-fold-to-regexp)

    ;; Bindings:
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    ;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    ;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    ;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
    ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    ;; (global-set-key (kbd "C-x l") 'counsel-locate)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

  (defun setup-which-key  ()
    (use-package which-key
      :init
      (which-key-mode)
      (which-key-setup-minibuffer)))

  ;; anything so trivial that there is no config necessary goes here
  (defun extra-packages ()
    (use-package restart-emacs)
    (use-package leuven-theme)
    (use-package treemacs))

  ;; execute installation and configuration of packages
  (use-package flycheck
    :init (global-flycheck-mode))
  (setup-ivy)
  (setup-evil)
  (setup-projectile)
  (setup-magit)
  (setup-which-key)
  (extra-packages))

(defun languages ()
  "Setup for specific programming languages."

  (defun setup-lsp ()
    "Enable nice rendering of diagnostics like compile errors."
    (use-package lsp-mode
      :init (setq lsp-prefer-flymake nil))

    (use-package lsp-ui
      :init (setq lsp-ui-doc-position 'bottom))

    ;; Add company-lsp backend for auto-completion
    (use-package company-lsp))

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

  (setup-lsp)
  (docker)
  (scala))

(defun config ()
  "Global configuration variables and such."

  ;; backups to /tmp
  (setq backup-directory-alist `(("." . "/tmp/.emacs-saves")))
  (setq backup-by-copying t)

  ;; extraneous window chrome
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

