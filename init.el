;;; init --- the Emacs entrypoint
;;; Commentary:
;;;
;;; Just load my customizations and execute -- org-mode bootstrap from
;;; https://orgmode.org/worg/org-contrib/babel/intro.html#literate-emacs-init
;;;
;;; Code:
;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
                                "src" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))
  ;; load up Org-mode and Org-babel
  (require 'ob-tangle))

;; load up all literate org-mode files in this directory
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

(setq custom-file "~/.emacs.d/.emacs-custom.el")
(provide 'init)

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

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; set ensure to be the default
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; these go in bootstrap because packages installed
;; with use-package use :diminish and :delight
(use-package diminish)
(use-package delight)

(use-package all-the-icons)

(use-package treemacs
  :defer t
  )

(setq treemacs-no-png-images t)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
:after (treemacs magit))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t
        ) ; if nil, italics is universally disabled

  ;; Corrects (and improves) org-mode's native fontification.
  ;; TODO is this still relevant when also using org-modern? or do
  ;; they just conflict?
  (doom-themes-org-config)
  )

(use-package ef-themes)

;;  (load-theme 'ef-reverie)

(defvar light-theme-list '(doom-one-light
                           doom-acario-light
                           doom-fairy-floss
                           doom-flatwhite
                           doom-opera-light
                           doom-gruvbox-light
                           doom-horizon))

(defvar dark-theme-list '(doom-Iosvkem
                          doom-challenger-deep
                          doom-city-lights
                          doom-dark+
                          doom-dracula
                          doom-ephemeral
                          doom-fairy-floss
                          doom-gruvbox
                          doom-henna
                          doom-horizon
                          doom-laserwave
                          doom-material
                          doom-miramare
                          doom-molokai
                          doom-monokai-classic
                          doom-monokai-pro
                          doom-moonlight
                          doom-nord
                          doom-nova
                          doom-oceanic-next
                          doom-old-hope
                          doom-one
                          doom-opera
                          doom-outrun-electric
                          doom-palenight
                          doom-peacock
                          doom-plain
                          doom-rouge
                          doom-snazzy
                          doom-solarized-dark
                          doom-spacegrey
                          doom-tomorrow-night
                          doom-vibrant
                          doom-zenburn))

(defun choose-theme ()
  "Choose a theme interactively using Helm"
  (interactive)
  (let ((theme (choose-theme-impl light-theme-list dark-theme-list)))
    (load-theme theme t)))

(use-package solaire-mode)

;; treemacs got redefined as a normal window at some point
(push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
(push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist)

(solaire-global-mode +1)

(use-package spacious-padding
  :hook (after-init . spacious-padding-mode))

(use-package doom-modeline
  :config       (doom-modeline-def-modeline 'main
                  '(bar matches buffer-info remote-host buffer-position parrot selection-info)
                  '(misc-info minor-modes input-method buffer-encoding major-mode process vcs "  "))
  :hook (after-init . doom-modeline-mode))

;; 🙌 Emoji! 🙌
(use-package emojify
  :config
  (setq emojify-download-emojis-p t)
  (emojify-set-emoji-styles '(unicode))
  (add-hook 'after-init-hook #'global-emojify-mode))

;; recent files mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; ignore the elpa directory
(add-to-list 'recentf-exclude
             "elpa/*")

(use-package projectile
  :delight)
(use-package helm-projectile)
(use-package treemacs-projectile)
(projectile-mode +1)

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
    ;; don't let evil-collection manage go-mode
    ;; it is overriding gd
    (setq evil-collection-mode-list (delq 'go-mode evil-collection-mode-list))
    (evil-collection-init))

  ;; the evil-collection overrides the worktree binding :(
  (general-define-key
   :states 'normal
   :keymaps 'magit-status-mode-map
   "Z" 'magit-worktree)

  ;; add fd as a remap for esc
  (use-package evil-escape
    :delight)
  (evil-escape-mode 1)

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))
  (use-package evil-snipe)
  (evil-snipe-override-mode +1)
  ;; and disable in specific modes (an example below)
  ;; (push 'python-mode evil-snipe-disabled-modes)

  (use-package undo-tree
    :config
    (global-undo-tree-mode)
    (evil-set-undo-system 'undo-tree)
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

  ;; add some advice to undo-tree-save-history to suppress messages
  ;; when it saves its backup files
  (defun quiet-undo-tree-save-history (undo-tree-save-history &rest args)
    (let ((message-log-max nil)
          (inhibit-message t))
      (apply undo-tree-save-history args)))

  (advice-add 'undo-tree-save-history :around 'quiet-undo-tree-save-history)

  (setq-default evil-escape-key-sequence "fd")

  ;; unbind RET since it does the same thing as j and in some
  ;; modes RET is used for other things, and evil conflicts
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "RET") nil))
  )

(use-package general
  :init
  (setup-evil)
  :config
  (general-evil-setup))

(use-package helm
  :delight
  :config
  (use-package helm-ag)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (define-key helm-find-files-map "\t" 'helm-execute-persistent-action)
  (setq helm-always-two-windows nil)
  (setq helm-default-display-buffer-functions '(display-buffer-in-side-window))
  (helm-mode 1))

(use-package magit)
;; disable the default emacs vc because git is all I use,
;; for I am a simple man
(setq vc-handled-backends nil)

(use-package forge
  :after magit)

(use-package git-timemachine)

;; This lets git-timemachine's bindings take precedence over evils'
;; (got lucky and happened to find this while looking for the package name, ha!)
;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(use-package which-key
  :delight
  :init
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package pass)

(use-package fancy-compilation
  :commands (fancy-compilation-mode))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(setq fancy-compilation-override-colors nil)

(defvar isw-should-play-chime nil)
(setq isw-should-play-chime nil)
(defun isw-play-chime (buffer msg)
  (if (eq isw-should-play-chime t)
      (start-process-shell-command "chime" "*Messages*" "aplay /home/ian/.emacs.d/vendor/chime.wav")))
(add-to-list 'compilation-finish-functions 'isw-play-chime)

(defun toggle-screaming ()
  (interactive)
  (if (eq isw-should-play-chime t)
      (progn
        (setq isw-should-play-chime nil)
        (message "Screaming disabled."))
    (progn
      (setq isw-should-play-chime t)
      (message "Screaming enabled."))))

;; first disable the default startup screen
(setq inhibit-startup-screen t)
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5))
        )
  )

(setq dashboard-set-footer nil)

(use-package yasnippet
  :delight
  :config
  (use-package yasnippet-snippets))

(yas-global-mode 1)

(use-package prism)

(use-package git-gutter
    :delight
    :config
    (global-git-gutter-mode +1))

(global-hl-line-mode)
(setq global-hl-line-sticky-flag t)

(use-package rainbow-delimiters
  :config
  ;; set up rainbow delimiters for Emacs lisp
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  ;; and sql mode too, it's useful there
  (add-hook 'sql-mode-hook #'rainbow-delimiters-mode)
  )

(use-package restart-emacs)

(use-package s)

(use-package systemd)

;; auto-completion
(use-package company
  :delight
  :config
  ;; enable it everywhere
  (add-hook 'after-init-hook 'global-company-mode)

  ;; tab complete!
  (global-set-key "\t" 'company-complete-common))

;; icons
(use-package company-box
  :hook (company-mode . company-box-mode))

;; extra documentation when idling
(use-package company-quickhelp)
(company-quickhelp-mode)

;; linter
(use-package flycheck
  :delight
  ;; enable it everywhere
  :init (global-flycheck-mode))

(add-hook 'flycheck-error-list-mode-hook
          'visual-line-mode)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package ace-window)

(use-package highlight-indent-guides)

(use-package pc-bufsw)
(pc-bufsw)

(use-package ack)
(use-package ag)
(use-package wgrep-ack)

(use-package helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(use-package ace-jump-mode)

(use-package dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(use-package kubernetes
:ensure t
:commands (kubernetes-overview))
;; add this config if I experience issues with Emacs locking up
;;:config
;;(setq kubernetes-poll-frequency 3600
 ;;     kubernetes-redraw-frequency 3600))

(use-package kubernetes-evil
  :after kubernetes)

(use-package evil-mc)

(use-package elfeed)

(shell-command "chmod +x ~/.emacs.d/install-firacode-font.bash")
(shell-command "~/.emacs.d/install-firacode-font.bash")

(add-to-list 'default-frame-alist '(font . "Fira Code-10"))
(set-frame-font "Fira Code-10" nil t)

(use-package ligature
  :load-path "./vendor/"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))

  ;; ;; Enable ligatures in programming modes                                                           
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  ;; disabled combinations that could be ligatures
  ;;  "::"

 (global-ligature-mode 't))

(use-package lsp-mode
  :init
  ;; use flycheck
  (setq lsp-prefer-flymake nil)
  (setq lsp-headerline-breadcrumb-enable nil))

;; treemacs integration
(use-package lsp-treemacs)

;; the UI
(use-package lsp-ui)

;; add a longer delay to the help mouseover
(setq lsp-ui-doc-delay 1)

;; linking breaks treemacs
;; also it's annoying
(setq lsp-enable-links nil)

;; helm integration
(use-package helm-lsp)

(setq lsp-eldoc-enable-hover t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-include-signature t)
(setq lsp-ui-doc-position 'at-point)
(setq lsp-ui-doc-use-childframe t)
(setq lsp-ui-doc-use-webkit nil)
(setq lsp-lens-enable nil)

(general-define-key
 :states 'normal
 :keymaps 'prog-mode-map
 ",d"     'lsp-describe-thing-at-point
 ",gg"    'lsp-find-definition
 ",gt"    'lsp-find-type-definition
 ",i"     'lsp-find-implementation
 ",n"     'lsp-rename
 ",r"     'lsp-ui-peek-find-references
 ",R"     'lsp-find-references
 ",x"     'lsp-execute-code-action
 ",lsp"   'lsp-workspace-restart
 "gd"     'lsp-find-definition
 )

(use-package yaml-mode)
(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
;;(add-hook 'yaml-mode-hook 'origami-mode)

(general-define-key
 :states  'normal
 :keymaps 'yaml-mode-map
 "zo"     'origami-open-node-recursively
 "zO"     'origami-open-all-nodes
 "zc"     'origami-close-node-recursively)

(use-package rego-mode)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)))

  ;; show code blocks w/ monospace font
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (add-hook 'markdown-mode-hook 'variable-pitch-mode)
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch)
               (set-face-attribute 'markdown-pre-face nil :inherit 'fixed-pitch)))

;; this can go here because it affects Markdown's live preview mode
;; but I should consider putting it somewhere more general maybe?
(add-hook 'eww-mode-hook 'visual-line-mode)

(use-package dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(put 'dockerfile-image-name 'safe-local-variable #'stringp)

(use-package auto-virtualenv)
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
(setenv "WORKON_HOME" "~/.virtualenvs")

(defun set-gopls-lib-dirs ()
  "Add $GOPATH/pkg/mod to the 'library path'."
  ;; stops lsp from continually asking if Go projects should be imported
  (setq lsp-clients-go-library-directories
        (list
         "/usr"
         (concat (getenv "GOPATH") "/pkg/mod"))))

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         (go-mode . set-gopls-lib-dirs)
         (go-mode . yas-minor-mode))
  :config
  ;; fixes ctrl-o after goto-definition by telling evil that godef-jump jumps
  ;; I don't believe I need to do this anymore, as I use lsp instead of godef now
  (evil-add-command-properties #'godef-jump :jump t))

;; enable golangci-lint to work with flycheck
(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup))

(use-package gotest)
(advice-add 'go-test-current-project :before #'projectile-save-project-buffers)
(advice-add 'go-test-current-test :before #'projectile-save-project-buffers)
(add-hook 'go-test-mode-hook 'visual-line-mode)

(use-package gorepl-mode)

(use-package dap-mode)
(require 'dap-dlv-go)
(dap-mode 0)
(dap-ui-mode 0)
(dap-ui-controls-mode 0)
(tooltip-mode 1)
(setq dap-ui-variable-length 100)

(general-define-key
 :states  'normal
 :keymaps 'go-mode-map
 ",a"     'go-import-add
 ",d"     'lsp-describe-thing-at-point
 "gd"    'lsp-find-definition
 ",gt"    'lsp-find-type-definition
 ",i"     'lsp-find-implementation
 ",n"     'lsp-rename
 ",r"     'lsp-ui-peek-find-references
 ",R"     'lsp-find-references
 ",tp"    'go-test-current-project
 ",tt"    'go-test-current-test
 ",tf"    'go-test-current-file
 ",x"     'lsp-execute-code-action
 ",lsp"   'lsp-workspace-restart
 "gd"     'lsp-find-definition

 ;; using the ,c namespace for repl and debug stuff to follow the C-c
 ;; convention found in other places in Emacs
 ",cc"     'dap-debug
 ",cr"     'gorepl-run
 ",cg"     'gorepl-run-load-current-file
 ",cx"     'gorepl-eval-region
 ",cl"     'gorepl-eval-line
  )

(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; disable "Organize Imports" warning that never goes away
(add-hook 'go-mode-hook
          (lambda ()
            ;; Go likes origami-mode
            ;; (origami-mode)
            ;; lsp ui sideline code actions are annoying in Go
            (setq-local lsp-ui-sideline-show-code-actions nil)))

;; sets the visual tab width to 2 spaces per tab in Go buffers
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'tab-width) 2)))


(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(setq lsp-file-watch-threshold 5000)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.GOPATH\\'"))

(use-package rust-mode
  :mode (("\\.rs$" . rust-mode))
  :hook ((rust-mode . lsp-deferred)))


(general-define-key
 :states  'normal
 :keymaps 'rust-mode-map
 ",d"     'lsp-describe-thing-at-point
 ",gg"    'lsp-find-definition
 ",gt"    'lsp-find-type-definition
 ",i"     'lsp-find-implementation
 ",n"     'lsp-rename
 ",r"     'lsp-find-references
 ",x"     'lsp-execute-code-action
 ",lsp"   'lsp-workspace-restart
 "gd"     'lsp-find-definition
 )

(defun lsp-rust-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

(add-hook 'rust-mode-hook #'lsp-rust-install-save-hooks)

(use-package web-mode
  :mode (("\\.html$" . web-mode)
         ("\\.html.tmpl$" . web-mode)
         ("\\.js$"   . web-mode)
         ("\\.jsx$"  . web-mode)
         ("\\.ts$"   . web-mode)
         ("\\.tsx$"  . web-mode)
         ("\\.css$"  . web-mode)
         ("\\.svelte$" . web-mode))
  :hook
  ((web-mode . lsp-deferred))

  :config
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-quoting nil))

(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

;; web-mode can provide syntax highlighting for many template
;; engines, but it can't detect the right one if the template uses a generic ending.
;; If a project uses a generic ending for its templates, such
;; as .html, add it below. It would be more elegant to handle this by
;; setting this variable in .dir-locals.el for each project,
;; unfortunately due to this https://github.com/fxbois//issues/799 that
;; is not possible :(

;;(setq web-mode-engines-alist '(
;;        ("go" . ".*example_project_dir/.*\\.html\\'")
        ;; add more projects here..
;;        ))

(use-package lsp-tailwindcss)

(add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save)

(use-package json-mode
  :mode (("\\.json$" . json-mode ))
  )

(add-hook 'json-mode-hook 'highlight-indent-guides-mode)

(add-hook 'sh-mode-hook
          (lambda ()
            (defvar-local indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(use-package fish-mode)

(use-package salt-mode)
(add-hook 'salt-mode-hook
        (lambda ()
            (flyspell-mode 1)))

(add-hook 'salt-mode-hook 'highlight-indent-guides-mode)

(general-define-key
 :states  'normal
 :keymaps 'sh-mode-map
 ",c" (general-simulate-key "C-x h C-M-x")
 )

(use-package vyper-mode)

(use-package elixir-mode
  :hook
  ((elixir-mode . lsp-deferred))
  )
;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

(use-package sql-indent)

(general-define-key
 :states 'normal
 :keymaps 'sql-mode-map
 "gd" 'evil-goto-definition
 )

(add-hook 'sql-mode-hook #'rainbow-delimiters-mode)

(use-package robot-mode)

(use-package jira-markup-mode)

(use-package racket-mode)

(use-package adaptive-wrap
  :config
  (setq-default adaptive-wrap-extra-indent 2)
  (defun adaptive-and-visual-line-mode (hook)
    (add-hook hook (lambda ()
                      (progn
                        (visual-line-mode)
                        (adaptive-wrap-prefix-mode)))))

  (mapc 'adaptive-and-visual-line-mode
        (list
         'markdown-mode
         'go-mode-hook
         'sql-mode-hook
         'js2-mode-hook
         'yaml-mode-hook
         'rjsx-mode-hook))

  (add-hook 'compilation-mode-hook
            #'adaptive-wrap-prefix-mode)
  (setq compilation-scroll-output t))

(defun find-initfile ()
  "Open main config file."
  (interactive)
  (find-file "~/.emacs.d/readme.org"))

(defun find-initfile-other-frame ()
  "Open main config file in a new frame."
  (interactive)
  (find-file-other-frame "~/.emacs.d/readme.org"))

(defun reload-initfile ()
  "Reload the main config file."
  (interactive)
  (org-babel-tangle "~/.emacs.d/readme.org")
  (byte-compile-file "~/.emacs.d/ian.el"))

(defun close-client-frame ()
  "Exit emacsclient."
  (interactive)
  (server-edit "Done"))

(defun last-window ()
  "Switch to the last window."
  (interactive)
  (other-window -1 t))

(defun toggle-line-numbers-rel-abs ()
  "Toggles line numbers between relative and absolute numbering"
  (interactive)
  (if (equal display-line-numbers-type 'relative)
      (setq display-line-numbers-type 'absolute)
    (setq display-line-numbers-type 'relative))
  (if (equal display-line-numbers-mode t)
      (progn
        (display-line-numbers-mode -1)
        (display-line-numbers-mode))))

(defun random-theme (light-theme-list dark-theme-list)
  "Choose a random theme from the appropriate list based on the current time"
  (let* ((now (decode-time))
         (themes (if (and (>= (nth 2 now) 10) (< (nth 2 now) 15))
                     light-theme-list
                   dark-theme-list)))
    (nth (random (length themes)) themes)))

(defun load-next-favorite-theme ()
  "Switch to a random theme appropriate for the current time."
  (interactive)
  (let ((theme (random-theme light-theme-list dark-theme-list)))
    (load-theme theme t)
    (message "Switched to theme: %s" theme)))

;; define the spacebar as the global leader key, following the
;; Spacemacs pattern, which I've been using since 2014
(general-create-definer my-leader-def
  :prefix "SPC")

;; define SPC m for minor mode keys, even though I use , sometimes
(general-create-definer my-local-leader-def
  :prefix "SPC m")

;; global keybindings with LEADER
(my-leader-def 'normal 'override
  "aa"     'ace-jump-mode
  "ag"     'org-agenda
  "bb"     'helm-buffers-list
  "TAB"    #'switch-to-prev-buffer
  "br"     'revert-buffer
  "bd"     'evil-delete-buffer
  "ds"     (defun ian-desktop-save ()
             (interactive)
             (desktop-save "~/desktop-saves"))
  "dr"     (defun ian-desktop-read ()
             (interactive)
             (desktop-read "~/desktop-saves"))
  "cc"     'projectile-compile-project

  "ec"     'flycheck-clear
  "el"     'flycheck-list-errors
  "en"     'flycheck-next-error
  "ep"     'flycheck-previous-error
  "Fm"     'make-frame
  "Ff"     'toggle-frame-fullscreen
  "ff"     'helm-find-files
  "fr"     'helm-recentf
  "fd"     'dired
  "fed"    'find-initfile
  "feD"    'find-initfile-other-frame
  "feR"    'reload-initfile
  "gb"     'magit-blame
  "gs"     'magit-status
  "gg"     'magit
  "gt"     'git-timemachine
  "gd"     'magit-diff
  "go"     'browse-at-remote
  "gptm"   'gptel-menu
  "gptc"   'gptel
  "gi"     'helm-imenu
  "jj"     'bookmark-jump
  "js"     'bookmark-set
  "jo"     'org-babel-tangle-jump-to-org

  "kh"     'helm-info-kagi
  "ks"     'kagi-fastgpt-shell
  "kp"     'kagi-fastgpt-prompt
  "kf"     'kagi-proofread
  "kr"     'kagi-summarize-region
  "kb"     'kagi-summarize-buffer
  "ku"     'kagi-summarize-url
  "kt"     'kagi-translate

  "ic"     'insert-char
  "is"     'yas-insert-snippet
  "n"      '(:keymap narrow-map)
  "oo"     'browse-url-at-point
  "p"      'projectile-command-map
  "pf"     'helm-projectile-find-file
  "p!"     'projectile-run-async-shell-command-in-root
  "si"     'yas-insert-snippet
  "sn"     'yas-new-snippet
  "sp"     'helm-projectile-ag
  "qq"     'save-buffers-kill-terminal
  "qr"     'restart-emacs
  "qz"     'delete-frame
  "ta"     'treemacs-add-project-to-workspace
  "thi"    (defun ian-theme-information ()
             "Display the last applied theme."
             (interactive)
             (let ((last-theme (car (reverse custom-enabled-themes))))
               (if last-theme
                   (message "Last applied theme: %s" last-theme)
                 (message "No themes are currently enabled."))))
  "thr"    'load-random-theme
  "thl"    (defun ian-load-light-theme ()
             (interactive)
             (load-theme
              (nth
               (random
                (length light-theme-list)) light-theme-list)))
  "thd"    (defun ian-load-dark-theme ()
             (interactive)
             (load-theme
              (nth
               (random
                (length
                 dark-theme-list)) dark-theme-list)))
  "thh"    'choose-theme
  "thc"    'load-theme
  "thn"    'load-next-favorite-theme
  "tnn"    'display-line-numbers-mode
  "tnt"    'toggle-line-numbers-rel-abs
  "tr"     'treemacs-select-window
  "ts"     'toggle-screaming
  "tt"     'toggle-transparency
  "tp"     (defun ian-toggle-prism () (interactive) (prism-mode 'toggle))
  "tw"     'whitespace-mode
  "w-"     'split-window-below
  "w/"     'split-window-right
  "wb"     'last-window
  "wj"     'evil-window-down
  "wk"     'evil-window-up
  "wh"     'evil-window-left
  "wl"     'evil-window-right
  "wd"     'delete-window
  "wD"     'delete-other-windows
  "ww"     'ace-window
  "wo"     'other-window
  "w="     'balance-windows
  "W"      '(:keymap evil-window-map)
  "SPC"    'helm-M-x
  )

;; global VISUAL mode map
(general-vmap
  ";" 'comment-or-uncomment-region)

;; top right button on my trackball is equivalent to click (select) +
;; RET (open) on files in Treemacs
(general-define-key
   :keymaps 'treemacs-mode-map
   "<mouse-8>" 'treemacs-RET-action)

(use-package evil-org)

(use-package org-download)

(use-package company-org-block) ;; TODO configuration

(use-package ox-jira)

(use-package org-web-tools)

(setq org-export-coding-system 'utf-8)

;; Fontify the whole line for headings (with a background color).
(setq org-fontify-whole-heading-line t)

;; disable the weird default editing window layout in org-mode
;; instead, just replace the current window with the editing one..
(setq org-src-window-setup 'current-window)

;; indent and wrap long lines in Org
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

;; enable execution of languages from Babel
(org-babel-do-load-languages 'org-babel-load-languages
                             '(
                               (sql . t)
                               (python . t)
                               (shell . t)
                               )
                             )

(my-local-leader-def
  :states  'normal
  :keymaps 'org-mode-map
  "y"      'org-store-link
  "i"      'org-toggle-inline-images
  "p"      'org-insert-link
  "x"      'org-babel-execute-src-block
  "s"      'org-insert-structure-template
  "e"      'org-edit-src-code
  "t"      'org-babel-tangle
  "o"      'org-export-dispatch
  "TAB"    'org-toggle-heading
  )

(general-define-key
 :states  'normal
 :keymaps 'org-mode-map
 "TAB"    'evil-toggle-fold)

;; github-flavored markdown
(use-package ox-gfm)

;; htmlize prints the current buffer or file, as it would appear in
;; Emacs, but in HTML! It's super cool and TODO I need to move this
;; use-package statement somewhere I can talk about htmlize outside of
;; a comment
(use-package htmlize)

;; enable markdown export
(eval-after-load "org"
  (progn
    '(require 'ox-md nil t)
    '(require 'ox-gfm nil t)))

;; todo states
(setq org-todo-keywords
      '((sequence "TODO(t)"     "|" "IN PROGRESS(p)" "|" "DONE(d)" "|" "STUCK(s)" "|" "WAITING(w)")
        (sequence "OPEN(o)" "|" "INVESTIGATE(v)" "|" "IMPLEMENT(i)" "|" "REVIEW(r)" "|" "MERGED(m)" "|" "RELEASED(d)" "|" "ABANDONED(a)")
        (sequence "QUESTION(q)" "|" "ANSWERED(a)")))

;; todo faces
(setq org-todo-keyword-faces
      '(("IN PROGRESS" . org-warning) ("STUCK" . org-done)
        ("WAITING" . org-warning)))

;; enable org-protocol
(require 'org-protocol)

;; enter follows links.. how was this not a default?
(setq org-return-follows-link  t)

(add-hook 'org-mode-hook 'variable-pitch-mode)

(defun ian-org-fixed-pitch ()
  "Fix fixed pitch text in Org Mode"
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch))

(add-hook 'org-mode-hook 'ian-org-fixed-pitch)

(eval-when-compile
  (require 'easy-mmode)
  (require 'ox))

(define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
  "Attempt to export Org as HTML with useful link IDs.
Instead of random IDs like \"#orga1b2c3\", use heading titles,
made unique when necessary."
  :global t
  (if unpackaged/org-export-html-with-useful-ids-mode
      (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
    (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))

(defun unpackaged/org-export-get-reference (datum info)
  "Like `org-export-get-reference', except uses heading titles instead of random numbers."
  (let ((cache (plist-get info :internal-references)))
    (or (car (rassq datum cache))
        (let* ((crossrefs (plist-get info :crossrefs))
               (cells (org-export-search-cells datum))
               ;; Preserve any pre-existing association between
               ;; a search cell and a reference, i.e., when some
               ;; previously published document referenced a location
               ;; within current file (see
               ;; `org-publish-resolve-external-link').
               ;;
               ;; However, there is no guarantee that search cells are
               ;; unique, e.g., there might be duplicate custom ID or
               ;; two headings with the same title in the file.
               ;;
               ;; As a consequence, before re-using any reference to
               ;; an element or object, we check that it doesn't refer
               ;; to a previous element or object.
               (new (or (cl-some
                         (lambda (cell)
                           (let ((stored (cdr (assoc cell crossrefs))))
                             (when stored
                               (let ((old (org-export-format-reference stored)))
                                 (and (not (assoc old cache)) stored)))))
                         cells)
                        (when (org-element-property :raw-value datum)
                          ;; Heading with a title
                          (unpackaged/org-export-new-title-reference datum cache))
                        ;; NOTE: This probably breaks some Org Export
                        ;; feature, but if it does what I need, fine.
                        (org-export-format-reference
                         (org-export-new-reference cache))))
               (reference-string new))
          ;; Cache contains both data already associated to
          ;; a reference and in-use internal references, so as to make
          ;; unique references.
          (dolist (cell cells) (push (cons cell new) cache))
          ;; Retain a direct association between reference string and
          ;; DATUM since (1) not every object or element can be given
          ;; a search cell (2) it permits quick lookup.
          (push (cons reference-string datum) cache)
          (plist-put info :internal-references cache)
          reference-string))))

(defun unpackaged/org-export-new-title-reference (datum cache)
  "Return new reference for DATUM that is unique in CACHE."
  (cl-macrolet ((inc-suffixf (place)
                             `(progn
                                (string-match (rx bos
                                                  (minimal-match (group (1+ anything)))
                                                  (optional "--" (group (1+ digit)))
                                                  eos)
                                              ,place)
                                ;; HACK: `s1' instead of a gensym.
                                (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                           (match-string 2 ,place)))
                                        (suffix (if suffix
                                                    (string-to-number suffix)
                                                  0)))
                                  (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
    (let* ((title (org-element-property :raw-value datum))
           (ref (url-hexify-string (substring-no-properties title)))
           (parent (org-element-property :parent datum)))
      (while (--any (equal ref (car it))
                    cache)
        ;; Title not unique: make it so.
        (if parent
            ;; Append ancestor title.
            (setf title (concat (org-element-property :raw-value parent)
                                "--" title)
                  ref (url-hexify-string (substring-no-properties title))
                  parent (org-element-property :parent parent))
          ;; No more ancestors: add and increment a number.
          (inc-suffixf ref)))
      ref)))

(add-hook 'org-mode-hook 'unpackaged/org-export-html-with-useful-ids-mode)

(setq org-pretty-entities nil)

(server-start)

(use-package browse-at-remote)

(setq tramp-default-method "ssh")

(setq warning-minimum-level :emergency)

(defun load-theme--disable-old-theme (theme &rest args)
  "Disable current theme before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'load-theme--disable-old-theme)

(defun load-theme--save-new-theme (theme &rest args)
  (setq ian-current-theme theme))
(advice-add 'load-theme :before #'load-theme--save-new-theme)

(defun ian-restart-org-advice (&rest _args)
  (org-mode-restart))
(advice-add 'load-theme :after #'ian-restart-org-advice)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '95 '(100 . 100)))))

(defun er-switch-to-previous-buffer ()
  (concat
    "Switch to previously open buffer."
    "Repeated invocations toggle between the two most recently open buffers.")
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

(setq-default frame-title-format '("%f [%m]"))

(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . "/tmp/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t)

;; autosave
(setq auto-save-visited-interval 300)
(auto-save-visited-mode
 :diminish
 )

(add-to-list 'default-frame-alist '(width . 128))
(add-to-list 'default-frame-alist '(height . 60))

;; hide some modes that are everywhere
(diminish 'eldoc-mode)
(diminish 'undo-tree-mode)
(diminish 'auto-revert-mode)
(diminish 'evil-collection-unimpaired-mode)
(diminish 'yas-minor-mode-major-mode)

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

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(scroll-bar-mode -1)
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

(context-menu-mode t)

(xterm-mouse-mode 1)

(setq custom-unlispify-tag-names nil)

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
;; (use-package apheleia
;;   :config
;;   (apheleia-global-mode +1))

(use-package elpher)

(use-package gemini-mode)

(setq require-final-newline nil)

(use-package caps-lock)

(defvar window-swap-origin nil)

(defun window-swap-start (event)
  "Start swapping windows using mouse events."
  (interactive "e")
  (setq window-swap-origin (posn-window (event-start event))))

(defun window-swap-end (event)
  "End swapping windows using mouse events."
  (interactive "e")
  (let ((origin window-swap-origin)
        (target (posn-window (event-end event))))
    (window-swap-states origin target))
  (setq window-swap-origin nil))

(global-set-key (kbd "<C-S-mouse-1>") 'window-swap-start)
(global-set-key (kbd "<C-S-drag-mouse-1>") 'window-swap-end)

(use-package kagi
  :custom
  (kagi-api-token  (password-store-get "kagi-token"))

  ;; Universal Summarizer settings
  (kagi-summarizer-default-language "EN")
  (kagi-summarizer-cache t))

(use-package ob-kagi-fastgpt
  :ensure nil  ; provided by the kagi package
  :after org
  :config
  (ob-kagi-fastgpt-setup))

(use-package gptel)

(setq
 gptel-model 'llama3.2:latest
 gptel-backend (gptel-make-ollama "Ollama"
                 :host "localhost:11434" 
                 :stream t
                 :models '((mistral:latest)
                           (llama3.2:latest))))

(gptel-make-kagi "Kagi"
  :key (password-store-get "kagi-token"))

(use-package emacs-everywhere)

(use-package casual-dired
  :bind (:map dired-mode-map ("C-x" . 'casual-dired-tmenu)))

(let ;; find the hostname and assign it to a variable
     ((hostname (string-trim-right
                 (shell-command-to-string "hostname"))))

   (progn
     (org-babel-tangle-file
      (concat "~/.emacs.d/local/" hostname ".org")
      (concat hostname ".el"))

     (load (concat "~/.emacs.d/local/" hostname ".el"))
     (require 'local)))
