(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer
    (write-file custom-file)))
(load custom-file)

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(use-package use-package
  :custom
  (use-package-always-ensure t)
  (package-native-compile t)
  (warning-minimum-level :emergency))

;; Install use-package support
(elpaca elpaca-use-package
        ;; Enable use-package :ensure support for Elpaca.
        (elpaca-use-package-mode))

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.
(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

;; these go in bootstrap because we're configuring use-package
(use-package diminish)
(use-package delight)

;; transient needs to be manually updated early to solve a dependency issue with Magit
;; todo remove after Emacs 30 is released, I think
(use-package transient
  :ensure (:wait t))

(use-package all-the-icons)

(use-package treemacs
  :defer t
  :ensure (:wait t)
  :demand t
  )

(setq treemacs-no-png-images nil)

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

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t))

;;  (load-theme 'ef-reverie)

(use-package consult)
(use-package consult-dir
:bind (
  :map vertico-local-completion-map))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; enhance marginalia with icons
(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark)
(use-package embark-consult)

;; Enable vertico
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode)
  :init

  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (savehist-mode)
  )

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  :bind
  ;; Configure SPC for separator insertion
  (:map corfu-map ("SPC" . corfu-insert-separator))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

;; A few more useful configurations...
;; Support opening new minibuffers from inside existing minibuffers.
(setq enable-recursive-minibuffers t)

;; Hide commands in M-x which do not work in the current mode.  Vertico
;; commands are hidden in normal buffers. 
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Emacs 30 and newer: Disable Ispell completion function.
;; Try `cape-dict' as an alternative.
;; (text-mode-ispell-word-completion nil)

;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package solaire-mode
  :demand t
  :config
  ;; treemacs got redefined as a normal window at some point
  (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
  (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist)
  (solaire-global-mode +1)
  )

(use-package doom-modeline
  :config       (doom-modeline-def-modeline 'main
                  '(bar matches buffer-info remote-host buffer-position parrot selection-info)
                  '(misc-info minor-modes input-method buffer-encoding major-mode process vcs "  "))
  :hook (after-init . doom-modeline-mode))

;; 🙌 Emoji! 🙌
(use-package emojify
  :hook
  (after-init . global-emojify-mode)
  :init
  (emojify-set-emoji-styles '(unicode))
  (setq emojify-download-emojis-p t))

;; recent files mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; ignore the elpa directory
(add-to-list 'recentf-exclude
             "elpa/*")

(use-package projectile
  :demand t
  :delight
  :config
  (use-package treemacs-projectile)
  (projectile-mode +1)
  )

(use-package general
  :demand t
  :ensure (:wait t)
  :config
  (general-evil-setup))

(use-package evil
  :demand t
  :ensure (:wait t)
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
;; in magit
(general-define-key
 :states 'normal
 :keymaps 'magit-status-mode-map
 "Z" 'magit-worktree)

(general-define-key
 :states 'normal
 "RET" 'embark-act
 )

(general-define-key
 :states 'normal
 :keymaps 'prog-mode-map
 "gd" 'evil-goto-definition
 )

;; add fd as a remap for esc
(use-package evil-escape
  :ensure (:wait t)
  :delight)
(evil-escape-mode 1)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
(use-package evil-snipe

  :config
  (evil-snipe-override-mode +1)
  )
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

(use-package magit
  :after (transient)
  :ensure (:wait t)
  )
;; disable the default emacs vc because git is all I use,
;; for I am a simple man
(setq vc-handled-backends nil)

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
  (which-key-setup-minibuffer)
  :custom
  (which-key-max-description-length 40)
  (which-key-lighter nil)
  (which-key-sort-order 'which-key-description-order))

(use-package pass)

(use-package fancy-compilation
  :commands (fancy-compilation-mode)

  :config
  (with-eval-after-load 'compile
    (fancy-compilation-mode)))

(setq fancy-compilation-override-colors nil)

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
  :demand t
  :after (transient)
  :delight
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)
(use-package consult-yasnippet)

(require 'epa-file)
(epa-file-enable)

(use-package prism)

(use-package git-gutter
    :delight
    :config
    (global-git-gutter-mode +1))

(global-hl-line-mode)
(setq global-hl-line-sticky-flag t)

(use-package beacon
  :init
  (beacon-mode))

(use-package volatile-highlights
  :init
  (volatile-highlights-mode 1))

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

(use-package pc-bufsw
  :init
  (pc-bufsw))

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
  (ligature-set-ligatures
   'prog-mode
   '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-"
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

(use-package python-mode
  :hook ((python-mode . yas-minor-mode)
         (python-mode . eglot-ensure)))

(use-package go-mode
  :hook ((go-mode . yas-minor-mode)
         (go-mode . eglot-ensure))
  :config
  ;; fixes ctrl-o after goto-definition by telling evil that godef-jump jumps
  (evil-add-command-properties #'godef-jump :jump t))


;; enable golangci-lint to work with flycheck
(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup))

;; https://github.com/joaotavora/eglot/issues/574#issuecomment-1401023985
(defun my-eglot-organize-imports () (interactive)
       (eglot-code-actions nil nil "source.organizeImports" t))

(defun install-my-eglot-organize-imports () 
  (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
  (add-hook 'before-save-hook 'eglot-format-buffer nil t))

(add-hook 'go-mode-hook #'install-my-eglot-organize-imports)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(use-package gotest)
(advice-add 'go-test-current-project :before #'projectile-save-project-buffers)
(advice-add 'go-test-current-test :before #'projectile-save-project-buffers)
(add-hook 'go-test-mode-hook 'visual-line-mode)

(general-define-key
 :states  'normal
 :keymaps 'go-mode-map
 ",a"     'go-import-add
 ",tp"    'go-test-current-project
 ",tt"    'go-test-current-test
 ",tf"    'go-test-current-file

 ;; using the ,c namespace for repl and debug stuff to follow the C-c
 ;; convention found in other places in Emacs
 ",cc"     'dap-debug
 ",cr"     'gorepl-run
 ",cg"     'gorepl-run-load-current-file
 ",cx"     'gorepl-eval-region
 ",cl"     'gorepl-eval-line

 ",x"      'eglot-code-actions
 ",n"      'go-rename
  )

(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; sets the visual tab width to 2 spaces per tab in Go buffers
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'tab-width) 2)))

(use-package rust-mode
  :mode (("\\.rs$" . rust-mode)))

(use-package web-mode
  :mode (("\\.html$" . web-mode)
         ("\\.html.tmpl$" . web-mode)
         ("\\.js$"   . web-mode)
         ("\\.jsx$"  . web-mode)
         ("\\.ts$"   . web-mode)
         ("\\.tsx$"  . web-mode)
         ("\\.css$"  . web-mode)
         ("\\.svelte$" . web-mode))
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

(use-package json-mode
  :mode (("\\.json$" . json-mode ))
  )

(add-hook 'json-mode-hook 'highlight-indent-guides-mode)

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

(use-package elixir-mode)

;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

(use-package sqlformat
  :config
  (setq sqlformat-command 'pgformatter))

(use-package gdscript-mode
  :straight (gdscript-mode
             :type git
             :host github
             :repo "godotengine/emacs-gdscript-mode")
:hook (gdscript-mode . eglot-ensure))

(add-to-list 'auto-mode-alist '("\\.gd\\'" . gdscript-mode))

(use-package lua-mode
  :hook ((lua-mode . eglot-ensure)))

(add-hook 'lua-mode-hook
          (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))

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
    "TAB"    #'switch-to-prev-buffer
    "bb"     'consult-buffer
    "bl"     'ibuffer
    "bs"     'consult-buffer-other-window
    "bR"     'revert-buffer
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
    "ff"     'find-file
    "Ff"     'toggle-frame-fullscreen
    "fd"     'consult-dir
    "fr"     'consult-recent-file
    "fed"    'find-initfile
    "feD"    'find-initfile-other-frame
    "gb"     'magit-blame
    "gl"     'consult-line
    "gs"     'magit-status
    "gg"     'magit
    "gt"     'git-timemachine
    "gd"     'magit-diff
    "gi"     'consult-imenu
    "go"     'browse-at-remote
    "gptm"   'gptel-menu
    "gptc"   'gptel
    "jj"     'bookmark-jump
    "js"     'bookmark-set
    "jo"     'org-babel-tangle-jump-to-org

    "ks"     'kagi-fastgpt-shell
    "kp"     'kagi-fastgpt-prompt
    "kf"     'kagi-proofread
    "kr"     'kagi-summarize-region
    "kb"     'kagi-summarize-buffer
    "ku"     'kagi-summarize-url
    "kt"     'kagi-translate

    "ic"     'insert-char
    "is"     'consult-yasnippet
    "n"      '(:keymap narrow-map)
    "oo"     'browse-url-at-point
    "p"      'projectile-command-map
    "p!"     'projectile-run-async-shell-command-in-root
    "ps"     'consult-git-grep
    "si"     'yas-insert-snippet
    "sn"     'yas-new-snippet
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
    "thc"    'consult-theme
    "tm"     'toggle-menu-bar-mode-from-frame
    "tnn"    'display-line-numbers-mode
    "tnt"    'toggle-line-numbers-rel-abs
    "tr"     'treemacs-select-window
    "ts"     'toggle-scroll-bar
    "tt"     'toggle-transparency
    "tp"     (defun ian-toggle-prism ()
               (interactive)
               (prism-mode 'toggle))
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
    "x"      '(:keymap embark-command-map)
    "xx"     'embark-dwim
    "SPC"    'execute-extended-command
)

  ;; global VISUAL mode map
  (general-vmap
    ";" 'comment-or-uncomment-region)

  ;; top right button on my trackball is equivalent to click (select) +
  ;; RET (open) on files in Treemacs
  (general-define-key
   :keymaps 'treemacs-mode-map
   "<mouse-8>" 'treemacs-RET-action)

(use-package org
:custom
(org-startup-indented t)
(org-hide-emphasis-markers t)
(org-startup-with-inline-images t)
(org-image-actual-width '(450))
(org-fold-catch-invisible-edits 'error)
(org-pretty-entities t)
(org-use-sub-superscripts "{}")
(org-id-link-to-org-use-id t)
(org-fold-catch-invisible-edits 'show))

(use-package evil-org)

(use-package org-download)

(use-package org-web-tools)

(setq org-fontify-whole-heading-line t)



(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(use-package gnuplot)

(org-babel-do-load-languages 'org-babel-load-languages
                             '(
                               (gnuplot . t)
                               (sqlite . t)
                               (sql . t)
                               (python . t)
                               (shell . t)
                               (lua . t)
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

(use-package ox-gfm)

(use-package htmlize)

(eval-after-load "org"
  (progn
    '(require 'ox-md nil t)
    '(require 'ox-gfm nil t)))

(setq org-export-coding-system 'utf-8)

(use-package ox-epub
  :demand t
  :init
  (require 'ox-org))

(setq org-todo-keyword-faces
      '(("IN PROGRESS" . org-warning) ("STUCK" . org-done)
        ("WAITING" . org-warning)))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
	("j" "Journal" entry (file+datetree "~/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")
	("x" "Web" entry (file+datetree "~/org/web-journal.org")
	 "* %:annotation\n  %i\n  %a")))

;; enable org-protocol
(require 'org-protocol)

(setq org-return-follows-link  t)

(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))

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

(setq org-src-window-setup 'other-frame)

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(server-start)

(use-package browse-at-remote)

(setq tramp-default-method "ssh")

(setq warning-minimum-level :emergency)

(defun load-theme--save-new-theme (theme &rest args)
  (setq ian-current-theme theme))
(advice-add 'load-theme :before #'load-theme--save-new-theme)

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

(setq initial-major-mode 'org-mode
      initial-scratch-message "#+title: Scratch Buffer\n\n")

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

(pixel-scroll-precision-mode nil) ;; turning this on is nice with a mouse but shit with a touchpad -- maybe it can be turned on conditionally

(setq
 redisplay-dont-pause t
 scroll-margin 0
 scroll-step 1
 scroll-conservatively 100000000
 scroll-preserve-screen-position 1)

(context-menu-mode t)

(xterm-mouse-mode 1)

(setq custom-unlispify-tag-names nil)

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

(use-package gptel
  :config
  (setq gptel-model 'gemma3:12b-it-qat
	gptel-backend (gptel-make-ollama "Ollama"
			:host "localhost:11434"
			:stream t
			:models '(gemma3:12b-it-qat)))

  (gptel-make-kagi "Kagi"
    :key (password-store-get "kagi-token"))

  (gptel-make-openai "Synthetic"
    :host "api.synthetic.new"
    :key (password-store-get "synthetic.new-token")
    :models '(hf:mistralai/Mistral-7B-Instruct-v0.3)
    ))

(use-package aider
  :config
  ;; use my personal config file
  (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu-2cols) ;; for wider screen
  ;; or use aider-transient-menu-2cols / aider-transient-menu-1col, for narrow screen
  (aider-magit-setup-transients) ;; add aider magit function to magit menu
  ;; auto revert buffer
  (global-auto-revert-mode 1)
  (auto-revert-mode 1))

(setq confirm-kill-emacs 'yes-or-no-p)

(defun silly-business/new-blog-post ()
  "Create a new silly.business blog post."
  (interactive)
  (let* ((post-title (read-string "Enter the title of the new post: "))
         (post-slug (replace-regexp-in-string "\\s-+" "-" post-title))
         (timestamp (format-time-string "%Y-%m-%d-%H:%M")))
    (shell-command (concat "cd ~/silly.business && hugo new blog/"
                           timestamp (format "-%s.org" post-slug)))
    (find-file (format "~/silly.business/content/blog/%s.org"
                       (concat timestamp "-" post-slug)))
               ))

(setq-default use-short-answers t)

(let ;; find the hostname and assign it to a variable
     ((hostname (string-trim-right
                 (shell-command-to-string "cat /etc/hostname"))))

   (progn
     (org-babel-tangle-file
      (concat "~/.emacs.d/local/" hostname ".org")
      (concat hostname ".el"))

     (load (concat "~/.emacs.d/local/" hostname ".el"))
     (require 'local)))
