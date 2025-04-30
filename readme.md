# Prologue

This is my Emacs configuration. It is written in [Org Mode](https://orgmode.org/) format, which means that I can display a static representation here, but the [source repository](https://github.com/gigawhitlocks/emacs-configs) and document ([plain text view](https://raw.githubusercontent.com/gigawhitlocks/emacs-configs/refs/heads/master/readme.org)), are interactive when opened in Emacs.

It follows the concept of "[literate programming](https://en.wikipedia.org/wiki/Literate_programming)" and both defines my Emacs configuration (as well as a few other, related things) and includes my notes about why I made those changes, and what I was doing at the time, as well as whatever other commentary I felt like including at the time (related or otherwise).

At least, that's the goal. In reality, it's a messy living document that I use to configure Emacs and to keep track of what I've done. I don't always take the best of notes, but it is sufficient for me to keep moving forward. If you search around, you may find ideas and code that you can repurpose for your own uses.


# Entrypoint

The source code below is extracted to `init.el` by calling `M-x org-babel-tangle`. The rest of this file is extracted to `readme.el` by this entrypoint in `init.el`. This allows me to only maintain `readme.org` as it will be re-extracted at startup every time. If this whole file is tangled to `init.el` by `init.el`, then a bootstrapping problem is introduced. So this part remains static, and the rest of the config can live in its Org file.

```emacs-lisp
(setq dotfiles-dir
      (file-name-directory
       (or (buffer-file-name) load-file-name)))

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
```

Load up all literate org-mode files in this directory:

```emacs-lisp
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))
```


# External Configuration

Some people use a "dotfiles" repository to keep track of their configuration for other programs but Org does a great job at helping me organize and install configuration so before continuing to configure Emacs, I will install configuration for any other user-level utilities that I frequently use for development, like `git`.

```emacs-lisp
(let ((org-files (directory-files
                    (concat dotfiles-dir "/dotfiles/") t "\\.org\\'")))
  (dolist (file org-files)
    (org-babel-tangle-file file)))
```


# Customize

Emacs provides a menu-based customization interface that makes configuration files like this one entirely optional, and sometimes Emacs prompts the user for things and saves their preferences to a "custom file." By default, that file is *this* file, but the auto-generated code is nasty, disposable, and almost always specific to the system where I've made some interactive choice &#x2013; for instance to trust local variables set in the header of a file like this one &#x2013; and after a long time I've realized it's too troublesome to check in those changes. So this setting tells Customize to write those settings to their own file, and this file is ignored in `.gitignore`.

```emacs-lisp
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer
    (write-file custom-file)))
(load custom-file)
```


# Package Manager Bootstrap

After tangling the source files and loading `init.el`, the first thing that must be done is to prepare to manage third party packages, because my config is built on top of the work of many third party packages. I like to install and manage all of the packages I use as part of my configuration so that it can be duplicated across computers (more or less) and managed with `git`, so I use `use-package` to ensure that packages are installed from my configuration file.

Bootstrap sets up Elpaca and configures `use-package` to use it.


## Install Elpaca

```emacs-lisp
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
```


## Configure `use-package`

```emacs-lisp
(use-package use-package
  :custom
  (use-package-always-ensure t)
  (package-native-compile t)
  (warning-minimum-level :emergency))
```


## Configure Elpaca to use `use-package`

```emacs-lisp
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
```


# Fundamental Package Installation and Configuration

First I need to install packages with a large effect and on which other packages are likely to depend. Configuration here should be config that must run early, before variables are set or language-related packages, which will likely rely on these being set.


## Upgrade Transient to MELPA version

Before I can get really started I need one hack: `magit` and some other packages want a newer version of `transient` than provided in my current Emacs version. MELPA has an updated version but `use-package` doesn't pull it by default when it's pulled in as a dependency, so explicitly install that before getting into the rest of the packages:

```emacs-lisp
;; transient needs to be manually updated early to solve a dependency issue with Magit
;; todo remove after Emacs 30 is released, I think
(use-package transient
  :ensure (:wait t))
```

I think I can delete this section about `transient` when I updated to Emacs 30


## Icons

Treemacs and Doom themes both rely upon `all-the-icons` to look nice

```emacs-lisp
(use-package all-the-icons)
```

Along the way nerd-icons also gets installed. On first run or after clearing out elpa/, need to run the following:

    M-x nerd-icons-install-fonts
    M-x all-the-icons-install-fonts

This installs the actual fonts and only needs to be called once. Maybe I'll automate it someday.


## Treemacs

Treemacs provides a file browser on the left hand side of Emacs that I have grown to really like. It's great for exploring unfamiliar projects and modules. It's installed early because many things have integrations with it, including some themes.

```emacs-lisp
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
```


## Theme

I'm mainly using the Doom Emacs theme pack. I think they're really nice to look at, especially with `solaire-mode`.


### Theme packs

-   Doom

    ```emacs-lisp
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
    ```

-   ef-themes

    Protesilaos Stavrou has a nice theme pack too:
    
    ```emacs-lisp
    (use-package ef-themes)
    ```

-   modus-themes

    ```emacs-lisp
    (use-package modus-themes
      :custom
      (modus-themes-italic-constructs t)
      (modus-themes-bold-constructs t)
      (modus-themes-mixed-fonts t))
    ```


### Default theme

I prefer to load a theme per-system, but it's nice to have it documented here. Add a line like the following to the appropriate file in `local/`

```emacs-lisp
;;  (load-theme 'ef-reverie)
```


## Navigation and Completion, and the Minibuffer

The next few packages work closely together to enhance some of the core functionality of Emacs related to navigation, buffer management, and running commands.


### Consult (commands to list, search, and preview files and buffers in the minibuffer)

Consult adds search and navigation commands that build upon the built-in completing-read

```emacs-lisp
(use-package consult)
(use-package consult-dir
:bind (
  :map vertico-local-completion-map))
```


### Marginalia (more metadata in completions and the minibuffer)

Marginalia enhances the same native Emacs search interface with extra information about whatever is being displayed. It's used by both Vertico and Consult to display extra information about the actions they offer.

```emacs-lisp
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
```


### Orderless (better interactive matching)

Orderless allows pattern matching to be "better." With the default configuration, which is what I have below, the main obvious difference from vanilla Emacs is that now matching works anywhere in the target string and not just the beginning. That's a big win. This is applied everywhere Emacs does matching.

```emacs-lisp
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
```


### Embark (contextual actions)

Embark allows you to call commands on whatever the cursor is on (thing "at-point") and shows stuff that is relevant to the context. It has some integrations with consult that seem very powerful and I don't fully understand them yet, but I'm adding them in here so I can figure them out. Lots of searching and matching goodness for working across many files and buffers, I think.

```emacs-lisp
(use-package embark)
(use-package embark-consult)
```


### Vertico (minibuffer behavior)

Finally, Vertico makes `M-x` more featureful, and allows me to display command history when it is invoked. I map `M-x` to `SPC SPC` due to my historical use of Spacemacs, and Vertico keeps Emacs feeling like home for someone used to Helm.

Below is, actually, the default config. I didn't write any of this. It's kind of wild.

```emacs-lisp
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
```


### Tab Completion

Corfu handles tab completion outside of the minibuffer, and allows multiple terms separated by spaces, using the rules from completing-read &#x2013; in this case, what I've defined in the Orderless section above.

```emacs-lisp
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
```


### Global Configuration

Below is some final, global configuration related to Vertico and Corfu & configure how completion and the minibuffer work.

```emacs-lisp
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
```


## Solaire Mode

Also some visual candy that makes "real" buffers more visible by changing the background color slightly vs e.g. **compilation** or magit buffers

```emacs-lisp
(use-package solaire-mode
  :demand t
  :config
  ;; treemacs got redefined as a normal window at some point
  (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
  (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist)
  (solaire-global-mode +1)
  )
```


## Doom Modeline

The Doom Emacs project also provides a fancy modeline to go along with their themes.

```emacs-lisp
(use-package doom-modeline
  :config       (doom-modeline-def-modeline 'main
                  '(bar matches buffer-info remote-host buffer-position parrot selection-info)
                  '(misc-info minor-modes input-method buffer-encoding major-mode process vcs "  "))
  :hook (after-init . doom-modeline-mode))
```


## Emoji 🙏

Provided by [emojify](https://github.com/iqbalansari/emacs-emojifyjjjjj). Run `emojify-download-emoji`

```emacs-lisp
;; 🙌 Emoji! 🙌
(use-package emojify
  :hook
  (after-init . global-emojify-mode)
  :init
  (emojify-set-emoji-styles '(unicode))
  (setq emojify-download-emojis-p t))
```


## Configure Recent File Tracking

Emacs comes with `recentf-mode` which helps me remember what I was doing after I restart my session.

```emacs-lisp
;; recent files mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; ignore the elpa directory
(add-to-list 'recentf-exclude
             "elpa/*")
```


## Install and Configure Projectile

[`projectile`](https://projectile.readthedocs.io/en/latest/) is a fantastic package that provides all kinds of project context-aware functions for things like:

-   running grep, but only inside the project
-   compiling the project from the project root without doing anything
-   find files within the project, again without having to do anything extra

It's great, it gets installed early, can't live without it. 💘 `projectile`

```emacs-lisp
(use-package projectile
  :demand t
  :delight
  :config
  (use-package treemacs-projectile)
  (projectile-mode +1)
  )
```


## Install and Configure Keybindings Helper

[General](https://github.com/noctuid/general.el) provides more consistent and convenient keybindings, especially with `evil-mode`.

It's mostly used below in the [global keybindings](#Global%20Keybindings) section.

```emacs-lisp
(use-package general
  :demand t
  :ensure (:wait t)
  :config
  (general-evil-setup))
```


## Install and Configure Evil Mode

[`evil-mode`](https://github.com/emacs-evil/evil) fundamentally changes Emacs so that while editing all of the modes and keybindings from `vim` are present. It's controversial but I think modal editing is brilliant and have been using `vim` bindings for twenty-odd years now. No going back.

```emacs-lisp
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
```


## Install and Configure Magit

[Magit](https://github.com/magit/magit) is an incredible integrated `git` UI for Emacs.

```emacs-lisp
(use-package magit
  :after (transient)
  :ensure (:wait t)
  )
;; disable the default emacs vc because git is all I use,
;; for I am a simple man
(setq vc-handled-backends nil)
```


## Install and Configure `git-timemachine`

`git-timeline` lets you step through the history of a file.

```emacs-lisp
(use-package git-timemachine)

;; This lets git-timemachine's bindings take precedence over evils'
;; (got lucky and happened to find this while looking for the package name, ha!)
;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))
```


## Install and Configure `which-key`

It can be difficult to to remember and discover all of the available shortcuts in Emacs, so [`which-key`](https://github.com/justbur/emacs-which-key) pops up a special buffer to show you available shortcuts whenever you pause in the middle of a keyboard shortcut for more than a few seconds. It's really lovely.

```emacs-lisp
(use-package which-key
  :delight
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  :custom
  (which-key-max-description-length 40)
  (which-key-lighter nil)
  (which-key-sort-order 'which-key-description-order))
```


## Set up `pass` for secrets handling

```emacs-lisp
(use-package pass)
```


## Handle "fancy" output in compilation buffer

The external package `fancy-compilation-mode` handles colorization and "clever" use of ANSI to create progress bars and stupid shit like that, which show up in things like npm output and Docker output when BuildKit is set to NORMAL. You can, of course, set the BuildKit output style to PLAIN, but sometimes you're eg editing a file where NORMAL is hard-coded in the Makefile target you want to run when using `compilation-mode` and fighting project defaults isn't what you want to spend your time on.

```emacs-lisp
(use-package fancy-compilation
  :commands (fancy-compilation-mode)

  :config
  (with-eval-after-load 'compile
    (fancy-compilation-mode)))
```

I don't like how fancy-compilation-mode overrides colors by default, but luckily this can be disabled.

```emacs-lisp
(setq fancy-compilation-override-colors nil)
```


## Configure the Startup Splashscreen

Following Spacemacs's style, I use the [`emacs-dashboard`](https://github.com/emacs-dashboard/emacs-dashboard) project and [`all-the-icons`](https://github.com/domtronn/all-the-icons.el) to provide an aesthetically pleasing splash screen with useful links to recently used files on launch.

Actually, looking at the project page, the icons don't seem to be working for me. Maybe I need to enable them. I'll investigate later.

```emacs-lisp
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
```


## Install templating tool and default snippets

YASnippet is really cool and allow fast insertion of boilerplate using templates. I've been meaning to use this more. [Here are the YASnippet docs.](https://www.emacswiki.org/emacs/Yasnippet)

```emacs-lisp
(use-package yasnippet
  :demand t
  :after (transient)
  :delight
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)
(use-package consult-yasnippet)
```


## Encryption

```emacs-lisp
(require 'epa-file)
(epa-file-enable)
```


# Extra Packages

Packages with a smaller effect on the experience.


## prism colors by indent level

It takes over the color theme and I don't know if I want it on all the time but it's interesting and I want to have it installed so that I can turn it on in certain situations, like editing highly nested YAML, where it might be invaluable. If I can remember to use it :)

```emacs-lisp
(use-package prism)
```


## git-gutter shows unstaged changes in the gutter

```emacs-lisp
(use-package git-gutter
    :delight
    :config
    (global-git-gutter-mode +1))
```


## Highlight the current line

I like to highlight the current line so that it is easy to identify where my cursor is.

```emacs-lisp
(global-hl-line-mode)
(setq global-hl-line-sticky-flag t)
```


## Rainbow delimiters make it easier to identify matching parentheses

```emacs-lisp
(use-package rainbow-delimiters
  :config
  ;; set up rainbow delimiters for Emacs lisp
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  ;; and sql mode too, it's useful there
  (add-hook 'sql-mode-hook #'rainbow-delimiters-mode)
  )
```


## restart-emacs does what it says on the tin

```emacs-lisp
(use-package restart-emacs)
```


## s is a string manipulation utility

I use this for a trim() function far down below. I think it gets pulled in as a dependency anyway, but in any case it provides a bunch of helper functions and stuff. [Docs are here.](https://github.com/magnars/s.el)

```emacs-lisp
(use-package s)
```


## a systemd file mode

Just provides syntax highlighting in `.unit` files.

```emacs-lisp
(use-package systemd)
```


## Install and Configure Flycheck for Linting

[Flycheck](https://www.flycheck.org/en/latest/) is an on-the-fly checker that hooks into most language backends.

```emacs-lisp
;; linter
(use-package flycheck
  :delight
  ;; enable it everywhere
  :init (global-flycheck-mode))

(add-hook 'flycheck-error-list-mode-hook
          'visual-line-mode)
```


## Install `exec-path-from-shell` to manage the PATH

[exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) mirrors PATH in zsh or Bash in macOS or Linux into Emacs so that the PATH in the shell and the PATH when calling commands from Emacs are the same.

```emacs-lisp
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))
```


## ace-window provides an ace-jump experience for switching windows

```emacs-lisp
(use-package ace-window)
```


## Install a mode for drawing indentation guides

This mode adds subtle coloration to indentation whitespace for whitespace-delimited languages like YAML where sometimes it can be difficult to see the nesting level of a given headline in deeply-nested configuration.

```emacs-lisp
(use-package highlight-indent-guides)
```


## Quick buffer switcher

> PC style quick buffer switcher for Emacs
> 
> This switches Emacs buffers according to most-recently-used/least-recently-used order using C-tab and C-S-tab keys. It is similar to window or tab switchers that are available in PC desktop environments or applications.

Bound by default to `C-<TAB>` and `C-S-<TAB>`, I have decided that these are sane defaults. Just install this and turn it on.

```emacs-lisp
(use-package pc-bufsw
  :init
  (pc-bufsw))
```


## Writeable grep mode with ack

Writable grep mode allows you to edit the results from running grep on a project and easily save changes back to all of the original files

```emacs-lisp
(use-package ack)
(use-package ag)
(use-package wgrep-ack)
```


## Better help buffers

```emacs-lisp
(use-package helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
```


## Quickly jump around buffers

```emacs-lisp
(use-package ace-jump-mode)
```


## Dumb jump

Dumb jump provides an interface to grep that does a pretty good job of finding definitions when a smarter backend like LSP is not available. This registers it as a backend for XREF.

```emacs-lisp
(use-package dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
```


## Kubernetes Mode

Provides an interactive Kubernetes Mode inspired by `magit`. Since `magit` is one of my favorite tools, I have to try out the Kubernetes mode as well.

```emacs-lisp
(use-package kubernetes
:ensure t
:commands (kubernetes-overview))
;; add this config if I experience issues with Emacs locking up
;;:config
;;(setq kubernetes-poll-frequency 3600
 ;;     kubernetes-redraw-frequency 3600))
```

I need the `evil` compatiblity mode, too, because I run `evil`.

```emacs-lisp
(use-package kubernetes-evil
  :after kubernetes)
```


## multiple cursors

```emacs-lisp
(use-package evil-mc)
```


## elfeed

```emacs-lisp
(use-package elfeed)
```


# Font

The FiraCode font is a programming-focused font with ligatures that looks nice and has a open license so I'm standardizing my editor configuration on that font


## FiraCode Font Installation Script

Installing fonts is always a pain so I'm going to use a variation of the installation script that the FireCode devs provide under their manual installation guide. This should be Linux-distribution agnostic, even though the font can be installed as a system package with on all of my systems on 2022-02-19 Sat with just

    sudo apt install fonts-firacode

because I don't intend to use Ubuntu as my only system forever. I just happen to be on Ubuntu on 2022-02-19 Sat.

But first, I want to be able to run this script every time Emacs starts, but only have the script actually do anything if the font is not already installed.

This guard will check to see if there's any font with 'fira' in it (case insensitive) and if so, just exits the script. This will happen on most executions.

```bash
set -eo pipefail
[[ $(fc-list | grep -i fira) != "" ]] && exit 0
```

Now here's the standard installation script

```bash
fonts_dir="${HOME}/.local/share/fonts"
if [ ! -d "${fonts_dir}" ]; then
    mkdir -p "${fonts_dir}"
fi

version=5.2
zip=Fira_Code_v${version}.zip
curl --fail --location --show-error https://github.com/tonsky/FiraCode/releases/download/${version}/${zip} --output ${zip}
unzip -o -q -d ${fonts_dir} ${zip}
rm ${zip}

# for now we need the Symbols font, too
zip=FiraCode-Regular-Symbol.zip
curl --fail --location --show-error https://github.com/tonsky/FiraCode/files/412440/${zip} --output ${zip}
unzip -o -q -d ${fonts_dir} ${zip}
rm ${zip}

fc-cache -f
```

This installation script was sourced from <https://github.com/tonsky/FiraCode/wiki/Linux-instructions#installing-with-a-package-manager>


## Enable FiraCode Font

Calling the script from above will install the font

```emacs-lisp
(shell-command "chmod +x ~/.emacs.d/install-firacode-font.bash")
(shell-command "~/.emacs.d/install-firacode-font.bash")
```

Enable it

```emacs-lisp
(add-to-list 'default-frame-alist '(font . "Fira Code-10"))
(set-frame-font "Fira Code-10" nil t)
```


## Configure FiraCode special features

FiraCode offers ligatures for programming symbols, which is cool.

```emacs-lisp
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
```


# Language Configuration

This section contains all of the IDE-like features in my configuration.


## YAML

```emacs-lisp
(use-package yaml-mode)
(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
;;(add-hook 'yaml-mode-hook 'origami-mode)

(general-define-key
 :states  'normal
 :keymaps 'yaml-mode-map
 "zo"     'origami-open-node-recursively
 "zO"     'origami-open-all-nodes
 "zc"     'origami-close-node-recursively)
```


## Rego

[whatever that is](https://www.openpolicyagent.org/docs/latest/policy-language/)

```emacs-lisp
(use-package rego-mode)
```


## Markdown

```emacs-lisp
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
```


## Docker

```emacs-lisp
(use-package dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(put 'dockerfile-image-name 'safe-local-variable #'stringp)
```


## Python

Just enable snippets and LSP in Python mode and that's enough for me

```emacs-lisp
(use-package python-mode
  :hook ((python-mode . yas-minor-mode)
         (python-mode . eglot-ensure)))
```


## Go

Go is my primary language so it's my most dynamic and complicated configuration, however it degrades gracefully so if not everything is installed, the rest of it still works.


### Dependencies

Go support requires some dependencies. I will try to list them all here. Stuff I have installed has some overlap because of the in-progress move to LSP, but I'll prune it later.

-   First, `go` itself must be installed, install however, and available on the `PATH`.

-   `gopls`, the language server for LSP mentioned above <https://github.com/golang/tools/blob/master/gopls/doc/user.md>. I have been just running this off of `master` so I can experience all the latest ~~bugs~~ features, so clone the gopls project (TODO find the url for it and put a link here) and `go install` it. After you're done `gopls` should also be on the `PATH`. [Directions for configuring `gopls` through this file are found here.](https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#gopls-configuration)

-   `golint` has to be installed independently

```bash
$ go get https://github.com/golang/lint
```

-   [`golangci-lint`](https://github.com/golangci/golangci-lint) is a meta linter that calls a bunch of 3rd party linters (configurable) and replaces the old one that used to freeze my computer. `go-metalinter`, I think, is what it was called. Anyway, it used to crash my computer and *apparently* that was a common experience. Anyway `golangci-lint` must be installed independently, too:

```bash
# install it into ./bin/
$ curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s v1.23.6
```


### Initial Setup

```emacs-lisp
(use-package go-mode
  :hook ((go-mode . yas-minor-mode)
         (go-mode . eglot-ensure))
  :config
  ;; fixes ctrl-o after goto-definition by telling evil that godef-jump jumps
  (evil-add-command-properties #'godef-jump :jump t))


;; enable golangci-lint to work with flycheck
(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup))
```


### Eglot Config

Since Go has auto formatting and imports management as a first-party feature I like to enable that as an automatic step before save in Emacs so that I do not have to remember to remove unwantetd imports, or to add new ones, or to format my code, literally ever. I am totally pampered by this state of affairs and Go is my bae for having all of these features.

```emacs-lisp
;; https://github.com/joaotavora/eglot/issues/574#issuecomment-1401023985
(defun my-eglot-organize-imports () (interactive)
       (eglot-code-actions nil nil "source.organizeImports" t))

(defun install-my-eglot-organize-imports () 
  (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
  (add-hook 'before-save-hook 'eglot-format-buffer nil t))

(add-hook 'go-mode-hook #'install-my-eglot-organize-imports)
```

The Go Emacs docs suggest using this snippet which I think might help with some freezing I've been seeing when stepping into previously unseen library code

```emacs-lisp
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)
```

Preliminary testing suggests it might do the trick


### Package and Configuration for Executing Tests

```emacs-lisp
(use-package gotest)
(advice-add 'go-test-current-project :before #'projectile-save-project-buffers)
(advice-add 'go-test-current-test :before #'projectile-save-project-buffers)
(add-hook 'go-test-mode-hook 'visual-line-mode)
```


### Mode-Specific Keybindings

```emacs-lisp
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
```


### Hooks

```emacs-lisp
;; sets the visual tab width to 2 spaces per tab in Go buffers
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'tab-width) 2)))
```


## Rust

To install the Rust language server:

1.  Install `rustup`.
2.  Run `rustup component add rls rust-analysis rust-src`.

```emacs-lisp
(use-package rust-mode
  :mode (("\\.rs$" . rust-mode)))
```


## Web

After some amount of searching and fumbling about I have discovered [`web-mode`](http://web-mode.org/) which appears to be the one-stop-shop solution for all of your HTML and browser-related needs. It handles a whole slew of web-related languages and templating formats and plays nicely with LSP. It's also the only package that I could find that supported `.tsx` files at all.

So yay for `web-mode`!

```emacs-lisp
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
```


### enable jsx mode for all .js and .jsx files

If working on projects that do not use JSX, might need to move this to a project-specific config somewhere.

For now though, this is sufficient for me

```emacs-lisp
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))
```

Thanks to <https://prathamesh.tech/2015/06/20/configuring-web-mode-with-jsx/>


### Setting highlighting for special template modes

```emacs-lisp
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
```


## JSON

```emacs-lisp
(use-package json-mode
  :mode (("\\.json$" . json-mode ))
  )

(add-hook 'json-mode-hook 'highlight-indent-guides-mode)
```

```
Default Keybindings
    C-c C-f: format the region/buffer with json-reformat (https://github.com/gongo/json-reformat)
    C-c C-p: display a path to the object at point with json-snatcher (https://github.com/Sterlingg/json-snatcher)
    C-c P: copy a path to the object at point to the kill ring with json-snatcher (https://github.com/Sterlingg/json-snatcher)
    C-c C-t: Toggle between true and false at point
    C-c C-k: Replace the sexp at point with null
    C-c C-i: Increment the number at point
    C-c C-d: Decrement the number at point
```


## Fish

```emacs-lisp
(use-package fish-mode)
```


## Salt

```emacs-lisp
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
```


## Elixir

```emacs-lisp
(use-package elixir-mode)

;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
```


## SQL


### Autoformatting

Using [sqlformat.el](https://github.com/purcell/sqlformat) to set up auto-format on save.

The formatter calls out to `pg_format` for now but I want to explore using `sqlfluff`, which is available in distro repos and `pip`, when I have the time to explore the configuration, and at some point I will have to make this smarter so that I can edit SQL intended for non-Postgres DBs with the same convenience. It seems like I can just set `sqlformat-command` and `sqlformat-args` in `.dir-locals.el` if I need to change them for a specific project.

Shout-out to [Aditya Athalye](https://evalapply.org) for suggesting this package & helping me improve my config.

```emacs-lisp
(use-package sqlformat
  :config
  (setq sqlformat-command 'pgformatter))
```


## Emacs Lisp

I don't have any custom configuration for Emacs Lisp yet, but I am going to use this space to collect tools and resources that might become useful in the future, and which I may install.


### A collection of development modes and utilities

<https://github.com/p3r7/awesome-elisp>


### editing s-exps

<https://github.com/p3r7/awesome-elisp#lispy> <https://github.com/abo-abo/lispy>


## GDScript

```emacs-lisp
(use-package gdscript-mode
  :straight (gdscript-mode
             :type git
             :host github
             :repo "godotengine/emacs-gdscript-mode")
:hook (gdscript-mode . eglot-ensure))

(add-to-list 'auto-mode-alist '("\\.gd\\'" . gdscript-mode))
```


## Lua


### Language Server

The language server for lua unfortunately must be manually installed: <https://github.com/LuaLS/lua-language-server/releases>

Fetch it and run it

    ./lua-language-server --socket=5050

Or tell `eglot` where it is, actually.

```emacs-lisp
(use-package lua-mode
  :hook ((lua-mode . eglot-ensure)))
```


### Auto-format on save

```emacs-lisp
(add-hook 'lua-mode-hook
          (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
```


# Adaptive Wrap and Visual Line Mode

Here I've done some black magic fuckery for a few modes. Heathens in modern languages and also some other prose modes don't wrap their long lines at 80 characters like God intended so instead of using visual-column-mode which I think does something similar but probably would've been easier, I've defined an abomination of a combination of `visual-line-mode` (built-in) and [adaptive-wrap-prefix-mode](https://elpa.gnu.org/packages/adaptive-wrap.html) to ****dynamically (visually) wrap and indent long lines in languages like Go with no line length limit**** so they look nice on my screen at any window width and don't change the underlying file — and it's actually pretty cool.

```emacs-lisp
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
```


# Global Keybindings


## Helper Functions

```emacs-lisp
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
```


## Main Global Keymap

These are all under SPACE, following the Spacemacs pattern. Yeah, my configuration is a little of Spacemacs, a little of Doom, and a little of whatever I feel inspired by.

These keybindings are probably the most opinionated part of my configuration. They're shortcuts I can remember, logically or not.

```emacs-lisp
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
```


# Org Mode Settings


## Global Settings

```emacs-lisp
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
```


## Some default evil bindings

```emacs-lisp
(use-package evil-org)
```


## Image drag-and-drop for org-mode

```emacs-lisp
(use-package org-download)
```


## Install some tools for archiving web content into Org

```emacs-lisp
(use-package org-web-tools)
```


## Fontify the whole line for headings (with a background color)

```emacs-lisp
(setq org-fontify-whole-heading-line t)
```


## disable the default editing window layout

instead, just replace the current window with the editing one..

```emacs-lisp

```


## indent and wrap long lines

```emacs-lisp
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
```


## Allow `gnuplot` to work with Org

```emacs-lisp
(use-package gnuplot)
```


## Enable execution of languages from Babel

```emacs-lisp
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
```


## set Org-specific keybindings

```emacs-lisp
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
```


## Export Settings


### GitHub-flavored markdown

```emacs-lisp
(use-package ox-gfm)
```


### HTMLize

htmlize prints the current buffer or file, as it would appear in Emacs, but in HTML! It's super cool

```emacs-lisp
(use-package htmlize)
```


### enable markdown export

```emacs-lisp
(eval-after-load "org"
  (progn
    '(require 'ox-md nil t)
    '(require 'ox-gfm nil t)))
```


### explicitly set utf-8 output (apparently)

```emacs-lisp
(setq org-export-coding-system 'utf-8)
```


### custom todo states

```emacs-lisp
(setq org-todo-keywords
      '((sequence "TODO(t)"     "|" "IN PROGRESS(p)" "|" "DONE(d)" "|" "STUCK(s)" "|" "WAITING(w)")
        (sequence "OPEN(o)" "|" "INVESTIGATE(v)" "|" "IMPLEMENT(i)" "|" "REVIEW(r)" "|" "MERGED(m)" "|" "RELEASED(d)" "|" "ABANDONED(a)")
        (sequence "QUESTION(q)" "|" "ANSWERED(a)")))
```


### epub export

```emacs-lisp
(use-package ox-epub
  :demand t
  :init
  (require 'ox-org))
```


## `TODO` Faces

```emacs-lisp
(setq org-todo-keyword-faces
      '(("IN PROGRESS" . org-warning) ("STUCK" . org-done)
        ("WAITING" . org-warning)))
```


## capture templates

```emacs-lisp
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("x" "Web" entry (file+datetree "~/org/web-journal.org")
         "* %:annotation\n  %i\n  %a")))
```

Below is the bookmarklet source:

```js
javascript:void(location="org-protocol://capture?" + new URLSearchParams({template: 'x', url: window.location.href, title: document.title, body: window.getSelection()}));
```

lifted from this discussion <https://mail.gnu.org/archive/html/emacs-orgmode/2024-12/txt_aK4ExGDIn.txt> and combined with the template `x` above


## Org-Protocol

Org-Protocol is super cool! It enables things like bookmarklets to bookmark things to Org files!

```emacs-lisp
;; enable org-protocol
(require 'org-protocol)
```


## enter follows links.. how was this not a default?

```emacs-lisp
(setq org-return-follows-link  t)
```


## Use mixed-pitched fonts

```emacs-lisp
(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))
```


## Useful anchors in HTML export

This is taken from [github.com/alphapapa's Unpackaged.el](https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors) collection, unmodified.

```emacs-lisp
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
```


## Define how org-edit-src behaves

Do `M-x describe-variable RET org-src-window-setup` to see the options

```emacs-lisp
(setq org-src-window-setup 'other-frame)
```


## Show hidden emphasis markers

```emacs-lisp
(use-package org-appear
  :hook
  (org-mode . org-appear-mode))
```


# Miscellaneous standalone global configuration changes


## Start server

```emacs-lisp
(server-start)
```


## Opening the Remote Repo in the Browser from Emacs

<https://github.com/rmuslimov/browse-at-remote>

```emacs-lisp
(use-package browse-at-remote)
```


## Opening Sources in Emacs from the Browser

<https://orgmode.org/worg/org-contrib/org-protocol.html>

First use this `.desktop` file to register a handler for the new protocol scheme:

```desktop
[Desktop Entry]
Name=org-protocol
Comment=Intercept calls from emacsclient to trigger custom actions
Categories=Other;
Keywords=org-protocol;
Icon=emacs
Type=Application
Exec=emacsclient -- %u
Terminal=false
StartupWMClass=Emacs
MimeType=x-scheme-handler/org-protocol;
```

After tangling that file to its destination, run the following command to update the database:

```bash
update-desktop-database ~/.local/share/applications/
```


### Manual Steps:

1.  The first time, add a button in the browser by creating a bookmarklet containing the following target:

```
javascript:location.href="org-protocol://open-source?url=" +encodeURIComponent(location.href);
```

1.  Add an entry to `org-protocol-project-alist`, defined in the local machine's hostname-specific config found in `local/`. An example can be found on the Worg page above, but here it is again for easy reference:

```emacs-lisp
(setq org-protocol-project-alist
      '(("Worg"
         :base-url "https://orgmode.org/worg/"
         :working-directory "/home/user/worg/"
         :online-suffix ".html"
         :working-suffix ".org")
        ("My local Org-notes"
         :base-url "http://localhost/org/"
         :working-directory "/home/user/org/"
         :online-suffix ".php"
         :working-suffix ".org")))
```

N.B. this code block does ****not**** get tangled into `init.el`.

-   TODO automate the cloning of unknown repos and addition to this list

    I want to be able to press the button on new repos that I haven't cloned yet, and have them dumped to a sane location and then added to the list and opened.


## TRAMP settings

Only one setting at the moment: use `ssh` instead of `scp` when accessing files with `ssh:` schemes

```emacs-lisp
(setq tramp-default-method "ssh")
```


## Disable most warnings

Honestly I'm not good enough at Emacs to make sense of most of them anyway

```emacs-lisp
(setq warning-minimum-level :emergency)
```


## Theme Switching Helpers

Save the current theme to a global variable so it can be referenced later

```emacs-lisp
(defun load-theme--save-new-theme (theme &rest args)
  (setq ian-current-theme theme))
(advice-add 'load-theme :before #'load-theme--save-new-theme)
```


## Line Numbers in Programming Buffers

```emacs-lisp
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
```


## Transparency toggle

I definitely lifted this from somewhere but failed to document where I got it :\\ Probably from Spacemacs. Thanks, Spacemacs.

```emacs-lisp
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
```


## Switch to last buffer

This one lifted from <https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/>

TODO: Make this behave like alt-tab in Windows, but for buffers. I think `hycontrol` may come in handy (Hyperbole).

```emacs-lisp
(defun er-switch-to-previous-buffer ()
  (concat
    "Switch to previously open buffer."
    "Repeated invocations toggle between the two most recently open buffers.")
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
```


## Fix Home/End keys

Emacs has weird behavior by default for Home and End and this change makes the behavior "normal" again.

```emacs-lisp
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
```


## Customize the frame (OS window) title

Taken from StackOverflow, at least for now, which does 90% of what I want and can serve as a future reference of how to customize this aspect of Emacs. This displays the file name and major mode in the OS title bar. Will have to find the documentation that defines the format string passed to `frame-title-format` at some point.

```emacs-lisp
(setq-default frame-title-format '("%f [%m]"))
```


## Tweak align-regexp

Configure align-regexp to use spaces instead of tabs. This is mostly for this file. When my keybindings are in two columns and `M-x align-regexp` uses tabs, the columns look aligned in Emacs but unaligned on GitHub. Using spaces faces this. This snippet effects that change.

Lifted from StackOverflow:

<https://stackoverflow.com/questions/22710040/emacs-align-regexp-with-spaces-instead-of-tabs>

```emacs-lisp
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))
```


## Configure automatic backup/recovery files

I don't like how Emacs puts temp files in the same directory as the file, as this litters the current working directory and makes git branches dirty. These are some tweaks to store those files in `/tmp`.

```emacs-lisp
(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . "/tmp/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t)
```


## Autosave

Automatically saves the file when it's been idle for 5 minutes.

```emacs-lisp
;; autosave
(setq auto-save-visited-interval 300)
(auto-save-visited-mode
 :diminish
 )
```


## Default window size

Just a bigger size that I prefer..

```emacs-lisp
(add-to-list 'default-frame-alist '(width . 128))
(add-to-list 'default-frame-alist '(height . 60))
```


## Scratch buffer settings

```emacs-lisp
(setq initial-major-mode 'org-mode
      initial-scratch-message "#+title: Scratch Buffer\n\n")
```


## Unclutter global modeline

Some global minor modes put themselves in the modeline and it gets noisy, so remove them from the modeline.

```emacs-lisp
;; hide some modes that are everywhere
(diminish 'eldoc-mode)
(diminish 'undo-tree-mode)
(diminish 'auto-revert-mode)
(diminish 'evil-collection-unimpaired-mode)
(diminish 'yas-minor-mode-major-mode)
```


## Less annoying bell

Flashes the modeline foreground instead of whatever the horrible default behavior was (I don't even remember).

```emacs-lisp
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
```

(from Emacs wiki)


## Remove toolbar, scrollbars, and menu

Removes the toolbar and menu bar (file menu, etc) in Emacs because I just use `M-x` for everything.

```emacs-lisp
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(scroll-bar-mode -1)
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)
```


## Enable modern scrolling

```emacs-lisp
(pixel-scroll-precision-mode t)

(setq
 redisplay-dont-pause t
 scroll-margin 0
 scroll-step 1
 scroll-conservatively 100000000
 scroll-preserve-screen-position 1)
```


## Enable context menu on right click

```emacs-lisp
(context-menu-mode t)
```


## Enable the mouse in the terminal

```emacs-lisp
(xterm-mouse-mode 1)
```


## Disable "nice" names in Customize

I prefer that Customize display the names of variables that I can change in this file, rather than the human-readable names for people who customize their Emacs through `M-x customize`

```emacs-lisp
(setq custom-unlispify-tag-names nil)
```


## Don't require a final newline

Very occasionally this causes problems and it's not something that I actually care about. To be honest I do not know why Emacs has a default behavior where it adds a newline to the end of the file on save.

```emacs-lisp
(setq require-final-newline nil)
```


## Caps lock mode

For those of us who did away with the caps lock button but write SQL sometimes

```emacs-lisp
(use-package caps-lock)
```


## Allow swapping windows with ctrl + shift + left-click-drag

```emacs-lisp
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
```


## Kagi integration

I love Kagi and even if it costs a few cents per query I would like to have it accessible from Emacs. Uses API key stored in `~/.secret.el~` as configured in the "load secrets" section above


### Basic config

```emacs-lisp
(use-package kagi
  :custom
  (kagi-api-token  (password-store-get "kagi-token"))

  ;; Universal Summarizer settings
  (kagi-summarizer-default-language "EN")
  (kagi-summarizer-cache t))
```


### Org Babel Support

Kagi FastGPT is also supported in Org Babel blocks, which will be nice if I ever use it and want to capture the resposnes alongside notes

```emacs-lisp
(use-package ob-kagi-fastgpt
  :ensure nil  ; provided by the kagi package
  :after org
  :config
  (ob-kagi-fastgpt-setup))
```

Then create a source block with 'language' ‘kagi-fastgpt’:

```
Can Kagi FastGPT be used in Org mode?
```


## LLM integration

```emacs-lisp
(use-package gptel

  :config
  (setq
   gptel-model 'gemma3:12b-it-qat
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434" 
                   :stream t
                   :models '((gemma3:12b-it-qat)
                             )))

  (gptel-make-kagi "Kagi"
    :key (password-store-get "kagi-token")))
```


## Confirm before exit

<span class="timestamp-wrapper"><span class="timestamp">&lt;2024-11-16 Sat&gt;</span></span>: Don't know why I didn't do this sooner! With my muscle memory for `:wq` I close Emacs by mistake *constantly* &#x2013; especially since I've been using `vim` bindings now for multiple decades and I use `emacsclient` heavily, so a lot of the time I actually do wish to call `evil-exit`&#x2026; just not on that last frame!

```emacs-lisp
(setq confirm-kill-emacs 'yes-or-no-p)
```


## Start a new blog post

I used Kagi FastGPT to generate about half of this. It taught me about `read-string` and `replace-regexp-in-string` and wrote the little regexp for me. I tweaked the output to put the blog in the right place and open the new file once it's created. I guess it was nice to have some of it generated.

```emacs-lisp
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
```


## Short Answers

Instead of constantly typing `yes` and `no` to prompts, I can constantly type `y` and `n`!

```emacs-lisp
(setq-default use-short-answers t)
```


# Hostname-based tweaks

This is a simple convention that I use for loading machine-specific configuration for the different machines I run Emacs on.

1.  looks for Org files in `/home/$USER/.emacs.d/local/` with a name that is the same as the hostname of the machine.
2.  shells out to call `hostname` to determine the hostname.
3.  tangles that .org file to a .el file and executes it

This allows configuration to diverge to meet needs that are unique to a specific workstation.

```emacs-lisp
(let ;; find the hostname and assign it to a variable
     ((hostname (string-trim-right
                 (shell-command-to-string "cat /etc/hostname"))))

   (progn
     (org-babel-tangle-file
      (concat "~/.emacs.d/local/" hostname ".org")
      (concat hostname ".el"))

     (load (concat "~/.emacs.d/local/" hostname ".el"))
     (require 'local)))
```

There must be an Org file in `local/` named `$(hostname).org` or init actually breaks. This isn't great but for now I've just been making a copy of one of the existing files whenever I start on a new machine. It may someday feel worth my time to automate this, but so far it hasn't been worth it, and I just create `local/"$(hostname).org"` as part of initial setup, along with other tasks that I do not automate in this file.


# Launching Emacsclient

[Nifty shell function for hassle-free starting of emacsclient](https://www.emacswiki.org/emacs/EmacsClient#h5o-18)

```bash
args=""
nw=false
# check if emacsclient is already running
if pgrep -U $(id -u) emacsclient > /dev/null; then running=true; fi

# check if the user wants TUI mode
for arg in "$@"; do
    if [ "$arg" = "-nw" ] || [ "$arg" = "-t" ] || [ "$arg" = "--tty" ]
    then
        nw=true
    fi
done

# if called without arguments - open a new gui instance
if [ "$#" -eq "0" ] || [ "$running" != true ]; then
    args=(-c $args)           # open emacsclient in a new window
fi
if [ "$#" -gt "0" ]; then
    # if 'em -' open standard input (e.g. pipe)
    if [[ "$1" == "-" ]]; then
        TMP="$(mktemp /tmp/emacsstdin-XXX)"
        cat >$TMP
        args=($args --eval '(let ((b (generate-new-buffer "*stdin*"))) (switch-to-buffer b) (insert-file-contents "'${TMP}'") (delete-file "'${TMP}'"))')
    else
        args=($@ $args)
    fi
fi

# emacsclient $args
if $nw; then
    emacsclient "${args[@]}"
else
    (nohup emacsclient "${args[@]}" > /dev/null 2>&1 &) > /dev/null
fi
```


# Running Emacs properly from the GUI

This `.desktop` file calls `emacs` when it's not already running, and `emacsclient` otherwise. Slow on first launch, then fast for every new frame thereafter.

Tangling this file will install the .desktop file to the correct location (`~/.local/share/applications/Emacsclient.desktop`).

```toml
[Desktop Entry]
Name=Emacs
GenericName=Text Editor
Comment=Edit text
MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
Exec=emacsclient -c -a "emacs" %F
Icon=emacs
Type=Application
Terminal=false
Categories=Development;TextEditor;Utility;
StartupWMClass=Emacs
```


## TODO Figure out how to run Emacs as a daemon so that closing the last frame doesn't exit

Launching in headless mode introduces some font problems (fonts don't load when changing themes) that I haven't been able to debug.


# Compiling Emacs from Source

Some notes on the dependencies that I found were needed to build Emacs 30.1 on Ubuntu with the configuration flags that I like


## Fetch the release

```shell
mkdir -p ~/emacs
cd ~/emacs
test -f emacs-30.1.tar.xz || wget -c https://ftpmirror.gnu.org/emacs/emacs-30.1.tar.xz 
tar xf emacs-30.1.tar.xz
```


## Dependencies:

[This helpful blog post](https://troglobit.com/post/2024-03-03-building-emacs-with-jit/) clued me in to how to figure out which version of `libgccjit` is needed; run `gcc --version` to see the major version the OS is going to call when calling `make` and install `libgccjit-$VERSION-dev`. Thanks @troglobit!


## Build the release

```shell
./autogen.sh
./configure --with-imagemagick --with-x-toolkit=gtk3 --with-native-compilation --with-mailutils --with-tree-sitter
make -j $(nproc)
sudo make install
```


### Final notes:

-   I removed `--with-json` in this release; seems unnecessary now
-   I had to remove support for [xwidgets due to this issue](https://www.reddit.com/r/emacs/comments/1fpd3dk/problem_compiling_latest_git_version/) in 30.1. Revisit in the future? Maybe I wasn't using them.
