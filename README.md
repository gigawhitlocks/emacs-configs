- [Custom Config and Travelogue](#org9e60674)
  - [Justification](#org182b487)
    - [It's actually an interactive Lisp interpeter](#org5e36674)
    - [Org Mode](#org8c693fc)
  - [Featuring](#orgd6e529b)
    - [evil-mode](#org3ed54bc)
    - [helm](#org6e1ad88)
    - [magit](#org987caac)
    - [projectile](#orgf493463)
    - [general](#orgad4c936)
    - [which-key](#orgd39fb01)
    - [leuven-theme](#org4e40436)
- [`init.el` Entrypoint](#org14f4529)
- [My Customizations](#orgbd1c439)
  - [Bootstrap](#org24d37a7)
  - [Global Package Installation and Configuration](#org8cea890)
  - [Language Configuration](#org07954ba)
    - [General](#orge71ef31)
    - [Scala](#org1bfb508)
    - [YAML](#org79d5285)
    - [Docker](#orgd995ea6)
    - [Python](#orga237074)
    - [Go](#orgbf4d6fd)
    - [Web](#org9e0a7c0)
    - [Enable modes](#orgded3ea4)
  - [Configuration Variables](#org33a2859)
    - [Global Keybindings](#orgd62a4df)
    - [Mode-Local Keybindings](#org71cee22)
    - [Org Mode Settings](#orgdaeaca5)
    - [Hostname-based Tweaks](#orgfb8dc3a)
    - [Misc Settings](#org3839304)
    - [Publish to README.md](#org1d4839b)
    - [Run Stuff](#orgb329dfd)
- [Packages to Try](#org620d645)
  - [emmet-mode](#org64f1346)
  - [yasnippet-snippets](#org85d3190)
- [Notes and Such](#org61d57e6)
  - [System-local settings](#org20e94b2)
  - [Hyperbole](#org03cc06f)
  - [Monospace Fonts](#orgfa62b5b)
    - [<https://github.com/adobe-fonts/source-code-pro/tree/master>](#org9dc5274)
    - [<https://github.com/be5invis/Iosevka>](#org3516989)
    - [<https://github.com/googlefonts/Inconsolata>](#org5b3efb9)
    - [<https://github.com/tonsky/FiraCode>](#orgac74a90)
    - [<https://github.com/source-foundry/Hack>](#org88280be)
  - [Proportional Fonts](#org7fb956b)
  - [ERC](#org74a552c)
  - [Mail](#orgb7bcbd9)
  - [Emoji](#org13e1bae)



<a id="org9e60674"></a>

# Custom Config and Travelogue

This file contains the configuration necessary to transform a GNU Emacs 26 installation on Linux or Mac OS X into my very own personalized, hand-crafted, artisanal programming and text-editing environment maintained for my own use, hopefully in a way that can remain maintainable for the ongoing future.

There are many configurations like this one, but this one is mine.


<a id="org182b487"></a>

## Justification

Emacs is an abberation among modern software, and it is my favorite piece of software. It's actually ancient software, but it is still actively maintained and has an excited and crafty userbase as a community. It's a text editor on the surface, and certainly Emacs can be used fairly quickly (after following the tutorial) for basic text editing, and perhaps its author even thought of it as merely a text editor. But..


<a id="org5e36674"></a>

### It's actually an interactive Lisp interpeter

It's really a text editor with a Lisp interpreter attached, and Lisp turns out to be especially good at parsing text and for building domain specific languages (DSLs) for adding functionality to the existing language. Now, it just so happens that this particular dialect of Lisp comes with a native GUI library &#x2013; Emacs itself &#x2013; that it understands natively.

Suddenly using the program and programming it become similar tasks. This is the brilliance of Emacs. "But I don't have time to program my editor!" Yeah, nobody really does, that's why this repository is open source (in case someone can use it) and it's why we lean heavily on third party packages.

But because Emacs is primarily written in Emacs Lisp, you can explore the source code of Emacs directly from the editor itself, edit it, and execute the new version and have it affect your current session at runtime.

Here's a trivial example. A new Emacs user does the tutorial and learns that `M-x` runs a command. One of the first ones they learn is `find-file`. `find-file` queries the user for a file location and then opens that file in a new buffer and displays it in the current window. It's also accessible via `C-x C-f` by default. The same function is `(find-file)` in Emacs Lisp. Run with an argument, it won't query the user. Any action you can do interactively can be automated through Emacs Lisp. Because of this flexibility, the world of 3rd party packages is *awesome*.

Oh yeah, and then there's Org Mode. Learning Emacs is worth it just for Org Mode.


<a id="org8c693fc"></a>

### Org Mode

This configuration is written in Org syntax.

Org Mode is a display mode for content written in the Org markup language, which provides syntax for annotating the content similar to Markdown, as well as syntax for attaching metadata to the content so that it can be acted upon from Emacs Lisp or other languages, through an extension called Babel, which is often referred to as `org-babel`. Org and Babel allow you to write prose alongside code and export ("tangle") it to source files or execute it interactively while writing the document and automatically include the results of execution alongside the code and commentary in the document.

Org Mode allows you to easily collapse heading subtrees, execute code blocks, edit code blocks in the native mode of the language present in the code block, and write software in the literate programming style, where there's more explanation and exposition than code, like this configuration.


<a id="orgd6e529b"></a>

## Featuring

My config features many 3rd party packages. I'd like to give special attention to some of my favorites


<a id="org3ed54bc"></a>

### evil-mode

Bringing a sane editing experience to Emacs.


<a id="org6e1ad88"></a>

### helm

Helm brings an awesome command UI to Emacs that completely alters the experience (for the better, in my opinion).


<a id="org987caac"></a>

### magit

The best `git` UI on the planet.


<a id="orgf493463"></a>

### projectile

Allowing jumping around inside of version-controlled projects, searching, all kinds of goodies.


<a id="orgad4c936"></a>

### general

Sane key bindings.


<a id="orgd39fb01"></a>

### which-key

Pops up a window to show available commands, given a prefix. Awesome for discoverability, which is essential for a platform as extensible as Emacs.


<a id="org4e40436"></a>

### leuven-theme

Beautiful old-school light theme. Eventually I'll choose a dark theme to accompany this one.


<a id="org14f4529"></a>

# `init.el` Entrypoint

First of all there's `init.el`. Emacs runs this file by default after it's done initializing the underlying system, so it's where our code traditionally goes. This file needs to be checked in so that there's something executable for Emacs to run when this repo is pulled down anew, so it can't be an `org` file. We need `init.el` to call `org-babel-load-file` on this file to turn it into a `.el` that we can load and execute.

So `init.el` just holds the bare minimum code. We use an [example from orgmode.org](https://orgmode.org/worg/org-contrib/babel/intro.html#literate-emacs-init) to load the Org files and tangle them. Then we `require` the output from the call to tangle, and run `main`.

```emacs-lisp
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
  (require 'org-install)
  (require 'ob-tangle))

;; load up all literate org-mode files in this directory
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

(require '~/.emacs.d/ian.el)
(main)

;; Load automatic and interactive customizations from this computer
(shell-command "touch ~/.emacs.d/.emacs-custom.el")
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)
(provide 'init)
```

The rest of the code that is executed begins with the routines defined by this file.


<a id="orgbd1c439"></a>

# My Customizations

After running the `init.el` entrypoint, this file is tangled to `ian.el` and executed. Right now all configuration other than the entrypoint is in this file.


<a id="org24d37a7"></a>

## Bootstrap

The bootstrap function sets up the main software repository for downloading 3rd party packages and a tool for managing installing those packages.

```emacs-lisp
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

  ;; manual PATH management
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (add-to-list 'exec-path "/usr/local/bin" t)

  (require 'package)
  ;; (add-to-list
  ;;  'package-archives
  ;;  '("melpa" . "http://melpa.org/packages/"))
  (setq package-archives
	'(("melpa" . "https://gitlab.com/d12frosted/elpa-mirror/raw/master/melpa/")
	  ("org"   . "https://gitlab.com/d12frosted/elpa-mirror/raw/master/org/")
	  ("gnu"   . "https://gitlab.com/d12frosted/elpa-mirror/raw/master/gnu/")))
  (package-initialize)

  ;; Now install use-package to enable us to use it
  ;; to manage the rest of our packages

  (unless (package-installed-p 'use-package)
    (progn
      (unless package-archive-contents
	(package-refresh-contents))
      (package-install 'use-package)))

  ;; set ensure to be the default
  (require 'use-package-ensure)
  (setq use-package-always-ensure t)

  ;; allow use-package to install system tools via apt, brew
  (use-package use-package-ensure-system-package)

  ;; sane keybindings from the start
  (use-package general
    :config
    (general-evil-setup))

  ;; hydra allows for sub-menus to pop up.. it's cool, and it's a dependency for sbt-hydra
  (use-package hydra)

  ;; these go in bootstrap because packages installed
  ;; with use-package use :diminish and :delight
  (use-package diminish)
  (use-package delight))
```


<a id="org8cea890"></a>

## Global Package Installation and Configuration

The `global-packages` function is responsible for installing deep dependencies. These are packages essential to my workflow.

```emacs-lisp
(defun global-packages ()
  "Install and configure packages used with many modes and standalone modes and applications."
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25)

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

    ;; TODO probably org-mode dependencies will need to be their own function
    ;; bindings for org-mode
    (use-package evil-org)
    ;; image drag-and-drop for org-mode
    (use-package org-download)

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

  ;; forge lets us interact with git forges e.g. GitHub, Gogs, Gitlab
  (use-package forge
    :after magit)
  ;; If you store the token in a file like ~/.authinfo, then note that auth-source’s parsing of that file is brittle.
  ;; Make sure the file ends with a newline character, that there are no empty or invalid lines, and that all comments are prefixed with #.

  (use-package which-key
    :delight
    :init
    (which-key-mode)
    (which-key-setup-minibuffer))


  (defun ansi ()
    ;; enable ANSI escape codes in compilation buffer
    (use-package ansi-color)
    ;; slightly modified from
    ;; https://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
    (defun colorize-compilation ()
      "Colorize from `compilation-filter-start' to `point'."
      (let ((inhibit-read-only t))
	(ansi-color-apply-on-region
	 compilation-filter-start (point))))

    (add-hook 'compilation-filter-hook
	      #'colorize-compilation))
  (ansi)

  (defun dashboard ()
    ;; provides a nice looking dashboard at launch
    ;; see more here https://github.com/emacs-dashboard/emacs-dashboard
    (use-package all-the-icons) ;; provides optional icons for dashboard
    (use-package dashboard
      :config
      (dashboard-setup-startup-hook)
      (setq dashboard-startup-banner 'logo)
      (setq dashboard-center-content t)
      (setq dashboard-items '((recents  . 5)
			      (bookmarks . 5)
			      (projects . 5))
	    )
			      ;; (registers . 5)
			      ;; (agenda . 5)
      )
    )
  ;; anything so trivial that there is no config necessary goes here
  (defun extra-packages ()

    (use-package git-gutter
      :config
      (global-git-gutter-mode +1))
    ;; git-gutter does not play nicely with linum-mode
    ;; investigate long-term solution?

    ;; provides highlighting of the current line
    (global-hl-line-mode)
    (setq global-hl-line-sticky-flag t)

    (use-package restart-emacs)
    (use-package yasnippet
      :delight
      :config
      (use-package yasnippet-snippets))
    (use-package systemd)
    (use-package ranger)
    (use-package htmlize)
    (setq ranger-show-literal nil)
    ;; themes
    ;;(use-package color-theme-sanityinc-tomorrow)
    ;;(use-package leuven-theme)
    (use-package centered-window)
    )

  ;; auto-completion
  (use-package company
    :delight
    :config
    ;; enable it everywhere
    (add-hook 'after-init-hook 'global-company-mode)

    ;; tab complete!
    (global-set-key "\t" 'company-complete-common))

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
      (use-package helm-descbinds
	:config
	(helm-descbinds-mode))

      (global-set-key (kbd "M-x") #'helm-M-x)
      (define-key helm-find-files-map "\t" 'helm-execute-persistent-action)
      (setq helm-always-two-windows nil)
      (setq helm-default-display-buffer-functions '(display-buffer-in-side-window))
      (helm-mode 1)))

  ;; gnu hyperbole
  (use-package hyperbole
    :config
    )

  (setup-evil)
  (setup-projectile)
  (setup-magit)
  (setup-helm)
  (dashboard)
  (extra-packages)
  )
```


<a id="org07954ba"></a>

## Language Configuration


<a id="orge71ef31"></a>

### General

```emacs-lisp
(defun languages ()
  "Setup for specific programming languages."

  (defun setup-lsp ()
    "Enable nice rendering of diagnostics like compile errors."
    (use-package lsp-mode
      :init
      (setq lsp-prefer-flymake nil)) ;; use flycheck

    (use-package lsp-ui
      :init (setq lsp-ui-doc-position 'bottom))

    (use-package helm-lsp)

    ;; Add lsp backend for other tools
    (use-package company-lsp)
    (use-package lsp-origami))
```


<a id="org1bfb508"></a>

### Scala

```emacs-lisp
;; snippet from https://scalameta.org/metals/docs/editors/emacs.html

(defun scala ()
  "Enable scala-mode and sbt-mode."

  (setq lsp-enable-file-watchers nil)
  ;; (set (make-local-variable 'lsp-enable-file-watchers) nil)

  ;; this was taken from the install instructions 4/24/2019
  (use-package scala-mode
    :hook ((scala-mode . lsp-deferred))
    :mode "\\.s\\(cala\\|bt\\)$")

  (general-define-key
   :states 'normal
   :keymaps 'scala-mode-map
   "gd" 'lsp-find-definition
   "gh" 'lsp-describe-thing-at-point)

  (use-package sbt-mode
    :commands sbt-start sbt-command
    :config
    ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
    ;; allows using SPACE when in the minibuffer
    (substitute-key-definition
     'minibuffer-complete-word
     'self-insert-command
     minibuffer-local-completion-map)
    (setq sbt:prefer-nested-projects t)
    (setq sbt:scroll-to-bottom-on-output t))

    ;; general LSP setup is in General because LSP isn't just used for Scala
  )

```


<a id="org79d5285"></a>

### YAML

```emacs-lisp
(use-package yaml-mode)
```


<a id="orgd995ea6"></a>

### Docker

```emacs-lisp
(defun docker ()
  (use-package dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))
```


<a id="orga237074"></a>

### Python

```emacs-lisp
(defun python ()

  (use-package auto-virtualenv)
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)

  (use-package anaconda-mode
    :config
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

```


<a id="orgbf4d6fd"></a>

### Go

```emacs-lisp

(defun go ()
  ;;

  (defun set-gopls-lib-dirs ()
    "Add $GOPATH/pkg/mod to the 'library path'."
    ;; stops lsp from continually asking if Go projects should be imported
    (setq lsp-clients-go-library-directories
	  (list
	   "/usr"
	   (concat (getenv "GOPATH") "/pkg/mod"))))

  ;; native go mode
  (use-package go-mode
    :hook ((go-mode . lsp-deferred)
	   (go-mode . set-gopls-lib-dirs))
    :config
    ;; fixes ctrl-o after goto-definition by telling evil that godef-jump jumps
    ;; presumably for lsp this is #'lsp-find-definition here instead
    (evil-add-command-properties #'godef-jump :jump t))

  (general-define-key
   :states 'normal
   :keymaps 'go-mode-map
   "gd" 'lsp-find-definition)

  (autoload 'go-mode "go-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

  ;; autocompletion
  ;; https://github.com/mdempsky/gocode
  ;; and https://github.com/mdempsky/gocode/tree/master/emacs-company
  (use-package company-go)

  ;; disable auto-completion of non-Go things in Go files
  (add-hook 'go-mode-hook (lambda ()
			    (set (make-local-variable 'company-backends) '(company-go))
			    (company-mode)))

  ;; disable "Organize Imports" warning that never goes away
  (add-hook 'go-mode-hook
	    (lambda ()
	      (origami-mode)
	      (setq-local lsp-ui-sideline-show-code-actions nil)))

  ;; super important -- eldoc support adds things like type signatures in modeline
  (use-package go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup)

  ;; gofmt before save
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; go guru integration provides lots of code analysis commands
  (use-package go-guru
    :config
    (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

  (load-file "~/.emacs.d/vendor/go-dlv.el")
  (require 'go-dlv)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; gofmt doesn't enforce a line length limit, so many projects also do not.	   ;;
  ;; theoretically I'm fine with this, but no word-wrap looks ugly and unindented  ;;
  ;; code wrapping to the second line breaks the visual flow of the program.	   ;;
  ;; 										   ;;
  ;; so to fix this, adaptive-wrap word-wraps and then visually indents text	   ;;
  ;; 1 indent level past the indent level of the wrapped line, so that it appears  ;;
  ;; to be nicely formatted, without editing the actual source text		   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package adaptive-wrap
    :config
    (setq-default adaptive-wrap-extra-indent 1)
    (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
    (add-hook 'go-mode-hook #'visual-line-mode))

  (load-file "~/.emacs.d/vendor/go-dlv.el")
  (require 'go-dlv)

  ;; adaptive-wrap
  (use-package adaptive-wrap
    :config
    (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
    (add-hook 'go-mode-hook #'visual-line-mode))

  )


;; go
```


<a id="org9e0a7c0"></a>

### Web

```emacs-lisp
(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-auto-pairing t)

(use-package impatient-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . impatient-mode-hook))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . httpd-start-hook)))
```


<a id="orgded3ea4"></a>

### Enable modes

```emacs-lisp
(setup-lsp)
(go)
(python)
(docker)
(scala))
```


<a id="org33a2859"></a>

## Configuration Variables


<a id="orgd62a4df"></a>

### Global Keybindings

```emacs-lisp
(defun config ()
  "Global configuration variables and such."

  ;; helper functions for keybindings
  ;; this one lifted from https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
  (defun er-switch-to-previous-buffer ()
    "Switch to previously open buffer. Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

  ;; override Home/End behavior to be more like modern applications
  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line)

  (general-create-definer my-leader-def
    ;; :prefix my-leader
    :prefix "SPC")

  (general-create-definer my-local-leader-def
    ;; :prefix my-local-leader
    :prefix "SPC m")

  ;; global keybindings
  (my-leader-def
    :keymaps 'normal

    ;; buffer control
    "bb"	'switch-to-buffer
    "TAB"	#'switch-to-prev-buffer
    "bd"	'evil-delete-buffer

    ;; compile
    "cc"        'compile

    ;; errors
    "ec"	'flycheck-clear
    "el"	'flycheck-list-errors
    "en"	'flycheck-next-error
    "ep"	'flycheck-previous-error

    "Fm"        'make-frame

    ;; hmm
    "ff"	'helm-find-files
    "fr"        'helm-recentf
    "fed"	'(lambda () (interactive)
		   (find-file "~/.emacs.d/ian.org"))

    "feD"	'(lambda () (interactive)
		   (find-file-other-frame "~/.emacs.d/ian.org"))
    "feR"	'(lambda () (interactive)
		   (org-babel-tangle "~/.emacs.d/ian.org")
		   (byte-compile-file "~/.emacs.d/ian.el"))

    ;; git
    "gb"	'magit-blame
    "gs"	'magit-status
    "gg"	'magit
    "gd"	'magit-diff

    ;; hyperbole
    "h"        'hyperbole
    ;; bookmarks (j for jump)
    "jj"	'bookmark-jump
    "js"	'bookmark-set
    "jo"        'org-babel-tangle-jump-to-org

    "ic"         'insert-char
    ;; projectile
    "p"	'projectile-command-map
    "pf"	'helm-projectile-find-file
    "sp"	'helm-projectile-ack

    ;; quitting
    "qq"	'save-buffers-kill-terminal
    "qr"	'restart-emacs

    ;; simple toggles
    "tn"	'linum-mode

    ;; window control
    "w-"	'split-window-below
    "w/"	'split-window-right
    "wj"	(lambda () (interactive)
		  (select-window (window-in-direction 'below)))
    "wk"	(lambda () (interactive)
		  (select-window (window-in-direction 'above)))
    "wh"	(lambda () (interactive)
		  (select-window (window-in-direction 'left)))
    "wl"	(lambda () (interactive)
		  (select-window (window-in-direction 'right)))
    "wd"	'delete-window
    "wD"	'delete-other-windows
    "wo"	'other-window
    "w="        'balance-windows

    ";"         'comment-line

    "SPC"	'helm-M-x
    )
```


<a id="org71cee22"></a>

### Mode-Local Keybindings

```emacs-lisp
(my-local-leader-def 'normal emacs-lisp-mode-map
  "e" 'eval-last-sexp)

(my-local-leader-def
  :states 'normal
  :keymaps 'org-mode-map
  "y" 'org-store-link
  "p" 'org-insert-link
  "x" 'org-babel-execute-src-block
  "e" 'org-edit-src-code)

 (my-local-leader-def
   :states 'normal
   :keymaps 'go-mode-map
   "g"   'go-guru-map)

```


<a id="orgdaeaca5"></a>

### Org Mode Settings

```emacs-lisp

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
			       (shell . t)
			       )
			     )

;; github-flavored markdown
(use-package ox-gfm)

;; enable markdown export
(eval-after-load "org"
  (progn
    '(require 'ox-md nil t)
    '(require 'ox-gfm nil t)))
```


<a id="orgfb8dc3a"></a>

### Hostname-based Tweaks

```emacs-lisp

(let ;; find the hostname and assign it to a variable
     ((hostname (string-trim-right
		 (shell-command-to-string "hostname"))))

   (progn
     (org-babel-tangle-file
      (concat "~/.emacs.d/local/" hostname ".org")
      (concat hostname ".el"))

     (load (concat "~/.emacs.d/local/" hostname ".el"))
     (require 'local)))

```


<a id="org3839304"></a>

### Misc Settings

```emacs-lisp
;; backups to /tmp
(setq backup-directory-alist `(("." . "/tmp/.emacs-saves")))
(setq backup-by-copying t)

;; set default window size
(add-to-list 'default-frame-alist '(width . 128))
(add-to-list 'default-frame-alist '(height . 60))

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
(defun create-gif (duration)
  "Create a gif of the current frame with the DURATION provided."
  (interactive "sDuration: ")

  (defun width ()
    "get the width of the frame"
    (+ 10 (frame-pixel-width)))

  (defun height ()
    "get the height of the frame"
    (+ 50 (frame-pixel-height)))

  (defun y ()
    "get the y position of the frame"
    (frame-parameter nil 'top))

  (defun x ()
    "get the x position of the frame"
    (cond ((numberp (frame-parameter nil 'left))
	   (frame-parameter nil 'left))
	  (t
	   0)))

  (defun filename()
    "get the timestamped filename of the gif"
    (concat " ~/emacs-gifs/" (format-time-string "%Y-%m-%dT%T") ".gif"))

  (if (not (file-directory-p "~/emacs-gifs"))
      (make-directory "~/emacs-gifs"))
  (start-process-shell-command 
  "create-gif" "*Messages*"
  (format "byzanz-record -d %s -w %d -h %d -x %d -y %d %s"
  duration (width) (height) (x) (y) (filename))))



;; remove extraneous window chrome
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(scroll-bar-mode -1)

;; turn off startup
(setq inhibit-startup-screen t))

```


<a id="org1d4839b"></a>

### Publish to README.md

```emacs-lisp
(defun publish ()
  "Publishes (to github flavored markdown for now."
  (org-gfm-export-to-markdown)
  (rename-file "ian.md" "README.md"))
```


<a id="orgb329dfd"></a>

### Run Stuff

Main is called in `init.el` and runs the rest of of the config.

```emacs-lisp

(defun main()
  "Initialize everything!"
  (bootstrap)
  (global-packages)
  (languages)
  (config)
  (publish)
  (server-start))

(provide '~/.emacs.d/ian.el)
;;; ian.el ends here
```


<a id="org620d645"></a>

# Packages to Try


<a id="org64f1346"></a>

## emmet-mode

Emmet is the "zen coding" plugin for really fast HTML authoring <https://github.com/smihica/emmet-mode>


<a id="org85d3190"></a>

## yasnippet-snippets

Some default snippets &#x2013; don't install until we're ready to figure out how to use them <https://github.com/AndreaCrotti/yasnippet-snippets>


<a id="org61d57e6"></a>

# Notes and Such


<a id="org20e94b2"></a>

## DONE System-local settings

Include all `.el` files from the untracked folder `local-variables/` and run them as the final step. This allows for customization at the end of the configuration for specific things that are dependent on the computer on which this config is being run. For instance, anything with sensitive details or URLs can be symlinked from a private repo to this one for inclusion in the config without sharing secrets with the whole Internet.

1.  Ensure that `local-variables/` exists and create it if it does not.
2.  Load anything that's in there &#x2013; be sure to fail sanely if there's nothing there!
3.  That's it, there is no three.


<a id="org03cc06f"></a>

## DONE Hyperbole

```
17:41 user1: is there a way to do the equivalent of C-x C-e on a #+INCLUDE: directive in Org?
17:46 user2: Of course: C-a C-c ' C-x h M-w M-x org-mark-ring-goto C-y C-k
17:51 user1: I could probably transform that string of commands into a Lisp function.. and then write an implicit button rule for Hyperbole so that I can shift+middle-click on an #+INCLUDE: directive and have it drop the contents of the file inside my org file..
17:52 user1: that'd be the correct behavior
```


<a id="orgfa62b5b"></a>

## DONE Monospace Fonts

Just going to keep note of some options


<a id="org9dc5274"></a>

### <https://github.com/adobe-fonts/source-code-pro/tree/master>

Default in Spacemacs


<a id="org3516989"></a>

### <https://github.com/be5invis/Iosevka>

Kinda tall, skinny


<a id="org5b3efb9"></a>

### <https://github.com/googlefonts/Inconsolata>

Has ligatures


<a id="orgac74a90"></a>

### <https://github.com/tonsky/FiraCode>

More ligatures, but you have to Do Stuff in Emacs <https://github.com/tonsky/FiraCode/wiki/Emacs-instructions> Described as "cool" on IRC


<a id="org88280be"></a>

### <https://github.com/source-foundry/Hack>

I mean, it's called "Hack"


<a id="org7fb956b"></a>

## Proportional Fonts

I don't want proportional fonts everywhere, but it'd be nice to have them in writing-focused modes like Org! Xah Lee has an example where he does something similar to what I'd want <http://ergoemacs.org/emacs/emacs_proportional_font.html>


<a id="org74a552c"></a>

## ERC

This is something I'd like eventually. Maybe? Here's a Reddit thread on the topic. <https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/>


<a id="orgb7bcbd9"></a>

## Mail

Eventually.


<a id="org13e1bae"></a>

## DONE Emoji

<https://github.com/iqbalansari/emacs-emojify>
