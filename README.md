- [Ian's Custom Emacs Environment with Associated Notes and Commentary](#org6cc290c)
  - [Justification](#org32b52ce)
  - [Featuring](#orgd4e6a3d)
- [Entrypoint](#org268f301)
- [My Environment](#orga0fcd67)
  - [Bootstrap](#org311a1b7)
  - [Package Installation and Configuration](#org4674026)
  - [Language Configuration](#org5a4fbf0)
  - [Global Environment Configuration](#org5c70761)
  - [Render this file for display on Github](#org73b4e95)
  - [Run Stuff](#org992fb82)
- [Notes and Such](#orge3de7a0)
  - [System-local settings](#org8b6f3d0)
  - [Hyperbole](#org563e2d3)
  - [Monospace Fonts](#orgbc8b182)
  - [Proportional Fonts](#org173aae2)
  - [Authentication and Secrets in Emacs](#org92bd316)
  - [Packages to Try](#orgaf81db4)



<a id="org6cc290c"></a>

# Ian's Custom Emacs Environment with Associated Notes and Commentary

This file contains the configuration necessary to transform a GNU Emacs 26 installation (only tested on Linux) into my very own personalized, hand-crafted, artisanal programming and text-editing environment maintained for my own use, hopefully in a way that can remain maintainable for the ongoing future.

There are many configurations like this one, but this one is mine.


<a id="org32b52ce"></a>

## Justification

Emacs is an abberation among modern software, and it is my favorite piece of software. It's actually ancient software, but it is still actively maintained and has an excited and crafty userbase as a community. It's a text editor on the surface, and certainly Emacs can be used fairly quickly (after following the tutorial) for basic text editing, and perhaps its author even thought of it as merely a text editor. But..


### It's actually an interactive Lisp interpreter

It's really a text editor with a Lisp interpreter attached, and Lisp turns out to be especially good at parsing text and for building domain specific languages (DSLs) for adding functionality to the existing language. Now, it just so happens that this particular dialect of Lisp comes with a native GUI library &#x2013; Emacs itself &#x2013; that it understands natively.

Suddenly using the program and programming it become similar tasks. This is the brilliance of Emacs. "But I don't have time to program my editor!" Yeah, nobody really does, that's why this repository is open source (in case someone can use it) and it's why we lean heavily on third party packages.

But because Emacs is primarily written in Emacs Lisp, you can explore the source code of Emacs directly from the editor itself, edit it, and execute the new version and have it affect your current session at runtime.

Here's a trivial example. A new Emacs user does the tutorial and learns that `M-x` runs a command. One of the first ones they learn is `find-file`. `find-file` queries the user for a file location and then opens that file in a new buffer and displays it in the current window. It's also accessible via `C-x C-f` by default. The same function is `(find-file)` in Emacs Lisp. Run with an argument, it won't query the user. Any action you can do interactively can be automated through Emacs Lisp. Because of this flexibility, the world of 3rd party packages is *awesome*.

Oh yeah, and then there's Org Mode. Learning Emacs is worth it just for Org Mode.


### Org Mode

This configuration is written in Org syntax.

Org Mode is a display mode for content written in the Org markup language, which provides syntax for annotating the content similar to Markdown, as well as syntax for attaching metadata to the content so that it can be acted upon from Emacs Lisp or other languages, through an extension called Babel, which is often referred to as `org-babel`. Org and Babel allow you to write prose alongside code and export ("tangle") it to source files or execute it interactively while writing the document and automatically include the results of execution alongside the code and commentary in the document.

Org Mode allows you to easily collapse heading subtrees, execute code blocks, edit code blocks in the native mode of the language present in the code block, and write software in the literate programming style, where there's more explanation and exposition than code, like this configuration.


<a id="orgd4e6a3d"></a>

## Featuring

My config features many 3rd party packages. I'd like to give special attention to some of my favorites


### evil-mode

Bringing a sane editing experience to Emacs.


### helm

Helm brings an awesome command UI to Emacs that completely alters the experience (for the better, in my opinion).


### magit

The best `git` UI on the planet.


### projectile

Allowing jumping around inside of version-controlled projects, searching, all kinds of goodies.


### general

Sane key bindings.


### which-key

Pops up a window to show available commands, given a prefix. Awesome for discoverability, which is essential for a platform as extensible as Emacs.


### leuven-theme

Beautiful old-school light theme. Eventually I'll choose a dark theme to accompany this one.


<a id="org268f301"></a>

# Entrypoint

This is `init.el`. Using Org for my configuration is a personal choice &#x2013; Emacs runs `init.el` at the beginning of execution. This piece of code tangles to `init.el`, and `init.el` containing the following must be checked in, because this snippet tangles *this* file and executes `main`, so it is this piece of code that starts the whole process of loading all of this configuration.

`init.el` just holds the bare minimum code so that the bulk of the configuration can be checked in once, inside this file, rather than twice like the contents of `init.el`. I'm using an [example from orgmode.org](https://orgmode.org/worg/org-contrib/babel/intro.html#literate-emacs-init) to load the Org files and tangle them, then `require` the output from the call to tangle, run `main`, and I'm done.

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


<a id="orga0fcd67"></a>

# My Environment

The point of Emacs, and the reason every Emacs user should write his or her own configuration, is to build your own environment so that it behaves how you want it to.

This may seem to be a lot of work, and it is. But if a serious guitar player might have a custom guitar built, I see no reason I shouldn't use a custom environment for my craft.

**This is where my environment definition begins**

After running the `init.el` entrypoint, this file is tangled to `ian.el` and executed. Right now all configuration other than the entrypoint is in this file.


<a id="org311a1b7"></a>

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
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			   ("melpa" . "https://melpa.org/packages/")
			   ("org" . "http://orgmode.org/elpa/")))
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

  (use-package use-package-ensure-system-package)

  ;; these go in bootstrap because packages installed
  ;; with use-package use :diminish and :delight
  (use-package diminish)
  (use-package delight))
```


<a id="org4674026"></a>

## Package Installation and Configuration

The `global-packages` function is responsible for installing deep dependencies. These are packages essential to my workflow. Configuration here should be config that must run early, before variables are set or language-related packages, which will likely rely on these being set.

Also this is the one part I have not managed to break up and it's a giant blob. Good luck, any readers, including me.

```emacs-lisp
(defun global-packages ()
  "Install and configure packages used with many modes and standalone modes and applications."

  ;; left hand side tree view like neotree
  ;; nice for exploring smaller projects
  (use-package treemacs)

  ;; clean whitespace on save in all modes
  (add-hook 'before-save-hook 'whitespace-cleanup)

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

  (defun setup-projectile ()
	(use-package projectile
	  :delight)
	(use-package helm-projectile)
	(use-package treemacs-projectile)
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

	;; add fd as a remap for esc
	(use-package evil-escape
	  :delight)

	(evil-escape-mode 1)
	(setq-default evil-escape-key-sequence "fd"))
  ;; sane keybindings from the start

  (use-package general
	:init
	(setup-evil)
	:config
	(general-evil-setup))

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

  ;; customizations to compilation mode

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

	;; hugo blog management
	(use-package easy-hugo
	  :config
	  (setq easy-hugo-basedir "~/keming.org")
	  (add-to-list 'evil-emacs-state-modes 'easy-hugo-mode))

  (use-package git-gutter
	  :delight
	  :config
	  (global-git-gutter-mode +1))
	;; git-gutter does not play nicely with linum-mode
	;; investigate long-term solution?

	;; provides highlighting of the current line
	(global-hl-line-mode)
	(setq global-hl-line-sticky-flag t)

	;; set up rainbow delimiters for Elisp
	(use-package rainbow-delimiters
	  :config
	  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
	  )


	(use-package restart-emacs)
	(use-package s) ;; elisp string manipulation utility lib
	(use-package yasnippet
	  :delight
	  :config
	  (use-package yasnippet-snippets))
	(use-package systemd)
	(use-package ranger
	  :config
	  (setq ranger-show-literal nil))
	(use-package htmlize)
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

  (setup-projectile)
  (setup-magit)
  (setup-helm)
  (dashboard)
  (extra-packages))
```


<a id="org5a4fbf0"></a>

## Language Configuration


### General

```emacs-lisp
(defun languages ()
  "Setup for specific programming languages."

  (defun setup-lsp ()
	"Enable nice rendering of diagnostics like compile errors."
	(use-package lsp-mode
	  :init
	  (setq lsp-prefer-flymake nil)) ;; use flycheck

	  (use-package lsp-ui)
	  (use-package lsp-ui
	:config
	(setq lsp-ui-doc-position 'bottom))

	;; (use-package lsp-ui
	;;   :init (setq lsp-ui-doc-position 'bottom))

	(use-package helm-lsp)

	;; Add lsp backend for other tools
	(use-package lsp-treemacs)
	(use-package company-lsp)
	(use-package lsp-origami))
```


### YAML

```emacs-lisp
(use-package yaml-mode)
```


### Markdown

```emacs-lisp
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . gfm-mode)
	 ("\\.markdown\\'" . gfm-mode)))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'variable-pitch-mode)

;; this can go here because it affects Markdown's live preview mode
;; but I should consider putting it somewhere more general maybe?
(add-hook 'eww-mode-hook 'visual-line-mode)
```


### Docker

```emacs-lisp
(defun docker ()
  (use-package dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))
```


### Python

```emacs-lisp
(defun python ()

  (use-package auto-virtualenv)
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)

  (use-package anaconda-mode
	:config
	(add-hook 'python-mode-hook 'anaconda-mode)
	(add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

  (setenv "WORKON_HOME" "~/.virtualenvs")

```


### Go

Go support requires some dependencies. I will try to list them all here. Stuff I have installed has some overlap because of the in-progress move to LSP, but I'll prune it later.

-   First, `go` itself, install however you choose. I like to add my GOPATH and GOROOT to `~/.profile` so that they show up in both my shell and in Emacs.

-   `go install` `godef` for definitions <https://github.com/rogpeppe/godef>\`
-   `gopls`, the language server for LSP mentioned above <https://github.com/golang/tools/blob/master/gopls/doc/user.md>

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
   :states  'normal
   :keymaps 'go-mode-map
   ",a"     'go-import-add
   ",d"     'lsp-describe-thing-at-point
   ",g"     'lsp-find-definition
   ",i"     'lsp-find-implementation
   ",n"     'lsp-rename
   ",r"     'lsp-find-references
   ",t"     'lsp-find-type-definition
   ",x"     'lsp-execute-code-action
   "gd"     'lsp-find-definition
   )

  (autoload 'go-mode "go-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

  ;; autocompletion
  ;; https://github.com/mdempsky/gocode
  ;; and https://github.com/mdempsky/gocode/tree/master/emacs-company
  ;;  (use-package company-go)

  ;; disable auto-completion of non-Go things in Go files
  ;; (add-hook 'go-mode-hook (lambda ()
  ;;				(set (make-local-variable 'company-backends) '(company-go))
  ;;				(company-mode)))

  ;; disable "Organize Imports" warning that never goes away
  (add-hook 'go-mode-hook
		(lambda ()
		  (origami-mode)
		  (setq-local lsp-ui-sideline-show-code-actions nil)))

  ;; super important -- eldoc support adds things like type signatures in modeline
  ;; but I think LSP might provide this functionality, too? we'll see..
  ;; (use-package go-eldoc)
  ;; (add-hook 'go-mode-hook 'go-eldoc-setup)

  ;; sets the visual tab width to 2 spaces per tab in Go buffers
  (add-hook 'go-mode-hook (lambda ()
				(set (make-local-variable 'tab-width) 2)))
  ;; gofmt before save
  (add-hook 'before-save-hook 'gofmt-before-save)

  (load-file "~/.emacs.d/vendor/go-dlv.el")
  (require 'go-dlv)
  )


;; go
```


### Javascript

```emacs-lisp
;; Javascript / React config

(defun javascript ()
  ;; React JSX mode for .jsx files and component/*.js files
  (use-package rjsx-mode
	:hook ((rjsx-mode . lsp-deferred))
	:config
	(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
	:init
	(add-hook 'javascript-mode-hook #'lsp)))
```


### Web

```emacs-lisp
(defun web ()
  (use-package web-mode
	:mode (("\\.html$" . web-mode)
	   ("\\.css$"  . web-mode))
	:config
	(setq web-mode-enable-css-colorization t)
	(setq web-mode-enable-auto-pairing t))

  ;; web-mode can provide syntax highlighting for many template
  ;; engines, but it can't detect the right one if the template uses a generic ending.
  ;; If a project uses a generic ending for its templates, such
  ;; as .html, add it below. It would be more elegant to handle this by
  ;; setting this variable in .dir-locals.el for each project,
  ;; unfortunately due to this https://github.com/fxbois//issues/799 that
  ;; is not possible :(

  (setq web-mode-engines-alist '(
	  ("go" . ".*foo.party/.*\\.html\\'")
	  ;; add more projects here..
	  ))
  )
```


### Post-Config

Any config that needs to run after languages are loaded should go here.

```emacs-lisp
(defun post-config ()
  (use-package adaptive-wrap
	:config
	(setq-default adaptive-wrap-extra-indent 2)

	(defun adaptive-and-visual-line-mode (hook)
	  (add-hook hook (lambda ()
			(progn
			  (visual-line-mode)
			  (adaptive-wrap-prefix-mode)))))

	(mapc 'adaptive-and-visual-line-mode (list
					  'markdown-mode
					  'go-mode-hook
					  'js2-mode-hook
					  'yaml-mode-hook
					  'rjsx-mode-hook))
	(add-hook 'compilation-mode-hook
		  #'adaptive-wrap-prefix-mode)
	)

  ;; sane tab-width
  ;; I mean seriously Emacs, 8??
  ;; (setq tab-width 2)

  )
```


### Enable modes

```emacs-lisp
(setup-lsp)
(go)
(python)
(docker)
(javascript)
(web)
(post-config))
```


<a id="org5c70761"></a>

## Global Environment Configuration


### Misc Configuration

```emacs-lisp
(defun config ()
  "Global configuration variables and such. Global functions with keybindings must go here."

  ;; Automatically calls disable-theme on the current theme before
  ;; loading a new theme! Allows easy theme switching with just M-x load-theme
  ;; Thanks to https://www.simplify.ba/articles/2016/02/13/loading-and-unloading-emacs-themes/
  (defun load-theme--disable-old-theme(theme &rest args)
	"Disable current theme before loading new one."
	(mapcar #'disable-theme custom-enabled-themes))
  (advice-add 'load-theme :before #'load-theme--disable-old-theme)


  ;; I definitely lifted this from somewhere but failed to document where I got it :\
  ;; Probably from Spacemacs. Thanks, Spacemacs.
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

  ;; helper functions for keybindings
  ;; this one lifted from https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
  ;; TODO make this behave like alt-tab in Windows, but for buffers
  (defun er-switch-to-previous-buffer ()
	"Switch to previously open buffer. Repeated invocations toggle between the two most recently open buffers."
	(interactive)
	(switch-to-buffer (other-buffer (current-buffer) 1)))

  ;; override Home/End behavior to be more like modern applications
  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line)

  ;; configure align-regexp to use spaces instead of tabs
  ;; lifted from StackOverflow
  ;; https://stackoverflow.com/questions/22710040/emacs-align-regexp-with-spaces-instead-of-tabs
  (defadvice align-regexp (around align-regexp-with-spaces activate)
	(let ((indent-tabs-mode nil))
	  ad-do-it))
```


### Global Keybindings

```emacs-lisp
(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix "SPC")

(general-create-definer my-local-leader-def
  ;; :prefix my-local-leader
  :prefix "SPC m")

(general-define-key
 :states 'normal
 "TAB"  'origami-toggle-node

 "J"    'evil-scroll-page-down
 "K"    'evil-scroll-page-up

 "zm"   'origami-toggle-node
 "zM"   'origami-toggle-all-nodes

 "zc"   'origami-close-node
 "zC"   'origami-close-node-recursively

 "zo"   'origami-open-node
 "zO"   'origami-open-node-recursively)

(defun find-initfile ()
  "Open main config file."
  (interactive)
  (find-file "~/.emacs.d/ian.org"))

(defun find-initfile-other-frame ()
  "Open main config file in a new frame."
  (interactive)
  (find-file-other-frame "~/.emacs.d/ian.org"))

(defun reload-initfile ()
  "Reload the main config file."
  (interactive)
  (org-babel-tangle "~/.emacs.d/ian.org")
  (byte-compile-file "~/.emacs.d/ian.el"))

(defun close-client-frame ()
  "Exit emacsclient."
  (interactive)
  (server-edit "Done"))

;; global keybindings
(my-leader-def 'normal 'override
  "bb"     'helm-mini
  "TAB"    #'switch-to-prev-buffer
  "br"     'revert-buffer
  "bd"     'evil-delete-buffer
  "cc"     'projectile-compile-project
  "ec"     'flycheck-clear
  "el"     'flycheck-list-errors
  "en"     'flycheck-next-error
  "ep"     'flycheck-previous-error
  "Fm"     'make-frame
  "Fd"     'delete-frame
  "ff"     'helm-find-files
  "fr"     'helm-recentf
  "fed"    'find-initfile
  "feD"    'find-initfile-other-frame
  "feR"    'reload-initfile
  "gb"     'magit-blame
  "gs"     'magit-status
  "gg"     'magit
  "gd"     'magit-diff
  "hy"     'hyperbole
  "hu"     'easy-hugo
  "hn"     'easy-hugo-newpost
  "hp"     'easy-hugo-preview
  "jj"     'bookmark-jump
  "js"     'bookmark-set
  "jo"     'org-babel-tangle-jump-to-org
  "ic"     'insert-char
  "p"      'projectile-command-map
  "pf"     'helm-projectile-find-file
  "p!"     'projectile-run-async-shell-command-in-root
  "si"     'yas-insert-snippet
  "sn"     'yas-new-snippet
  "sp"     'helm-projectile-ack
  "qq"     'save-buffers-kill-terminal
  "qr"     'restart-emacs
  "tn"     'linum-mode
  "tt"     'toggle-transparency
  "tr"     'treemacs
  "ta"     'treemacs-add-project-to-workspace
  "w-"     'split-window-below
  "w/"     'split-window-right
  "wj"     'evil-window-down
  "wk"     'evil-window-up
  "wh"     'evil-window-left
  "wl"     'evil-window-right
  "wd"     'delete-window
  "wD"     'delete-other-windows
  "wo"     'other-window
  "w="     'balance-windows
  "SPC"    'helm-M-x
  )
```


### Org Mode Settings

```emacs-lisp
;; some default evil bindings
(use-package evil-org)
;; image drag-and-drop for org-mode
(use-package org-download)


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

(my-local-leader-def
  :states 'normal
  :keymaps 'org-mode-map
  "y" 'org-store-link
  "p" 'org-insert-link
  "x" 'org-babel-execute-src-block
  "e" 'org-edit-src-code)

;; github-flavored markdown
(use-package ox-gfm)

;; enable markdown export
(eval-after-load "org"
  (progn
	'(require 'ox-md nil t)
	'(require 'ox-gfm nil t)))
```


### Hostname-based Tweaks

Looks for Org files in `/home/$USER/.emacs.d/local/` with a name that is the same as the hostname of the machine. I don't know what this does if you try to run Emacs in Windows because I don't do that, but on Mac and Linux it shells out to call `hostname` to determine the hostname. Then Emacs tangles that .org file to a .el file and executes it, allowing configuration to diverge to meet needs that are unique to a specific workstation. This would be a neat feature to expand on at some point.

```emacs-lisp
;; simplifies setting a font and changing it immediately
(defun set-font (font)
  (set-face-attribute 'default nil :font font )
  (set-frame-font font nil t))

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


### Misc Settings

```emacs-lisp
;; backups to /tmp
(setq backup-directory-alist `(("." . "/tmp/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t)

;; autosave
(setq auto-save-visited-interval 300)
(auto-save-visited-mode
 :diminish
 )

;; set default window size
(add-to-list 'default-frame-alist '(width . 128))
(add-to-list 'default-frame-alist '(height . 60))

;; hide some modes that are everywhere
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

;; easily take gifs (if byzanz-record is available.. only works in X11)
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


### ERC

I don't like to check in my IRC nicks into this file, so I've utilized `/home/$USER/.authinfo` which is apparently a GNU standard. The format for this file follows this pattern:

`machine HOSTNAME login USER password PASSWORD port PORTNUMBER`

So we can use `sed` and `grep` which are available on all of my machines to look up the nick, and then once we have the nick, Emacs will get the rest automatically. Then I manage `.authinfo` manually on each machine.

I wrote a quick Bash one-liner to extract the login. Maybe I could've done this in elisp but Emacs makes it easy enough to mix languages, so here is the one-liner, which will tangle out to `get-nick.bash`:

```bash
cat ~/.authinfo | grep "$1" | sed 's/.*login \([^ ]\+ \).*/\1/g'
```

Then configure Emacs to use this to find the nick (and put in place the rest of the configuration that I would like for ERC):

```emacs-lisp
(defun irc ()
  "Connect to IRC."
  (interactive)

  (add-hook 'erc-mode-hook 'variable-pitch-mode)
  (add-hook 'erc-mode-hook 'visual-line-mode)

  (let ((
	 nick  (s-trim (shell-command-to-string
			"~/.emacs.d/get-nick.bash freenode"))
	 ))

	(erc-tls
	 ;; these days I only use Freenode
	 :server "irc.freenode.net"
	 :port 6697
	 :nick nick)
	)

  ;; if this list gets longer I probably don't want to keep it in public
  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" )))
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-rename-buffers t)

  )
```


<a id="org73b4e95"></a>

## Render this file for display on Github

This function registers a hook that will export this file to Github flavored Markdown and copy that to README.md so that this file is always the one that appears on the Github home page, but in the correct format and everything.

```emacs-lisp
(add-hook 'after-save-hook
	  '(lambda ()
		 (when (string=
			(file-name-nondirectory (buffer-file-name))
			"ian.org")
		   (org-gfm-export-to-markdown)
		   (if (find-buffer-visiting "~/.emacs.d/README.md")
		   (kill-buffer-ask (find-buffer-visiting "~/.emacs.d/README.md")))
		   (delete-file "README.md" t)
		   (rename-file "ian.md" "README.md")
		   )))
```


<a id="org992fb82"></a>

## Run Stuff

Main is called in `init.el` and runs the rest of of the config.

```emacs-lisp

(defun main()
  "Initialize everything!"
  (bootstrap)
  (global-packages)
  (languages)
  (config)
  ;;(publish)
  (server-start))

(provide '~/.emacs.d/ian.el)
;;; ian.el ends here
```


<a id="orge3de7a0"></a>

# Notes and Such

Miscellaneous stuff related to the config but not ready to be integrated, or just links, commentary, etc


<a id="org8b6f3d0"></a>

## DONE System-local settings

Include all `.el` files from the untracked folder `local-variables/` and run them as the final step. This allows for customization at the end of the configuration for specific things that are dependent on the computer on which this config is being run. For instance, anything with sensitive details or URLs can be symlinked from a private repo to this one for inclusion in the config without sharing secrets with the whole Internet.

1.  Ensure that `local-variables/` exists and create it if it does not.
2.  Load anything that's in there &#x2013; be sure to fail sanely if there's nothing there!
3.  That's it, there is no three.


<a id="org563e2d3"></a>

## DONE Hyperbole

```
17:41 user1: is there a way to do the equivalent of C-x C-e on a #+INCLUDE: directive in Org?
17:46 user2: Of course: C-a C-c ' C-x h M-w M-x org-mark-ring-goto C-y C-k
17:51 user1: I could probably transform that string of commands into a Lisp function.. and then write an implicit button rule for Hyperbole so that I can shift+middle-click on an #+INCLUDE: directive and have it drop the contents of the file inside my org file..
17:52 user1: that'd be the correct behavior
```


<a id="orgbc8b182"></a>

## DONE Monospace Fonts

Just going to keep note of some options


### <https://github.com/adobe-fonts/source-code-pro/tree/master>

Default in Spacemacs


### <https://github.com/be5invis/Iosevka>

Kinda tall, skinny


### <https://github.com/googlefonts/Inconsolata>

Has ligatures


### <https://github.com/tonsky/FiraCode>

More ligatures, but you have to Do Stuff in Emacs <https://github.com/tonsky/FiraCode/wiki/Emacs-instructions> Described as "cool" on IRC


### <https://github.com/source-foundry/Hack>

I mean, it's called "Hack"


<a id="org173aae2"></a>

## Proportional Fonts

I don't want proportional fonts everywhere, but it'd be nice to have them in writing-focused modes like Org!


<a id="org92bd316"></a>

## Authentication and Secrets in Emacs

Just stumbled on the use of `~/.authinfo.gpg` files with Emacs for storing secrets. Should probably learn how to do this (I bet it is super simple) because it will allow me to store configuration that relies on secrets more easily.

<https://www.emacswiki.org/emacs/GnusAuthinfo>


<a id="orgaf81db4"></a>

## Packages to Try

These are some things I have heard about and maybe have partially integrated, but haven't had the time for anything serious


### emmet-mode

Emmet is the "zen coding" plugin for really fast HTML authoring <https://github.com/smihica/emmet-mode>


### yasnippet-snippets

Some default snippets &#x2013; don't install until we're ready to figure out how to use them <https://github.com/AndreaCrotti/yasnippet-snippets>
