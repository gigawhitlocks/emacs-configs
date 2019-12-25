- [Ian's Custom Emacs Environment with Associated Notes and Commentary](#orgd3d409b)
  - [Justification](#org8c283bb)
	- [It's actually an interactive Lisp interpreter](#org03e8cf2)
	- [Org Mode](#orge6fb00b)
- [Entrypoint](#orga47429d)
- [My Environment](#org3361990)
  - [Bootstrap](#orgc337423)
  - [Package Installation and Configuration](#orgf4f36bc)
	- [Install and Configure Treemacs](#org37f1d96)
	- [Emoji 🙏](#org8bb5ab2)
	- [Configure Recent File Tracking](#orgd64abeb)
	- [Install and Configure Projectile](#org7969121)
	- [Install and Configure Evil Mode](#org90f34b3)
	- [Install and Configure General](#orgb82de7a)
	- [Install and Configure Magit](#orge211b7f)
	- [Install and Configure `which-key`](#org76d40fa)
	- [Colorize ANSI colors in `*compilation*`](#org574ec29)
	- [Install and Configure Aesthetic Dashboard](#orge473558)
	- [Install and Configure Blogging Tools](#org57ba1aa)
	- [YASnippet](#orgb12fa56)
	- [Extra Packages](#org5573a2e)
	- [Install and Configure Company for Auto-Completion](#orgb09383f)
	- [Install and Configure Flycheck for Linting](#orgcf87fb7)
	- [Install and Configure Helm for Command and Control](#org55fd79f)
	- [Install and Configure GNU Hyperbole](#orgf96e08c)
	- [Run everything that is in a function](#org2f48c76)
  - [Language Configuration](#org05e8a03)
	- [General](#org7e629f9)
	- [YAML](#org4d3109e)
	- [Markdown](#orgc5ab2f8)
	- [Docker](#org069ed1d)
	- [Python](#org3922d21)
	- [Go](#org655cc6a)
	- [Javascript](#orgf1579a9)
	- [Web](#orgb729d4f)
  - [Post-Config](#orgabd765e)
	- [Adaptive Wrap and Visual Line Mode](#orga221efd)
	- [Enable modes](#org231fb35)
  - [Global Environment Configuration](#org771d347)
	- [Misc Configuration](#orgfad7b1b)
	- [Global Keybindings](#org540d8a6)
	- [Org Mode Settings](#org274108a)
	- [Hostname-based tweaks](#orgd27f56b)
	- [Configure automatic backups/recovery files](#orgaece907)
	- [Clean whitespace on save in all modes](#org5fe68e3)
	- [Autosave](#org1a61ad6)
	- [Default window size](#org0ab8bf6)
	- [Unclutter global modeline](#org94762df)
	- [Less annoying bell](#org8c52afa)
	- [Easily create gifs of current Emacs frames](#org97bb285)
	- [Remove toolbar and menu](#org41f6a15)
  - [ERC (IRC config)](#org5ad23b3)
  - [Render this file for display on Github](#orgd41ada9)
  - [Run Stuff](#orga444b53)
- [Notes and Such](#orgfb76e43)
  - [Monospace Fonts](#orgb5eaab0)
	- [<https://github.com/adobe-fonts/source-code-pro/tree/master>](#org1a59e7d)
	- [<https://github.com/be5invis/Iosevka>](#org98accb7)
	- [<https://github.com/googlefonts/Inconsolata>](#orge206077)
	- [<https://github.com/tonsky/FiraCode>](#orga9ecff4)
	- [<https://github.com/source-foundry/Hack>](#orgb09ad06)
  - [Proportional Fonts](#org5484c70)
  - [Authentication and Secrets in Emacs](#org390a4b0)
  - [Packages to Try](#org408cbd5)
	- [emmet-mode](#orgda545c1)
	- [yasnippet-snippets](#orgb3f843c)



<a id="orgd3d409b"></a>

# Ian's Custom Emacs Environment with Associated Notes and Commentary

This file contains the configuration necessary to transform a GNU Emacs 26 installation (only tested on Linux) into my very own personalized, hand-crafted, artisanal programming and text-editing environment maintained for my own use, hopefully in a way that can remain maintainable for the ongoing future.

There are many configurations like this one, but this one is mine.


<a id="org8c283bb"></a>

## Justification

Emacs is an abberation among modern software, and it is my favorite piece of software. It's actually ancient software, but it is still actively maintained and has an excited and crafty userbase as a community. It's a text editor on the surface, and certainly Emacs can be used fairly quickly (after following the tutorial) for basic text editing, and perhaps its author even thought of it as merely a text editor. But..


<a id="org03e8cf2"></a>

### It's actually an interactive Lisp interpreter

It's really a text editor with a Lisp interpreter attached, and Lisp turns out to be especially good at parsing text and for building domain specific languages (DSLs) for adding functionality to the existing language. Now, it just so happens that this particular dialect of Lisp comes with a native GUI library &#x2013; Emacs itself &#x2013; that it understands natively.

Suddenly using the program and programming it become similar tasks. This is the brilliance of Emacs. "But I don't have time to program my editor!" Yeah, nobody really does, that's why this repository is open source (in case someone can use it) and it's why we lean heavily on third party packages.

But because Emacs is primarily written in Emacs Lisp, you can explore the source code of Emacs directly from the editor itself, edit it, and execute the new version and have it affect your current session at runtime.

Here's a trivial example. A new Emacs user does the tutorial and learns that `M-x` runs a command. One of the first ones they learn is `find-file`. `find-file` queries the user for a file location and then opens that file in a new buffer and displays it in the current window. It's also accessible via `C-x C-f` by default. The same function is `(find-file)` in Emacs Lisp. Run with an argument, it won't query the user. Any action you can do interactively can be automated through Emacs Lisp. Because of this flexibility, the world of 3rd party packages is *awesome*.

Oh yeah, and then there's Org Mode. Learning Emacs is worth it just for Org Mode.


<a id="orge6fb00b"></a>

### Org Mode

This configuration is written in Org syntax.

[Org Mode](https://orgmode.org) is a major mode for content written in the Org markup language, which provides syntax for annotating the content similar to Markdown, as well as syntax for attaching metadata to the content so that it can be acted upon from Emacs Lisp or other languages, through an extension called Babel, which is often referred to as `org-babel`. Org and Babel allow you to write prose alongside code and export ("tangle") it to source files or execute it interactively while writing the document and automatically include the results of execution alongside the code and commentary in the document.

Org Mode allows you to easily collapse heading subtrees, execute code blocks, edit code blocks in the native mode of the language present in the code block, and write software in the literate programming style, where there's more explanation and exposition than code, like this configuration.


<a id="orga47429d"></a>

# Entrypoint

This is `init.el`. Using Org for my configuration is a personal choice, so I want Emacs to load this file first when it starts up. By default, Emacs runs `init.el` at the beginning of execution. This piece of code tangles to `init.el`, and `init.el` containing the following must be checked in, because this snippet tangles *this* file and executes `main`, so ****it is this piece of code that starts the whole process of loading all of this configuration****.

`init.el` just holds the bare minimum code so that the bulk of the configuration can be checked in once, inside this file, rather than twice like the contents of `init.el`. I'm using an [example from orgmode.org](https://orgmode.org/worg/org-contrib/babel/intro.html#literate-emacs-init) to load the Org files and tangle them, then `require` the output of this file from the call to tangle, run `main`, and I'm done.

****NOTE**** The filename `ian.org` is hardcoded in this entrypoint routine in the place of the main configuration file. This is because, despite the public nature of my config, it is not intended to be used in whole by anyone but me. This allows me certain shortcuts, like hostname-specific configuration, and convention-over-configuration in ways I find intuitive without overly detailed documentation. It is <span class="underline">my</span> config, after all, so my main config file is `ian.org`.

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


<a id="org3361990"></a>

# My Environment

The point of Emacs, and the reason every Emacs user should write his or her own configuration, is to build your own environment so that it behaves how you want it to.

This may seem to be a lot of work, and it is. But if a serious guitar player might have a custom guitar built, I see no reason I shouldn't use a custom environment for my craft.

**This is where my environment definition begins**

After running the `init.el` entrypoint, this file is tangled to `ian.el` and executed. Right now all configuration other than the entrypoint is in this file.


<a id="orgc337423"></a>

## Bootstrap

The first thing that must be done is to prepare to manage third party packages, because my config is built on top of the work of many third party packages. I like to install and manage all of the packages I use as part of my configuration so that it can be duplicated across computers (more or less) and managed with `git`, so I use `use-package` to ensure that packages are installed from my configuration file.

Bootstrap sets up the ELPA, Melpa, and Org Mode repositories, sets up the package manager, installs [`use-package`](https://github.com/jwiegley/use-package) if it is not found, configures `use-package` and installs a few extra packages that acoutrement `use-package` and will be used heavily throughout.

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

Once this is done I need to install and configure any third party packages that are used in many modes throughout Emacs. Some of these modes fundamentally change the Emacs experience and need to be present before everything can be configured.


<a id="orgf4f36bc"></a>

## Package Installation and Configuration

The `global-packages` function is responsible for installing packages with a large effect and which other packages are likely to depend. These are packages essential to my workflow. Configuration here should be config that must run early, before variables are set or language-related packages, which will likely rely on these being set.

```emacs-lisp
(defun global-packages ()
  "Install and configure packages used with many modes and standalone modes and applications."
```


<a id="org37f1d96"></a>

### Install and Configure Treemacs

Treemacs provides a neotree-like file tree on the left hand side of Emacs. I bind it to a global key to make it pop up, and add frequent projects to it manually on each machine. It has become essential to my workflow, especially for flipping between modes. LSP is aware of it, too, which adds some really cool features likes function lists inside. Just wish I could adjust the width.

```emacs-lisp
;; left hand side tree view like neotree
;; nice for exploring smaller projects
(use-package treemacs)
```


<a id="org8bb5ab2"></a>

### Emoji 🙏

Provided by [emojify](https://github.com/iqbalansari/emacs-emojify).

```emacs-lisp
;; 🙌 Emoji! 🙌
(use-package emojify
  :config
  (setq emojify-download-emojis-p t)
  (emojify-set-emoji-styles '(unicode))
  (add-hook 'after-init-hook #'global-emojify-mode))
```


<a id="orgd64abeb"></a>

### Configure Recent File Tracking

Emacs comes with `recentf-mode` which helps me remember what I was doing after I restart my session.

```emacs-lisp
;; recent files mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
```


<a id="org7969121"></a>

### Install and Configure Projectile

[`projectile`](https://projectile.readthedocs.io/en/latest/) is a fantastic package that provides all kinds of project context-aware functions for things like:

-   running grep, but only inside the project
-   compiling the project from the project root without doing anything
-   find files within the project, again without having to do anything extra

It's great, it gets installed early, can't live without it. 💘 `projectile`

```emacs-lisp
(defun setup-projectile ()
  (use-package projectile
	:delight)
  (use-package helm-projectile)
  (use-package treemacs-projectile)
  (projectile-mode +1))
```

Oh yeah and it integrates with Helm and Treemacs, beautiful.


<a id="org90f34b3"></a>

### Install and Configure Evil Mode

[`evil-mode`](https://github.com/emacs-evil/evil) fundamentally changes Emacs so that while editing all of the modes and keybindings from `vim` are present. It's controversial but I think modal editing is brilliant and have been using `vim` bindings since the mid-aughts. No going back.

```emacs-lisp
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
```


<a id="orgb82de7a"></a>

### Install and Configure General

[General](https://github.com/noctuid/general.el) provides more convenient keybindings, especially with `evil-mode`.

```emacs-lisp
(use-package general
  :init
  (setup-evil)
  :config
  (general-evil-setup))
```


<a id="orge211b7f"></a>

### Install and Configure Magit

[Magit](https://github.com/magit/magit) is an incredible integrated `git` UI for Emacs.

```emacs-lisp
(defun setup-magit ()
  (use-package magit)
  ;; disable the default emacs vc because git is all I use,
  ;; for I am a simple man
  (setq vc-handled-backends nil)
  (use-package evil-magit))

;; forge lets us interact with git forges e.g. GitHub, Gogs, Gitlab
(use-package forge
  :after magit)
```


<a id="org76d40fa"></a>

### Install and Configure `which-key`

It can be difficult to to remember and discover all of the available shortcuts in Emacs, so [`which-key`](https://github.com/justbur/emacs-which-key) pops up a special buffer to show you available shortcuts whenever you pause in the middle of a keyboard shortcut for more than a few seconds. It's really lovely.

```emacs-lisp
(use-package which-key
  :delight
  :init
  (which-key-mode)
  (which-key-setup-minibuffer))

;; customizations to compilation mode
```


<a id="org574ec29"></a>

### Colorize ANSI colors in `*compilation*`

If you run a command through `M-x compile` by default Emacs prints ANSI codes literally, but a lot of tools use these for colors and this makes it so Emacs shows colors in the `*compilation*` buffer.

```emacs-lisp
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
```


<a id="orge473558"></a>

### Install and Configure Aesthetic Dashboard

Following Spacemacs' style, uses the [`emacs-dashboard`](https://github.com/emacs-dashboard/emacs-dashboard) project and [`all-the-icons`](https://github.com/domtronn/all-the-icons.el) to provide an aesthetically pleasing splash screen with useful links to recently used files on launch.

```emacs-lisp
(defun dashboard ()
  ;; first disable the default startup screen
  (setq inhibit-startup-screen t)
  (use-package all-the-icons)
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
  )
```


<a id="org57ba1aa"></a>

### Install and Configure Blogging Tools

Easy Hugo is an interactive UI for managing a Hugo site.

```emacs-lisp
(use-package easy-hugo
  :init
  (setup-evil)
  :config
  (setq easy-hugo-basedir "~/keming.org")
  (add-to-list 'evil-emacs-state-modes 'easy-hugo-mode))
```


<a id="orgb12fa56"></a>

### YASnippet

Snippets are really cool and allow fast insertion of boilerplate using templates. I've been meaning to use this more. [Here are the YASnippet docs.](https://www.emacswiki.org/emacs/Yasnippet)

```emacs-lisp
(use-package yasnippet
  :delight
  :config
  (use-package yasnippet-snippets))
```


<a id="org5573a2e"></a>

### Extra Packages

Packages with a smaller effect on the experience.

```emacs-lisp
  ;; anything so trivial that there is no config necessary goes here
(defun extra-packages ()
```

1.  git-gutter shows unstaged changes in the gutter

	```emacs-lisp
	(use-package git-gutter
		:delight
		:config
		(global-git-gutter-mode +1))
	  ;; git-gutter does not play nicely with linum-mode
	  ;; investigate long-term solution?
	```

2.  a config setting highlighting of the current line

	I like to highlight the current line so that it is easy to identify where my cursor is.

	```emacs-lisp
	(global-hl-line-mode)
	(setq global-hl-line-sticky-flag t)
	```

3.  Rainbow delimiters make it easier to identify matching parentheses

	```emacs-lisp
	;; set up rainbow delimiters for Elisp
	(use-package rainbow-delimiters
	  :config
	  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
	  )
	```

4.  restart-emacs does what it says on the tin

	```emacs-lisp
	(use-package restart-emacs)
	```

5.  s is a string manipulation utility

	I use this for a trim() function far down below. I think it gets pulled in as a dependency anyway, but in any case it provides a bunch of helper functions and stuff. [Docs are here.](https://github.com/magnars/s.el)

	```emacs-lisp
	(use-package s)
	```

6.  a systemd file mode

	```emacs-lisp
	(use-package systemd)
	```

7.  ranger is a standalone file browser

	```emacs-lisp
	(use-package ranger
	  :config
	  (setq ranger-show-literal nil))
	```

8.  I don't remember what htmlize is or does

	```emacs-lisp
	(use-package htmlize)
	```

9.  centered-window centers the text in a column

	Seems to be similar to `olivetti-mode` and might be what was interfering with that.

	```emacs-lisp
	(use-package centered-window)
	)
	```


<a id="orgb09383f"></a>

### Install and Configure Company for Auto-Completion

Great tab-complete and auto-complete with [Company Mode](https://github.com/company-mode/company-mode).

```emacs-lisp
;; auto-completion
(use-package company
  :delight
  :config
  ;; enable it everywhere
  (add-hook 'after-init-hook 'global-company-mode)

  ;; tab complete!
  (global-set-key "\t" 'company-complete-common))
```


<a id="orgcf87fb7"></a>

### Install and Configure Flycheck for Linting

Flycheck is an on-the-fly checker that hooks into most language backends.

```emacs-lisp
;; linter
(use-package flycheck
  :delight
  ;; enable it everywhere
  :init (global-flycheck-mode))
```


<a id="org55fd79f"></a>

### Install and Configure Helm for Command and Control

Helm is a full-featured command and control package that fundamentally alters a number of core Emacs functions, including what appears when you press `M-x` (with the way I have it configured, anyway).

```emacs-lisp
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

```


<a id="orgf96e08c"></a>

### Install and Configure GNU Hyperbole

[GNU Hyperbole](https://www.gnu.org/software/hyperbole/) adds a bunch of window control features, namely the ability to swap two windows by hitting Shift + Right Click and dragging the window to a new position!

It adds a slew of other features as well &#x2013; hyperlinks between documents, a rolodex, a list builder. I don't use it all, but the parts I use are pretty critical.

```emacs-lisp
;; gnu hyperbole
(use-package hyperbole)
```


<a id="org2f48c76"></a>

### Run everything that is in a function

I think I intended for this to be more modular but really I should remove these functions and separate things with Org instead..

1.  TODO refactor and remove this

	```emacs-lisp
	(setup-projectile)
	(setup-magit)
	(setup-helm)
	(dashboard)
	(extra-packages))
	```


<a id="org05e8a03"></a>

## Language Configuration


<a id="org7e629f9"></a>

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


<a id="org4d3109e"></a>

### YAML

```emacs-lisp
(use-package yaml-mode)
```


<a id="orgc5ab2f8"></a>

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


<a id="org069ed1d"></a>

### Docker

```emacs-lisp
(defun docker ()
  (use-package dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))
```


<a id="org3922d21"></a>

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


<a id="org655cc6a"></a>

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


<a id="orgf1579a9"></a>

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


<a id="orgb729d4f"></a>

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


<a id="orgabd765e"></a>

## Post-Config

Any config that needs to run after languages are loaded should go here.

```emacs-lisp
(defun post-config ()
  (use-package adaptive-wrap
```


<a id="orga221efd"></a>

### Adaptive Wrap and Visual Line Mode

Here I've done some black magic fuckery for a few modes. Heathens in modern languages and also some other prose modes don't wrap their long lines at 80 characters like God intended so instead of using visual-column-mode which I think does something similar but probably would've been easier, I've defined an abomination of a combination of `visual-line-mode` (built-in) and [`adaptive-wrap-prefix-mode`](https://elpa.gnu.org/packages/adaptive-wrap.html) to ****dynamically (visually) wrap and indent long lines in languages like Go with no line length limit**** so they look nice on my screen at any window width and don't change the underlying file — and it's actually pretty cool.

```emacs-lisp
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
)
```


<a id="org231fb35"></a>

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


<a id="org771d347"></a>

## Global Environment Configuration


<a id="orgfad7b1b"></a>

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


<a id="org540d8a6"></a>

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
  "h"     'hyperbole
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


<a id="org274108a"></a>

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


<a id="orgd27f56b"></a>

### Hostname-based tweaks

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

There must be an Org file in `local/` named `$(hostname).org` or init actually breaks. This isn't great but for now I've just been making a copy of one of the existing files whenever I start on a new machine.


<a id="orgaece907"></a>

### Configure automatic backups/recovery files

I don't like how Emacs puts temp files in the same directory as the file, as this litters the current working directory and makes git branches dirty. These are some tweaks to store those files in `/tmp`.

```emacs-lisp
(setq backup-directory-alist `(("." . "/tmp/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t)
```


<a id="org5fe68e3"></a>

### Clean whitespace on save in all modes

```emacs-lisp
(add-hook 'before-save-hook 'whitespace-cleanup)
```


<a id="org1a61ad6"></a>

### Autosave

Automatically saves the file when it's been idle for 5 minutes.

```emacs-lisp
;; autosave
(setq auto-save-visited-interval 300)
(auto-save-visited-mode
 :diminish
 )
```


<a id="org0ab8bf6"></a>

### Default window size

Just a bigger size that I prefer..

```emacs-lisp
(add-to-list 'default-frame-alist '(width . 128))
(add-to-list 'default-frame-alist '(height . 60))
```


<a id="org94762df"></a>

### Unclutter global modeline

Some global minor modes put themselves in the modeline and it gets noisy, so remove them from the modeline.

```emacs-lisp
;; hide some modes that are everywhere
(diminish 'eldoc-mode)
(diminish 'undo-tree-mode)
(diminish 'auto-revert-mode)
```


<a id="org8c52afa"></a>

### Less annoying bell

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


<a id="org97bb285"></a>

### Easily create gifs of current Emacs frames

Figures out the frame size and passes it to `byzanz-record`. Only works if `byzanz-record` is installed (it's in the repos in most distros) and only works in X11.

```emacs-lisp
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
```


<a id="org41f6a15"></a>

### Remove toolbar and menu

Removes the toolbar and menu bar (file menu, etc) in Emacs because I just use `M-x` for everything.

```emacs-lisp
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(scroll-bar-mode -1))
```


<a id="org5ad23b3"></a>

## ERC (IRC config)

I don't like to check in my IRC nicks into this file, so I've utilized `/home/$USER/.authinfo` which is apparently a GNU standard. The format for this file follows this pattern:

`machine HOSTNAME login USER password PASSWORD port PORTNUMBER`

So we can use `sed` and `grep` which are available on all of my machines to look up the nick, and then once we have the nick, Emacs will get the rest automatically. Then I manage `.authinfo` manually on each machine.

I wrote a quick Bash one-liner to extract the login. Maybe I could've done this in elisp but Emacs makes it easy enough to mix languages, so here is the one-liner, which will tangle out to `get-nick.bash`:

```bash
grep "$1" ~/.authinfo | sed 's/.*login \([^ ]\+ \).*/\1/g'
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


<a id="orgd41ada9"></a>

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


<a id="orga444b53"></a>

## Run Stuff

Main is called in `init.el` and runs the rest of of the config. No configuration should be added after this.

```emacs-lisp

(defun main()
  "Initialize everything!"
  (bootstrap)
  (global-packages)
  (languages)
  (config)
  (server-start))

(provide '~/.emacs.d/ian.el)
;;; ian.el ends here
```


<a id="orgfb76e43"></a>

# Notes and Such

Miscellaneous stuff related to the config but not ready to be integrated, or just links, commentary, etc


<a id="orgb5eaab0"></a>

## Monospace Fonts

Just going to keep note of some options


<a id="org1a59e7d"></a>

### <https://github.com/adobe-fonts/source-code-pro/tree/master>

Default in Spacemacs


<a id="org98accb7"></a>

### <https://github.com/be5invis/Iosevka>

Kinda tall, skinny


<a id="orge206077"></a>

### <https://github.com/googlefonts/Inconsolata>

Has ligatures


<a id="orga9ecff4"></a>

### <https://github.com/tonsky/FiraCode>

More ligatures, but you have to Do Stuff in Emacs <https://github.com/tonsky/FiraCode/wiki/Emacs-instructions> Described as "cool" on IRC


<a id="orgb09ad06"></a>

### <https://github.com/source-foundry/Hack>

I mean, it's called "Hack"


<a id="org5484c70"></a>

## Proportional Fonts

I don't want proportional fonts everywhere, but it'd be nice to have them in writing-focused modes like Org!


<a id="org390a4b0"></a>

## Authentication and Secrets in Emacs

Just stumbled on the use of `~/.authinfo.gpg` files with Emacs for storing secrets. Should probably learn how to do this (I bet it is super simple) because it will allow me to store configuration that relies on secrets more easily.

<https://www.emacswiki.org/emacs/GnusAuthinfo>


<a id="org408cbd5"></a>

## Packages to Try

These are some things I have heard about and maybe have partially integrated, but haven't had the time for anything serious


<a id="orgda545c1"></a>

### emmet-mode

Emmet is the "zen coding" plugin for really fast HTML authoring <https://github.com/smihica/emmet-mode>


<a id="orgb3f843c"></a>

### yasnippet-snippets

Some default snippets &#x2013; don't install until we're ready to figure out how to use them <https://github.com/AndreaCrotti/yasnippet-snippets>
