# Prologue

This is my Emacs configuration. It is written in [Org Mode](https://orgmode.org/) format, which means that I can display a static representation here, but the [source repository](https://github.com/gigawhitlocks/emacs-configs) and document ([plain text view](https://raw.githubusercontent.com/gigawhitlocks/emacs-configs/refs/heads/master/readme.org)), are interactive when opened in Emacs.

It follows the concept of "[literate programming](https://en.wikipedia.org/wiki/Literate_programming)" and both defines my Emacs configuration (as well as a few other, related things) and includes my notes about why I made those changes, and what I was doing at the time, as well as whatever other commentary I felt like including at the time (related or otherwise).

At least, that's the goal. In reality, it's a messy living document that I use to configure Emacs and to keep track of what I've done. I don't always take the best of notes, but it is sufficient for me to keep moving forward. If you search around, you may find ideas and code that you can repurpose for your own uses.


# Entrypoint

First I need to configure Emacs to load this file (`readme.org`) as its first action when it starts up. By default, Emacs runs `init.el` at the beginning of execution. The following piece of code [tangles](https://orgmode.org/manual/Extracting-source-code.html) to `init.el`, and `init.el` containing the following must be checked in, because this snippet tangles *this* file (`readme.org`), so ****it is this piece of code that starts the whole process of loading all of this configuration****.

I'm using an [example from orgmode.org](https://orgmode.org/worg/org-contrib/babel/intro.html#literate-emacs-init) to load the Org files and tangle them, then `require` the output of this file from the call to tangle, run `main`, and I'm done.

```emacs-lisp
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
```

The rest of the code that is executed begins with the routines defined by this file.


# Package Manager Bootstrap

After tangling the source files and loading `init.el`, the first thing that must be done is to prepare to manage third party packages, because my config is built on top of the work of many third party packages. I like to install and manage all of the packages I use as part of my configuration so that it can be duplicated across computers (more or less) and managed with `git`, so I use `use-package` to ensure that packages are installed from my configuration file.

Bootstrap sets up the ELPA, Melpa, and Org Mode repositories, sets up the package manager, configures `use-package` and installs a few extra packages that acoutrement `use-package` and will be used heavily throughout. It used to install `use-package` itself, however, it has since been upstreamed and that step has been removed. 🎉

```emacs-lisp
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
```

Once this is done I need to install and configure any third party packages that are used in many modes throughout Emacs. Some of these modes fundamentally change the Emacs experience and need to be present before everything can be configured.


# Fundamental Package Installation and Configuration

First I need to install packages with a large effect and on which other packages are likely to depend. These are packages essential to my workflow. Configuration here should be config that must run early, before variables are set or language-related packages, which will likely rely on these being set.


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

Treemacs provides a file browser on the left hand side of Emacs that I have grown to really like. It's great for exploring unfamiliar projects and modules.

It's installed early because many things have integrations with it, including some themes.

```emacs-lisp
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


### Default theme

Prefer to load a theme per-system, but it's nice to have it documented here. Add a line like the following to the appropriate file in `local/`

```emacs-lisp
;;  (load-theme 'ef-reverie)
```


### Theme lists

-   Light themes

    ```emacs-lisp
    (defvar light-theme-list '(doom-one-light
                               doom-acario-light
                               doom-ayu-light
                               doom-bluloco-light
                               doom-earl-grey
                               doom-feather-light
                               doom-flatwhite
                               doom-gruvbox-light
                               doom-homage-white
                               doom-material
                               doom-opera-light
                               doom-gruvbox-light))
    ```

-   Dark themes

    ```emacs-lisp
    (defvar dark-theme-list '(doom-1337
                              doom-acario-light
                              doom-ayu-dark
                              doom-ayu-mirage
                              doom-badger
                              doom-bluloco-dark
                              doom-Iosvkem
                              doom-challenger-deep
                              doom-city-lights
                              doom-dark+
                              doom-dracula
                              doom-ephemeral
                              doom-fairy-floss
                              doom-feather-dark
                              doom-gruvbox
                              doom-henna
                              doom-homage-black
                              doom-horizon
                              doom-ir-black
                              doom-lantern
                              doom-laserwave
                              doom-manegarm
                              doom-miramare
                              doom-material-dark
                              doom-meltbus
                              doom-molokai
                              doom-monokai-classic
                              doom-monokai-machine
                              doom-monokai-pro
                              doom-monokai-ristretto
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
    ```


### Entrypoint

```emacs-lisp
(defun choose-theme ()
  "Choose a theme interactively using Helm"
  (interactive)
  (let ((theme (choose-theme-impl light-theme-list dark-theme-list)))
    (load-theme theme t)))
```

-   TODO change the name of choose-theme

    the name is too generic and it should be prefixed with something to avoid namespace collisions


## Solaire Mode

Also some visual candy that makes "real" buffers more visible by changing the background color slightly vs e.g. **compilation** or magit buffers

```emacs-lisp
(use-package solaire-mode)

;; treemacs got redefined as a normal window at some point
(push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
(push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist)

(solaire-global-mode +1)
```


## Spacious Padding

More eye candy:

> It increases the padding or spacing of frames and windows on demand. The idea with this package is to provide the means to easily toggle between terse and spacious views, depending on the user’s needs.

Don't know if I'll keep this one but I wanted to try it out

```emacs-lisp
(use-package spacious-padding
  :hook (after-init . spacious-padding-mode))
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

Provided by [emojify](https://github.com/iqbalansari/emacs-emojify). Run `emojify-download-emoji`

```emacs-lisp
;; 🙌 Emoji! 🙌
(use-package emojify
  :config
  (setq emojify-download-emojis-p t)
  (emojify-set-emoji-styles '(unicode))
  (add-hook 'after-init-hook #'global-emojify-mode))
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
  :delight)
(use-package helm-projectile)
(use-package treemacs-projectile)
(projectile-mode +1)
```


## Install and Configure Evil Mode

[`evil-mode`](https://github.com/emacs-evil/evil) fundamentally changes Emacs so that while editing all of the modes and keybindings from `vim` are present. It's controversial but I think modal editing is brilliant and have been using `vim` bindings for twenty-odd years now. No going back.

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
    ;; don't let evil-collection manage go-mode
    ;; it is overriding gd
    (setq evil-collection-mode-list (delq 'go-mode evil-collection-mode-list))
    (evil-collection-init))

  ;; the evil-collection overrides the worktree binding :(
  (general-define-key
   :states 'normal
   :keymaps 'magit-status-mode-map
   "Z" 'magit-worktree)

  ;; I think I unbound or overrode this but I can't figure out where
  (general-define-key
   :states 'normal
   :keymaps 'prog-mode-map
   "gd" 'evil-goto-definition
   )

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
```


## Install and Configure Keybindings Helper

[General](https://github.com/noctuid/general.el) provides more consistent and convenient keybindings, especially with `evil-mode`.

It's mostly used below in the [global keybindings](#Global%20Keybindings) section.

```emacs-lisp
(use-package general
  :init
  (setup-evil)
  :config
  (general-evil-setup))
```


## Install and Configure Helm for Command and Control

[Helm](https://github.com/emacs-helm/helm) is a full-featured command and control package that fundamentally alters a number of core Emacs functions, including what appears when you press `M-x` (with the way I have it configured, anyway).

```emacs-lisp
(use-package helm
  :delight
  :config
  (use-package helm-ag)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (define-key helm-find-files-map "\t" 'helm-execute-persistent-action)
  (setq helm-always-two-windows nil)
  (setq helm-default-display-buffer-functions '(display-buffer-in-side-window))
  (helm-mode 1))
```


## Install and Configure Magit

[Magit](https://github.com/magit/magit) is an incredible integrated `git` UI for Emacs.

```emacs-lisp
(use-package magit)
;; disable the default emacs vc because git is all I use,
;; for I am a simple man
(setq vc-handled-backends nil)
```


### Allow magit to interact with git forges, like Github and Gitlab

```emacs-lisp
(use-package forge
  :after magit)
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
  (which-key-setup-minibuffer))
```


## Set up `pass` for secrets handling

```emacs-lisp
(use-package pass)
```


## Handle "fancy" output in compilation buffer

The external package `fancy-compilation-mode` handles colorization and "clever" use of ANSI to create progress bars and stupid shit like that, which show up in things like npm output and Docker output when BuildKit is set to NORMAL. You can, of course, set the BuildKit output style to PLAIN, but sometimes you're eg editing a file where NORMAL is hard-coded in the Makefile target you want to run when using `compilation-mode` and fighting project defaults isn't what you want to spend your time on.

```emacs-lisp
 (use-package fancy-compilation
  :commands (fancy-compilation-mode))

(with-eval-after-load 'compile
  (fancy-compilation-mode))
```

I don't like how fancy-compilation-mode overrides colors by default, but luckily this can be disabled.

```emacs-lisp
(setq fancy-compilation-override-colors nil)
```


## Scream when compilation is finished

Sometimes when the compile process takes more than a few seconds I change windows and get distracted. This hook plays a file through `aplay` (something else that will break on a non-Linux machine) to notify me that compilation is done. I was looking for something like a kitchen timer but I couldn't find one so right now the vendored sound is the [Wilhelm Scream](https://en.wikipedia.org/wiki/Wilhelm_scream).

```emacs-lisp
(defvar isw-should-play-chime nil)
(setq isw-should-play-chime nil)
(defun isw-play-chime (buffer msg)
  (if (eq isw-should-play-chime t)
      (start-process-shell-command "chime" "*Messages*" "aplay /home/ian/.emacs.d/vendor/chime.wav")))
(add-to-list 'compilation-finish-functions 'isw-play-chime)
```

A function for toggling the screaming on and off. I love scream-when-finished but sometimes I'm listening to music or something and it gets a little ridiculous.

```emacs-lisp
(defun toggle-screaming ()
  (interactive)
  (if (eq isw-should-play-chime t)
      (progn
        (setq isw-should-play-chime nil)
        (message "Screaming disabled."))
    (progn
      (setq isw-should-play-chime t)
      (message "Screaming enabled."))))
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
  :delight
  :config
  (use-package yasnippet-snippets))
```

Enable yas-mode everywhere

```emacs-lisp
(yas-global-mode 1)
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


## Install and Configure Company for Auto-Completion

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

;; icons
(use-package company-box
  :hook (company-mode . company-box-mode))

;; extra documentation when idling
(use-package company-quickhelp)
(company-quickhelp-mode)
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


## Configure Eldoc

```emacs-lisp
(use-package eldoc-box)
(setq eldoc-idle-delay 1.5)

(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)
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
(use-package pc-bufsw)
(pc-bufsw)
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

`auto-virtualenv` looks in `$WORKON_HOME` for virtualenvs, and then I can run `M-x pyvenv-workon RET project RET` to choose my virtualenv for `project`, found in `$WORKON_HOME`, or a symlink anyway.

```emacs-lisp
(use-package auto-virtualenv)
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
(setenv "WORKON_HOME" "~/.virtualenvs")
```

So the convention for use is:

1.  Create a virtualenv as usual for the project
2.  Symlink it inside ~/.virtualenvs
3.  `M-x pyvenv-workon`


## Go

Go is my primary language so it's my most dynamic and complicated configuration, however it degrades gracefully so if not everything is installed, the rest of it still works.


### Dependencies

Go support requires some dependencies. I will try to list them all here. Stuff I have installed has some overlap because of the in-progress move to LSP, but I'll prune it later.

-   First, `go` itself must be installed, install however, and avalailable on the `PATH`.

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


### Package and Configuration for Executing Tests

```emacs-lisp
(use-package gotest)
(advice-add 'go-test-current-project :before #'projectile-save-project-buffers)
(advice-add 'go-test-current-test :before #'projectile-save-project-buffers)
(add-hook 'go-test-mode-hook 'visual-line-mode)
```


### REPL

[Gore](https://github.com/motemen/gore) provides a REPL and [gorepl-mode](https://github.com/manute/gorepl-mode) lets you use it from Emacs. In order to use the REPL from Emacs, you must first install Gore:

```sh
go get -u github.com/motemen/gore/cmd/gore
```

Gore also uses gocode for code completion, so install that (even though Emacs uses go-pls for the same).

```sh
go get -u github.com/mdempsky/gocode
```

Once that's done `gorepl-mode` is ready to be installed:

```emacs-lisp
(use-package gorepl-mode)
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

SQL support is pretty good out of the box but Emacs strangely doesn't indent SQL by default. This package fixes that.

```emacs-lisp
(use-package sql-indent)
```

SQL doesn't &#x2013; as far as I'm aware, and I'm not taking the time to look harder at the moment anyway &#x2013; have an LSP backend (probably doesn't help that there are multiple dialects of SQL so I'd have to find one for PG or SQLite or whatever I'm using that day) so `lsp-find-definition` doesn't work. Below I set `gd` in evil-mode back to the default (`evil-goto-definition`) and add dumb jump as a backend to xref so that it can be used for finding SQL function definitions. Works pretty well but I haven't tested to see if the new hook & the new xref-show-definitions-function values will affect non-SQL modes negatively.

```emacs-lisp

```

Use rainbow delimeters in SQL

```emacs-lisp
(add-hook 'sql-mode-hook #'rainbow-delimiters-mode)
```


## Emacs Lisp

I don't have any custom configuration for Emacs Lisp yet, but I am going to use this space to collect tools and resources that might become useful in the future, and which I may install.


### A collection of development modes and utilities

<https://github.com/p3r7/awesome-elisp>


### editing s-exps

<https://github.com/p3r7/awesome-elisp#lispy> <https://github.com/abo-abo/lispy>


## Racket

Funny the twists of fate that bring us back to where we started. My interest in Emacs stemmed originally from an interest in Racket, and my inability to get vim to format Racket code appropriately. I never did wind up learning Racket, but I guess I might now, for entirely different reasons

```emacs-lisp
(use-package racket-mode) 
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
```


# Org Mode Settings


## Some default evil bindings

```emacs-lisp
(use-package evil-org)
```


## Image drag-and-drop for org-mode

```emacs-lisp
(use-package org-download)
```


## Autocomplete for Org blocks (like source blocks)

```emacs-lisp
(use-package company-org-block) ;; TODO configuration
```


## JIRA support in Org

```emacs-lisp
(use-package ox-jira)
```


## Install some tools for archiving web content into Org

```emacs-lisp
(use-package org-web-tools)
```


## More config I haven't organized

```emacs-lisp
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
```


## Use a variable-pitch font in Org-Mode

Org is mostly prose and prose should be read in a variable-pitch font where possible. This changes fonts in Org to be variable-pitch where it makes sense

```emacs-lisp
(add-hook 'org-mode-hook 'variable-pitch-mode)
```

Inside of code blocks I want a fixed-pitch font

```emacs-lisp
(defun ian-org-fixed-pitch ()
  "Fix fixed pitch text in Org Mode"
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch))

(add-hook 'org-mode-hook 'ian-org-fixed-pitch)
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


## Disable pretty entities

I find superscripts, subscripts, etc, are less common than verbatim underscores and such so I am changing the default for this setting

```emacs-lisp
(setq org-pretty-entities nil)
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

```conf
[Desktop Entry]
Name=org-protocol
Comment=Intercept calls from emacsclient to trigger custom actions
Categories=Other;
Keywords=org-protocol;
Icon=emacs
Type=Application
Exec=org-protocol %u
#Exec=emacsclient -- %u
Terminal=false
StartupWMClass=Emacs
MimeType=x-scheme-handler/org-protocol;
```

After tangling that file to its destination, run the following command to update the database:

    update-desktop-database ~/.local/share/applications/

Add the custom `org-protocol` script to intercept calls from the browser, do any necessary pre-processing, and hand off the corrected input to `emacsclient`:

```bash
# for some reason the bookmarklet strips a colon, so use sed to remove
# the botched prefix and rebuild it correctly
emacsclient -- org-protocol://open-source://$(echo "$@" | sed 's#org-protocol://open-source//##g') | tee /tmp/xdg-emacsclient
# that's probably a useless call to echo but whatever
```

For now this is extremely rudimentary and I will improve it as needed.


### Manual Steps:

1.  The first time, add a button in the browser by creating a bookmarklet containing the following target:

    javascript:location.href='org-protocol://open-source://'+encodeURIComponent(location.href)

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


## Theme Switching Helper

Automatically calls disable-theme on the current theme before loading a new theme! Allows easy theme switching with just `M-x load-theme`.

Thanks to <https://www.simplify.ba/articles/2016/02/13/loading-and-unloading-emacs-themes/>.

```emacs-lisp
(defun load-theme--disable-old-theme (theme &rest args)
  "Disable current theme before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'load-theme--disable-old-theme)
```

Save the current theme to a global variable so it can be referenced later

```emacs-lisp
(defun load-theme--save-new-theme (theme &rest args)
  (setq ian-current-theme theme))
(advice-add 'load-theme :before #'load-theme--save-new-theme)
```

There are a few occasions where the Org fixed-width fonts don't get reapplied correctly. This solves most of them, and eventually I may iterate on it, if the edge cases bother me enough.

```emacs-lisp
(defun ian-restart-org-advice (&rest _args)
  (org-mode-restart))
(advice-add 'load-theme :after #'ian-restart-org-advice)
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


## Smart formatting for many languages

```emacs-lisp
;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
;; (use-package apheleia
;;   :config
;;   (apheleia-global-mode +1))
```


## Add support for browsing Gemini-space

Gemini is a new (circa 2019) Gopher-ish hypertext protocol. Browsing in Emacs is nice.

Install a browser, elpher..

```emacs-lisp
(use-package elpher)
```

And a mode

```emacs-lisp
(use-package gemini-mode)
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
```


## Emacs Everywhere

Sadly this only works in X11 but there's a long Wayland support issue, and it looks like a lot of progress has been made! So hopefully this will get updated to work in Wayland before I upgrade to the next LTS.. whenever I do that, lol.

```emacs-lisp
(use-package emacs-everywhere)
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
                 (shell-command-to-string "hostname"))))

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

```conf
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

Some notes on the dependencies that I found were needed to build Emacs 29.1 on fresh Ubuntu with the configuration flags that I like

```shell
./autogen.sh
sudo apt-get install make autoconf libx11-dev libmagickwand-dev libgtk-3-dev libwebkit2gtk-4.0-dev libgccjit-11-dev libxpm-dev libgif-dev libgnutls28-dev libjansson-dev libncurses-dev texinfo libtree-sitter-dev
./configure --with-imagemagick --with-xwidgets --with-json --with-x-toolkit=gtk3 --with-native-compilation --with-mailutils --with-x --with-tree-sitter --without-toolkit-scroll-bars
```