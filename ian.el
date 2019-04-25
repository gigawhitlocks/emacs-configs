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
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    )

  (defun setup-evil ()
    "Install and configure evil-mode and related bindings."
    (use-package evil
      :init
      (setq evil-want-integration t)
      (setq evil-want-keybinding nil)
      :config
      (evil-mode 1))

    (use-package evil-collection
      :after evil
      :config
      (evil-collection-init))

    ;; add fd as a remap for esc
    (use-package evil-escape)
    (evil-escape-mode 1)
    (setq-default evil-escape-key-sequence "fd"))

  (defun setup-magit ()
    (use-package magit)
    ;; disable the default emacs vc because git is all I use,
    ;; for I am a simple man
    (setq vc-handled-backends nil)
    )

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
    ;; (global-set-key (kbd "<f6>") 'ivy-resume)
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
    ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

  (defun extra-packages ()
    (use-package restart-emacs)
    (use-package leuven-theme)
    (use-package treemacs))

  (use-package flycheck
    :init (global-flycheck-mode))

  (setup-ivy)
  (setup-evil)
  (setup-magit)
  (extra-packages))

(defun languages ()
  "Setup for specific programming languages."

  (defun setup-lsp ()
    "Enable nice rendering of diagnostics like compile errors."
    (use-package lsp-mode
      :init (setq lsp-prefer-flymake nil))

    (use-package lsp-ui)

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
      :hook (scala-mode . lsp))
    )

  (setup-lsp)
  (scala))

(defun config ()
  "Global variables and such."

  ;; disable menu bar and toolbar
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))


  ;; line numbers by default
  (global-linum-mode 1)
  )

(defun main()
  "Initialize everything!"
  (bootstrap)
  (global-packages)
  (languages)
  (config))

(main)

;;; ian.el ends here
;;                                         ......
;;                                .,;;xOkk0NWNNNN0c.
;;                           ...cONWWWWMMMMMMMMMMMWXd:;cxOkol:.
;;                        .cOXNWMMMMWNWMMMMMMMMMMMMMMMWMMMMWNWXl:,.
;;                   ..,lkKNWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWNWWNWNXKOx;
;;               .'cx0KXNNWWMMMMMMMMMMMMMMMMMMMMMMMMMMMMWWMMMMMWNWWNWMWXkoc.
;;              .dkk0XWMWWWWWWNNWMMMMMMMMMMMMMMMMMWWWWWWWNNNNXXKKNWWMMMMMMMNo.
;;          .;cclOKkxx0NWMWNKXNNWMMMMWWWWWWWWWWNXXNNWNX000KK0OOOOXXXWMMMMMMMMNd.
;;         :k0OxkX0kdodk0K0K000KXKKNWWNNKK0O0KKXK00OOkOOkxk0XNWWWMWWMMMMMMMMMMMK'
;;        :kk0KXNWNNX0OKNNX0kdddllodk0KXNWWWNXKXXKOOOKNN0OOKNWMMMMMMMMMMMMWWWWMMo
;;     .coxONWMMWWWWNXXNX0xoodd:.  ..,cox0KNX0XNK0xdkKXX0OkOXNWWMMMMMMMMMMWWMMMM0.
;;     .oOKWMWWWWWWN0ddl'..,cc,.         ..'lkKN0o;,:dkxooooxO00XWMMMMWNWMMMMMMMWO'
;;      .OWMWWWMWXd:'..    .,,.               '::'....',,';ccclldOKNWWNXKNMMMMMMMMK'
;;      :NMMMMMW0c'..                                 .....,;;:cloxO0KXXKKNWWWWMMMWd
;;      cWMMMWXx;,'..                                  ....',;:::loxO00KKKNXXNXNMMWK;
;;    oOKWMMM0l;''...                                 ....'',;:::clokKXXXNMWXXKKNNN0'
;;   ;NWMMMMNd;,.....                                  ...',,;::::cokXWMMMMMMWWNWNNO'
;;  .OWWMMMWKd:,....                                ......'',,;:::clxXMMMMMMMMMMMMWW0'
;; .OWMMMMMN0xc;'...                          ..';codkkxolldxxxkkOOOO0WMMMMMMMMMMMMMMK,
;; .kNMMMMWKOkdc'......                ...,:oxO0KNWMMMMMMMMMMMMMMMMWXWMMNNNWMMMMMMMMMM0'
;;   ;NMMMN0Okdc'... .................';cokKWMMMMMMMMMMMMMMMMMMMMMMMMMMMWXXXMMMMMMMMMMX,
;;   'XMMMWKkdl;,;:::clooollllcc:;,'',ckKNWMMMMMMMMMMMMMMMMMMMMMMMMMMW00XWNXWMMMMMMMMMX;
;;   cMMMMWKOxdx0NWWWWWMMWWWWWWWNXXKXNWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKXNMMWWMMMMMMMMNc
;;   oMMMMMWWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWc
;;   dMMMMWXONMMMMMMMMMMMMMMMMMMMMMMX00WMMMMMMMMMMMMMMMMMMMMMMMMMMN0WMMMMMMMMMMMMMMW0,
;;   dWWMMXOXWMMMMMMMMMMMMMMMMMMMMKc.  'oXMMMMMMMMMMMMMMMMMMMMMMMMXKWX00OO0NMMMMMMMNOl;.
;;   ;NMMMMMMMMMMMMMMMMMMMMMMMMMMMo.     ,KMMMMMMMMMMMMMMNNMMMMMMMMMKllloodKMMMMMMMWWX0o.
;;    oMMMMMMMMMMMMMMMMMMMMMMMMMMNc.      'xNMMMMMMMMMMMMMMMMMMMMMW0lclooodkKXNWWXxldXO;.
;;    .xWMMMMMMMMMMMMMMMMMMMMMMMWO:.       .:kXMMMMMMMMMMMMMMMMMMMNxcllooddddddXMXOd;;,..
;;      dMMMMMMMMMMMMMMMMMMMMMMMKo,.      ....;OWMMMMMMMMMMMMMMMMWXdllloodxddold0KK0l'...
;;      ,NMMMMMMMMMMMMMMMMMMMMW0xl,.      ..'',;lk00K0Okxdooooodxxolclloddxddol;.'lo;'''.
;;      ,XMMMMMMMMMMMMMMMMMW0dloxl'.     .......,lolc:,''..'',,;::ccclloodxdddl'..'''';Oc
;;     .xWMMMMMMMMMMMWX0xoc'..;:coc,....'';,,''.'clllllc::;;;;;:ccclllloodddddl'......;l.
;;     ,XMMMWWWMMW0dc'.      ':cxXX0xddddxk0K0Okdc..';loollcccccccllllloodddddl,....'.
;;      xMMMMWWWKl.          ;kKWMMMMWWMMMMWWWMWO,...',:loodooolllllllloodddxxdl;;;,.
;;      cXWMMWWNx,..         oNWMMMMMMMMMXd:;ccc,'''''';:coddddooooolllooddddxxc.'..
;;      .c0MMMWNkc'..       .OMMMMMMMMNOdc'........''',;::cloddooolllllloodxxxd'
;;        cNMMWNKd:,'.......'oO0XWNX0o'..'.........'',,:cclooollllcllooooodxkkx;
;;         ,cxXWNOoc;'''''..',,,:c::;,'..';::;;::c:::::ccloooolcccccloodddxkkkxl
;;           .oXNKxl:,''''',;::;,;;cox00kkkOO0KXXNXXXKKOkxollllccclloodddxkOOOkd.
;;             dNKxo:'...';:codddOXNWMWWNXKK0OkOOOOkkxooollooollllooddxxkkO00Okx.
;;             .KNOoc'. ..'clxKNWWWNXKOkkxxdolcc:;,'''',,;:codooddddxkkOO0KK0Okk;
;;              lNXko;.    .:dkkxxxdddolcc:;;,,'..''''',;;:lodddxxkkOO00KXXX0Oxx:
;;               cdOOo;..   .,:::cc::;,,,;;:::::;;;;;;::clloddxkk00KKKXXNNNX0kxxl
;;                 cX0d:'....,::,,;:cllccclooolllcccccloodddxxkO0KXNNNWWWWNKkdddo.
;;                .dNWXOdc:;;coollccloddddoolllccccclloddddxxkO0XNWWWMMMMWXOdoodo.
;;                 .cKMWNKOxdxkOOkxxxxdoolc::cc:;,,;;:cllodxkOKNWWMMMMMMMXkdloooo'
;;                   .cOWMMWXK0KK000Oxooc:,',;;;;,,,,:cloddxOKNWWMMMMMMWXkoollloo:...
;;                      l0NWMMWWNKOkxdoloc:,;;:lllccldxxxkk0XNMMMMMMMMXOdllcllooolokNOdoc,
;;                        .;KMMMMWX0OkkdddolllodxxxxkkOO00KNWMMMMMMMWKxocccclloolcco0MMWWNx.
;;                          .lXMMMMWWNXXXK0OkkkOKXXXKXXNWWMMMMMMMMWXkolccccclllcccclxXWX0KX0l.
;;                            ;0WMMMMMMMMMMWWNWWMMMMMMMMMMMMMMMMMXkollllllllcccccccldKNX00OOKKo.
;;                              ':OMMMMMMMMMMMMMMMMMMMMMMMMMMMMXOdooooollolc:::::cccOWNX000OkOKk'
;;                                ;XMMMMMMMMMMMMMMMMMMMMMMMMWXOdddoooloolc::::::::coXWNX0K000OkOkd,
;;                                 :XMMMMMMMMMMMMMMMMMMMMMMNOxddddooollcc::;;:::::dXMMMWNXKK0OOOkkx;
;;                                 .0MMMMMMMMMMMMMMMMMMMMWKkdddddoollcc::::::::::dNMMMMMMWXKOOOOOxxxl'.
;;                                  :XMMMMMMMMMMMMMMMMMMNOxxxxdddollcccc::::;;;;lXMMMMMMMMMNK0OkkkkOxkkxxxxxdc;,;:;'
;;                                   :0NMMMMMMMMMMMMMMMXkxxxxddoolcccccc:::;;;::0MMMMMMMMMMMMMWKOkkOk00XXXXKOdcloood
;;                                     lWMMMMMMMMMMMMWKkxxxddooolllccccc:::::::oNMMMMMMMMMMMMMMMWNXKOO0KXXdlllloooxx
;;                                     .0MMMMMMMMMMWXOxxddddoooolllcccccc::c::o0MMMMMMMMMMMMMMMMMMMWWNNX0Ooc:cooccol
;;                                     .kWMMMMMMMMN0kxdxddddoooolllccccccccccoKWMMMMMMMMMMMWWWMMMWMMMMMWXK0OOOOxlloc
;;                                  ,kocOWMMMMMMWKOkkxxxddddddooolllclllllcoONWWMMMMMMMMMMMWWWWWWWMMWMMMMMMWNNNXXKKK
;;                                  :WMMMMMMMMMN0OOkkkxxxxxddddoooollllooldXMMMMMMMMMMMMMMMMMMWWWWWWWMMMMWX0000K000N
;;                                 .0MMMMMMMMMWKOOOOkkkkkkxxxdddooooooloookWMMMMMMMMMMMMMMMMMMMWWWMWNWWWWN0OO00KKKKW
