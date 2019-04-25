(defun bootstrap () 
  ;; disable menu bar and toolbar
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

  (defun get-use-package ()
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

    (require 'use-package)
    (setq use-package-always-ensure t))

    (get-use-package)
)

(defun my-packages ()
  (use-package evil
	       :config
	       (evil-mode 1)
	       (modify-syntax-entry ?_ "w"))

  (use-package leuven-theme)
)

(defun main() 
  (bootstrap)
  (my-packages)
  )

(main)
