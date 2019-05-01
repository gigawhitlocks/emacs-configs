;;; init --- the Emacs entrypoint
;;; Commentary:
;;; Minimal additions to this file so that Customize can do its thing on individual machines
;;; and not get angry with one another in git.
;;;
;;; Just load my customizations and execute
;;;
;;; Code:

(require 'package)
(add-to-list
  'package-archives
  '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Now install use-package to enable us to use it
;; to manage the rest of our packages

(unless (package-installed-p 'use-package)
  (progn
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

;; use-package and package.el don't know how to install
;; an up-to-date version of org-mode
;; so part of this bootstrap process, since org-mode
;; is a built-in and we want changes from other
;; layers to apply to our updated Org as things are
;; installed, is to manually update Org before
;; even use-package is set up
;; credit https://github.com/jwiegley/use-package/issues/319

;; TODO this only seems to work after some other initialization step has run
;; one time. I'm not sure what that step is, just that this next step only
;; succeeds if it's not the first time we've run emacs with this configuration.
;; For now I'm going to leave this commented out, until I figure out
;; what is causing this behavior

(unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))
  (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))


(package-initialize)
(require '~/.emacs.d/ian.el)
(main)
(provide 'init)

;;; init.el ends here
