;;; init --- the Emacs entrypoint
;;; Commentary:
;;; Minimal additions to this file so that Customize can do its thing on individual machines
;;; and not get angry with one another in git.
;;;
;;; Just load my customizations and execute
;;;
;;; Code:

(package-initialize)
(require '~/.emacs.d/ian.el)
(main)
(provide 'init)
