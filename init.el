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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lsp-scala sbt-mode scala-mode dockerfile-mode anaconda-mode lsp-origami company-lsp lsp-ui lsp-mode treemacs origami leuven-theme color-theme-sanityinc-tomorrow restart-emacs evil-magit magit helm-projectile projectile evil-escape evil-org evil-collection evil flycheck company which-key delight diminish general use-package-ensure-system-package org use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
