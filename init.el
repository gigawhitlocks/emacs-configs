(setq dotfiles-dir
      (file-name-directory
       (or (buffer-file-name) load-file-name)))

(let* ((elpaca-org-dir (expand-file-name "elpaca/builds/org" dotfiles-dir))
       (load-path (if (file-directory-p elpaca-org-dir)
                      (cons elpaca-org-dir (or load-path nil))
                    (or load-path nil))))
  (require 'ob-tangle))

(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))
