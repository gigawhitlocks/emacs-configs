;; a bugfix for a warning that appears at startup saying 'package cl is deprecated'
(setq byte-compile-warnings '(not obsolete))
(setq package-enable-at-startup nil)
