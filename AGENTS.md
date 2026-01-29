# AGENTS.md - Emacs Configuration Repository

## Overview

This repository contains a literate Emacs configuration written in Org-mode. The primary source of truth is `readme.org`, which tangles (extracts) to generate the actual Elisp files. Configuration uses Evil mode for Vim-style editing and Elpaca as the package manager.

## File Structure

```
~/.emacs.d/
├── readme.org          # PRIMARY SOURCE - edit this file
├── readme.el           # GENERATED - do not edit directly
├── init.el             # GENERATED - bootstrap loader, do not edit
├── early-init.el       # Early initialization (manual edits OK)
├── custom.el           # EPHEMERAL - gitignored, safe to delete
├── local/              # Machine-specific configs (*.org -> *.el)
│   └── <hostname>.org  # Host-specific overrides
└── vendor/             # Vendored packages not in MELPA
```

**Critical**: Never edit `readme.el` or `init.el` directly. All changes go in `readme.org`.

## Build/Tangle Commands

### Regenerating Configuration

Tangle `readme.org` to regenerate Elisp files:
```
M-x org-babel-tangle    # From within readme.org buffer
```

The file has a local variable hook that auto-tangles on save:
```elisp
;; Header sets: (add-hook 'after-save-hook '(lambda () (org-babel-tangle)) nil 'local)
```

### Loading Configuration

```bash
# Start Emacs normally (loads init.el -> readme.org -> readme.el)
emacs

# Batch load for testing (no window)
emacs --batch --load ~/.emacs.d/init.el

# Fresh start with debug
emacs --debug-init
```

### Byte Compilation

```bash
# Compile all .el files
emacs --batch --eval "(byte-recompile-directory \"~/.emacs.d\" 0 'force)"
```

### Package Management (Elpaca)

```
M-x elpaca-browse       # Browse available packages
M-x elpaca-manager      # Manage installed packages
M-x elpaca-rebuild      # Rebuild a specific package
M-x elpaca-delete       # Remove a package
```

## Testing & Validation

### Verifying Configuration Loads

1. Start Emacs fresh: `emacs --debug-init`
2. Check `*Messages*` buffer for errors: `C-h e` or `SPC b b *Messages*`
3. Check `*Warnings*` buffer if present

### Linting Elisp

Flycheck is enabled globally. For manual checking:
```
M-x flycheck-buffer           # Check current buffer
M-x flycheck-list-errors      # Show error list (SPC e l)
M-x checkdoc                  # Check documentation strings
```

### Testing a Single Function

```
M-x eval-defun                # Evaluate function at point (C-M-x)
M-x eval-last-sexp            # Evaluate preceding expression (C-x C-e)
M-x ielm                      # Interactive Elisp REPL
```

## Code Style Guidelines

### Emacs Lisp Conventions

```elisp
;; Function naming: kebab-case, descriptive verbs
(defun my-project-do-something ()
  "Short docstring explaining purpose."
  ...)

;; Variables: kebab-case, nouns
(defvar my-project-default-value 42
  "Documentation for the variable.")

;; Private/internal: prefix with double hyphen
(defun my-project--internal-helper ()
  ...)
```

### use-package Pattern

```elisp
(use-package package-name
  :ensure t                    ; Install via Elpaca (default in this config)
  :defer t                     ; Lazy load when possible
  :after (dependency)          ; Load order dependencies
  :hook ((mode-hook . function))
  :bind (("key" . command))
  :custom
  (custom-variable value)
  :config
  (setup-code-here))
```

### Elpaca-Specific Patterns

```elisp
;; For packages requiring synchronous load
(use-package critical-package
  :ensure (:wait t))

;; For packages from Git repos
(use-package package-name
  :straight (package-name
             :type git
             :host github
             :repo "user/repo"))
```

### Documentation Style

Prefer prose documentation in Org-mode headings and paragraphs over inline comments. Code blocks in `readme.org` should be self-explanatory through surrounding context.

### Error Handling

```elisp
;; Use condition-case for recoverable errors
(condition-case err
    (risky-operation)
  (error (message "Failed: %s" err)))

;; Use when-let/if-let for nil-safe operations
(when-let ((result (maybe-nil-function)))
  (do-something-with result))
```

## Keybinding Conventions

### Evil Mode & Leader Key

This configuration uses Evil mode with `SPC` as the leader key (Spacemacs-style). Keybindings are defined using `general.el`.

```elisp
;; Global leader bindings
(my-leader-def 'normal 'override
  "ff"  'find-file           ; SPC f f
  "bb"  'consult-buffer      ; SPC b b
  "gs"  'magit-status)       ; SPC g s

;; Mode-specific bindings use comma as local leader
(general-define-key
 :states  'normal
 :keymaps 'go-mode-map
 ",tt"    'go-test-current-test
 ",tp"    'go-test-current-project)
```

### Key Binding Namespaces

| Prefix    | Purpose                    |
|-----------|----------------------------|
| `SPC SPC` | M-x (execute-extended-command) |
| `SPC b`   | Buffer operations          |
| `SPC f`   | File operations            |
| `SPC g`   | Git/Magit                  |
| `SPC p`   | Projectile/Project         |
| `SPC w`   | Window management          |
| `SPC e`   | Errors (Flycheck)          |
| `SPC t`   | Toggles                    |
| `,`       | Mode-specific (local leader) |

## Local/Machine-Specific Configuration

Host-specific settings live in `local/<hostname>.org` and are tangled to `local/<hostname>.el`. These are gitignored.

```elisp
;; Example: local/my-workstation.org tangles to local/my-workstation.el
;; Must provide 'local feature at end:
(provide 'local)
```

### Directory-Local Variables

Use `dir-locals-set-class-variables` for project-specific settings:

```elisp
(dir-locals-set-class-variables
 'my-project-dir
 '((go-mode . ((go-test-args . "-v -failfast")))))

(dir-locals-set-directory-class
 "~/projects/my-project" 'my-project-dir)
```

## Important Notes

1. **Source of Truth**: Always edit `readme.org`, never the generated `.el` files
2. **Tangling**: Save `readme.org` to auto-tangle, or run `M-x org-babel-tangle`
3. **Custom.el**: This file is ephemeral and gitignored; safe to delete if issues arise
4. **First Run**: After cloning, run `M-x nerd-icons-install-fonts` and `M-x all-the-icons-install-fonts`
5. **Package Issues**: Delete `~/.emacs.d/elpaca/` to force full package reinstall
6. **Debugging**: Use `emacs --debug-init` to catch initialization errors
7. **Local Config**: Machine-specific settings go in `local/<hostname>.org`
