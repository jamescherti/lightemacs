;;; le-default-settings.el --- le-default-settings -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Miscellaneous Lightemacs default settings.

;;; Code:

;;; Better defaults

;; Allow Emacs to upgrade built-in packages, such as Org mode
(setq package-install-upgrade-built-in t)

;; (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
;; (setq byte-compile-warnings '(not lexical))
;; (setq warning-suppress-types '((lexical-binding)))
(setq warning-minimum-level :emergency)

(setq treesit-font-lock-level 4) ; Max: 4

;; Emacs automatically saves your changes to a file intermittently
(setq make-backup-files t)

;; Enable `auto-save-mode' to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)
(setq auto-save-interval 300)
(setq auto-save-timeout 30)

;; Load `lightemacs--ripgrep-executable' and `lightemacs--fdfind-executable'
(require 'le-lib)
(when lightemacs--ripgrep-executable
  (setq xref-search-program 'ripgrep))

;;; Minibuffer

(setq minibuffer-default-prompt-format " [%s]")
(add-hook 'lightemacs-on-first-input-hook #'minibuffer-depth-indicate-mode)

;;; Mode line

(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

;;; Frame

(add-hook 'after-init-hook #'window-divider-mode)

(provide 'le-default-settings)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-default-settings.el ends here
