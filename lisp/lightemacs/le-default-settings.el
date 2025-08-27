;;; le-default-settings.el --- le-default-settings -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Miscellaneous Lightemacs default settings.

;;; Code:

;;; Misc

;; TODO: move to m.e.?
(setq eldoc-documentation-strategy #'eldoc-documentation-compose)

;; (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
;; (setq byte-compile-warnings '(not lexical))
;; (setq warning-suppress-types '((lexical-binding)))
(setq warning-minimum-level :error)
;; (setq warning-minimum-level (if minimal-emacs-debug :warning :emergency))

(setq-default display-fill-column-indicator-character ?\N{U+2502})
;; (setq-default display-fill-column-indicator-character ?┊)

(setq treesit-font-lock-level 4) ; Max: 4

;;; Autosave

;; Enable `auto-save-mode' to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)
(setq auto-save-interval 300)  ; Number of input events between auto-saves
(setq auto-save-timeout 30)  ; Number of seconds idle time before auto-save

;;; Tools (ripgrep and fd)

;; Load `lightemacs--ripgrep-executable' and `lightemacs--fdfind-executable'
(require 'le-core-cli-tools)
(when lightemacs--ripgrep-executable
  (setq xref-search-program 'ripgrep))

;;; VC

(setq vc-git-print-log-follow t)

;;; Minibuffer

(setq minibuffer-default-prompt-format " [default %s]")
;; TODO use macro?
(add-hook 'lightemacs-on-first-input-hook #'minibuffer-depth-indicate-mode)

;;; Mode line

(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

;;; Frame

;; TODO use macro?
(add-hook 'after-init-hook #'window-divider-mode)

(provide 'le-default-settings)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-default-settings.el ends here
