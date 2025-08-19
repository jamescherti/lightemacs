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

;; Load `lightemacs--ripgrep-executable' and `lightemacs--fdfind-executable'
(require 'le-lib)

;; Allow Emacs to upgrade built-in packages, such as Org mode
(setq package-install-upgrade-built-in t)

(setq warning-suppress-types '((obsolete lexical-binding)))

(setq treesit-font-lock-level 4) ; Max: 4

;; Emacs automatically saves your changes to a file intermittently
(setq make-backup-files t)

(when lightemacs--ripgrep-executable
  (setq xref-search-program 'ripgrep))

;;; Minibuffer

(setq minibuffer-default-prompt-format " [%s]")
(add-hook 'lightemacs-on-first-input-hook #'minibuffer-depth-indicate-mode)

;;; Frame

(add-hook 'after-init-hook #'window-divider-mode)

(provide 'le-default-settings)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-default-settings.el ends here
