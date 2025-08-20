;;; le-group-yaml.el --- group-yaml -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module configures `yaml-mode' if Tree-sitter's `yaml-ts-mode' is not
;; available.
;;
;; (Tree-sitter's `yaml-ts-mode' offers more advanced syntax support than
;; `yaml-mode'. Consequently, when Tree-sitter is available, it is used by
;; default in place of `yaml-mode'.)

;;; Code:

(require 'lightemacs)

(defvar lightemacs-group-yaml-prefer-treesitter t
  "Non-nil indicates a preference for using Tree-sitter for YAML editing.

When non-nil and Tree-sitter support for YAML is available, the third-party
package `yaml-mode' will not be loaded; instead, `yaml-ts-mode' (Tree-sitter)
will be used.

Setting this variable to nil forces `yaml-mode' to load even if
Tree-sitter is available.")

(if (or (not lightemacs-group-yaml-prefer-treesitter)
        (not (if (fboundp 'treesit-language-available-p)
                 (treesit-language-available-p 'yaml)
               nil)))
    (lightemacs-load-modules '(le-yaml-mode)))

(provide 'le-group-yaml)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-group-yaml.el ends here
