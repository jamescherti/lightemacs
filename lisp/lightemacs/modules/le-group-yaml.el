;;; le-group-yaml.el --- group-yaml -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(require 'lightemacs-module)

;;; Load `yaml-mode' or `yaml-ts-mode'

(defvar lightemacs-group-yaml-prefer-yaml-ts-mode t
  "Non-nil indicates a preference for using Tree-sitter for YAML editing.

When non-nil and Tree-sitter support for YAML is available, the third-party
package `yaml-mode' will not be loaded; instead, `yaml-ts-mode' (Tree-sitter)
will be used.

Setting this variable to nil forces `yaml-mode' to load even if
Tree-sitter is available.")

;;; Maybe load `yaml-mode'

(when (or (not lightemacs-group-yaml-prefer-yaml-ts-mode)
          (not (if (fboundp 'treesit-ready-p)
                   (treesit-ready-p 'yaml)
                 nil)))
  (lightemacs-load-modules '(le-yaml-mode)))

;;; Configure `yaml-ts-mode'

(lightemacs-load-modules '(le-yaml-ts-mode))

(provide 'le-group-yaml)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-group-yaml.el ends here
