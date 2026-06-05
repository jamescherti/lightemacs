;;; le-maybe-markdown-ts.el --- group-markdown -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configures `markdown-mode' or `markdown-ts-mode' (available on Emacs >= 31)
;; to provide syntax highlighting, editing commands, and preview support for
;; Markdown documents. This group also configures `markdown-toc' when
;; `markdown-mode' is active.

;;; Code:

(require 'lightemacs-module)
(require 'treesit nil t)

;;; Variables

(defvar lightemacs-markdown-prefer-tree-sitter t
  "Non-nil indicates a preference for using Tree-sitter for Markdown editing.
When non-nil and Tree-sitter support for Markdown is available, the third-party
package `markdown-mode' will not be loaded; instead,
`markdown-ts-mode' (Tree-sitter) will be used. Setting this variable to nil
forces `markdown-mode' to load even if Tree-sitter is available.")

(defvar lightemacs-maybe-markdown-ts--tree-sitter nil)

;;; Choose between `markdown-mode' and `markdown-ts-mode'

(if (and (>= emacs-major-version 31)
         lightemacs-markdown-prefer-tree-sitter
         (fboundp 'treesit-ready-p)
         (treesit-ready-p 'markdown))
    (progn
      (setq lightemacs-maybe-markdown-ts--tree-sitter t)
      (lightemacs-module-load '(le-markdown-ts-mode)))
  (lightemacs-module-load '(le-markdown-mode)))

(provide 'le-maybe-markdown-ts)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-maybe-markdown-ts.el ends here
