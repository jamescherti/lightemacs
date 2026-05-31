;;; le-group-markdown.el --- group-markdown -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Enhance Markdown editing (e.g., README.md files).

;;; Code:

(require 'lightemacs-module)

(lightemacs-module-load
 '(;; Configures `markdown-mode' or `markdown-ts-mode' (available on
   ;; Emacs >= 31) to provide syntax highlighting, editing commands, and preview
   ;; support for Markdown documents.
   le-maybe-markdown-ts))

(unless (bound-and-true-p lightemacs-maybe-markdown-ts--tree-sitter)
  (lightemacs-module-load
   '(;; Generate or refresh the table of contents in Markdown files using the
     ;; 'markdown-toc-generate-or-refresh-toc' function.
     le-markdown-toc)))

(provide 'le-group-markdown)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-group-markdown.el ends here
