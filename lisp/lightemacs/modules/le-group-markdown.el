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

(require 'lightemacs-use-package)

(lightemacs-load-modules
 '(;; The markdown-mode package provides a major mode for Emacs for syntax
   ;; highlighting, editing commands, and preview support for Markdown
   ;; documents. It supports core Markdown syntax as well as extensions like
   ;; GitHub Flavored Markdown (GFM).
   le-markdown-mode

   ;; Automatically generate or refresh the table of contents in Markdown files
   ;; using the 'markdown-toc-generate-or-refresh-toc' function.
   le-markdown-toc))

(provide 'le-group-markdown)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-group-markdown.el ends here
