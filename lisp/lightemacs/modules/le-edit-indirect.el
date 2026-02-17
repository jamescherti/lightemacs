;;; le-edit-indirect.el --- le-edit-indirect -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The `edit-indirect' package enhances `markdown-mode' by allowing users to
;; edit individual code blocks in separate, isolated buffers without affecting
;; the surrounding Markdown content. When editing a fenced code block,
;; `edit-indirect' automatically opens it in a buffer with the appropriate major
;; mode (e.g., `python-mode' for Python code), providing full syntax
;; highlighting, indentation, and other language-specific features. This makes
;; it easier to work on embedded code snippets as if they were standalone files,
;; improving clarity, and reducing the risk of accidentally altering text
;; outside the block.

;; URL: https://github.com/Fanael/edit-indirect

;;; Code:

(require 'lightemacs-use-package)

(lightemacs-use-package edit-indirect
  :commands edit-indirect-mode
  :commands edit-indirect-region)

(provide 'le-edit-indirect)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-edit-indirect.el ends here
