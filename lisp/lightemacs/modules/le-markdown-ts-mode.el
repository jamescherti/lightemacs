;;; le-markdown-ts-mode.el --- le-markdown-ts-mode -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The `markdown-ts-mode' package is a built-in major mode for editing Markdown
;; documents with Tree-sitter support. It provides accurate syntax highlighting,
;; improved parsing, indentation, navigation commands, and integration with
;; Markdown preview workflows. The mode supports standard Markdown syntax as
;; well as common extensions, including GitHub Flavored Markdown (GFM).

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package markdown-ts-mode
  :if (>= emacs-major-version 31)
  :ensure nil
  :commands markdown-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :config
  (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode)))

(provide 'le-markdown-ts-mode)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-markdown-ts-mode.el ends here
