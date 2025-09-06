;;; le-markdown-mode.el --- le-markdown-mode -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The markdown-mode package provides a major mode for Emacs for syntax
;; highlighting, editing commands, and preview support for Markdown documents.
;; It supports core Markdown syntax as well as extensions like GitHub Flavored
;; Markdown (GFM).
;;
;; URL: https://github.com/jrblevin/markdown-mode

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package markdown-mode
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)

  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))

  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do))

  :init
  (setq markdown-fontify-code-blocks-natively t

        ;; Extra languages made available when inserting GFM code blocks.
        ;; Language strings must have be trimmed of whitespace and not
        ;; contain any curly braces. They may be of arbitrary
        ;; capitalization, though.
        markdown-gfm-additional-languages '("sh")

        ;; Other options
        ;; markdown-asymmetric-header t
        ;; markdown-italic-underscore t
        ;; markdown-make-gfm-checkboxes-buttons t
        ;; markdown-fontify-whole-heading-line t
        )
  )

(provide 'le-markdown-mode)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-markdown-mode.el ends here
