;;; le-markdown-mode.el --- le-markdown-mode -*- no-byte-compile: t; lexical-binding: t -*-

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

(require 'lightemacs)

(lightemacs-use-package
  markdown-mode
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)

  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(lightemacs-define-keybindings markdown-mode
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "C-c C-e") #'markdown-do)))


(provide 'le-markdown-mode)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-markdown-mode.el ends here
