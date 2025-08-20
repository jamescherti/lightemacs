;;; le-group-emacs-lisp.el --- group-emacs-lisp -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Enhance Emacs Lisp (Elisp) editing.

;;; Code:

(require 'lightemacs)

(lightemacs-load-modules '(;; Enable `highlight-defined-mode', a minor mode
                           ;; that highlights defined Emacs Lisp symbols in
                           ;; `emacs-lisp-mode' buffers.
                           le-highlight-defined

                           ;; Enable `page-break-lines-mode', a minor mode
                           ;; that visually replaces ASCII form-feed
                           ;; characters (typically `^L`) with horizontal line
                           ;; separators in buffers.
                           le-page-break-lines

                           ;; Enable `aggressive-indent-mode', a minor mode
                           ;; that Elisp code remains consistently indented.
                           ;; It automatically reindents after every
                           ;; modification, providing greater reliability than
                           ;; `electric-indent-mode'.
                           ;; le-group-emacs-lisp
                           le-aggressive-indent))

(provide 'le-group-emacs-lisp)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-group-emacs-lisp.el ends here
