;;; le-group-emacs-lisp.el --- group-emacs-lisp -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Enhance Emacs Lisp (Elisp) editing.

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-load-modules
 '(;; Enable `highlight-defined-mode', a minor mode that highlights defined
   ;; Emacs Lisp symbols in `emacs-lisp-mode' buffers.
   le-highlight-defined

   ;; Enable `page-break-lines-mode', a minor mode that visually replaces ASCII
   ;; form-feed characters (typically `^L`) with horizontal line separators in
   ;; buffers.
   le-page-break-lines

   ;; Enable `aggressive-indent-mode', a minor mode that Elisp code remains
   ;; consistently indented. It automatically reindents after every
   ;; modification, providing greater reliability than `electric-indent-mode'.
   ;; le-group-emacs-lisp
   le-aggressive-indent

   ;; elisp-refs provides an advanced code search for Emacs Lisp.
   ;;
   ;; It locates references to functions, macros, or variables by parsing the
   ;; code rather than performing a simple text search. This ensures accuracy,
   ;; avoiding false matches caused by comments or variables sharing names with
   ;; functions.
   le-elisp-refs

   ;; `easy-escape-minor-mode' improves the readability of Emacs Lisp regular
   ;; expressions through syntax highlighting and character composition.
   ;; Specifically, it hides double backslashes before regexp special characters
   ;; '()|', renders other doubled backslashes as single ones, and highlights
   ;; them with a distinct face. These transformations affect only the visual
   ;; presentation; the underlying buffer text remains unchanged.
   le-easy-escape))

(provide 'le-group-emacs-lisp)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-group-emacs-lisp.el ends here
