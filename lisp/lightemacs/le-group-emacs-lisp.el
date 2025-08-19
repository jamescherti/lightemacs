;;; le-group-emacs-lisp.el --- group-emacs-lisp -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Enhance Emacs Lisp (Elisp) editing:
;;   - Highlights defined Emacs Lisp symbols.
;;   - Maintain consistent indentation of Elisp code during editing.
;;   - Visually replace ASCII form-feed characters (typically `^L`) with
;;     horizontal lines to make page breaks easier to see.
;;   - Advanced code search for Emacs Lisp.

;;; Code:

(require 'lightemacs)

(lightemacs-load-modules '(;; Enable `highlight-defined-mode', a minor mode
                           ;; that highlights defined Emacs Lisp symbols in
                           ;; `emacs-lisp-mode' buffers.
                           le-highlight-defined

                           ;; Enable `page-break-lines', a minor mode that
                           ;; visually replaces ASCII form-feed characters
                           ;; (typically ^L) with horizontal line separators in
                           ;; buffers, making page breaks easier to see without
                           ;; affecting the actual text.
                           le-page-break-lines

                           ;; `aggressive-indent-mode' is a minor mode that
                           ;; Elisp code remains consistently indented. It
                           ;; automatically reindents after every modification,
                           ;; providing greater reliability than
                           ;; `electric-indent-mode'. To enable it for Elisp
                           ;; files, add the following to the
                           ;; ~/.emacs.d/config.el file:
                           le-aggressive-indent

                           ;; elisp-refs provides an advanced code search for
                           ;; Emacs Lisp.
                           ;;
                           ;; It locates references to functions, macros, or
                           ;; variables by parsing the code rather than
                           ;; performing a simple text search. This ensures
                           ;; accuracy, avoiding false matches caused by
                           ;; comments or variables sharing names with
                           ;; functions.
                           le-elisp-refs))

(add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
(add-hook 'emacs-lisp-mode-hook 'page-break-lines-mode)
(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

(provide 'le-group-emacs-lisp)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-group-emacs-lisp.el ends here
