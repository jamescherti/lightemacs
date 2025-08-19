;;; le-group-emacs-lisp.el --- group-emacs-lisp -*- no-byte-compile: t; lexical-binding: t -*-

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
                           le-highlight-defined))

(provide 'le-group-emacs-lisp)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-group-emacs-lisp.el ends here
