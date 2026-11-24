;;; le-group-emacs-lisp.el --- group-emacs-lisp -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Enhance Emacs Lisp (Elisp) editing.

;;; Code:

(require 'lightemacs)

(lightemacs-module-load
 '(;; Enable `highlight-defined-mode', a minor mode that highlights defined
   ;; Emacs Lisp symbols in `emacs-lisp-mode' buffers.
   le-highlight-defined

   ;; Enable `aggressive-indent-mode', a minor mode that Elisp code remains
   ;; consistently indented. It automatically reindents after every
   ;; modification, providing greater reliability than `electric-indent-mode'.
   ;; le-group-emacs-lisp
   le-aggressive-indent

   ;; `easy-escape-minor-mode' improves the readability of Emacs Lisp regular
   ;; expressions through syntax highlighting and character composition.
   ;; Specifically, it hides double backslashes before regexp special characters
   ;; '()|', renders other doubled backslashes as single ones, and highlights
   ;; them with a distinct face. These transformations affect only the visual
   ;; presentation; the underlying buffer text remains unchanged.
   le-easy-escape))

(provide 'le-group-emacs-lisp)

;;; le-group-emacs-lisp.el ends here
