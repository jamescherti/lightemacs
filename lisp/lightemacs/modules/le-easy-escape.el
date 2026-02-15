;;; le-easy-escape.el --- le-easy-escape -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `easy-escape-minor-mode' improves the readability of Emacs Lisp regular
;; expressions through syntax highlighting and character composition.
;; Specifically, it hides double backslashes before regexp special characters
;; '()|', renders other doubled backslashes as single ones, and highlights
;; them with a distinct face. These transformations affect only the visual
;; presentation; the underlying buffer text remains unchanged.
;;
;; By default, a single '\' is displayed instead of a double backslash, and
;; backslashes preceding parentheses or '|' are hidden. The displayed escape
;; character and its color can be customized via `easy-escape-character' and
;; `easy-escape-face', respectively. To reveal backslashes before '()|', set
;; `easy-escape-hide-escapes-before-delimiters' to nil.
;;
;; URL: https://github.com/cpitclaudel/easy-escape

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package easy-escape
  :commands easy-escape-minor-mode
  :hook (emacs-lisp-mode . easy-escape-minor-mode))

(provide 'le-easy-escape)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-easy-escape.el ends here
