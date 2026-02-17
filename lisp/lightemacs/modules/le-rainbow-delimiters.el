;;; le-rainbow-delimiters.el --- le-rainbow-delimiters -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The rainbow-delimiters provides a minor mode that highlights parentheses,
;; brackets, and braces according to their nesting depth, with each level
;; displayed in a distinct color. This makes it easier to identify matching
;; delimiters, navigate code structure, and understand which statements are at a
;; given depth.
;;
;; The mode is designed for high performance. Enabling it should not affect
;; scrolling or editing speed, even in delimiter-heavy languages like Clojure or
;; Emacs Lisp, and it works with any programming language.
;;
;; Colors used by rainbow-delimiters are fully customizable. The default palette
;; is intentionally subtle, providing visual guidance without being intrusive.
;; Many popular color themes, including Zenburn and Solarized, provide their own
;; face definitions for the mode.
;;
;; URL: https://github.com/Fanael/rainbow-delimiters

;;; Code:

(eval-and-compile
  (require 'le-core-package-manager))

(lightemacs-use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

(provide 'le-rainbow-delimiters)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-rainbow-delimiters.el ends here
