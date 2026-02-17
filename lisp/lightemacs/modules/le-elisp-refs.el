;;; le-elisp-refs.el --- le-elisp-refs -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; elisp-refs provides an advanced code search for Emacs Lisp.
;;
;; It locates references to functions, macros, or variables by parsing the code
;; rather than performing a simple text search. This ensures accuracy, avoiding
;; false matches caused by comments or variables sharing names with functions.
;;
;; URL: https://github.com/Wilfred/elisp-refs

;;; Code:

(eval-and-compile
  (require 'le-core-package-manager))

(lightemacs-use-package elisp-refs
  :commands (elisp-refs-function
             elisp-refs-macro
             elisp-refs-variable
             elisp-refs-special
             elisp-refs-symbol))

(provide 'le-elisp-refs)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-elisp-refs.el ends here
