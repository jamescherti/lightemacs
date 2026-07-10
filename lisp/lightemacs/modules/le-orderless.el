;;; le-orderless.el --- le-orderless -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Enable flexible, unordered matching (Orderless) for Vertico. This
;; allows typing multiple parts of a candidate in any order, making it easier to
;; find functions, variables, or files even if you only remember fragments.
;;
;; Benefits:
;; - Faster navigation: You don't need to type exact prefixes or full names.
;; - More intuitive: Matches any order of typed substrings.
;; - Works well with Vertico, and other completion frameworks.
;;
;; Example in Vertico:
;; - Typing "main test" matches "test_main.py"
;; - Typing "read me" matches "README.md"
;;
;; URL: https://github.com/oantolin/orderless

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package orderless
  :demand t
  :init
  (setq completion-pcm-leading-wildcard t)
  (setq completion-category-defaults nil)
  :config
  (setq completion-styles '(orderless basic))
  ;; Using `basic' for file completion instead of `partial-completion' for
  ;; files.
  ;;
  ;; Restricting the file category to `basic' preserves literal path completion,
  ;; avoids unexpected expansions, and delegates all fuzzy and out-of-order
  ;; matching to Orderless, resulting in more predictable and consistent
  ;; completion behavior.
  (setq completion-category-overrides '((file (styles basic)))))

(provide 'le-orderless)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-orderless.el ends here
