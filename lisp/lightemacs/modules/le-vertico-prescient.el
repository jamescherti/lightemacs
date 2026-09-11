;;; le-vertico-prescient.el --- le-vertico-prescient -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; When prescient.el is used with Vertico, prescient.el enhances minibuffer
;; completion by dynamically reordering candidates based on frequency and
;; recency, making it faster to select commonly used options while preserving
;; consistent, predictable filtering. **Example:** When running `M-x` and
;; repeatedly selecting the command `compile`, prescient.el will place `compile`
;; near the top of the Vertico minibuffer list in future sessions, reducing the
;; need to type its full name.

;; URL: https://github.com/radian-software/prescient.el

;;; Code:

;; When prescient.el is used with Vertico, prescient.el enhances minibuffer
;; completion by dynamically reordering candidates based on frequency and
;; recency, making it faster to select commonly used options while preserving
;; consistent, predictable filtering.
;;
;; Example: When running M-x and repeatedly selecting the command `compile`,
;; prescient.el will place 'compile' near the top of the Vertico minibuffer list
;; in future sessions, reducing the need to type its full name.
;;
;; (prescient.el is a library for sorting and filtering lists of candidates,
;; such as those presented by packages like Vertico or Corfu. The main benefit
;; of prescient.el is that it adaptively orders candidates based on both
;; frequency and recency of selection, making frequently used options appear
;; first without sacrificing predictable filtering results.)
;;
;; URL: https://github.com/radian-software/prescient.el

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-module-load '(prescient))

(lightemacs-use-package vertico-prescient
  :after (prescient vertico)
  :commands vertico-prescient-mode
  :init
  (lightemacs-module-setq-maybe vertico-prescient
    vertico-prescient-enable-sorting t
    ;; Don't override `display-sort-function'
    vertico-prescient-override-sorting nil
    ;; Use Orderless instead
    vertico-prescient-enable-filtering nil)
  :config
  (vertico-prescient-mode 1))

(provide 'le-vertico-prescient)

;;; le-vertico-prescient.el ends here
