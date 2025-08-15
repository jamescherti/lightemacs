;;; mod-vertico-prescient.el --- mod-vertico-prescient -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Evil is an extensible vi layer for Emacs. It emulates the main features of
;; Vim, and provides facilities for writing custom extensions.
;;
;; This configures the packages evil and evil-collection.
;;
;; URL: https://github.com/emacs-evil/evil

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

;; URL: https://github.com/radian-software/prescient.el

;;; Evil

(use-package vertico-prescient
  :defer t
  :commands vertico-prescient-mode
  :hook
  (vertico-mode . vertico-prescient-mode)
  :init
  (setq vertico-prescient-enable-filtering t)
  (setq vertico-prescient-enable-sorting t)

  ;; If non-nil, `vertico-sort-override-function' is set to
  ;; `prescient-completion-sort' and sorting is forced on
  ;; (`vertico-prescient-enable-sorting' becomes non-nil).
  ;;
  ;; Disabled here to avoid overriding sorting in `recentf'.
  (setq vertico-prescient-override-sorting nil))

(provide 'mod-vertico-prescient)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-vertico-prescient.el ends here
