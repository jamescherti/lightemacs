;;; le-corfu-prescient.el --- le-corfu-prescient -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

;; When prescient.el is used with Corfu, prescient.el improves both in-buffer
;; completions and pop-up completion menus by making candidate ordering more
;; predictable and adaptive to recent usage, thus speeding up repeated
;; selections.
;;
;; Example: If you frequently choose the completion "printf" when editing C
;; code, prescient.el will gradually move "printf" toward the top of the list
;; whenever similar candidates are offered, reducing the number of keystrokes
;; needed to select it.
;;
;; (prescient.el is a library for sorting and filtering lists of candidates,
;; such as those presented by packages like Vertico or Corfu. The main benefit
;; of prescient.el is that it adaptively orders candidates based on both
;; frequency and recency of selection, making frequently used options appear
;; first without sacrificing predictable filtering results.)

;; URL: https://github.com/radian-software/prescient.el

(eval-and-compile
  (require 'lightemacs)
  (require 'le-corfu)
  (require 'le-prescient))

(lightemacs-load-modules '(le-prescient))

(lightemacs-use-package corfu-prescient
  :after corfu
  :commands corfu-prescient-mode
  :hook (corfu-mode . corfu-prescient-mode)
  :init
  (setq corfu-prescient-enable-sorting t)

  ;; Do not override `display-sort-function'
  (setq corfu-prescient-override-sorting nil)

  ;; Use Orderless instead
  (setq corfu-prescient-enable-filtering nil))

(provide 'le-corfu-prescient)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-corfu-prescient.el ends here
