;;; le-undo-fu.el --- le-undo-fu -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The undo-fu package is a lightweight wrapper around Emacs' built-in undo
;; system, providing more convenient undo/redo functionality while preserving
;; access to the full undo history.
;;
;; The default undo system in Emacs has two main issues that undo-fu fixes:
;; - Redo requires two steps: To redo an action after undoing, you need to press
;;   a key twice, which can be annoying and inefficient.
;; - Accidental over-redo: When redoing, it's easy to go too far back, past the
;;   point where you started the undo, which makes it hard to return to the
;;   exact state you wanted to restore.

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package undo-fu
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint))

(with-eval-after-load 'evil
  (setq evil-undo-system 'undo-fu))

(provide 'le-undo-fu)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-undo-fu.el ends here
