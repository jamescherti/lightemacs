;;; le-avy.el --- le-avy -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Avy is an Emacs package that provides a fast and efficient method for
;; navigating to visible text in a buffer by jumping directly to characters,
;; words, or lines. It allows the user to type a sequence of characters or
;; select from highlighted targets to move the cursor instantly, reducing the
;; need for repetitive cursor motions or scrolling.
;;
;; URL: https://github.com/abo-abo/avy

;;; Code:

(require 'lightemacs-module)

(lightemacs-module-package avy
  :commands (avy-goto-char
             avy-goto-word-0
             avy-goto-word-1
             avy-goto-char-2
             avy-goto-char-timer
             avy-goto-line
             avy-next)

  :bind (("C-:"   . avy-goto-char)
         ("C-'"   . avy-goto-char-2)
         ("M-g j" . avy-goto-char-timer)  ;; TODO: Change?
         ("M-g w" . avy-goto-word-1)
         ;; Conflicts with Consult: (TODO)
         ;; ("M-g f" . avy-goto-line)
         ;; ("M-g e" . avy-goto-word-0)
         ("M-g W" . avy-goto-word-0)
         ("M-g l" . avy-goto-line))

  :init
  (setq
   ;; List of modes to ignore when searching for candidates.
   avy-ignored-modes '(image-mode doc-view-mode pdf-view-mode exwm-mode)
   ;; Determine the list of windows to consider in search of candidates.
   avy-all-windows nil
   ;; The alternative ‘avy-all-windows’ for use with M-x
   ;; universal-argument.
   avy-all-windows-alt t
   ;; When non-nil, a gray background will be added during the selection.
   avy-background nil
   ;; This is unpredictible
   avy-single-candidate-jump nil))

(provide 'le-avy)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-avy.el ends here
