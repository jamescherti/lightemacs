;;; le-avy.el --- le-avy -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
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

(eval-and-compile
  (require 'lightemacs))

(eval-and-compile
  (require 'use-package))

(lightemacs-use-package
  avy
  :commands (avy-goto-char
             avy-goto-word-0
             avy-goto-word-1
             avy-goto-char-2
             avy-goto-char-timer
             avy-goto-line
             avy-next)
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
   avy-background nil  ; t is not compatible with all themes
   ;; This is unpredictible
   avy-single-candidate-jump nil)

  (lightemacs-define-keybindings avy
    (global-set-key (kbd "C-:") 'avy-goto-char)
    (global-set-key (kbd "C-'") 'avy-goto-char-2)
    (global-set-key (kbd "M-g j") 'avy-goto-char-timer)  ;; TODO Change?
    (global-set-key (kbd "M-g w") 'avy-goto-word-1)

    ;; The following ones have been changes because they conflict with Consult
    ;; TODO Should consult be changed instead?
    ;; (global-set-key (kbd "M-g f") 'avy-goto-line)  ;; Conflict with Consult
    ;; (global-set-key (kbd "M-g e") 'avy-goto-word-0)  ;; Conflict with Consult
    (global-set-key (kbd "M-g W") 'avy-goto-word-0)
    (global-set-key (kbd "M-g l") 'avy-goto-line)))

(provide 'le-avy)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-avy.el ends here
