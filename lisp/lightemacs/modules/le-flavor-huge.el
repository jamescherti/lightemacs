;;; le-flavor-huge.el --- Group: Default modules -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The "huge" flavor enables is a good flavor for software developers.

;;; Code:

(require 'lightemacs)

(lightemacs-module-load
 '(le-flavor-big
   le-compile-angel

   le-gcmh ; The Garbage Collector Magic Hack
   le-corfu-popupinfo
   le-nerd-icons-corfu

   le-diff-hl
   le-magit

   le-prescient
   le-vertico-prescient
   le-corfu-prescient

   le-csv-mode
   le-git-modes

   le-dumb-jump

   ;; Code Folding
   ;; Includes:
   ;;   le-outline-indent le-hideshow le-treesit-fold le-kirigami
   le-group-code-folding

   ;; Elisp
   le-elisp-autofmt
   le-page-break-lines

   ;; The bufferfile package provides helper functions
   ;; to delete, rename, or copy buffer files.
   le-bufferfile

   ;; The persist-text-scale Emacs package provides
   ;; `persist-text-scale-mode', which ensures that all
   ;; adjustments made with `text-scale-increase' and
   ;; `text-scale-decrease' are persisted and restored
   ;; across sessions.
   le-persist-text-scale

   ;; Misc
   le-helpful
   le-avy
   le-expand-region
   le-org-appear
   le-stripspace))

;;; Provide

(provide 'le-flavor-huge)

;;; le-flavor-huge.el ends here
