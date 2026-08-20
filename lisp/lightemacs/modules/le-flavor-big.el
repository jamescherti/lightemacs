;;; le-flavor-big.el --- Group: Default modules -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The "big" flavor enables is a good flavor for software developers.

;;; Code:

(require 'lightemacs-module)

(lightemacs-module-load
 '(le-compile-angel

   ;; Essential
   le-flavor-essential

   ;; Prescient
   le-prescient
   le-vertico-prescient
   le-corfu-prescient

   ;; Filetypes
   le-csv-mode
   le-maybe-markdown-ts
   le-maybe-yaml-ts
   le-git-modes

   ;; Built-in
   le-which-key
   le-winner
   le-display-line-numbers

   ;; Development
   le-apheleia
   le-diff-hl
   le-yasnippet
   le-yasnippet-snippets
   le-dtrt-indent
   le-dumb-jump
   le-flymake
   le-group-code-folding

   ;; Development: Elisp
   le-group-emacs-lisp
   le-paredit

   ;; Misc
   le-helpful
   le-avy
   le-buffer-terminator
   le-bufferfile
   le-expand-region
   le-indent-bars
   le-kirigami
   le-magit
   le-org-appear
   le-outline
   le-outline-indent
   le-persist-text-scale
   le-stripspace
   le-vim-tab-bar))

;;; Provide

(provide 'le-flavor-big)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-flavor-big.el ends here
