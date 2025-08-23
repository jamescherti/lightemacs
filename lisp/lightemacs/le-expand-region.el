;;; le-expand-region.el --- le-expand-region -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The expand-region package lets you grow your text selection step by step.
;;
;; When you press 'C-=', it first selects a small chunk, like a word. Press it
;; again, and it will expand to something bigger, like the sentence containing
;; that word. Press again, and it might select the whole paragraph, then maybe
;; the entire function, and so on.
;;
;; You just keep pressing 'C-=' until it highlights exactly the amount of text
;; you want.
;;
;; URL: https://github.com/magnars/expand-region.el

;;; Code:

(require 'lightemacs)

(lightemacs-use-package
  expand-region
  :commands (er/expand-region
             er/mark-word
             er/mark-symbol
             er/mark-symbol-with-prefix
             er/mark-next-accessor
             er/mark-method-call
             er/mark-inside-quotes
             er/mark-outside-quotes
             er/mark-inside-pairs
             er/mark-outside-pairs
             er/mark-comment
             er/mark-url
             er/mark-email
             er/mark-defun)
  :commands er/expand-region)

(lightemacs-define-keybindings expand-region
  (global-set-key (kbd "C-=") #'er/expand-region))

(provide 'le-expand-region)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-expand-region.el ends here
