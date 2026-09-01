;;; le-flavor-essential.el --- Group: Default modules -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Default modules.

;;; Code:

(require 'lightemacs-module)

(lightemacs-module-load
 '(le-flavor-micro

   ;; Preserve the minibuffer history between sessions. It saves the history of
   ;; inputs in the minibuffer, such as commands, search strings, and other
   ;; prompts, to a file.
   le-savehist

   ;; Automatically insert matching delimiters (), {}...
   le-elec-pair

   ;; `show-paren-mode' highlights matching pairs of parentheses and other
   ;; paired characters, improving code readability and helping to quickly
   ;; identify unbalanced expressions.
   le-paren

   ;; Recentf is an maintains a list of recently accessed files, making it
   ;; easier to reopen files you have worked on recently.
   le-recentf

   ;; Remember the last location within a file upon reopening. This is
   ;; beneficial for resuming work at the precise point where you previously
   ;; left off.
   le-saveplace

   ;; Autorevert is a feature that automatically updates the contents of a
   ;; buffer to reflect changes made to the underlying file on disk.
   le-autorevert

   ;; Built-in Emacs terminal emulator
   le-term))

;;; Provide

(provide 'le-flavor-essential)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-flavor-essential.el ends here
