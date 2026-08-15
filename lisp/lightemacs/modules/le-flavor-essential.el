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

   ;; `dired': Filter dotfiles, omit files, and files listed in .gitignore
   le-dired-filter

   ;; Corfu enhances in-buffer completion by displaying a compact popup with
   ;; current candidates, positioned either below or above the point. Candidates
   ;; can be selected by navigating up or down.
   le-corfu

   ;; (Cape integrates with corfu)
   ;;
   ;; Cape, or Completion At Point Extensions, extends the capabilities of
   ;; in-buffer completion. It integrates with Corfu or the default completion
   ;; UI, by providing additional backends through
   ;; completion-at-point-functions.
   le-cape

   ;; (Vertico, Consult, and Embark collectively enhance Emacs' completion and
   ;; navigation capabilities.)
   ;;
   ;; Vertico provides a vertical completion interface, making it easier to
   ;; navigate and select from completion candidates (e.g., when M-x is
   ;; pressed).
   le-vertico

   ;; (Vertico, Consult, and Embark collectively enhance Emacs' completion and
   ;; navigation capabilities.)
   ;;
   ;; Consult offers a suite of commands for efficient searching, previewing,
   ;; and interacting with buffers, file contents, and more, improving various
   ;; tasks.
   le-consult

   ;; (Vertico, Consult, and Embark collectively enhance Emacs' completion and
   ;; navigation capabilities.)
   ;;
   ;; Embark integrates with these tools to provide context-sensitive actions
   ;; and quick access to commands based on the current selection, further
   ;; improving user efficiency and workflow within Emacs. Together, they create
   ;; a cohesive and powerful environment for managing completions and
   ;; interactions.
   le-embark

   ;; Integration between Embark and Consult
   le-embark-consult

   ;; Enable flexible, unordered matching (Orderless) for Vertico. This allows
   ;; typing multiple parts of a candidate in any order, making it easier to
   ;; find functions, variables, or files even if you only remember fragments.
   ;;
   ;; Example in Vertico:
   ;; - Typing "main test" matches "test_main.py"
   ;; - Typing "read me" matches "README.md"
   le-orderless

   ;; Marginalia enriches minibuffer completions with contextual annotations. It
   ;; Enhances Vertico by adding rich annotations to completion candidates, such
   ;; as file sizes, documentation, or metadata.
   le-marginalia

   ;; Autorevert is a feature that automatically updates the contents of a
   ;; buffer to reflect changes made to the underlying file on disk.
   le-autorevert

   ;; The undo-fu package is a lightweight wrapper around Emacs' built-in undo
   ;; system, providing more convenient undo/redo functionality while preserving
   ;; access to the full undo history.
   le-undo-fu

   ;; The undo-fu-session package complements undo-fu by enabling the saving and
   ;; restoration of undo history across Emacs sessions, even after restarting.
   le-undo-fu-session

   ;; Configure `org-mode' and `org-agenda'
   le-org

   ;; Built-in Emacs terminal emulator
   le-term))

;;; Provide

(provide 'le-flavor-essential)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-flavor-essential.el ends here
