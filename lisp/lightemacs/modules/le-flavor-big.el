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

(require 'lightemacs)

(lightemacs-module-load
 '(;; Auto compile all .el files
   le-compile-angel

   le-exec-path-from-shell

   ;; le-flavor-essential enables:
   ;;   le-theme le-default-settings le-dired le-savehist le-elec-pair le-paren
   ;;   le-recentf le-saveplace le-autorevert
   le-flavor-essential

   le-so-long

   ;; The Garbage Collector Magic Hack
   le-gcmh

   ;; The undo-fu package is a lightweight wrapper around Emacs' built-in undo
   ;; system, providing more convenient undo/redo functionality while preserving
   ;; access to the full undo history.
   le-undo-fu

   ;; The undo-fu-session package complements undo-fu by enabling the saving and
   ;; restoration of undo history across Emacs sessions, even after restarting.
   le-undo-fu-session

   ;; `dired': Filter dotfiles, omit files, and files listed in .gitignore
   le-dired-filter

   ;; Corfu enhances in-buffer completion by displaying a compact popup with
   ;; current candidates, positioned either below or above the point. Candidates
   ;; can be selected by navigating up or down.
   le-corfu
   ;; TODO move to huge
   ;; le-corfu-popupinfo
   ;; le-nerd-icons-corfu

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

   ;; prescient.el is a library for sorting and
   ;; filtering lists of candidates, such as those
   ;; presented by packages like Vertico or Corfu.
   ;; TODO move to huge
   ;; le-prescient
   ;; le-vertico-prescient
   ;; le-corfu-prescient

   ;; Filetypes
   ;; TODO move to huge
   ;; le-csv-mode
   le-maybe-markdown-ts
   le-maybe-yaml-ts
   ;; TODO move to huge
   ;; le-git-modes

   ;; The persist-text-scale Emacs package provides
   ;; `persist-text-scale-mode', which ensures that all
   ;; adjustments made with `text-scale-increase' and
   ;; `text-scale-decrease' are persisted and restored
   ;; across sessions.
   ;; TODO Move to huge
   ;; le-persist-text-scale

   ;; This package provides functions to detects the
   ;; indentation offset used in existing source code
   ;; files and automatically adjusts Emacs settings
   ;; accordingly, thereby simplifying the editing of
   ;; files created in external environments.
   le-dtrt-indent

   ;; Development
   le-yasnippet
   ;; TODO Move to huge
   ;; le-yasnippet-snippets
   ;; TODO Move to huge
   ;; le-dumb-jump
   le-flymake

   ;; Code Folding
   ;; Includes:
   ;;   le-outline le-outline-indent le-hideshow le-treesit-fold le-kirigami
   ;; TODO Move to huge
   ;; le-group-code-folding

   ;; Development: Elisp
   ;; le-group-emacs-lisp includes:
   ;;   le-highlight-defined le-aggressive-indent le-elisp-refs le-easy-escape
   le-group-emacs-lisp
   ;; TODO Move to huge
   ;; le-elisp-autofmt
   ;; TODO move to flavor huge
   ;; le-page-break-lines

   ;; The bufferfile package provides helper functions
   ;; to delete, rename, or copy buffer files.
   ;; TODO Move to huge
   ;; le-bufferfile

   ;; Misc
   le-helpful
   le-avy

   ;; TODO move to flavor huge
   ;; le-expand-region

   ;; TODO move to flavor huge
   ;; le-org-appear

   ;; TODO move to flavor huge
   ;; le-stripspace
   ))

;;; Provide

(provide 'le-flavor-big)

;;; le-flavor-big.el ends here
