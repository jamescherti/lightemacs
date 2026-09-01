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
 '(;; le-flavor-essential enables:
   ;;   le-theme le-default-keybindings
   ;;   le-default-settings le-dired le-savehist
   ;;   le-elec-pair le-paren le-recentf le-saveplace
   ;;   le-autorevert le-org le-term
   le-flavor-essential

   ;; The undo-fu package is a lightweight wrapper
   ;; around Emacs' built-in undo system, providing more
   ;; convenient undo/redo functionality while
   ;; preserving access to the full undo history.
   le-undo-fu

   ;; The undo-fu-session package complements undo-fu by
   ;; enabling the saving and restoration of undo
   ;; history across Emacs sessions, even after
   ;; restarting.
   le-undo-fu-session

   ;; `dired': Filter dotfiles, omit files, and files
   ;; listed in .gitignore
   le-dired-filter

   ;; Corfu enhances in-buffer completion by displaying
   ;; a compact popup with current candidates,
   ;; positioned either below or above the point.
   ;; Candidates can be selected by navigating up or
   ;; down.
   le-corfu

   ;; (Cape integrates with corfu)
   ;;
   ;; Cape, or Completion At Point Extensions, extends
   ;; the capabilities of in-buffer completion. It
   ;; integrates with Corfu or the default completion
   ;; UI, by providing additional backends through
   ;; completion-at-point-functions.
   le-cape

   ;; (Vertico, Consult, and Embark collectively enhance
   ;; Emacs' completion and navigation capabilities.)
   ;;
   ;; Vertico provides a vertical completion interface,
   ;; making it easier to navigate and select from
   ;; completion candidates (e.g., when M-x is pressed).
   le-vertico

   ;; (Vertico, Consult, and Embark collectively enhance
   ;; Emacs' completion and navigation capabilities.)
   ;;
   ;; Consult offers a suite of commands for efficient
   ;; searching, previewing, and interacting with
   ;; buffers, file contents, and more, improving
   ;; various tasks.
   le-consult

   ;; (Vertico, Consult, and Embark collectively enhance
   ;; Emacs' completion and navigation capabilities.)
   ;;
   ;; Embark integrates with these tools to provide
   ;; context-sensitive actions and quick access to
   ;; commands based on the current selection, further
   ;; improving user efficiency and workflow within
   ;; Emacs. Together, they create a cohesive and
   ;; powerful environment for managing completions and
   ;; interactions.
   le-embark

   ;; Integration between Embark and Consult
   le-embark-consult

   ;; Enable flexible, unordered matching (Orderless)
   ;; for Vertico. This allows typing multiple parts of
   ;; a candidate in any order, making it easier to find
   ;; functions, variables, or files even if you only
   ;; remember fragments.
   ;;
   ;; Example in Vertico:
   ;; - Typing "main test" matches "test_main.py"
   ;; - Typing "read me" matches "README.md"
   le-orderless

   ;; Marginalia enriches minibuffer completions with
   ;; contextual annotations. It Enhances Vertico by
   ;; adding rich annotations to completion candidates,
   ;; such as file sizes, documentation, or metadata.
   le-marginalia

   ;; Filetype: Markdown
   ;; Provides a major mode for Emacs for syntax
   ;; highlighting, editing commands, and preview
   ;; support for Markdown documents.
   le-markdown-mode

   ;; Vim tab bar
   le-vim-tab-bar

   ;; prescient.el is a library for sorting and
   ;; filtering lists of candidates, such as those
   ;; presented by packages like Vertico or Corfu.
   le-prescient
   le-vertico-prescient
   le-corfu-prescient

   ;; Filetypes
   le-csv-mode
   le-maybe-markdown-ts
   le-maybe-yaml-ts
   le-git-modes

   ;; The persist-text-scale Emacs package provides
   ;; `persist-text-scale-mode', which ensures that all
   ;; adjustments made with `text-scale-increase' and
   ;; `text-scale-decrease' are persisted and restored
   ;; across sessions.
   le-persist-text-scale

   ;; The `which-key' mode dynamically displays
   ;; available keybindings in a popup or dedicated
   ;; buffer.
   le-which-key

   ;; Built-in
   le-winner
   le-display-line-numbers

   ;; This package provides functions to detects the
   ;; indentation offset used in existing source code
   ;; files and automatically adjusts Emacs settings
   ;; accordingly, thereby simplifying the editing of
   ;; files created in external environments.
   le-dtrt-indent

   ;; Development
   le-apheleia
   le-diff-hl
   le-yasnippet
   le-yasnippet-snippets
   le-dumb-jump
   le-flymake
   le-group-code-folding

   ;; Development: Elisp
   le-group-emacs-lisp
   le-paredit

   ;; The bufferfile package provides helper functions
   ;; to delete, rename, or copy buffer files.
   le-bufferfile

   ;; Misc
   le-helpful
   le-avy
   le-buffer-terminator
   le-expand-region
   le-indent-bars
   le-kirigami
   le-magit
   le-org-appear
   le-outline
   le-outline-indent
   le-stripspace))

;;; Provide

(provide 'le-flavor-big)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-flavor-big.el ends here
