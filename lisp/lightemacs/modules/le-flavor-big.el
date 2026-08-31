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
   ;; ----------------------------
   ;; le-theme le-default-keybindings
   ;; le-default-settings le-dired le-savehist
   ;; le-elec-pair le-paren le-recentf le-saveplace
   ;; le-dired-filter le-corfu le-cape le-vertico
   ;; le-consult le-embark le-embark-consult
   ;; le-orderless le-marginalia le-autorevert
   ;; le-undo-fu le-undo-fu-session le-org
   ;; le-term
   le-flavor-essential

   le-compile-angel

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

   ;; The persist-text-scale Emacs package provides `persist-text-scale-mode',
   ;; which ensures that all adjustments made with `text-scale-increase' and
   ;; `text-scale-decrease' are persisted and restored across sessions.
   le-persist-text-scale

   ;; The `which-key' mode dynamically displays
   ;; available keybindings in a popup or dedicated
   ;; buffer.
   le-which-key

   ;; Built-in
   le-winner
   le-display-line-numbers

   ;; This package provides functions to detects the indentation offset used in
   ;; existing source code files and automatically adjusts Emacs settings
   ;; accordingly, thereby simplifying the editing of files created in external
   ;; environments.
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

   ;; The bufferfile package provides helper functions to delete, rename, or
   ;; copy buffer files.
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
