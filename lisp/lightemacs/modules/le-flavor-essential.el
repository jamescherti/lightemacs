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

(lightemacs-load-modules
 '(;; Custom keybindings
   le-keybindings

   ;; Default settings (minibuffer, Emacs...) Configure enhanced default
   ;; settings, including improved defaults, backup files, warnings to ignore, a
   ;; minibuffer depth indicator, window behavior...
   le-default-settings

   ;; Update the ellipsis in `outline-mode' and `outline-minor-mode' using
   ;; `lightemacs-ellipsis'
   le-outline

   ;; Modern code folding based on indentation levels. Automatically enabled for
   ;; Python and Yaml.
   le-outline-indent

   ;; The persist-text-scale Emacs package provides `persist-text-scale-mode',
   ;; which ensures that all adjustments made with `text-scale-increase' and
   ;; `text-scale-decrease' are persisted and restored across sessions. As a
   ;; result, the text size in each buffer remains consistent, even after
   ;; restarting Emacs.
   le-persist-text-scale

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

   ;; Configure `dired' to hide details such as file ownership and permissions,
   ;; and to group directories first.
   le-dired

   ;; `dired': Filter dotfiles, omit files, and files listed in .gitignore
   le-dired-filter

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

   ;; Remember the last location within a file upon reopening. This is
   ;; beneficial for resuming work at the precise point where you previously
   ;; left off.
   le-saveplace

   ;; Track changes in the window configuration, allowing undoing actions such
   ;; as closing windows using `winner-undo'.
   le-winner

   ;; Recentf is an maintains a list of recently accessed files, making it
   ;; easier to reopen files you have worked on recently.
   le-recentf

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

   ;; Configures the consult-dir package, which quickly insert directory paths
   ;; into the minibuffer. It supports switching to recent directories, project
   ;; roots, bookmarks, or TRAMP hosts. Similar to tools like autojump or fasd,
   ;; it enables fast directory-jumping in Emacs.
   le-consult-dir

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

   ;; prescient.el is a library for sorting and filtering lists of candidates,
   ;; such as those presented by packages like Vertico or Corfu.
   le-prescient

   ;; When used with Vertico, prescient.el enhances minibuffer completion by
   ;; dynamically reordering candidates based on frequency and recency, making
   ;; it faster to select commonly used options while preserving consistent,
   ;; predictable filtering.
   le-vertico-prescient

   ;; When used with Corfu, prescient.el improves both in-buffer completions and
   ;; pop-up completion menus by making candidate ordering more predictable and
   ;; adaptive to recent usage, thus speeding up repeated selections.
   le-corfu-prescient

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

   ;; The bufferfile package provides helper functions to delete, rename, or
   ;; copy buffer files.
   le-bufferfile

   ;; Vim tab bar
   le-vim-tab-bar

   ;; Filetype: Markdown
   ;; The markdown-mode package provides a major mode for Emacs for syntax
   ;; highlighting, editing commands, and preview support for Markdown
   ;; documents. It supports core Markdown syntax as well as extensions like
   ;; GitHub Flavored Markdown (GFM).
   le-markdown-mode

   ;; Automatically generate or refresh the table of contents in Markdown files
   ;; using 'M-x markdown-toc-generate-or-refresh-toc'
   le-markdown-toc

   ;; Configure `org-mode' and `org-agenda'
   le-org

   ;; Org-appear temporarily reveals normally hidden elements (such as emphasis
   ;; markers, links, or entities) when the cursor enters them, and hides them
   ;; again when the cursor leaves.
   le-org-appear

   ;; This package provides functions to detects the indentation offset used in
   ;; existing source code files and automatically adjusts Emacs settings
   ;; accordingly, thereby simplifying the editing of files created in external
   ;; environments.
   le-dtrt-indent

   ;; The wgrep (Writable Grep) package enables you to convert a grep, rgrep,
   ;; Embark Export buffers into an editable interface. It allows in-place
   ;; modification of matched lines within the results buffer, which can then be
   ;; propagated back to the corresponding files upon confirmation. This
   ;; facilitates precise, bulk edits across multiple files efficiently,
   ;; eliminating the need to open each file individually, and effectively
   ;; transforms the grep results buffer into a controlled, multi-file editing
   ;; environment.
   le-wgrep

   ;; The `which-key' mode dynamically displays available keybindings in a popup
   ;; or dedicated buffer as a key sequence is entered. It facilitates discovery
   ;; and retention of key combinations by presenting context-sensitive
   ;; completions, thereby enhancing navigation through complex or highly
   ;; customized keymaps.
   le-which-key))

;;; Provide

(provide 'le-flavor-essential)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-flavor-essential.el ends here
