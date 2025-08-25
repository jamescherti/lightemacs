;;; le-group-default-modules.el --- Group: Default modules -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Default modules.

;;; Code:

(require 'lightemacs)

;; Hides or abbreviates mode indicators in the Emacs
;; mode line for a cleaner display
(require 'le-diminish)

;; Modules enabled by default
;; --------------------------
;; This loaded the default theme specified
;; in the `lightemacs-theme-name' variable.
;;
;; The default theme tomorrow-night-deepblue is a
;; beautiful deep blue variant of the Tomorrow
;; Night theme, which is renowned for its elegant
;; color palette that is pleasing to the eyes.
(require 'le-theme)

;; Custom keybindings
(require 'le-keybindings)

;; Default settings (minibuffer, Emacs...)
;; Configure enhanced default settings, including
;; improved defaults, backup files, warnings to
;; ignore, a minibuffer depth indicator, window
;; behavior...
(require 'le-default-settings)

;; Gcmh optimizes
;; Emacs’ garbage collection behavior by adjusting
;; the garbage collection threshold dynamically.
;; Instead of collecting memory frequently during
;; normal editing, gcmh increases the threshold
;; while Emacs is idle, reducing interruptions and
;; improving perceived performance.
(require 'le-gcmh)

;; Update the ellipsis in `outline-mode' and
;; `outline-minor-mode' using `lightemacs-ellipsis'
(require 'le-outline)

;; Modern code folding based on indentation levels.
;; Automatically enabled for Python and Yaml.
(require 'le-outline-indent)

;; The stripspace Emacs package provides minor mode
;; that automatically removes trailing whitespace
;; and blank lines at the end of the buffer when
;; saving.
(require 'le-stripspace)

;; The persist-text-scale Emacs package provides
;; `persist-text-scale-mode', which ensures that
;; all adjustments made with `text-scale-increase'
;; and `text-scale-decrease' are persisted and
;; restored across sessions. As a result, the text
;; size in each buffer remains consistent, even
;; after restarting Emacs.
(require 'le-persist-text-scale)

;; Corfu enhances in-buffer completion by
;; displaying a compact popup with current
;; candidates, positioned either below or above the
;; point. Candidates can be selected by navigating
;; up or down.
(require 'le-corfu)

;; (Cape integrates with corfu)
;;
;; Cape, or Completion At Point Extensions, extends
;; the capabilities of in-buffer completion. It
;; integrates with Corfu or the default completion
;; UI, by providing additional backends through
;; completion-at-point-functions.
(require 'le-cape)

;; Configure `dired' to hide details such as file
;; ownership and permissions, and to group
;; directories first.
(require 'le-dired)

;; `dired': Filter dotfiles, omit files, and files
;; listed in .gitignore
(require 'le-dired-filter)

;; Preserve the minibuffer history between
;; sessions. It saves the history of inputs in the
;; minibuffer, such as commands, search strings,
;; and other prompts, to a file.
(require 'le-savehist)

;; Automatically insert matching delimiters (), {}...
(require 'le-elec-pair)

;; `show-paren-mode' highlights matching pairs of
;; parentheses and other paired characters, improving
;; code readability and helping to quickly identify
;; unbalanced expressions.
(require 'le-paren)

;; Remember the last location within a file upon
;; reopening. This is beneficial for resuming work
;; at the precise point where you previously left
;; off.
(require 'le-saveplace)

;; Track changes in the window configuration,
;; allowing undoing actions such as closing windows
;; using `winner-undo'.
(require 'le-winner)

;; Recentf is an maintains a list of recently
;; accessed files, making it easier to reopen files
;; you have worked on recently.
(require 'le-recentf)

;; (Vertico, Consult, and Embark collectively
;; enhance Emacs' completion and navigation
;; capabilities.)
;;
;; Vertico provides a vertical completion
;; interface, making it easier to navigate and
;; select from completion candidates (e.g., when
;; M-x is pressed).
(require 'le-vertico)

;; (Vertico, Consult, and Embark collectively
;; enhance Emacs' completion and navigation
;; capabilities.)
;;
;; Consult offers a suite of commands for efficient
;; searching, previewing, and interacting with
;; buffers, file contents, and more, improving
;; various tasks.
(require 'le-consult)

;; Configures the consult-dir package, which quickly insert directory paths into
;; the minibuffer. It supports switching to recent directories, project roots,
;; bookmarks, or TRAMP hosts. Similar to tools like autojump or fasd, it
;; enables fast directory-jumping in Emacs.
(require 'le-consult-dir)

;; (Vertico, Consult, and Embark collectively
;; enhance Emacs' completion and navigation
;; capabilities.)
;;
;; Embark integrates with these tools to provide
;; context-sensitive actions and quick access to
;; commands based on the current selection, further
;; improving user efficiency and workflow within
;; Emacs. Together, they create a cohesive and
;; powerful environment for managing completions
;; and interactions.
(require 'le-embark)

;; Integration between Embark and Consult
(require 'le-embark-consult)

;; Dumb-jump/Dumber-jump provide context-aware 'go to
;; definition' functionality for 50+ programming
;; languages without requiring a language server. It
;; works by using simple heuristics and regular
;; expression searches to locate the definitions of
;; functions, variables, and symbols across project
;; files. Unlike more sophisticated language-aware
;; tools, `dumb-jump' does not parse code
;; semantically, which makes it lightweight and fast,
;; but sometimes less precise. It integrates with
;; popular navigation packages like `xref', allowing
;; implementations with minimal configuration. users
;; to jump to definitions, references, or
(require 'le-dumb-jump)

;; The expand-region package grows your selection
;; step by step with 'C-=', starting from a word
;; and expanding to larger units like a sentence,
;; paragraph, or entire function.
(require 'le-expand-region)

;; prescient.el is a library for sorting and
;; filtering lists of candidates, such as those
;; presented by packages like Vertico or Corfu.
(require 'le-prescient)

;; When used with Vertico, prescient.el enhances
;; minibuffer completion by dynamically reordering
;; candidates based on frequency and recency,
;; making it faster to select commonly used options
;; while preserving consistent, predictable
;; filtering.
(require 'le-vertico-prescient)

;; When used with Corfu, prescient.el improves both
;; in-buffer completions and pop-up completion
;; menus by making candidate ordering more
;; predictable and adaptive to recent usage, thus
;; speeding up repeated selections.
(require 'le-corfu-prescient)

;; Enable flexible, unordered matching (Orderless)
;; for Vertico. This allows typing multiple
;; parts of a candidate in any order, making it
;; easier to find functions, variables, or files even
;; if you only remember fragments.
;;
;; Example in Vertico:
;; - Typing "main test" matches "test_main.py"
;; - Typing "read me" matches "README.md"
(require 'le-orderless)

;; Marginalia enriches minibuffer completions with
;; contextual annotations.
;; It Enhances Vertico by adding rich annotations
;; to completion candidates, such as file sizes,
;; documentation, or metadata.
(require 'le-marginalia)

;; Autorevert is a feature that automatically
;; updates the contents of a buffer to reflect
;; changes made to the underlying file on disk.
(require 'le-autorevert)

;; The undo-fu package is a lightweight wrapper
;; around Emacs' built-in undo system, providing
;; more convenient undo/redo functionality while
;; preserving access to the full undo history.
(require 'le-undo-fu)

;; The undo-fu-session package complements undo-fu
;; by enabling the saving and restoration of undo
;; history across Emacs sessions, even after
;; restarting.
(require 'le-undo-fu-session)

;; The bufferfile package provides helper functions
;; to delete, rename, or copy buffer files.
(require 'le-bufferfile)

;; Vim tab bar
(require 'le-vim-tab-bar)

;; Filetype: Markdown
;; The markdown-mode package provides a major mode
;; for Emacs for syntax highlighting, editing
;; commands, and preview support for Markdown
;; documents. It supports core Markdown syntax as
;; well as extensions like GitHub Flavored Markdown
;; (GFM).
(require 'le-markdown-mode)

;; Automatically generate or refresh the table of
;; contents in Markdown files using
;; 'M-x markdown-toc-generate-or-refresh-toc'
(require 'le-markdown-toc)

;; Configure `org-mode' and `org-agenda'
(require 'le-org)

;; Org-appear temporarily reveals normally hidden
;; elements (such as emphasis markers, links, or
;; entities) when the cursor enters them, and hides
;; them again when the cursor leaves.
(require 'le-org-appear)

;; This package provides functions to detects the
;; indentation offset used in existing source code
;; files and automatically adjusts Emacs settings
;; accordingly, thereby simplifying the editing of
;; files created in external environments.
(require 'le-dtrt-indent)

;; The wgrep (Writable Grep) package enables you to convert a grep, rgrep,
;; Embark Export buffers into an editable interface. It allows in-place
;; modification of matched lines within the results buffer, which can then be
;; propagated back to the corresponding files upon confirmation. This
;; facilitates precise, bulk edits across multiple files efficiently,
;; eliminating the need to open each file individually, and effectively
;; transforms the grep results buffer into a controlled, multi-file editing
;; environment.
(require 'le-wgrep)

;; The `which-key' mode dynamically displays available keybindings in a popup or
;; dedicated buffer as a key sequence is entered. It facilitates discovery and
;; retention of key combinations by presenting context-sensitive completions,
;; thereby enhancing navigation through complex or highly customized keymaps.
(require 'le-which-key)

;; Show line numbers on the left side of the buffer. The numbers update
;; automatically as you add, remove, or scroll lines, but they don’t change the
;; actual text.
(require 'le-display-line-numbers)

;; Avy is an Emacs package that provides a fast and efficient method for
;; navigating to visible text in a buffer by jumping directly to characters,
;; words, or lines. It allows the user to type a sequence of characters or
;; select from highlighted targets to move the cursor instantly, reducing the
;; need for repetitive cursor motions or scrolling.
(require 'le-avy)

;; Ace Window provides a fast and efficient method for switching between windows
;; in a frame. Instead of cycling through windows sequentially or using more
;; cumbersome key sequences, Ace Window displays a single-letter label on each
;; visible window, allowing the user to jump directly to a target window by
;; pressing the corresponding key.
(require 'ace-window)

;;; Provide

(provide 'le-group-default-modules)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-group-default-modules.el ends here
