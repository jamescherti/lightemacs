;;; lem-group-default-modules.el --- Group: Default modules -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Default modules.

;;; Code:

;;; Evil

(require 'lightemacs)

(lightemacs-load-modules '(;; Modules enabled by default
                           ;; --------------------------
                           ;; This loaded the default theme specified
                           ;; in the `lightemacs-default-theme' variable.
                           ;;
                           ;; The default theme tomorrow-night-deepblue is a
                           ;; beautiful deep blue variant of the Tomorrow
                           ;; Night theme, which is renowned for its elegant
                           ;; color palette that is pleasing to the eyes.
                           lem-default-theme

                           ;; Custom keybindings
                           lem-keybindings

                           ;; Default settings (minibuffer, Emacs...)
                           ;; Configure enhanced default settings, including
                           ;; improved defaults, backup files, warnings to
                           ;; ignore, a minibuffer depth indicator, window
                           ;; behavior...
                           lem-default-settings

                           ;; Gcmh optimizes
                           ;; Emacs’ garbage collection behavior by adjusting
                           ;; the garbage collection threshold dynamically.
                           ;; Instead of collecting memory frequently during
                           ;; normal editing, gcmh increases the threshold
                           ;; while Emacs is idle, reducing interruptions and
                           ;; improving perceived performance.
                           lem-gcmh

                           ;; Modern code folding based on indentation levels.
                           ;; Automatically enabled for Python and Yaml.
                           lem-outline-indent

                           ;; Update the ellipsis in `outline-mode' and
                           ;; `outline-minor-mode' using `lightemacs-ellipsis'
                           lem-outline

                           ;; The stripspace Emacs package provides minor mode
                           ;; that automatically removes trailing whitespace
                           ;; and blank lines at the end of the buffer when
                           ;; saving.
                           lem-stripspace

                           ;; The yasnippet package provides a template system
                           ;; that enhances text editing by enabling users to
                           ;; define and use snippets, which are predefined
                           ;; templates of code or text.
                           lem-yasnippet

                           ;; The yasnippet-snippets package with a
                           ;; comprehensive collection of bundled templates
                           ;; for numerous programming and markup languages,
                           ;; including C, C++, C#, Perl, Python, Ruby, SQL,
                           ;; LaTeX, HTML, CSS...
                           lem-yasnippet-snippets

                           ;; The persist-text-scale Emacs package provides
                           ;; `persist-text-scale-mode', which ensures that
                           ;; all adjustments made with `text-scale-increase'
                           ;; and `text-scale-decrease' are persisted and
                           ;; restored across sessions. As a result, the text
                           ;; size in each buffer remains consistent, even
                           ;; after restarting Emacs.
                           lem-persist-text-scale

                           ;; Corfu enhances in-buffer completion by
                           ;; displaying a compact popup with current
                           ;; candidates, positioned either below or above the
                           ;; point. Candidates can be selected by navigating
                           ;; up or down.
                           lem-corfu

                           ;; (Cape integrates with corfu)
                           ;;
                           ;; Cape, or Completion At Point Extensions, extends
                           ;; the capabilities of in-buffer completion. It
                           ;; integrates with Corfu or the default completion
                           ;; UI, by providing additional backends through
                           ;; completion-at-point-functions.
                           lem-cape

                           ;; Configure `dired' to hide details such as file
                           ;; ownership and permissions, and to group
                           ;; directories first.
                           lem-dired

                           ;; `dired': Filter dotfiles, omit files, and files
                           ;; listed in .gitignore
                           lem-dired-filter

                           ;; Preserve the minibuffer history between
                           ;; sessions. It saves the history of inputs in the
                           ;; minibuffer, such as commands, search strings,
                           ;; and other prompts, to a file.
                           lem-savehist

                           ;; Automatically insert matching delimiters (), {}...
                           lem-elec-pair

                           ;; `show-paren-mode' highlights matching pairs of
                           ;; parentheses and other paired characters, improving
                           ;; code readability and helping to quickly identify
                           ;; unbalanced expressions.
                           lem-paren

                           ;; Remember the last location within a file upon
                           ;; reopening. This is beneficial for resuming work
                           ;; at the precise point where you previously left
                           ;; off.
                           lem-saveplace

                           ;; Recentf is an maintains a list of recently
                           ;; accessed files, making it easier to reopen files
                           ;; you have worked on recently.
                           lem-recentf

                           ;; (Vertico, Consult, and Embark collectively
                           ;; enhance Emacs' completion and navigation
                           ;; capabilities.)
                           ;;
                           ;; Vertico provides a vertical completion
                           ;; interface, making it easier to navigate and
                           ;; select from completion candidates (e.g., when
                           ;; M-x is pressed).
                           lem-vertico

                           ;; (Vertico, Consult, and Embark collectively
                           ;; enhance Emacs' completion and navigation
                           ;; capabilities.)
                           ;;
                           ;; Consult offers a suite of commands for efficient
                           ;; searching, previewing, and interacting with
                           ;; buffers, file contents, and more, improving
                           ;; various tasks.
                           lem-consult

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
                           lem-embark

                           ;; Integration between Embark and Consult
                           lem-embark-consult

                           ;; The expand-region package grows your selection
                           ;; step by step with 'C-=', starting from a word
                           ;; and expanding to larger units like a sentence,
                           ;; paragraph, or entire function.
                           lem-expand-region

                           ;; prescient.el is a library for sorting and
                           ;; filtering lists of candidates, such as those
                           ;; presented by packages like Vertico or Corfu.
                           lem-prescient

                           ;; When used with Vertico, prescient.el enhances
                           ;; minibuffer completion by dynamically reordering
                           ;; candidates based on frequency and recency,
                           ;; making it faster to select commonly used options
                           ;; while preserving consistent, predictable
                           ;; filtering.
                           lem-vertico-prescient

                           ;; When used with Corfu, prescient.el improves both
                           ;; in-buffer completions and pop-up completion
                           ;; menus by making candidate ordering more
                           ;; predictable and adaptive to recent usage, thus
                           ;; speeding up repeated selections.
                           lem-corfu-prescient

                           ;; Marginalia enriches minibuffer completions with
                           ;; contextual annotations.
                           ;; It Enhances Vertico by adding rich annotations
                           ;; to completion candidates, such as file sizes,
                           ;; documentation, or metadata.
                           lem-marginalia

                           ;; Autorevert is a feature that automatically
                           ;; updates the contents of a buffer to reflect
                           ;; changes made to the underlying file on disk.
                           lem-autorevert

                           ;; The undo-fu package is a lightweight wrapper
                           ;; around Emacs' built-in undo system, providing
                           ;; more convenient undo/redo functionality while
                           ;; preserving access to the full undo history.
                           lem-undo-fu

                           ;; The undo-fu-session package complements undo-fu
                           ;; by enabling the saving and restoration of undo
                           ;; history across Emacs sessions, even after
                           ;; restarting.
                           lem-undo-fu-session

                           ;; The bufferfile package provides helper functions
                           ;; to delete, rename, or copy buffer files.
                           lem-bufferfile

                           ;; Vim tab bar
                           lem-vim-tab-bar

                           ;; Filetype: Markdown
                           ;; The markdown-mode package provides a major mode
                           ;; for Emacs for syntax highlighting, editing
                           ;; commands, and preview support for Markdown
                           ;; documents. It supports core Markdown syntax as
                           ;; well as extensions like GitHub Flavored Markdown
                           ;; (GFM).
                           lem-markdown-mode

                           ;; Automatically generate or refresh the table of
                           ;; contents in Markdown files using
                           ;; 'M-x markdown-toc-generate-or-refresh-toc'
                           lem-markdown-toc

                           ;; Configure `org-mode' and `org-agenda'
                           lem-org

                           ;; Org-appear temporarily reveals normally hidden
                           ;; elements (such as emphasis markers, links, or
                           ;; entities) when the cursor enters them, and hides
                           ;; them again when the cursor leaves.
                           lem-org-appear

                           ;; Filetype: Emacs Lisp
                           ;;
                           ;; 1. Enable `highlight-defined-mode', a minor mode
                           ;; that highlights defined Emacs Lisp symbols in
                           ;; `emacs-lisp-mode' buffers.
                           ;;
                           ;; 2. Enable `page-break-lines-mode', a minor mode
                           ;; that visually replaces ASCII form-feed
                           ;; characters (typically `^L`) with horizontal line
                           ;; separators in buffers.
                           ;;
                           ;; 3. Enable `aggressive-indent-mode', a minor mode
                           ;; that Elisp code remains consistently indented.
                           ;; It automatically reindents after every
                           ;; modification, providing greater reliability than
                           ;; `electric-indent-mode'.
                           lem-group-emacs-lisp

                           ;; This package provides functions to detects the
                           ;; indentation offset used in existing source code
                           ;; files and automatically adjusts Emacs settings
                           ;; accordingly, thereby simplifying the editing of
                           ;; files created in external environments.
                           lem-dtrt-indent

                           ;; Compile-angel speeds up Emacs by ensuring that
                           ;; all Elisp libraries are both byte-compiled and
                           ;; native-compiled.
                           lem-compile-angel))

(provide 'group-default-modules)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lem-group-default-modules.el ends here
