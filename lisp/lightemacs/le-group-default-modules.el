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

(lightemacs-load-modules '(;; Hides or abbreviates mode indicators in the Emacs
                           ;; mode line for a cleaner display
                           le-diminish

                           ;; Compile-angel speeds up Emacs by ensuring that
                           ;; all Elisp libraries are both byte-compiled and
                           ;; native-compiled.
                           ;;
                           ;; NOTE: Load this first, before any other packages,
                           ;; to ensure proper initialization.
                           ;; le-compile-angel

                           ;; Modules enabled by default
                           ;; --------------------------
                           ;; This loaded the default theme specified
                           ;; in the `lightemacs-theme-name' variable.
                           ;;
                           ;; The default theme tomorrow-night-deepblue is a
                           ;; beautiful deep blue variant of the Tomorrow
                           ;; Night theme, which is renowned for its elegant
                           ;; color palette that is pleasing to the eyes.
                           le-theme

                           ;; Custom keybindings
                           le-keybindings

                           ;; Default settings (minibuffer, Emacs...)
                           ;; Configure enhanced default settings, including
                           ;; improved defaults, backup files, warnings to
                           ;; ignore, a minibuffer depth indicator, window
                           ;; behavior...
                           le-default-settings

                           ;; Gcmh optimizes
                           ;; Emacs’ garbage collection behavior by adjusting
                           ;; the garbage collection threshold dynamically.
                           ;; Instead of collecting memory frequently during
                           ;; normal editing, gcmh increases the threshold
                           ;; while Emacs is idle, reducing interruptions and
                           ;; improving perceived performance.
                           le-gcmh

                           ;; Update the ellipsis in `outline-mode' and
                           ;; `outline-minor-mode' using `lightemacs-ellipsis'
                           le-outline

                           ;; Modern code folding based on indentation levels.
                           ;; Automatically enabled for Python and Yaml.
                           le-outline-indent

                           ;; The stripspace Emacs package provides minor mode
                           ;; that automatically removes trailing whitespace
                           ;; and blank lines at the end of the buffer when
                           ;; saving.
                           le-stripspace

                           ;; The persist-text-scale Emacs package provides
                           ;; `persist-text-scale-mode', which ensures that
                           ;; all adjustments made with `text-scale-increase'
                           ;; and `text-scale-decrease' are persisted and
                           ;; restored across sessions. As a result, the text
                           ;; size in each buffer remains consistent, even
                           ;; after restarting Emacs.
                           le-persist-text-scale

                           ;; Corfu enhances in-buffer completion by
                           ;; displaying a compact popup with current
                           ;; candidates, positioned either below or above the
                           ;; point. Candidates can be selected by navigating
                           ;; up or down.
                           le-corfu

                           ;; (Cape integrates with corfu)
                           ;;
                           ;; Cape, or Completion At Point Extensions, extends
                           ;; the capabilities of in-buffer completion. It
                           ;; integrates with Corfu or the default completion
                           ;; UI, by providing additional backends through
                           ;; completion-at-point-functions.
                           le-cape

                           ;; Configure `dired' to hide details such as file
                           ;; ownership and permissions, and to group
                           ;; directories first.
                           le-dired

                           ;; `dired': Filter dotfiles, omit files, and files
                           ;; listed in .gitignore
                           le-dired-filter

                           ;; Preserve the minibuffer history between
                           ;; sessions. It saves the history of inputs in the
                           ;; minibuffer, such as commands, search strings,
                           ;; and other prompts, to a file.
                           le-savehist

                           ;; Automatically insert matching delimiters (), {}...
                           le-elec-pair

                           ;; `show-paren-mode' highlights matching pairs of
                           ;; parentheses and other paired characters, improving
                           ;; code readability and helping to quickly identify
                           ;; unbalanced expressions.
                           le-paren

                           ;; Remember the last location within a file upon
                           ;; reopening. This is beneficial for resuming work
                           ;; at the precise point where you previously left
                           ;; off.
                           le-saveplace

                           ;; Track changes in the window configuration,
                           ;; allowing undoing actions such as closing windows
                           ;; using `winner-undo'.
                           le-winner

                           ;; Recentf is an maintains a list of recently
                           ;; accessed files, making it easier to reopen files
                           ;; you have worked on recently.
                           le-recentf

                           ;; (Vertico, Consult, and Embark collectively
                           ;; enhance Emacs' completion and navigation
                           ;; capabilities.)
                           ;;
                           ;; Vertico provides a vertical completion
                           ;; interface, making it easier to navigate and
                           ;; select from completion candidates (e.g., when
                           ;; M-x is pressed).
                           le-vertico

                           ;; (Vertico, Consult, and Embark collectively
                           ;; enhance Emacs' completion and navigation
                           ;; capabilities.)
                           ;;
                           ;; Consult offers a suite of commands for efficient
                           ;; searching, previewing, and interacting with
                           ;; buffers, file contents, and more, improving
                           ;; various tasks.
                           le-consult

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
                           le-embark

                           ;; Integration between Embark and Consult
                           le-embark-consult

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
                           le-dumb-jump

                           ;; The expand-region package grows your selection
                           ;; step by step with 'C-=', starting from a word
                           ;; and expanding to larger units like a sentence,
                           ;; paragraph, or entire function.
                           ;; le-expand-region

                           ;; prescient.el is a library for sorting and
                           ;; filtering lists of candidates, such as those
                           ;; presented by packages like Vertico or Corfu.
                           le-prescient

                           ;; When used with Vertico, prescient.el enhances
                           ;; minibuffer completion by dynamically reordering
                           ;; candidates based on frequency and recency,
                           ;; making it faster to select commonly used options
                           ;; while preserving consistent, predictable
                           ;; filtering.
                           le-vertico-prescient

                           ;; When used with Corfu, prescient.el improves both
                           ;; in-buffer completions and pop-up completion
                           ;; menus by making candidate ordering more
                           ;; predictable and adaptive to recent usage, thus
                           ;; speeding up repeated selections.
                           le-corfu-prescient

                           ;; Enable flexible, unordered matching (Orderless)
                           ;; for Vertico. This allows typing multiple
                           ;; parts of a candidate in any order, making it
                           ;; easier to find functions, variables, or files even
                           ;; if you only remember fragments.
                           ;;
                           ;; Example in Vertico:
                           ;; - Typing "main test" matches "test_main.py"
                           ;; - Typing "read me" matches "README.md"
                           le-orderless

                           ;; Marginalia enriches minibuffer completions with
                           ;; contextual annotations.
                           ;; It Enhances Vertico by adding rich annotations
                           ;; to completion candidates, such as file sizes,
                           ;; documentation, or metadata.
                           le-marginalia

                           ;; Autorevert is a feature that automatically
                           ;; updates the contents of a buffer to reflect
                           ;; changes made to the underlying file on disk.
                           le-autorevert

                           ;; The undo-fu package is a lightweight wrapper
                           ;; around Emacs' built-in undo system, providing
                           ;; more convenient undo/redo functionality while
                           ;; preserving access to the full undo history.
                           le-undo-fu

                           ;; The undo-fu-session package complements undo-fu
                           ;; by enabling the saving and restoration of undo
                           ;; history across Emacs sessions, even after
                           ;; restarting.
                           le-undo-fu-session

                           ;; The bufferfile package provides helper functions
                           ;; to delete, rename, or copy buffer files.
                           le-bufferfile

                           ;; Vim tab bar
                           le-vim-tab-bar

                           ;; Filetype: Markdown
                           ;; The markdown-mode package provides a major mode
                           ;; for Emacs for syntax highlighting, editing
                           ;; commands, and preview support for Markdown
                           ;; documents. It supports core Markdown syntax as
                           ;; well as extensions like GitHub Flavored Markdown
                           ;; (GFM).
                           le-markdown-mode

                           ;; Configure `yaml-mode' if Tree-sitter's
                           ;; `yaml-ts-mode' is not available.
                           ;; le-group-yaml

                           ;; Automatically generate or refresh the table of
                           ;; contents in Markdown files using
                           ;; 'M-x markdown-toc-generate-or-refresh-toc'
                           le-markdown-toc

                           ;; Configure `org-mode' and `org-agenda'
                           le-org

                           ;; Org-appear temporarily reveals normally hidden
                           ;; elements (such as emphasis markers, links, or
                           ;; entities) when the cursor enters them, and hides
                           ;; them again when the cursor leaves.
                           le-org-appear

                           ;; This package provides functions to detects the
                           ;; indentation offset used in existing source code
                           ;; files and automatically adjusts Emacs settings
                           ;; accordingly, thereby simplifying the editing of
                           ;; files created in external environments.
                           le-dtrt-indent

                           ;; The yasnippet package provides a template system
                           ;; that enhances text editing by enabling users to
                           ;; define and use snippets, which are predefined
                           ;; templates of code or text.
                           ;; le-yasnippet

                           ;; The yasnippet-snippets package with a
                           ;; comprehensive collection of bundled templates
                           ;; for numerous programming and markup languages,
                           ;; including C, C++, C#, Perl, Python, Ruby, SQL,
                           ;; LaTeX, HTML, CSS...
                           ;; le-yasnippet-snippets

                           ;; Apheleia is a package that runs code formatters
                           ;; asynchronously without disrupting the cursor
                           ;; position. Code formatters like Shfmt, Black and
                           ;; Prettier ensure consistency and improve
                           ;; collaboration by automating formatting, but
                           ;; running them on save can introduce latency (e.g.,
                           ;; Black takes around 200ms on an empty file) and
                           ;; unpredictably move the cursor when modifying
                           ;; nearby text.
                           ;; le-apheleia
                           ))

(provide 'le-group-default-modules)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-group-default-modules.el ends here
