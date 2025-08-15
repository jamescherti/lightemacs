;;; early-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The Lightemacs project is an Emacs framework.

;;; Code:

;;; Global variables

(defvar lightemacs-default-theme 'tomorrow-night-deepblue
  "Name of the default theme to load, if available.
Set this to nil to disable early theme loading.")

(defvar lightemacs-ellipsis " ▼"
  "String used to indicate folded sections in Org-mode and Outline-mode.
This ellipsis appears at the end of a heading or section that has been
collapsed. It provides a visual cue that more content is hidden. You can
customize this variable to use a different character or string (such as '…',
'▶', or other Unicode symbols) to match your visual preference or theme. This
variable is buffer-local in Org-mode and Outline-mode, affecting only the
display of folded text.")

;; Modules enabled by default:
(defvar lightemacs-modules '(;; Vim keybindings (Disabled):
                             ;; ---------------------------
                             ;; group-evil

                             ;; Treesitter (Better syntax highlighting)
                             ;; ---------------------------------------
                             ;; treesit-auto

                             ;; Modules enabled by default
                             ;; --------------------------
                             ;; This loaded the default theme specified
                             ;; in the `lightemacs-default-theme' variable.
                             ;;
                             ;; The default theme tomorrow-night-deepblue is a
                             ;; beautiful deep blue variant of the Tomorrow
                             ;; Night theme, which is renowned for its elegant
                             ;; color palette that is pleasing to the eyes.
                             default-theme

                             ;; Default settings (minibuffer, Emacs...)
                             ;; TODO: Add it to README.md
                             default-settings

                             ;; Gcmh optimizes
                             ;; Emacs’ garbage collection behavior by adjusting
                             ;; the garbage collection threshold dynamically.
                             ;; Instead of collecting memory frequently during
                             ;; normal editing, gcmh increases the threshold
                             ;; while Emacs is idle, reducing interruptions and
                             ;; improving perceived performance.
                             gcmh

                             ;; Modern code folding based on indentation levels.
                             ;; Automatically enabled for Python and Yaml.
                             outline-indent

                             ;; The indent-bars package enhances code
                             ;; readability by providing visual indentation
                             ;; guides, optimized for speed and customization.
                             ;; (Useful for Yaml or Python files.)
                             indent-bars

                             ;; Update the ellipsis in `outline-mode' and
                             ;; `outline-minor-mode' using `lightemacs-ellipsis'
                             outline

                             ;; The stripspace Emacs package provides minor mode
                             ;; that automatically removes trailing whitespace
                             ;; and blank lines at the end of the buffer when
                             ;; saving.
                             stripspace

                             ;; The yasnippet package provides a template system
                             ;; that enhances text editing by enabling users to
                             ;; define and use snippets, which are predefined
                             ;; templates of code or text.
                             yasnippet

                             ;; The yasnippet-snippets package with a
                             ;; comprehensive collection of bundled templates
                             ;; for numerous programming and markup languages,
                             ;; including C, C++, C#, Perl, Python, Ruby, SQL,
                             ;; LaTeX, HTML, CSS...
                             yasnippet-snippets

                             ;; The persist-text-scale Emacs package provides
                             ;; `persist-text-scale-mode', which ensures that
                             ;; all adjustments made with `text-scale-increase'
                             ;; and `text-scale-decrease' are persisted and
                             ;; restored across sessions. As a result, the text
                             ;; size in each buffer remains consistent, even
                             ;; after restarting Emacs.
                             persist-text-scale

                             ;; Corfu enhances in-buffer completion by
                             ;; displaying a compact popup with current
                             ;; candidates, positioned either below or above the
                             ;; point. Candidates can be selected by navigating
                             ;; up or down.
                             corfu

                             ;; (Cape integrates with corfu)
                             ;;
                             ;; Cape, or Completion At Point Extensions, extends
                             ;; the capabilities of in-buffer completion. It
                             ;; integrates with Corfu or the default completion
                             ;; UI, by providing additional backends through
                             ;; completion-at-point-functions.
                             cape

                             ;; Configure `dired' to hide details such as file
                             ;; ownership and permissions, and to group
                             ;; directories first.
                             dired

                             ;; `dired': Filter dotfiles, omit files, and files
                             ;; listed in .gitignore
                             dired-filter

                             ;; Preserve the minibuffer history between
                             ;; sessions. It saves the history of inputs in the
                             ;; minibuffer, such as commands, search strings,
                             ;; and other prompts, to a file.
                             savehist

                             ;; Remember the last location within a file upon
                             ;; reopening. This is beneficial for resuming work
                             ;; at the precise point where you previously left
                             ;; off.
                             saveplace

                             ;; Recentf is an maintains a list of recently
                             ;; accessed files, making it easier to reopen files
                             ;; you have worked on recently.
                             recentf

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
                             embark

                             ;; (Vertico, Consult, and Embark collectively
                             ;; enhance Emacs' completion and navigation
                             ;; capabilities.)
                             ;;
                             ;; Vertico provides a vertical completion
                             ;; interface, making it easier to navigate and
                             ;; select from completion candidates (e.g., when
                             ;; M-x is pressed).
                             vertico

                             ;; (Vertico, Consult, and Embark collectively
                             ;; enhance Emacs' completion and navigation
                             ;; capabilities.)
                             ;;
                             ;; Consult offers a suite of commands for efficient
                             ;; searching, previewing, and interacting with
                             ;; buffers, file contents, and more, improving
                             ;; various tasks.
                             consult

                             ;; prescient.el is a library for sorting and
                             ;; filtering lists of candidates, such as those
                             ;; presented by packages like Vertico or Corfu.
                             prescient

                             ;; When used with Vertico, prescient.el enhances
                             ;; minibuffer completion by dynamically reordering
                             ;; candidates based on frequency and recency,
                             ;; making it faster to select commonly used options
                             ;; while preserving consistent, predictable
                             ;; filtering.
                             prescient-vertico

                             ;; When used with Corfu, prescient.el improves both
                             ;; in-buffer completions and pop-up completion
                             ;; menus by making candidate ordering more
                             ;; predictable and adaptive to recent usage, thus
                             ;; speeding up repeated selections.
                             prescient-corfu

                             ;; Marginalia enriches minibuffer completions with
                             ;; contextual annotations.
                             ;; It Enhances Vertico by adding rich annotations
                             ;; to completion candidates, such as file sizes,
                             ;; documentation, or metadata.
                             marginalia

                             ;; Autorevert is a feature that automatically
                             ;; updates the contents of a buffer to reflect
                             ;; changes made to the underlying file on disk.
                             autorevert

                             ;; Custom keybindings
                             keybindings

                             ;; The undo-fu package is a lightweight wrapper
                             ;; around Emacs' built-in undo system, providing
                             ;; more convenient undo/redo functionality while
                             ;; preserving access to the full undo history.
                             undo-fu

                             ;; The undo-fu-session package complements undo-fu
                             ;; by enabling the saving and restoration of undo
                             ;; history across Emacs sessions, even after
                             ;; restarting.
                             undo-fu-session

                             ;; Vim tab bar
                             vim-tab-bar

                             ;; Filetype: Markdown
                             ;; The markdown-mode package provides a major mode
                             ;; for Emacs for syntax highlighting, editing
                             ;; commands, and preview support for Markdown
                             ;; documents. It supports core Markdown syntax as
                             ;; well as extensions like GitHub Flavored Markdown
                             ;; (GFM).
                             markdown-mode

                             ;; Automatically generate or refresh the table of
                             ;; contents in Markdown files using
                             ;; 'M-x markdown-toc-generate-or-refresh-toc'
                             markdown-toc

                             ;; Configure `org-mode' and `org-agenda'
                             org

                             ;; Org-appear temporarily reveals normally hidden
                             ;; elements (such as emphasis markers, links, or
                             ;; entities) when the cursor enters them, and hides
                             ;; them again when the cursor leaves.
                             org-appear

                             ;; Filetype: Elisp
                             ;; `aggressive-indent-mode' is a minor mode that
                             ;; Elisp code remains consistently indented. It
                             ;; automatically reindents after every
                             ;; modification, providing greater reliability than
                             ;; `electric-indent-mode'.
                             aggressive-indent

                             ;; Compile-angel speeds up Emacs by ensuring that
                             ;; all Elisp libraries are both byte-compiled and
                             ;; native-compiled.
                             compile-angel)
  "Modules that are enabled by default.")

(defvar lightemacs-user-emacs-directory user-emacs-directory
  "Directory beneath lightemacs files are placed.")

;;; Reduce cluttering

;; Emacs, by default, stores various configuration files, caches, backups, and
;; other data in the ~/.emacs.d directory. Over time, this directory can become
;; cluttered with numerous files, making it difficult to manage and maintain.
;;
;; A common solution to this issue is installing the no-littering package;
;; however, this package is not essential.
;;
;; An alternative lightweight approach is to simply change the default
;; ~/.emacs.d directory to ~/.emacs.d/var/, which will contain all the files
;; that Emacs typically stores in the base directory.
(setq user-emacs-directory (expand-file-name "var/" lightemacs-user-emacs-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq minimal-emacs-user-directory lightemacs-user-emacs-directory)

;;; Functions

(defun lightemacs--modules-dir ()
  "Return the path to the modules directory."
  (expand-file-name "lisp/modules"
                    lightemacs-user-emacs-directory))

(defun lightemacs--load-modules (lightemacs-modules)
  "Load all modules listed in LIGHTEMACS-MODULES."
  (let ((modules-dir (lightemacs--modules-dir)))
    (dolist (feature-symbol lightemacs-modules)
      (let* ((feature-str (format "mod-%s" feature-symbol))
             (feature-symbol (intern feature-str))
             (module-file (expand-file-name (format "%s.el" feature-str)
                                            modules-dir)))
        (when init-file-debug
          (message "[LIGHTEMACS LOAD MODULE] %s" module-file))
        (require feature-symbol module-file)
        ;; (if (file-exists-p module-file)
        ;;     (require feature-symbol module-file)
        ;;   (message "The module '%s' could not be found" module-file))
        ))))

(defun lightemacs--load-default-theme ()
  "Load the theme defined in `lightemacs-default-theme' if it is installed."
  (when (and lightemacs-default-theme
             (member lightemacs-default-theme (custom-available-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme lightemacs-default-theme t)))

(defun lightemacs--load-init-file (filename)
  "Load a file of Lisp init file named FILENAME."
  (load (expand-file-name (format "init/%s" filename)
                          (lightemacs--modules-dir))
        nil
        (not (bound-and-true-p init-file-debug))
        'nosuffix))

;;; Other parameters

;; Allow Emacs to upgrade built-in packages, such as Org mode
(setq package-install-upgrade-built-in t)

;;; Load minimal-emacs.d early-init.el

(lightemacs--load-init-file "early-init.el")

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
