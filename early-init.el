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

;; Lightemacs provides a range of modules that can be selectively enabled or
;; disabled according to your preferences, with all modules ensuring packages
;; are loaded only when needed, enabling exceptionally fast, deferred startup.
(defvar lightemacs-modules '(;; Default modules
                             lem-group-default-modules

                             ;; Vim keybindings (DISABLED)
                             ;; --------------------------
                             ;; lem-group-evil

                             ;; Treesitter, Better syntax highlighting (DISABLED)
                             ;; -------------------------------------------------
                             ;; lem-treesit-auto
                             )
  "Modules that are enabled by default.")

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

(defvar lightemacs-cycle nil
  "If non-nil, enables cycling through candidates in supported plugins.
This enabled or disable cycling in plugins such as Vertico and Consult.
When nil, cycling is disabled, so selection stops at the first or last candidate
instead of wrapping around.")

(defcustom lightemacs-verbose nil
  "Enable displaying messages.
When set to non-nil, this option will cause messages to be shown during the
compilation process, providing feedback on the compilation status."
  :type 'boolean
  :group 'lightemacs)

(defvar lightemacs-user-emacs-directory user-emacs-directory
  "Directory beneath Lightemacs files are placed.")

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

;;; Variables

(setq load-prefer-newer t)
(setq use-package-always-ensure t)

;;; Internal variables

(defvar lightemacs--modules-dir (expand-file-name
                                 "lisp/lightemacs"
                                 lightemacs-user-emacs-directory))

;;; Load minimal-emacs.d early-init.el

(add-to-list 'load-path lightemacs--modules-dir)
(require 'lightemacs)

(lightemacs-load-init-file "early-init.el")

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
