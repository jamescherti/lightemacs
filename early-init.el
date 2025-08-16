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
                             group-default-modules

                             ;; Vim keybindings (DISABLED)
                             ;; --------------------------
                             ;; group-evil

                             ;; Treesitter, Better syntax highlighting (DISABLED)
                             ;; -------------------------------------------------
                             ;; mod-treesit-auto
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

;;; Functions

(defun lightemacs--modules-dir ()
  "Return the path to the modules directory."
  (expand-file-name "lisp/modules"
                    lightemacs-user-emacs-directory))

(defun lightemacs-load-modules (lightemacs-modules)
  "Load all modules listed in LIGHTEMACS-MODULES."
  (let ((modules-dir (lightemacs--modules-dir)))
    (dolist (feature-symbol lightemacs-modules)
      (let* ((feature-str (format "%s" feature-symbol))
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

(defmacro lightemacs-verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  `(progn
     (when lightemacs-verbose
       (message (concat "[lightemacs] " ,(car args)) ,@(cdr args)))))

;;; Other parameters

;;; Load minimal-emacs.d early-init.el

(lightemacs--load-init-file "early-init.el")

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
