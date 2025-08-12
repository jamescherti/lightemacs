;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

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

(defvar lightemacs-modules '(;; Compile-angel speeds up Emacs by ensuring that
                             ;; all Elisp libraries are both byte-compiled and
                             ;; native-compiled.
                             compile-angel

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

                             ;; Custom keybindings
                             keybindings

                             ;; Vim keybindings
                             evil
                             evil-toggle-comment
                             evil-snipe
                             evil-surround

                             ;; Vim tab bar
                             vim-tab-bar)
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

;;; Load minimal-emacs.d early-init.el

(defun lightemacs-load-init-file (filename)
  "Load a file of Lisp init file named FILENAME."
  (load (expand-file-name (format "lisp/init/%s" filename)
                          lightemacs-user-emacs-directory)
        nil
        (not (bound-and-true-p init-file-debug))
        'nosuffix))

;; Load minimal-emacs.d early-init.el
(lightemacs-load-init-file "early-init.el")

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
