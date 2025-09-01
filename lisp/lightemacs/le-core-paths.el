;;; le-core-paths.el --- le-core-paths -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Default paths.

;;; Code:

;;; Default paths

(defvar lightemacs-user-directory user-emacs-directory
  "Directory beneath Lightemacs files are placed.
Note that this should end with a directory separator.")

(defvar lightemacs-local-directory
  (expand-file-name "lisp/local/" lightemacs-user-directory))

(defvar lightemacs-local-modules-directory
  (expand-file-name "modules/" lightemacs-local-directory))

(defvar lightemacs-core-directory
  (expand-file-name "lisp/lightemacs/" lightemacs-user-directory))

(defvar lightemacs-modules-directory
  (expand-file-name "modules/" lightemacs-core-directory))

(defvar lightemacs-var-directory
  (expand-file-name "var/" lightemacs-user-directory))

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

(defvar lightemacs--declutter-done nil)  ; Idempotency
(unless lightemacs--declutter-done
  (setq lightemacs--declutter-done t)
  (setq user-emacs-directory lightemacs-var-directory)
  (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
  (setq minimal-emacs-user-directory (expand-file-name
                                      "init/" lightemacs-core-directory)))

(setq custom-theme-directory
      (expand-file-name "themes/" minimal-emacs-user-directory))

(setq custom-file (expand-file-name "custom.el" lightemacs-var-directory))

(provide 'le-core-paths)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-core-paths.el ends here
