;;; le-core-paths.el --- le-core-paths -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(provide 'le-core-paths)

;;; le-core-paths.el ends here
