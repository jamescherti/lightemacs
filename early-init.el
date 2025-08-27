;;; early-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Lightemacs is a Fast and Lightweight Emacs Framework.

;;; Code:

;;; Temporary increase `gc-cons-threshold' and set `load-prefer-newer' to t

;; This will be restored later by minimal-emacs.d
(setq minimal-emacs--backup-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

(setq load-prefer-newer t)

;;; Global variables

(defvar lightemacs-user-directory user-emacs-directory
  "Directory beneath Lightemacs files are placed.
Note that this should end with a directory separator.")

(defvar lightemacs-modules-directory
  (expand-file-name "lisp/lightemacs/" lightemacs-user-directory))

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
(defvar lightemacs--declutter-done nil)
(unless lightemacs--declutter-done
  (setq lightemacs--declutter-done t)
  (setq user-emacs-directory (expand-file-name "var/"
                                               lightemacs-user-directory))
  (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
  (setq minimal-emacs-user-directory (expand-file-name
                                      "init/" lightemacs-modules-directory)))

;;; Update `load-path'

(eval-and-compile
  (add-to-list 'load-path lightemacs-modules-directory))

;;; Load lightemacs.el

(eval-and-compile
  (require 'lightemacs))

;;; Defaults

(setq minimal-emacs-frame-title-format "%b – Lightemacs")
(setq package-enable-at-startup nil)
(setq minimal-emacs-load-compiled-init-files lightemacs-load-compiled-init-files)
(setq minimal-emacs-package-initialize-and-refresh nil)  ; Managed by Lightemacs

;;; Load pre-early-init.el

(lightemacs-load-user-init
 (expand-file-name "pre-early-init.el" lightemacs-user-directory)
 :no-error)

;;; Load minimal-emacs.d early-init.el

(lightemacs-load-user-init
 (expand-file-name "early-init.el" minimal-emacs-user-directory))

;;; Load post-early-init.el

(lightemacs-load-user-init
 (expand-file-name "post-early-init.el" lightemacs-user-directory)
 :no-error)

;;; early-init.el ends here
