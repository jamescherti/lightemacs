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

;;; Update defaults

;; This will be restored later by minimal-emacs.d
(setq minimal-emacs--backup-gc-cons-threshold gc-cons-threshold)
(defvar minimal-emacs--backup-gc-cons-percentage gc-cons-percentage)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(setq load-prefer-newer t)

(setq package-enable-at-startup nil)
(setq minimal-emacs-frame-title-format "%b – Lightemacs")
(setq minimal-emacs-package-initialize-and-refresh nil)  ; Managed by Lightemacs
(setq minimal-emacs-gc-cons-percentage 0.1)
(setq minimal-emacs-gc-cons-threshold (* 40 1024 1024))
(setq minimal-emacs-gc-cons-threshold-restore-delay 3)
(setq minimal-emacs-ui-features '(context-menu tooltips))

;;; Global variables

(defvar lightemacs-user-directory user-emacs-directory
  "Directory beneath Lightemacs files are placed.
Note that this should end with a directory separator.")

(defvar lightemacs-modules-directory
  (expand-file-name "lisp/lightemacs/" lightemacs-user-directory))

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

(defvar lightemacs--declutter-done nil)
(unless lightemacs--declutter-done
  (setq lightemacs--declutter-done t)
  (setq user-emacs-directory lightemacs-var-directory)
  (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
  (setq minimal-emacs-user-directory (expand-file-name
                                      "init/" lightemacs-modules-directory)))

(setq custom-theme-directory
      (expand-file-name "themes/" minimal-emacs-user-directory))

(setq custom-file (expand-file-name "custom.el" lightemacs-var-directory))

;;; Update `load-path'

(eval-and-compile
  (add-to-list 'load-path lightemacs-modules-directory))

;;; Load config.el

(load (expand-file-name "config" lightemacs-user-directory)
      :no-error
      (not (bound-and-true-p init-file-debug)))

;;; Stale .elc files may lead to errors; ensure they are recompiled and current.

(require 'le-core-compile)

(when (bound-and-true-p lightemacs-byte-compile-core)
  (dolist (file '("lightemacs.el"
                  "le-core-cli-tools.el"
                  ;; le-core-elpaca.el
                  ;; le-core-straight.el
                  "le-core-compile.el"
                  "le-core-use-package.el"))
    (lightemacs--byte-compile-if-outdated (expand-file-name
                                           file lightemacs-modules-directory)))

  ;; Configuration
  (dolist (file '("pre-early-init.el"
                  "config.el"
                  "post-early-init.el"
                  "pre-init.el"
                  "post-init.el"))
    (lightemacs--byte-compile-if-outdated
     (expand-file-name file lightemacs-user-directory)
     :no-error))

  ;; Lightemacs init files
  (dolist (file '("init.el"))
    (lightemacs--byte-compile-if-outdated
     (expand-file-name file lightemacs-user-directory)))

  ;; Minimal-emacs.d init files
  (dolist (file '("init.el"
                  "early-init.el"))
    (lightemacs--byte-compile-if-outdated
     (expand-file-name file minimal-emacs-user-directory))))

;;; Load lightemacs.el

(eval-and-compile
  (require 'lightemacs))

(setq minimal-emacs-load-compiled-init-files lightemacs-load-compiled-init-files)

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
