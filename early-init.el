;;; early-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(setq minimal-emacs-load-compiled-init-files t)

;;; Load config.el

;; Defined later in le-core-paths
(setq lightemacs-user-directory user-emacs-directory)
(setq lightemacs-local-directory (expand-file-name "lisp/local/"
                                                   lightemacs-user-directory))

;;; Update `load-path'

(add-to-list 'load-path (expand-file-name "lisp/lightemacs"
                                          user-emacs-directory))
(require 'le-core-paths)

(add-to-list 'load-path (expand-file-name "modules/"
                                          lightemacs-local-directory))
(add-to-list 'load-path lightemacs-local-modules-directory)
(add-to-list 'load-path lightemacs-core-directory)
(add-to-list 'load-path lightemacs-modules-directory)

;; Load defaults

(require 'le-core-defaults)

;; Minimal-emacs.d defaults
(setq minimal-emacs-frame-title-format "%b – Lightemacs")
(setq minimal-emacs-package-initialize-and-refresh nil)  ; Managed by Lightemacs
(setq minimal-emacs-gc-cons-percentage 0.1)
(setq minimal-emacs-gc-cons-threshold (* 40 1024 1024))
(setq minimal-emacs-gc-cons-threshold-restore-delay 3)
(setq minimal-emacs-ui-features '(context-menu tooltips))

(setq package-enable-at-startup nil)
(setq package-archive-priorities '(("gnu"          . 90)
                                   ("nongnu"       . 80)
                                   ("melpa"        . 70)
                                   ("melpa-stable" . 50)))
(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

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

;;; Load: lightemacs.el

(require 'lightemacs)

;;; Load: config.el

(load (expand-file-name "config" lightemacs-local-directory)
      :no-error
      (not (bound-and-true-p init-file-debug)))

;;; Function: `lightemacs-user-pre-early-init'

(when (fboundp 'lightemacs-user-pre-early-init)
  (funcall 'lightemacs-user-pre-early-init))

;;; Adjust CPUs

(when (and lightemacs-native-comp-excluded-cpus
           (boundp 'native-comp-async-jobs-number))
  (setq native-comp-async-jobs-number
        (lightemacs--calculate-native-comp-async-jobs-number)))

;;; Load pre-early-init.el | TODO remove

;; (lightemacs-load-user-init
;;  (expand-file-name "pre-early-init.el" lightemacs-local-directory)
;;  :no-error)

;;; Load minimal-emacs.d early-init.el

(lightemacs-load-user-init
 (expand-file-name "early-init.el" minimal-emacs-user-directory))

(setq custom-file (expand-file-name "custom.el" lightemacs-var-directory))

;;; Function: `lightemacs-user-post-early-init'

(when (fboundp 'lightemacs-user-post-early-init)
  (funcall 'lightemacs-user-post-early-init))

;;; Load post-early-init.el | TODO remove

;; (lightemacs-load-user-init
;;  (expand-file-name "post-early-init.el" lightemacs-local-directory)
;;  :no-error)

;;; early-init.el ends here
