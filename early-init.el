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

;;; Update `load-path'

(add-to-list 'load-path (expand-file-name "lisp/lightemacs" user-emacs-directory))
(require 'le-core-paths)

(add-to-list 'load-path lightemacs-local-modules-directory)
(add-to-list 'load-path lightemacs-core-directory)
(add-to-list 'load-path lightemacs-modules-directory)

;;; Load config.el

(load (expand-file-name "config" lightemacs-local-directory)
      :no-error
      (not (bound-and-true-p init-file-debug)))

;;; Load lightemacs.el

(require 'le-core-defaults)
(require 'lightemacs)

;;; Load pre-early-init.el

(lightemacs-load-user-init
 (expand-file-name "pre-early-init.el" lightemacs-local-directory)
 :no-error)

;;; Load minimal-emacs.d early-init.el

(lightemacs-load-user-init
 (expand-file-name "early-init.el" minimal-emacs-user-directory))

;;; Load post-early-init.el

(lightemacs-load-user-init
 (expand-file-name "post-early-init.el" lightemacs-local-directory)
 :no-error)

;;; Load config.el

(when lightemacs-native-comp-excluded-cpus
  (setq native-comp-async-jobs-number
        (lightemacs--calculate-native-comp-async-jobs-number)))

(setq minimal-emacs-load-compiled-init-files lightemacs-load-compiled-init-files)

;;; early-init.el ends here
