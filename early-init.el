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

(defvar lightemacs--early-init-done nil)
(unless lightemacs--early-init-done
  (setq lightemacs--early-init-done t)
  (setq load-prefer-newer t)
  (push (expand-file-name "lisp/lightemacs/" user-emacs-directory) load-path)

  ;; Update defaults
  (setq minimal-emacs--backup-gc-cons-threshold gc-cons-threshold)
  (defvar minimal-emacs--backup-gc-cons-percentage gc-cons-percentage)
  (setq gc-cons-threshold most-positive-fixnum)
  (setq gc-cons-percentage 1.0)

  ;; Defined later in le-core-defaults

  (setq lightemacs-user-directory user-emacs-directory)

  ;; Load defaults
  (require 'le-core-defaults)
  (unless lightemacs-package-manager
    (setq lightemacs-package-manager 'builtin-package))

  ;; Overwrite Minimal-emacs.d defaults
  (setq minimal-emacs-frame-title-format "%b - Lightemacs")
  (setq minimal-emacs-package-initialize-and-refresh nil)
  (setq minimal-emacs-gc-cons-percentage 0.1)
  (setq minimal-emacs-gc-cons-threshold (* 40 1024 1024))
  (setq minimal-emacs-gc-cons-threshold-restore-delay 3)
  (setq minimal-emacs-ui-features '())

  (setq minimal-emacs-load-pre-early-init nil)
  (setq minimal-emacs-load-post-early-init nil)
  (setq minimal-emacs-load-pre-init nil)
  (setq minimal-emacs-load-post-init nil)

  (setq package-enable-at-startup nil)

  (setq minimal-emacs-load-compiled-init-files t)

  (push lightemacs-local-modules-directory load-path)
  (push lightemacs-modules-directory load-path)

  ;; Reduce cluttering
  ;;
  ;; Emacs, by default, stores various configuration files, caches, backups, and
  ;; other data in the ~/.emacs.d directory. Over time, this directory can
  ;; become cluttered with numerous files, making it difficult to manage and
  ;; maintain.
  ;;
  ;; A common solution to this issue is installing the no-littering package;
  ;; however, this package is not essential.
  ;;
  ;; An alternative lightweight approach is to simply change the default
  ;; ~/.emacs.d directory to ~/.emacs.d/var/, which will contain all the files
  ;; that Emacs typically stores in the base directory.
  (setq user-emacs-directory lightemacs-var-directory)
  (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
  (setq minimal-emacs-user-directory (expand-file-name
                                      "init/" lightemacs-core-directory))

  (setq custom-theme-directory
        (expand-file-name "themes/" minimal-emacs-user-directory))

  ;; Evil defaults

  ;; Load: config.el
  (load (expand-file-name "config" lightemacs-local-directory)
        :no-error
        (not (bound-and-true-p init-file-debug)))

  ;; Function: `lightemacs-user-pre-early-init'
  (when (fboundp 'lightemacs-user-pre-early-init)
    (funcall 'lightemacs-user-pre-early-init))

  ;; Load: lightemacs.el
  (require 'lightemacs)

  ;; Load minimal-emacs.d early-init.el
  (lightemacs-load-user-init
   (expand-file-name "early-init.el" minimal-emacs-user-directory))

  ;; Adjust variables after early-init
  (when (and lightemacs-native-comp-excluded-cpus
             (boundp 'native-comp-async-jobs-number))
    (setq native-comp-async-jobs-number
          (lightemacs--calculate-native-comp-async-jobs-number)))

  (setq custom-file (expand-file-name "custom.el" lightemacs-var-directory))

  ;; Function: `lightemacs-user-post-early-init'
  (when (fboundp 'lightemacs-user-post-early-init)
    (funcall 'lightemacs-user-post-early-init)))

;;; early-init.el ends here
