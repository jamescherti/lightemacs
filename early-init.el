;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Lightemacs is a Fast and Lightweight Emacs Framework.

;;; Code:

(defvar lightemacs--early-init-loaded nil)
(unless lightemacs--early-init-loaded
  (setq lightemacs--early-init-loaded t)
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

  ;; Load: config.el
  (load (expand-file-name "config" lightemacs-local-directory)
        :no-error
        (not (bound-and-true-p init-file-debug)))

  ;; Run hooks: `lightemacs-pre-early-init-hook'

  (run-hooks 'lightemacs-pre-early-init-hook)

  ;; Load: lightemacs.el
  (require 'lightemacs)

  ;; Load minimal-emacs.d early-init.el
  (if (fboundp 'lightemacs-load-user-init)
      (lightemacs-load-user-init
       (expand-file-name "early-init.el" minimal-emacs-user-directory))
    (error "Undefined: lightemacs-load-user-init"))

  ;; Increase the number of CPUs
  (when (and (bound-and-true-p lightemacs-native-comp-excluded-cpus)
             (numberp lightemacs-native-comp-excluded-cpus)
             (featurep 'native-compile)
             (fboundp 'native-comp-available-p)
             (native-comp-available-p)
             (fboundp 'lightemacs--calculate-native-comp-async-jobs-number))
    (setq native-comp-async-jobs-number
          (lightemacs--calculate-native-comp-async-jobs-number)))

  (setq custom-file (expand-file-name "custom.el" lightemacs-var-directory))

  ;; Run hooks: `lightemacs-post-early-init-hook'
  (run-hooks 'lightemacs-post-early-init-hook))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
