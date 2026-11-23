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

(setq load-prefer-newer t)

(let* ((source-file (expand-file-name "early-init.el" user-emacs-directory)))
  ;; Tools that byte-compile an entire Emacs configuration can leave
  ;; early-init.elc or .eln behind when the user edits early-init.el. By
  ;; default, Emacs can load that older bytecode even when the source is newer.
  ;; Setting `load-prefer-newer' inside early-init.el is too late to affect this
  ;; choice because Emacs selects the file before evaluating its contents.
  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2026-06/msg01711.html
  (when init-file-debug
    (message "[early-init] Source file: %s" source-file)
    (message "[early-init] Loaded file: %s" load-true-file-name))
  (if (and load-true-file-name
           (member (file-name-extension load-true-file-name) '("elc" "eln"))
           (file-newer-than-file-p source-file load-true-file-name))
      (progn
        (message "[Lightemacs] %s is stale. Loading %s"
                 load-true-file-name source-file)
        (load source-file nil nil t))
    (declare-function lightemacs-load-user-init "lightemacs" (file &optional no-error))
    (setq minimal-emacs--backup-gc-cons-threshold gc-cons-threshold)
    (setq minimal-emacs--backup-gc-cons-percentage gc-cons-percentage)
    (setq gc-cons-threshold most-positive-fixnum)
    (setq gc-cons-percentage 1.0)
    (defvar lightemacs-byte-compile-additional-directories nil)
    (defvar lightemacs-byte-and-native-compile-after-load)

    ;; Paths
    (setq lightemacs-user-directory user-emacs-directory)
    (setq lightemacs-builtin-lisp-directory
          (expand-file-name "lisp/lightemacs/" lightemacs-user-directory))
    (setq lightemacs-var-directory
          (expand-file-name "var/" lightemacs-user-directory))
    (setq lightemacs-lisp-directory
          (expand-file-name "lisp/" lightemacs-user-directory))
    (setq lightemacs-local-lisp-directory
          (expand-file-name "lisp/local/" lightemacs-user-directory))
    (setq lightemacs-local-modules-directory
          (expand-file-name "modules/" lightemacs-local-lisp-directory))
    (setq lightemacs-builtin-modules-directory
          (expand-file-name "modules/" lightemacs-builtin-lisp-directory))

    (setq minimal-emacs-user-directory
          (expand-file-name "init/" lightemacs-builtin-lisp-directory))
    (setq custom-theme-directory
          (expand-file-name "themes/" lightemacs-user-directory))
    ;; early-init.el starts here
    (push lightemacs-builtin-lisp-directory load-path)

    (push lightemacs-builtin-modules-directory load-path)
    ;; (let ((default-directory lightemacs-builtin-modules-directory))
    ;;   (normal-top-level-add-subdirs-to-load-path))

    (push lightemacs-local-modules-directory load-path)
    (when (file-directory-p lightemacs-local-modules-directory)
      (let ((default-directory lightemacs-local-modules-directory))
        (normal-top-level-add-subdirs-to-load-path)))

    (setq package-user-dir (expand-file-name "elpa" lightemacs-var-directory))
    (setq custom-file (expand-file-name "custom.el" lightemacs-var-directory))

    ;; Reduce cluttering
    ;;
    ;; Emacs, by default, stores various configuration files, caches, backups,
    ;; and other data in the ~/.emacs.d directory. Over time, this directory can
    ;; become cluttered with numerous files, making it difficult to manage and
    ;; maintain.
    ;;
    ;; A common solution to is installing the no-littering package; however,
    ;; this package is not essential. An alternative lightweight approach is to
    ;; change the default ~/.emacs.d directory to ~/.emacs.d/var/, which will
    ;; contain all the files that Emacs typically stores in the base directory.
    (setq user-emacs-directory lightemacs-var-directory)
    (require 'lightemacs)

    (setq lightemacs-package-manager 'builtin-package)

    (if (and (fboundp 'lightemacs--on-run-first-input-hook)
             (fboundp 'lightemacs--on-run-first-file-hook)
             (fboundp 'lightemacs--on-run-first-buffer-hook))
        (unless noninteractive
          (add-hook 'pre-command-hook #'lightemacs--on-run-first-input-hook)

          (add-hook 'find-file-hook #'lightemacs--on-run-first-file-hook)
          (add-hook 'dired-initial-position-hook #'lightemacs--on-run-first-file-hook)

          (add-hook 'find-file-hook #'lightemacs--on-run-first-buffer-hook)
          (add-hook 'window-buffer-change-functions #'lightemacs--on-run-first-buffer-hook)
          (add-hook 'server-visit-hook #'lightemacs--on-run-first-buffer-hook))
      (error "Undefined lightemacs--on-* functions"))

    ;; Overwrite Minimal-emacs.d defaults
    (defvaralias 'lightemacs-frame-title-format 'minimal-emacs-frame-title-format)
    (defvaralias 'lightemacs-ui-features 'minimal-emacs-ui-features)
    (setq minimal-emacs-package-initialize-and-refresh nil)
    (setq minimal-emacs-gc-cons-percentage 0.1)
    (setq minimal-emacs-gc-cons-threshold (* 40 1024 1024))
    (setq minimal-emacs-load-pre-early-init nil)
    (setq minimal-emacs-load-post-early-init nil)
    (setq minimal-emacs-load-pre-init nil)
    (setq minimal-emacs-load-post-init nil)
    (setq minimal-emacs-load-compiled-init-files t)

    ;; Load: config.el
    (let ((config-file (expand-file-name
                        "config"
                        lightemacs-local-lisp-directory))
          (minimal-emacs-early-init (expand-file-name
                                     "early-init.el"
                                     minimal-emacs-user-directory)))
      (if (fboundp 'lightemacs-byte-and-native-compile)
          (progn
            (load config-file nil (not (bound-and-true-p init-file-debug)))
            (if (fboundp 'lightemacs-byte-and-native-compile)
                (add-hook 'after-load-functions #'lightemacs-byte-and-native-compile)
              (error "Undefined: `lightemacs-byte-and-native-compile'"))

            (when lightemacs-byte-and-native-compile-after-load
              (lightemacs-byte-and-native-compile config-file)

              (let ((early-init-file (expand-file-name "early-init.el"
                                                       lightemacs-user-directory))
                    (init-file (expand-file-name "init.el" lightemacs-user-directory)))
                (let ((lightemacs-byte-compile-additional-directories
                       (list lightemacs-user-directory)))
                  (lightemacs-byte-and-native-compile early-init-file)
                  (lightemacs-byte-and-native-compile init-file)))))
        (error "Undefined: `lightemacs-byte-and-native-compile'"))

      ;; Run hooks: `lightemacs-pre-early-init-hook'
      (run-hooks 'lightemacs-pre-early-init-hook)

      ;; Add lightemacs directories to `trusted-content'
      (unless (< emacs-major-version 30)
        (unless (boundp 'trusted-content)
          (setq trusted-content nil))

        (dolist (dir (list lightemacs-local-lisp-directory
                           lightemacs-builtin-lisp-directory))
          ;; Ensure the path ends with a slash so it registers as a directory
          (push (file-name-as-directory dir) trusted-content)))

      ;; Load minimal-emacs.d early-init.el
      (lightemacs-load-user-init minimal-emacs-early-init)

      ;; Run hooks: `lightemacs-post-early-init-hook'
      (run-hooks 'lightemacs-post-early-init-hook))))

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; early-init.el ends here
