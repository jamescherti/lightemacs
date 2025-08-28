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

;;; Update `load-path' and byte compile init files

;;; Load config.el

(load (expand-file-name "config" lightemacs-user-directory)
      :no-error
      (not (bound-and-true-p init-file-debug)))

;;; Stale .elc files may lead to errors; ensure they are recompiled and current.

(require 'bytecomp)

(defmacro lightemacs--early-verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  (declare (indent 0) (debug t))
  `(when lightemacs-verbose
     (message (concat "[lightemacs] " ,(car args)) ,@(cdr args))))

(defun lightemacs--byte-compile-if-outdated (el-file &optional no-error)
  "Byte-compile EL-FILE into .elc if the .elc is missing or outdated.
If optional second arg NO-ERROR is non-nil, report no error if FILE doesn’t
exist."
  (let* ((elc-file (funcall
                    (if (bound-and-true-p byte-compile-dest-file-function)
                        byte-compile-dest-file-function
                      #'byte-compile-dest-file)
                    el-file)))
    (cond
     ((not elc-file)
      (message "WARNING: Byte-compile: Cannot detect the .elc path for: %S"
               el-file))

     ((and (file-exists-p elc-file)
           (not (file-newer-than-file-p el-file elc-file)))
      (lightemacs--early-verbose-message
       "IGNORED (Up to date): Byte-compile: %S" el-file))

     ((and lightemacs-verbose
           (not (file-writable-p elc-file)))
      (lightemacs--early-verbose-message
       (concat "IGNORED: Byte-compile: Destination .elc is read-only: %S. "
               "Ensure you have write permissions to allow recompilation.")
       elc-file))

     (t
      (let* ((delete-elc nil)
             (noninteractive t)
             (byte-compile-warnings nil)
             (result (condition-case err
                         (byte-compile-file el-file)
                       (error
                        ;; Return error
                        nil))))
        (unless result
          (setq delete-elc t))

        (lightemacs--early-verbose-message
         "Byte-compile: %S (Result: %s)" el-file (cond
                                                  ((eq result t)
                                                   "success")
                                                  ((not result)
                                                   "error")
                                                  (t
                                                   result)))

        (when delete-elc
          (let ((delete-by-moving-to-trash nil))
            (lightemacs--early-verbose-message "Delete .elc file: %S" elc-file)
            (ignore-errors
              (delete-file elc-file)))))))))

;; Modules and libraries
(dolist (file '("lightemacs.el"
                "le-core-cli-tools.el"
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
   (expand-file-name file minimal-emacs-user-directory)))

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
