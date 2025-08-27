;;; early-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The Lightemacs project is an Emacs framework.

;;; Code:

;;; Temporary increase `gc-cons-threshold'

(setq minimal-emacs--backup-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

;;; Variables

(defvar lightemacs-user-emacs-directory user-emacs-directory
  "Directory beneath Lightemacs files are placed.")

(setq minimal-emacs-frame-title-format "%b – Lightemacs")

(setq package-enable-at-startup nil)
(setq load-prefer-newer t)
(setq minimal-emacs-load-compiled-init-files nil)
(setq minimal-emacs-package-initialize-and-refresh nil)  ; Managed by Lightemacs

;;; Internal variables

(defvar lightemacs--modules-dir (expand-file-name
                                 "lisp/lightemacs"
                                 lightemacs-user-emacs-directory))

(defvar lightemacs--minimal-emacs-dir (expand-file-name
                                       "init" lightemacs--modules-dir))

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
(setq user-emacs-directory (expand-file-name "var/" lightemacs-user-emacs-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq minimal-emacs-user-directory lightemacs-user-emacs-directory)

;;; Functions

(defun lightemacs--remove-el-file-suffix (filename)
  "Remove the Elisp file suffix from FILENAME and return it (.el, .el.gz...)."
  (let ((suffixes (mapcar (lambda (ext) (concat ".el" ext))
                          load-file-rep-suffixes)))
    (catch 'done
      (dolist (suffix suffixes filename)
        (when (string-suffix-p suffix filename)
          (setq filename (substring filename 0 (- (length suffix))))
          (throw 'done t))))
    filename))

(defun lightemacs-load-user-init (file &optional no-error)
  "Load a file of Lisp init file named FILENAME.
If optional second arg NO-ERROR is non-nil,
report no error if FILE doesn’t exist."
  (let ((init-file (expand-file-name file
                                     lightemacs--minimal-emacs-dir)))
    (if (not minimal-emacs-load-compiled-init-files)
        (load init-file
              no-error
              (not (bound-and-true-p init-file-debug))
              :nosuffix)
      ;; Remove the file suffix (.el, .el.gz, etc.) to let the `load' function
      ;; select between .el and .elc files.
      (setq init-file (lightemacs--remove-el-file-suffix init-file))
      (load init-file no-error (not (bound-and-true-p init-file-debug))))))

;;; Byte Compile init files

(require 'bytecomp)

(defun lightemacs--byte-compile-if-outdated (el-file)
  "Byte-compile EL-FILE into .elc if the .elc is missing or outdated."
  (let* ((elc-file (byte-compile-dest-file el-file)))
    (when (and elc-file
               (or (not (file-exists-p elc-file))
                   (file-newer-than-file-p el-file elc-file))
               (file-writable-p elc-file))
      (byte-compile-file el-file))))

(add-to-list 'load-path lightemacs--modules-dir)

;; Lightemacs
(lightemacs--byte-compile-if-outdated
 (expand-file-name "lightemacs.el" lightemacs--modules-dir))

;; Lightemacs init files
(lightemacs--byte-compile-if-outdated
 (expand-file-name "init.el" lightemacs-user-emacs-directory))
(lightemacs--byte-compile-if-outdated
 (expand-file-name "early-init.el" lightemacs-user-emacs-directory))

;; Minimal Emacs init files
(lightemacs--byte-compile-if-outdated
 (expand-file-name "init.el" lightemacs--minimal-emacs-dir))
(lightemacs--byte-compile-if-outdated
 (expand-file-name "early-init.el" lightemacs--minimal-emacs-dir))

;;; Load minimal-emacs.d early-init.el

(lightemacs-load-user-init "early-init.el")

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
