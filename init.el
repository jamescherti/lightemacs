;;; init.el --- Init -*- lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Lightemacs is a Fast and Lightweight Emacs Framework.

;;; Code:

;;; Define variables for the byte compiler

(eval-when-compile
  ;; These variables are already defined in the early-init.el file
  (defvar lightemacs-user-directory (file-truename "."))
  (defvar minimal-emacs-user-directory (expand-file-name
                                        "lisp/lightemacs/init"
                                        lightemacs-user-directory))
  (defvar lightemacs-modules-directory (expand-file-name
                                        "lisp/lightemacs"
                                        lightemacs-user-directory))
  (add-to-list 'load-path lightemacs-modules-directory))

;;; Ensure required variables are declared

(unless (boundp 'minimal-emacs-user-directory)
  (error "Undefined variable: minimal-emacs-user-directory"))

;;; Load config.el

(lightemacs-load-user-init
 (expand-file-name "config.el" lightemacs-user-directory)
 :no-error)

;;; Update `load-path' and byte compile init files

(defun lightemacs--byte-compile-if-outdated (el-file)
  "Byte-compile EL-FILE into .elc if the .elc is missing or outdated."
  (when (file-readable-p el-file)
    (let* ((elc-file
            (funcall (if (bound-and-true-p byte-compile-dest-file-function)
                         byte-compile-dest-file-function
                       #'byte-compile-dest-file)
                     el-file))
           (noninteractive t))
      (when (and elc-file
                 (or (not (file-exists-p elc-file))
                     (file-newer-than-file-p el-file elc-file))
                 (file-writable-p elc-file))
        (when (bound-and-true-p lightemacs-verbose)
          (message "[lightemacs] Byte compiling: %s" el-file))
        (byte-compile-file el-file)))))

(condition-case err
    (progn
      (lightemacs--byte-compile-if-outdated
       (expand-file-name "lightemacs.el" lightemacs-modules-directory))

      ;; Lightemacs init files
      (dolist (file '("pre-early-init.el"
                      "config.el"
                      "early-init.el"
                      "post-early-init.el"
                      "pre-init.el"
                      "init.el"
                      "post-init.el"))
        (lightemacs--byte-compile-if-outdated
         (expand-file-name file lightemacs-user-directory)))

      ;; Minimal Emacs init files
      (lightemacs--byte-compile-if-outdated
       (expand-file-name "init.el" minimal-emacs-user-directory))
      (lightemacs--byte-compile-if-outdated
       (expand-file-name "early-init.el" minimal-emacs-user-directory)))
  (error
   (message "[Lightemacs] Warning: Byte-compile error: %S" err)))


;;; Load lightemacs.el

(eval-and-compile
  (require 'lightemacs))

;;; Use more CPUs for native compilation

(eval-when-compile
  (require 'comp-run))

(setq native-comp-async-jobs-number
      (lightemacs--calculate-native-comp-async-jobs-number))

;;; Load pre-init.el

(let ((el-file (expand-file-name "pre-init.el"
                                 lightemacs-user-directory)))
  (lightemacs-load-user-init el-file :no-error))

;;; Load init.el

(if (fboundp 'lightemacs-load-user-init)
    (funcall 'lightemacs-load-user-init
             (expand-file-name "init.el" minimal-emacs-user-directory)))

;;; Package manager

(cond
 ;; Straight
 ((eq lightemacs-package-manager 'straight)
  (require 'le-core-straight))

 ;; Elpaca
 ((eq lightemacs-package-manager 'elpaca)
  (require 'le-core-elpaca))

 ;; use-package (built-in)
 ((eq lightemacs-package-manager 'use-package)
  (lightemacs--byte-compile-if-outdated
   (expand-file-name "le-core-use-package.el" lightemacs-modules-directory))
  (require 'le-core-use-package))

 (t
  (error (concat "Invalid value for `lightemacs-package-manager': '%S'. Valid "
                 "choices are: 'straight, 'elpaca, or 'use-package.")
         lightemacs-package-manager)))

;;; Load modules, and post-init.el

;; Load all modules
(if (fboundp 'lightemacs-load-modules)
    (funcall 'lightemacs-load-modules lightemacs-modules)
  (error "Undefined function: lightemacs-load-modules"))

;; Load post-init.el
(let ((el-file (expand-file-name "post-init.el"
                                 lightemacs-user-directory)))
  (lightemacs-load-user-init el-file :no-error))

;;; init.el ends here
