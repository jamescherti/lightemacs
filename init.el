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
  (defvar lightemacs-local-directory
    (expand-file-name "lisp/local/" lightemacs-user-directory))
  (defvar minimal-emacs-user-directory (expand-file-name
                                        "lisp/lightemacs/init"
                                        lightemacs-user-directory))
  (defvar lightemacs-core-directory (expand-file-name
                                     "lisp/lightemacs"
                                     lightemacs-user-directory))
  (defvar lightemacs-modules-directory (expand-file-name
                                        "modules"
                                        lightemacs-core-directory))
  (add-to-list 'load-path lightemacs-modules-directory))

;;; Ensure required variables are declared

(unless (boundp 'minimal-emacs-user-directory)
  (error "Undefined variable: minimal-emacs-user-directory"))

;;; Load lightemacs.el

(eval-and-compile
  (require 'lightemacs))

;;; Compile modules

;; Stale .elc files can cause issues; recompiling them ensures they are up to
;; date. Compilation occurs in init.el rather than early-init.el so that
;; messages appear in the *Messages* buffer for user visibility.
(require 'le-core-compile-mod)

(when (bound-and-true-p lightemacs-byte-compile-core)
  (dolist (file '("lightemacs.el"
                  "le-core-cli-tools.el"
                  ;; le-core-elpaca.el  ; note byte compile
                  ;; le-core-straight.el  ; note byte compile
                  "le-core-package-manager.el"
                  "le-core-compile-mod.el"
                  "le-core-use-package.el"))
    (lightemacs--byte-compile-if-outdated (expand-file-name
                                           file lightemacs-core-directory)))

  ;; Configuration
  (dolist (file '("config.el"))
    (lightemacs--byte-compile-if-outdated
     (expand-file-name file lightemacs-local-directory)
     :no-error))

  (dolist (file '("pre-early-init.el"
                  "post-early-init.el"
                  "pre-init.el"
                  "post-init.el"))
    (lightemacs--byte-compile-if-outdated
     (expand-file-name file lightemacs-local-directory)
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


;;; Load pre-init.el

(let ((el-file (expand-file-name "pre-init.el"
                                 lightemacs-local-directory)))
  (lightemacs-load-user-init el-file :no-error))

;;; Load init.el

(if (fboundp 'lightemacs-load-user-init)
    (funcall 'lightemacs-load-user-init
             (expand-file-name "init.el" minimal-emacs-user-directory)))

(require 'le-core-package-manager)

;;; Load modules, and post-init.el

;; Load all modules
(if (fboundp 'lightemacs-load-modules)
    (progn
      (funcall 'lightemacs-load-modules lightemacs-core-modules)
      (funcall 'lightemacs-load-modules lightemacs-modules))
  (error "Undefined function: lightemacs-load-modules"))

;; Load post-init.el
(let ((el-file (expand-file-name "post-init.el"
                                 lightemacs-local-directory)))
  (lightemacs-load-user-init el-file :no-error))

;;; init.el ends here
