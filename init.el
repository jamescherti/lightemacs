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
