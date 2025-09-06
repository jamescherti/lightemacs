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
  (defvar lightemacs-user-directory (file-truename "."))
  (add-to-list 'load-path
               (expand-file-name "lisp/lightemacs" lightemacs-user-directory)))

(require 'le-core-paths)

;;; Load lightemacs.el

(require 'lightemacs)

;;; Load pre-init.el

(let ((el-file (expand-file-name "pre-init.el"
                                 lightemacs-local-directory)))
  (lightemacs-load-user-init el-file :no-error))

;;; Load init.el

(if (fboundp 'lightemacs-load-user-init)
    (funcall 'lightemacs-load-user-init
             (expand-file-name "init.el" minimal-emacs-user-directory)))

(require 'le-core-package-manager)

;;; Run `lightemacs-after-init-hook'

(defun lightemacs--run-after-init-hook ()
  "Run `lightemacs--run-after-init-hook` at the appropriate time."
  (run-hooks 'lightemacs-after-init-hook))

(cond
 ((eq lightemacs-package-manager 'elpaca)
  (add-hook 'elpaca-after-init-hook #'lightemacs--run-after-init-hook))

 (t
  (add-hook 'after-init-hook #'lightemacs--run-after-init-hook)))

;;; Load config.el

(load (expand-file-name "config" lightemacs-local-directory)
      :no-error
      (not (bound-and-true-p init-file-debug)))

(when (and lightemacs-native-comp-excluded-cpus
           (boundp 'native-comp-async-jobs-number))
  (setq native-comp-async-jobs-number
        (lightemacs--calculate-native-comp-async-jobs-number)))

;;; Load modules

;; Load all modules
(if (fboundp 'lightemacs-load-modules)
    (progn
      (funcall 'lightemacs-load-modules lightemacs-core-modules)
      (funcall 'lightemacs-load-modules lightemacs-modules))
  (error "Undefined function: lightemacs-load-modules"))

;;; Load post-init.el

(let ((el-file (expand-file-name "post-init.el"
                                 lightemacs-local-directory)))
  (lightemacs-load-user-init el-file :no-error))

;;; init.el ends here
