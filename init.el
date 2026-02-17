;;; init.el --- Init -*- lexical-binding: t; -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

;;; Load function: `lightemacs-user-pre-init'

(when (fboundp 'lightemacs-user-pre-init)
  (funcall 'lightemacs-user-pre-init))

;;; Load pre-init.el

(let ((el-file (expand-file-name "pre-init.el"
                                 lightemacs-local-directory)))
  (lightemacs-load-user-init el-file :no-error))

;;; Load init.el

(when (and (fboundp 'lightemacs-load-user-init)
           (boundp 'minimal-emacs-user-directory))
  (funcall 'lightemacs-load-user-init
           (expand-file-name "init.el" minimal-emacs-user-directory)))

;;; Load the package manager and refresh

(require 'lightemacs-package)

;;; Configure `lightemacs-after-init-hook'

(defun lightemacs--run-after-init-hook ()
  "Run `lightemacs--run-after-init-hook` at the appropriate time."
  (run-hooks 'lightemacs-after-init-hook))

(cond
 ((eq lightemacs-package-manager 'elpaca)
  (add-hook 'elpaca-after-init-hook #'lightemacs--run-after-init-hook))

 (t
  (add-hook 'after-init-hook #'lightemacs--run-after-init-hook)))

;;; Load user function: `lightemacs-user-init'

(when (fboundp 'lightemacs-user-init)
  (funcall 'lightemacs-user-init))

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

;;; Load function: `lightemacs-user-post-init'

(when (fboundp 'lightemacs-user-post-init)
  (funcall 'lightemacs-user-post-init))

;;; init.el ends here
