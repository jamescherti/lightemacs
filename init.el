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

;;; Require

(require 'lightemacs)
(require 'lightemacs-module)

;;; Call `lightemacs-user-pre-init'

(when (fboundp 'lightemacs-user-pre-init)
  (lightemacs-user-pre-init))

;;; Load the pre-init.el file | TODO remove

;; (let ((el-file (expand-file-name "pre-init.el"
;;                                  lightemacs-local-directory)))
;;   (lightemacs-load-user-init el-file :no-error))

;;; Configure the package manager

;; TODO Check if this is necessary
(unless lightemacs-package-manager
  (error "Invalid `lightemacs-package-manager': %S" lightemacs-package-manager))

(defvar lightemacs-use-package--package-manager-loaded nil)

(unless lightemacs-use-package--package-manager-loaded
  (when (bound-and-true-p lightemacs-package-manager)
    (cond
     ;; Straight
     ((eq lightemacs-package-manager 'straight)
      (require 'le-core-pm-straight))

     ;; Elpaca
     ((eq lightemacs-package-manager 'elpaca)
      (require 'le-core-pm-elpaca))

     ;; use-package (built-in)
     ((eq lightemacs-package-manager 'use-package)
      (require 'le-core-pm-use-package))

     (t
      (error
       (concat "[lightemacs]"
               "Invalid value for `lightemacs-package-manager': '%S'. Valid "
               "choices are: 'straight, 'elpaca, or 'use-package.")
       lightemacs-package-manager))))

  (setq lightemacs-use-package--package-manager-loaded t))

;;; Function: `lightemacs-user-pre-modules'

;; Load function: `lightemacs-user-pre-modules'
(when (fboundp 'lightemacs-user-pre-modules)
  (lightemacs-user-pre-modules))

;;; Load modules

;; Load all modules
(if (fboundp 'lightemacs-module-load)
    (progn
      (lightemacs-module-load lightemacs-core-modules)
      (lightemacs-module-load lightemacs-modules))
  (error "Undefined function: lightemacs-module-load"))

;;; Function: `lightemacs-user-post-modules'

;; Load function: `lightemacs-user-post-modules'
(when (fboundp 'lightemacs-user-post-modules)
  (lightemacs-user-post-modules))

;;; Function: `lightemacs-user-init'

(when (fboundp 'lightemacs-user-init)
  (lightemacs-user-init))

;; Load `lightemacs-user-init'

(when (and (fboundp 'lightemacs-load-user-init)
           (boundp 'minimal-emacs-user-directory))
  (lightemacs-load-user-init
   (expand-file-name "init.el" minimal-emacs-user-directory)))

;;; Load the post-init.el file | TODO remove

;; (let ((el-file (expand-file-name "post-init.el"
;;                                  lightemacs-local-directory)))
;;   (lightemacs-load-user-init el-file :no-error))

;;; Function: `lightemacs-user-post-init'

(when (fboundp 'lightemacs-user-post-init)
  (funcall 'lightemacs-user-post-init))

;;; Hook `lightemacs-after-init-hook'

(defun lightemacs--run-after-init-hook ()
  "Run `lightemacs--run-after-init-hook' at the appropriate time."
  (run-hooks 'lightemacs-after-init-hook))
(cond
 ((eq lightemacs-package-manager 'elpaca)
  (add-hook 'elpaca-after-init-hook #'lightemacs--run-after-init-hook))
 (t
  (add-hook 'after-init-hook #'lightemacs--run-after-init-hook)))

;;; init.el ends here
