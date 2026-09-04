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

;;; Write the automatically generated configuration

;; TODO add back?
;; (unless (memq lightemacs-package-manager '(straight elpaca builtin-package
;;                                                     ;; Deprecated:
;;                                                     use-package))
;;   (error "The value '%s' is not a valid `lightemacs-package-manager'"
;;          lightemacs-package-manager))

;;; require

(require 'lightemacs)

;;; Run hooks: `lightemacs-pre-init-hook'

(run-hooks 'lightemacs-pre-init-hook)

;;; Load: init.el

(when (and (fboundp 'lightemacs-load-user-init)
           (boundp 'minimal-emacs-user-directory))
  (lightemacs-load-user-init
   (expand-file-name "init.el" minimal-emacs-user-directory)))

;;; Compile core modules and init files

(require 'lightemacs-module)

(let ((init-files '("early-init"
                    "init")))
  (dolist (name init-files)
    ;; Compile Minimal-emacs init.el
    (let ((base-path (expand-file-name name minimal-emacs-user-directory)))
      (lightemacs--compile-module-maybe base-path))

    ;; Compile Lightemacs early-init.el
    (let ((base-path (expand-file-name name lightemacs-user-directory)))
      (lightemacs--compile-module-maybe base-path))))

(let ((core-modules '("le-core-cli-tools"
                      "le-core-defaults"
                      "lightemacs"
                      "lightemacs-module"
                      "lightemacs-use-package")))
  (dolist (module core-modules)
    (let ((base-path (expand-file-name module lightemacs-core-directory)))
      (lightemacs--compile-module-maybe base-path))))

;;; Configure the package manager

(defvar lightemacs-use-package--package-manager-loaded nil)

(unless lightemacs-use-package--package-manager-loaded
  (when (bound-and-true-p lightemacs-package-manager)
    (cond
     ;; Straight
     ((eq lightemacs-package-manager 'straight)
      (lightemacs--compile-module-maybe
       (expand-file-name "le-core-pm-straight"
                         lightemacs-core-directory))
      (require 'le-core-pm-straight))

     ;; Elpaca
     ((eq lightemacs-package-manager 'elpaca)
      (lightemacs--compile-module-maybe
       (expand-file-name "le-core-pm-elpaca"
                         lightemacs-core-directory))
      (require 'le-core-pm-elpaca))

     ;; use-package (built-in)
     ((or (eq lightemacs-package-manager 'builtin-package)
          (eq lightemacs-package-manager 'use-package))
      (lightemacs--compile-module-maybe
       (expand-file-name "le-core-pm-use-package"
                         lightemacs-core-directory))
      (require 'le-core-pm-use-package))

     (t
      (error
       (concat "[lightemacs]"
               "Invalid value for `lightemacs-package-manager': '%S'. Valid "
               "choices are: 'straight, 'elpaca, or 'use-package.")
       lightemacs-package-manager))))

  (setq lightemacs-use-package--package-manager-loaded t))

;;; Prepare hook `lightemacs-after-init-hook'

(defun lightemacs--run-after-init-hook ()
  "Run `lightemacs-after-init-hook' at the appropriate time."
  (unwind-protect
      (run-hooks 'lightemacs-after-init-hook)
    (cond
     ((eq lightemacs-package-manager 'elpaca)
      (remove-hook 'elpaca-after-init-hook 'lightemacs--run-after-init-hook))

     (t
      (remove-hook 'after-init-hook 'lightemacs--run-after-init-hook)))))

(defun lightemacs--run-emacs-startup-hook ()
  "Run `lightemacs-emacs-startup-hook' at the appropriate time."
  (unwind-protect
      (run-hooks 'lightemacs-emacs-startup-hook)
    (cond
     ((eq lightemacs-package-manager 'elpaca)
      (remove-hook 'elpaca-after-init-hook 'lightemacs--run-emacs-startup-hook))

     (t
      (remove-hook 'emacs-startup-hook 'lightemacs--run-emacs-startup-hook)))))

(cond
 ((eq lightemacs-package-manager 'elpaca)
  (add-hook 'elpaca-after-init-hook 'lightemacs--run-after-init-hook 104)
  (add-hook 'elpaca-after-init-hook 'lightemacs--run-emacs-startup-hook 105))

 (t
  (add-hook 'after-init-hook 'lightemacs--run-after-init-hook 104)
  (add-hook 'emacs-startup-hook 'lightemacs--run-emacs-startup-hook 105)))

;;; Run hooks: `lightemacs-before-modules-hook'

(run-hooks 'lightemacs-before-modules-hook)

;;; Additional paths

(defvar treesit-extra-load-path nil)

(let ((ts-dir (expand-file-name "tree-sitter" lightemacs-var-directory)))
  (add-to-list 'treesit-extra-load-path ts-dir))

;;; Generate loadable config

(defvar lightemacs-autogen-config-file nil)

(defun lightemacs--format-value (val)
  "Format VAL into a string representation for Elisp configuration."
  (cond
   ;; Booleans (t or nil)
   ((memq val '(t nil))
    (symbol-name val))
   ;; Strings (quoted)
   ((stringp val)
    (prin1-to-string val))
   ;; Lists (quoted as '(list))
   ((listp val)
    (format "'%s" (prin1-to-string val)))
   ;; Symbols (quoted as 'symbol)
   ((symbolp val)
    (format "'%s" (symbol-name val)))
   ;; Fallback for numbers or other types
   (t
    (prin1-to-string val))))

;; Inject the config path into the async native compiler environment
;; Inject the config path into the async native compiler environment
(setq native-comp-async-env-modifier-form
      `(progn
         (setq lightemacs-package-manager ',lightemacs-package-manager)
         (setq lightemacs-use-package--compiler-env-loaded t)

         (setq lightemacs-user-directory ,lightemacs-user-directory)
         (setq lightemacs-local-directory ,lightemacs-local-directory)
         (setq lightemacs-var-directory ,lightemacs-var-directory)
         (setq lightemacs-core-directory ,lightemacs-core-directory)

         (setq user-emacs-directory ,lightemacs-var-directory)

         ,(cond
           ((eq lightemacs-package-manager 'straight)
            `(progn
               (unless (fboundp 'straight-use-package)
                 ;; Disable all straight.el modification checks and builds
                 (setq straight-disable-compile t)
                 (setq straight-disable-native-compile t)
                 (setq straight-check-for-modifications nil)
                 (let ((lightemacs--no-bootstrap t))
                   (load (expand-file-name "le-core-pm-straight.el"
                                           ',lightemacs-core-directory)
                         nil
                         'nomessage)))))
           ((eq lightemacs-package-manager 'elpaca)
            `(progn
               (unless (fboundp 'elpaca)
                 (let ((lightemacs--no-bootstrap t))
                   (load (expand-file-name "le-core-pm-elpaca.el"
                                           ',lightemacs-core-directory)
                         nil
                         'nomessage))))))

         ;; Safely inject package variables only if they are bound
         ,@(when (boundp 'use-package-expand-minimally)
             `((setq use-package-expand-minimally ,use-package-expand-minimally)))

         ,@(when (boundp 'use-package-always-ensure)
             `((setq use-package-always-ensure ,use-package-always-ensure)))

         ,@(when (boundp 'package-user-dir)
             `((setq package-user-dir ,package-user-dir)))

         (setq treesit-extra-load-path ',treesit-extra-load-path)))

;;; Load modules

(require 'lightemacs-use-package)

(if (fboundp 'lightemacs-module-load)
    (progn
      (lightemacs-module-load lightemacs-core-modules)
      (lightemacs-module-load lightemacs-modules))
  (error "Undefined function: lightemacs-module-load"))

;;; Run hooks: `lightemacs-after-modules-hook'

(run-hooks 'lightemacs-after-modules-hook)

;;; Provide

(provide 'init)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; init.el ends here
