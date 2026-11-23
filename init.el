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

;;; Run hooks: `lightemacs-pre-init-hook'

(run-hooks 'lightemacs-pre-init-hook)

;;; Load: init.el

(let ((minimal-emacs-init (expand-file-name "init.el" minimal-emacs-user-directory)))
  (if (boundp 'minimal-emacs-user-directory)
      (lightemacs-load-user-init minimal-emacs-init)
    (error "Undefined: minimal-emacs-user-directory")))

;;; Configure the package manager

(unless (memq lightemacs-package-manager '(straight elpaca builtin-package))
  (error "The value '%s' is not a valid `lightemacs-package-manager'"
         lightemacs-package-manager))

(defvar lightemacs--package-manager-loaded nil)

(unless lightemacs--package-manager-loaded
  (when (bound-and-true-p lightemacs-package-manager)
    (cond
     ;; Straight
     ((eq lightemacs-package-manager 'straight)
      (require 'le-core-pm-straight))

     ;; Elpaca
     ((eq lightemacs-package-manager 'elpaca)
      (require 'le-core-pm-elpaca))

     ;; use-package (built-in)
     ((or (eq lightemacs-package-manager 'builtin-package)
          (eq lightemacs-package-manager 'use-package))
      (require 'le-core-pm-use-package))

     (t
      (error
       (concat "[lightemacs]"
               "Invalid value for `lightemacs-package-manager': '%S'. Valid "
               "choices are: 'straight, 'elpaca, or 'use-package.")
       lightemacs-package-manager))))

  (setq lightemacs--package-manager-loaded t))

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

(unless (boundp 'treesit-extra-load-path)
  (setq treesit-extra-load-path nil))

(let ((ts-dir (expand-file-name "tree-sitter" lightemacs-var-directory)))
  (add-to-list 'treesit-extra-load-path ts-dir))

;;; Inject the configuration into the async native compiler environment

(setq native-comp-async-env-modifier-form
      `(progn
         (setq lightemacs-package-manager ',lightemacs-package-manager)
         (setq lightemacs-user-directory ,lightemacs-user-directory)
         (setq lightemacs-local-lisp-directory ,lightemacs-local-lisp-directory)
         (setq lightemacs-builtin-lisp-directory ,lightemacs-builtin-lisp-directory)
         (setq lightemacs-var-directory ,lightemacs-var-directory)
         (setq user-emacs-directory ,lightemacs-var-directory)

         ;; Safely inject package variables only if they are bound
         ,@(when (boundp 'use-package-expand-minimally)
             `((setq use-package-expand-minimally ,use-package-expand-minimally)))

         ,@(when (boundp 'use-package-always-ensure)
             `((setq use-package-always-ensure ,use-package-always-ensure)))

         ,@(when (boundp 'package-user-dir)
             `((setq package-user-dir ,package-user-dir)))

         (setq treesit-extra-load-path ',treesit-extra-load-path)

         ,(cond
           ((eq lightemacs-package-manager 'elpaca)
            `(progn
               (unless (fboundp 'elpaca)
                 (let ((lightemacs--no-package-manager-bootstrap t))
                   (load (expand-file-name "le-core-pm-elpaca.el"
                                           ',lightemacs-builtin-lisp-directory)
                         nil
                         'nomessage)))))

           ((eq lightemacs-package-manager 'straight)
            `(progn
               ;; The async workers run via emacs -batch and do not load your
               ;; full user configuration. Therefore, global variables like
               ;; create-lockfiles revert to their default value of t inside the
               ;; worker processes.
               ;;
               ;; When multiple workers initialize straight.el simultaneously,
               ;; they concurrently attempt to verify or regenerate
               ;; straight-autoloads.el.
               ;;
               ;; The first worker creates a file system lock
               ;; (.#straight-autoloads.el). When a second worker tries to
               ;; access the file, it encounters the lock. Since it runs
               ;; non-interactively in batch mode, it cannot prompt you for a
               ;; resolution and immediately throws a lock conflict error.
               ;;
               ;; The following fixes: ■ Warning (native-compiler): Error:
               ;; file-locked
               ;; ("~/.emacs.d/var/straight/build/straight/straight-autoloads.el"
               ;; "user@myhost (pid 111111)" "Cannot resolve lock conflict in
               ;; batch mode")
               ;;
               ;; This ensures the background compilation processes act strictly
               ;; as read-only environments regarding your package manager
               ;; state, eliminating the race condition on the autoload file.
               (setq create-lockfiles nil)
               (unless (fboundp 'straight-use-package)
                 ;; Disable all straight.el modification checks and builds
                 ;;
                 ;; When I injected (setq straight-disable-compile t) into the
                 ;; async background workers, I created a configuration
                 ;; mismatch. When the background worker loaded straight.el, it
                 ;; saw the new rules (compile = t / disabled) and compared them
                 ;; against the existing build cache (compile = nil / enabled).
                 (setq straight-disable-compile t)
                 (setq straight-disable-native-compile t)

                 (setq straight-check-for-modifications nil)
                 (let ((lightemacs--no-package-manager-bootstrap t))
                   (load (expand-file-name "le-core-pm-straight.el"
                                           ',lightemacs-builtin-lisp-directory)
                         nil
                         'nomessage))))))))

;;; Load modules

(lightemacs-module-load lightemacs-modules)

;;; Run hooks: `lightemacs-after-modules-hook'

(run-hooks 'lightemacs-after-modules-hook)

;;; End

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; init.el ends here
