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

(unless (memq lightemacs-package-manager '(straight elpaca builtin-package
                                                    ;; Deprecated:
                                                    use-package))
  (error "The value '%s' is not a valid `lightemacs-package-manager'"
         lightemacs-package-manager))

;;; require

(require 'lightemacs)

;;; Run hooks: `lightemacs-pre-init-hook'

(run-hooks 'lightemacs-pre-init-hook)

;;; Load: init.el

(when (and (fboundp 'lightemacs-load-user-init)
           (boundp 'minimal-emacs-user-directory))
  (lightemacs-load-user-init
   (expand-file-name "init.el" minimal-emacs-user-directory)))

;;; Configure the package manager

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
     ((or (eq lightemacs-package-manager 'builtin-package)
          (eq lightemacs-package-manager 'use-package))
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

(defun lightemacs--generate-le-autogen-config ()
  "Generate the Elisp config."
  ;; Load le-autogen-config here
  (let* ((file-dir lightemacs-var-directory)
         (file-path (expand-file-name "le-autogen-config.el" file-dir)))
    (setq lightemacs-autogen-config-file file-path)
    ;; TODO Find another solution
    (setenv "LIGHTEMACS__INTERNAL_LOAD_CONFIG" file-path)
    ;; Ensure the local directory exists
    (unless (file-directory-p file-dir)
      (make-directory file-dir t))

    ;; Write the configuration to the file
    (with-temp-file file-path
      ;; Bind print variables to ensure large lists or deep structures
      ;; are not truncated by user settings during serialization.
      (let ((print-length nil)
            (print-level nil)
            (print-quoted t))

        (insert ";;; le-autogen-config.el --- Auto-generated "
                "configuration -*- no-byte-compile: t; lexical-binding: t -*-\n\n")
        (insert ";; This file is auto-generated by Lightemacs\n\n")
        (insert (format ";; Generated on: %s\n\n" (current-time-string)))

        ;; Package manager
        (insert (format "(setq lightemacs-package-manager %s)\n"
                        (lightemacs--format-value lightemacs-package-manager)))
        (insert "(setq lightemacs-use-package--compiler-env-loaded t)\n")

        (insert "\n")

        (insert (format "(setq lightemacs-user-directory %s)\n"
                        (lightemacs--format-value lightemacs-user-directory)))
        (insert (format "(setq lightemacs-local-directory %s)\n"
                        (lightemacs--format-value lightemacs-local-directory)))
        (insert (format "(setq lightemacs-var-directory %s)\n"
                        (lightemacs--format-value lightemacs-var-directory)))
        (insert (format "(setq lightemacs-core-directory %s)\n\n"
                        (lightemacs--format-value lightemacs-core-directory)))
        (insert "(setq user-emacs-directory lightemacs-var-directory)\n")
        (insert "\n")

        (when (eq lightemacs-package-manager 'elpaca)
          (insert "(unless (fboundp 'elpaca)\n"
                  "  (let ((lightemacs--no-bootstrap t))\n"
                  "    (load (expand-file-name \"le-core-pm-elpaca.el\"\n"
                  "                            lightemacs-core-directory)\n"
                  "          nil\n"
                  "          'nomessage)))\n\n"))

        (when (eq lightemacs-package-manager 'straight)
          (insert
           "(setq straight-disable-compile t)\n"
           "(setq straight-disable-native-compile t)\n"
           ;; Force the correct directory to avoid ~/.emacs.d/ leaks
           "(setq straight-base-dir lightemacs-var-directory)\n"
           ;; Disable all straight.el modification checks and builds
           "(setq straight-check-for-modifications nil)\n"
           "(unless (fboundp 'straight-use-package)\n"
           "  (let ((lightemacs--no-bootstrap t))\n"
           "    (load (expand-file-name \"le-core-pm-straight.el\"\n"
           "                            lightemacs-core-directory)\n"
           "          nil\n"
           "          'nomessage)))\n\n"))

        ;; use-package optimization
        (let ((val (if (boundp 'use-package-expand-minimally)
                       use-package-expand-minimally
                     t)))
          (insert (format "(setq use-package-expand-minimally %s)\n"
                          (lightemacs--format-value val)))
          (setq use-package-expand-minimally val))

        ;; use-package ensure inheritance
        (when (boundp 'use-package-always-ensure)
          (insert
           (format "(setq use-package-always-ensure %s)\n"
                   (lightemacs--format-value use-package-always-ensure))))
        (insert (format "(setq package-user-dir %s)\n"
                        (lightemacs--format-value package-user-dir)))

        (when (boundp 'treesit-extra-load-path)
          (insert (format "(setq treesit-extra-load-path %s)\n"
                          (lightemacs--format-value treesit-extra-load-path))))

        ;; Initialize the package system so the compiler knows packages are
        ;; already installed and does not attempt to clone them over the network.
        (when (memq lightemacs-package-manager '(builtin-package
                                                 use-package))
          (insert "\n(setq package-enable-at-startup nil)\n\n")
          (insert "\n(package-initialize)\n\n"))

        (insert "\n;;; le-autogen-config.el ends here\n")))))

;; Call it first for `lightemacs-package-manager'
(lightemacs--generate-le-autogen-config)

;; Inject the config path into the async native compiler environment
(setq native-comp-async-env-modifier-form
      `(progn
         (setq user-emacs-directory ,lightemacs-var-directory)
         (setq lightemacs-user-directory ,lightemacs-user-directory)
         (setq lightemacs-local-directory ,lightemacs-local-directory)
         (setq lightemacs-var-directory ,lightemacs-var-directory)
         (setq lightemacs-core-directory ,lightemacs-core-directory)
         (setq lightemacs-package-manager ',lightemacs-package-manager)

         ;; TODO
         ;; (setq package-user-dir ,package-user-dir)

         ;; TODO include?
         ;; (setq treesit-extra-load-path ',treesit-extra-load-path)

         (setenv "LIGHTEMACS__INTERNAL_LOAD_CONFIG" ,lightemacs-autogen-config-file)))

;; TODO this is broken
;; (setq native-comp-async-env-modifier-form
;;       `(progn
;;          (unless (bound-and-true-p lightemacs-use-package--compiler-env-loaded)
;;            (load ,lightemacs-autogen-config-file nil 'nomessage nil t)
;;
;;            ;; TODO remove
;;            ;; (setq use-package-expand-minimally ,use-package-expand-minimally)
;;            ;; (setq use-package-always-ensure
;;            ;;       ,(bound-and-true-p use-package-always-ensure))
;;            ;;
;;            ;; ;; Inject core paths from the parent session
;;            ;;
;;            ;; ;; Inject conditional package manager initialization blocks
;;            ;; ,(cond
;;            ;;   ((eq lightemacs-package-manager 'straight)
;;            ;;    `(progn
;;            ;;       (unless (fboundp 'straight-use-package)
;;            ;;         (let ((lightemacs--no-bootstrap t))
;;            ;;           (load (expand-file-name "le-core-pm-straight.el"
;;            ;;                                   lightemacs-core-directory)
;;            ;;                 nil
;;            ;;                 'nomessage)))))
;;            ;;   ((eq lightemacs-package-manager 'elpaca)
;;            ;;    `(progn
;;            ;;       (unless (fboundp 'elpaca)
;;            ;;         (let ((lightemacs--no-bootstrap t))
;;            ;;           (load (expand-file-name "le-core-pm-elpaca.el"
;;            ;;                                   lightemacs-core-directory)
;;            ;;                 nil
;;            ;;                 'nomessage)))))
;;            ;;   ((memq lightemacs-package-manager '(builtin-package use-package))
;;            ;;    `(progn
;;            ;;       (package-initialize))))
;;            )))

;;; Load modules

(require 'lightemacs-use-package)
(require 'lightemacs-module)

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
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; init.el ends here
