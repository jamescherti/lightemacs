;;; lightemacs-use-package.el --- lightemacs-use-package -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Package manager helpers.

;;; Code:

;;; Require

(require 'le-core-defun)
(require 'le-core-defaults)
(require 'use-package)
(eval-when-compile
  ;; `use-package-normalize-keywords'
  (require 'use-package-core))

;;; Variables

(defvar lightemacs-use-package-refresh-contents t
  "If non-nil, `lightemacs-use-package' may refresh package contents once.")

;; Internal variables


(defvar lightemacs-use-package--packages-refreshed nil
  "Non-nil if package contents have been refreshed during the current session.
Used by `lightemacs-use-package--before-package' to ensure that
`package-refresh-contents' is invoked at most once per Emacs session, avoiding
redundant network calls when installing multiple packages.")

(defvar lightemacs-use-package--installed nil
  "List of package symbols that have been installed during this session.
Used as a cache by `lightemacs-use-package--before-package' to skip re-checking
`package-installed-p' for packages that were already installed, improving
startup performance when configuring multiple packages.")

;;; lightemacs-use-package macro

(defun lightemacs-use-package--before-package (name
                                               _effective-args
                                               _normalized-args
                                               ensure-value
                                               &rest _args)
  "Ensure a package is installed before `lightemacs-use-package' expands.

NAME is the symbol identifying the package to install or configure.
EFFECTIVE-ARGS is the plist of keyword arguments supplied to `use-package'.
NORMALIZED-ARGS is the normalized version of NORMALIZED-ARGS.
ENSURE-VALUE is the value of :ensure.

This function performs the following steps when the package manager
is `use-package' and the :ensure property is non-nil."
  (when (and lightemacs-use-package-refresh-contents
             (eq lightemacs-package-manager 'use-package)
             ensure-value
             ;; TODO alternative to package-installed-p
             (not (package-installed-p name)))
    ;; Refresh packages
    (unless lightemacs-use-package--packages-refreshed
      (lightemacs-verbose-message
        "Refreshing package contents before installing %s" name)
      (setq lightemacs-use-package--packages-refreshed t)
      (condition-case err
          (package-refresh-contents)
        (error
         (display-warning 'lightemacs
                          (format "Failed to install package %s: %s"
                                  name (error-message-string err))
                          :error)))))

  (cond
   ;; TODO this is not idempotent
   ;; ;; Elpaca: Uses its own asynchronous queuing system.
   ;; ((and (eq lightemacs-package-manager 'elpaca)
   ;;       ensure-value
   ;;       (fboundp 'elpaca)
   ;;       (not (memq name lightemacs-use-package--installed)))
   ;;  (lightemacs-verbose-message "elpaca: Installing %s" name)
   ;;  (elpaca name)
   ;;  (push name lightemacs-use-package--installed))
   ;;
   ;; ;; Straight: Uses its own recipe-based cloning system.
   ;; ((and (eq lightemacs-package-manager 'straight)
   ;;       ensure-value
   ;;       (fboundp 'straight-use-package)
   ;;       (not (memq name lightemacs-use-package--installed)))
   ;;  (lightemacs-verbose-message "straight: Installing %s" name)
   ;;  (straight-use-package name)
   ;;  (push name lightemacs-use-package--installed))

   ((and (eq lightemacs-package-manager 'use-package)
         ensure-value
         (fboundp 'use-package-ensure-function)
         (not (memq name lightemacs-use-package--installed)))
    ;; Install the package
    (lightemacs-verbose-message "use-package: Installing %s" name)
    (when (fboundp 'use-package-ensure-function)
      (funcall use-package-ensure-function name (list ensure-value) nil))
    (push name lightemacs-use-package--installed))))

;; (defun lightemacs--set-keyword (args keyword &rest forms)
;;   "Safely set KEYWORD to FORMS in ARGS, replacing any existing declarations."
;;   (append (cons keyword forms) (lightemacs-use-package-delete-keyword args keyword)))

(defun lightemacs-use-package--plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun lightemacs-use-package--normalize (name args)
  "Normalize ARGS for the package NAME based on the active manager.

NAME is the symbol identifying the package.
ARGS is the raw property list of keywords supplied to `use-package\='.

This function processes the raw property list ARGS to ensure the
appropriate package management keywords are present before passing
them to `use-package\='."
  (unless (memq lightemacs-package-manager '(straight elpaca use-package))
    (error "The value '%s' is not a valid `lightemacs-package-manager'"
           lightemacs-package-manager))

  (let* ((effective-args (copy-sequence args))
         (normalized-args
          (when (and name
                     effective-args)
            (use-package-normalize-keywords name effective-args)))
         (disabled-is-member (plist-member normalized-args :disabled))
         (disabled-value (when disabled-is-member
                           (car (plist-get normalized-args :disabled))))
         (ensure-is-member (plist-member normalized-args :ensure))
         (ensure-value (when ensure-is-member
                         (car (plist-get normalized-args :ensure)))))
    ;; Short-circuit the expansion entirely if the package is disabled.
    (unless disabled-value
      (cond
       ;; Straight
       ((eq lightemacs-package-manager 'straight)
        (let ((straight-is-member (plist-member effective-args :straight))
              (ensure-bool-value (if ensure-value t nil)))
          (when ensure-is-member
            (setq effective-args (lightemacs-use-package--plist-delete
                                  effective-args
                                  :ensure)))

          ;; Add :straight ensure-value?
          (when (not straight-is-member)
            ;; If ':ensure nil' is present, translate that to
            ;; ':straight nil'
            (lightemacs-debug-message
              "[lightemacs] Added ':straight %s' to the %s package"
              ensure-bool-value name)
            ;; TODO: Should we just copy the value of ensure t or nil into
            ;; :straight?
            (setq effective-args
                  (append (list :straight
                                (if ensure-is-member
                                    ensure-bool-value
                                  t)
                                :ensure nil)
                          effective-args)))))

       ;; Use-package or Elpaca
       ;; TODO how about elpaca? (Answer: Elpaca uses standard :ensure syntax)
       ((memq lightemacs-package-manager '(use-package elpaca))
        ;; TODO remove :straight, in this case
        ;; (when (plist-member effective-args :straight)
        ;;   (setq effective-args (lightemacs-use-package--plist-delete
        ;;                         effective-args
        ;;                         :straight)))

        ;; TODO should this be removed when use-package-always-ensure is non-nil
        (when (bound-and-true-p lightemacs-debug)
          (message "[lightemacs] Added ':ensure nil' to the %s package"
                   name))
        (when ensure-is-member
          (lightemacs-use-package--plist-delete effective-args :ensure))
        (setq effective-args (append (list :ensure nil) effective-args)))))
    ;; Return the 3 elements as a list
    (list effective-args
          normalized-args
          (if ensure-is-member
              ensure-value
            t))))

(defmacro lightemacs-use-package (name &rest args)
  "Provide a formal interface for package configuration via `use-package'.
NAME and ARGS are the same arguments as the `use-package' macro.
Normalization and manager selection occur at macro-expansion time."
  (declare (indent defun))
  (pcase-let ((`(,effective-args ,normalized-args ,ensure-value)
               (lightemacs-use-package--normalize name args)))
    `(progn
       ;; This block is compiled into the .elc.
       ;; During batch compilation, the condition is true (it is compiling),
       ;; so the `unless` body is skipped and NOT expanded or executed.
       (unless (bound-and-true-p byte-compile-current-file)
         (lightemacs-use-package--before-package ',name ',effective-args
                                                 ',normalized-args
                                                 ',ensure-value))

       (use-package ,name ,@effective-args))))

;; (defmacro lightemacs-use-package (name &rest args)
;;   "Provide a formal interface for package configuration via `use-package'.
;; NAME and ARGS are the same arguments as the `use-package' macro.
;; Normalization and manager selection occur at macro-expansion time."
;;   (declare (indent defun))
;;   (pcase-let ((`(,effective-args ,normalized-args ,ensure-value)
;;                (lightemacs-use-package--normalize name args)))
;;     `(progn
;;        ;; This block is executed at runtime but skipped during compilation.
;;        (unless (bound-and-true-p byte-compile-current-file)
;;          (lightemacs-use-package--before-package ',name ',effective-args
;;                                                  ',normalized-args
;;                                                  ',ensure-value))
;;
;;        ;; The compiler must see use-package, but with :ensure forced to nil
;;        (use-package ,name ,@effective-args))))

;; (defmacro lightemacs-use-package (name &rest args)
;;   "Provide a formal interface for package configuration via `use-package`.
;; NAME and ARGS are the same arguments as the `use-package' macro."
;;   (declare (indent defun))
;;   (pcase-let ((`(,effective-args ,normalized-args ,ensure-value)
;;                (lightemacs-use-package--normalize name args)))
;;     (let* (;; We wrap in a 'let' to neutralize the downloader during expansion
;;            ;; (use-package-always-ensure nil)
;;            ;;
;;            ;; We generate the expansion of the inner use-package macro
;;            (expansion (macroexpand-all `(use-package ,name ,@effective-args))))
;;
;;       ;; 3. SIDE EFFECT: Print the expansion to the compiler's output
;;       (when (or (bound-and-true-p pre-commit-elisp-debug)
;;                 (bound-and-true-p byte-compile-current-file))
;;         (message "[DEBUG] Final expansion for %s:\n%S"
;;                  name (pp-to-string expansion)))
;;
;;       ;; 4. RETURN the expansion inside your progn
;;       `(progn
;;          (unless (bound-and-true-p byte-compile-current-file)
;;            (lightemacs-use-package--before-package ',name ',effective-args
;;                                                    ',normalized-args
;;                                                    ',ensure-value)
;;
;;            ;; Inject the already-expanded code directly
;;            ,expansion
;;
;;            ;; (use-package ,name ,@effective-args)
;;            )))))

;;; Provide

(provide 'lightemacs-use-package)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lightemacs-use-package.el ends here
