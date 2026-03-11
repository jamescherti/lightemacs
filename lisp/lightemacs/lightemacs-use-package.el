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

;; When lightemacs-use-package.el is compiled, the following top-level code is
;; translated into byte-code and is not executed at that moment.
;;
;; Execution occurs when another file (for example le-easysession.el) that
;; depends on it is compiled. When the compiler encounters (require
;; 'lightemacs-use-package) in le-easysession.el, it determines that the package
;; must be loaded into memory in order to expand the macros it defines. To
;; obtain those macro definitions, the compiler loads the already compiled
;; lightemacs-use-package.elc file into its environment. In Emacs, loading a
;; file implies evaluating all top-level forms sequentially. Because the
;; background Emacs process is compiling le-easysession.el at that moment, the
;; variable byte-compile-current-file (or comp-compiling) evaluates to t. The
;; when condition therefore succeeds, the message is emitted to the compilation
;; log, and le-autogen-config.el is loaded into the compiler process so that the
;; macro expansion environment has the required context.
(when (bound-and-true-p byte-compile-current-file)
  (let* ((macroexp-file (and (fboundp 'macroexp-file-name)
                             (macroexp-file-name)))
         (byte-comp-cur-file (and (boundp 'byte-compile-current-file)
                                  byte-compile-current-file))
         (env-file (getenv "LIGHTEMACS__INTERNAL_LOAD_CONFIG"))
         (current-file (or env-file
                           ;; Return the name of the file from which the code
                           ;; comes.
                           macroexp-file
                           byte-comp-cur-file
                           load-file-name))
         (current-dir (if current-file
                          (file-name-directory current-file)
                        default-directory))
         (config-file (expand-file-name "le-autogen-config.el" current-dir)))
    (message "[lightemacs] Loading the compiler configuration from: %s"
             current-file)
    (if (and current-file (file-exists-p config-file))
        ;; Pass 't' to MUST-SUFFIX to enforce exact file name matching
        (load config-file nil nil nil t)
      (error "[lightemacs] Could not locate le-autogen-config.el for %s"
             (or current-file default-directory)))))

(eval-and-compile
  (require 'lightemacs) ; lightemacs-verbose-message
  (require 'use-package)
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

;; TODO remove
;; (defun lightemacs-use-package--before-package (name
;;                                                effective-args
;;                                                _normalized-args
;;                                                ensure-value
;;                                                &rest _args)
;;   "Ensure a package is installed before `lightemacs-use-package' expands.
;;
;; NAME is the symbol identifying the package to install or configure.
;; EFFECTIVE-ARGS is the plist of keyword arguments supplied to `use-package'.
;; NORMALIZED-ARGS is the normalized version of EFFECTIVE-ARGS.
;; ENSURE-VALUE is the value of :ensure.
;;
;; This function performs the following steps when the package manager
;; is `use-package' and the :ensure property is non-nil."
;;   (lightemacs-debug-message
;;     "lightemacs-use-package--before-package: %s (ensure %s): %s"
;;     name ensure-value effective-args)
;;   ;; (when ensure-value
;;   ;;   (cond
;;   ;;    ((eq lightemacs-package-manager 'straight)
;;   ;;     ;; Strict validation: fail loudly if straight is not loaded yet
;;   ;;     (if (fboundp 'straight-use-package)
;;   ;;         (let ((straight-recipe (plist-get effective-args :straight)))
;;   ;;           ;; Execute straight directly so it runs during compilation and runtime
;;   ;;           (straight-use-package (if (and straight-recipe
;;   ;;                                          (not (eq straight-recipe t)))
;;   ;;                                     straight-recipe
;;   ;;                                   name)))
;;   ;;       (error "[lightemacs] 'straight-use-package' is void. You must bootstrap straight.el before loading %s" name)))
;;   ;;
;;   ;;    ;; ((and (or (eq lightemacs-package-manager 'builtin-package)
;;   ;;    ;;           (eq lightemacs-package-manager 'use-package))
;;   ;;    ;;       ensure-value
;;   ;;    ;;       (fboundp 'use-package-ensure-function)
;;   ;;    ;;       (not (memq name lightemacs-use-package--installed))
;;   ;;    ;;       ;; Do not refresh when :vc is used
;;   ;;    ;;       (not (plist-member normalized-args :vc)))
;;   ;;    ;;  (when (and lightemacs-use-package-refresh-contents
;;   ;;    ;;             (or (eq lightemacs-package-manager 'builtin-package)
;;   ;;    ;;                 (eq lightemacs-package-manager 'use-package))
;;   ;;    ;;             ensure-value
;;   ;;    ;;             ;; TODO alternative to package-installed-p?
;;   ;;    ;;             (not (package-installed-p name)))
;;   ;;    ;;    ;; Refresh packages
;;   ;;    ;;    (unless lightemacs-use-package--packages-refreshed
;;   ;;    ;;      (lightemacs-verbose-message
;;   ;;    ;;        "Refreshing package contents before installing: %s" name)
;;   ;;    ;;      (setq lightemacs-use-package--packages-refreshed t)
;;   ;;    ;;      (condition-case err
;;   ;;    ;;          (package-refresh-contents)
;;   ;;    ;;        (error
;;   ;;    ;;         (display-warning 'lightemacs
;;   ;;    ;;                          (format "Failed to install package %s: %s"
;;   ;;    ;;                                  name (error-message-string err))
;;   ;;    ;;                          :error)))))
;;   ;;    ;;
;;   ;;    ;;  ;; Install the package
;;   ;;    ;;  (lightemacs-verbose-message "use-package: Installing %s" name)
;;   ;;    ;;  (when (fboundp 'use-package-ensure-function)
;;   ;;    ;;    (funcall use-package-ensure-function name (list ensure-value) nil))
;;   ;;    ;;  (push name lightemacs-use-package--installed))
;;   ;;
;;   ;;    ;; ((eq lightemacs-package-manager 'elpaca)
;;   ;;    ;;  (unless (fboundp 'elpaca)
;;   ;;    ;;    (error "[lightemacs] 'elpaca' is void. You must bootstrap elpaca before loading %s" name))
;;   ;;    ;;  (elpaca name))
;;   ;;    ))
;;   )

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
ARGS is the raw property list of keywords supplied to `use-package'.

This function processes the raw property list ARGS to ensure the
appropriate package management keywords are present before passing
them to `use-package'."
  (unless (memq lightemacs-package-manager '(straight elpaca builtin-package
                                                      ;; Deprecated:
                                                      use-package))
    (error "The value '%s' is not a valid `lightemacs-package-manager'"
           lightemacs-package-manager))

  (let* ((effective-args (copy-sequence args))
         ;; (normalized-args
         ;;  (when (and name
         ;;             effective-args)
         ;;    (let ((use-package-always-ensure nil))
         ;;      (use-package-normalize-keywords name effective-args))))
         (disabled-is-member (plist-member effective-args :disabled))
         (disabled-value (when disabled-is-member
                           (car (plist-get effective-args :disabled))))
         (ensure-is-member (plist-member effective-args :ensure))
         (ensure-value (when ensure-is-member
                         (car (plist-get effective-args :ensure)))))

    ;; (when (plist-member normalized-args :ensure)
    ;;   (error ":ensure is not part of normalized keywords"))

    ;; Short-circuit the expansion entirely if the package is disabled.
    (unless disabled-value
      (cond
       ;; Straight
       ;; --------
       ((eq lightemacs-package-manager 'straight)
        ;; (message "IT IS STRAIGHT")
        (let ((straight-is-member (plist-member effective-args :straight)))
          ;; Explicitly append :ensure nil so package.el never attempts an
          ;; install
          ;; (setq effective-args (append (list :ensure nil) effective-args))

          ;; TODO forbit using :straight

          ;; Remove :vc
          ;; (message "STRAIGHT IS MEMBER: %s" straight-is-member)
          (when (not straight-is-member)
            (let ((vc-is-member (plist-member effective-args :vc)))
              (when vc-is-member
                (setq effective-args (lightemacs-use-package--plist-delete
                                      effective-args :vc))))

            ;; TODO support
            ;; (bound-and-true-p straight-use-package-by-default)

            (when (and ensure-is-member
                       ensure-value)
              (error "When straight is enabled, the only supported value for :ensure is nil"))

            ;; If ':ensure nil' is present, translate that to ':straight nil'. Default to t.
            (let ((straight-value (if ensure-is-member
                                      (if ensure-value
                                          t
                                        nil)
                                    ;; TODO global value?
                                    t)))
              (lightemacs-debug-message
                "[lightemacs] Added ':straight %s' to the %s package"
                straight-value name)

              (unless straight-value
                (setq effective-args (append (list :straight straight-value)
                                             effective-args)))


              ;; (setq ensure-is-member straight-value)
              (setq ensure-value straight-value)

              (unless ensure-is-member ; not user specified
                (lightemacs-debug-message
                  "[lightemacs] Added ':ensure nil' to the %s package" name)
                (setq effective-args (append (list :ensure nil)
                                             effective-args))))


            ;; (message "FINAL ENSURE%s" ensure-value)
            )))

       ;; Builtin package or Elpaca
       ;; -------------------------
       ((memq lightemacs-package-manager '(builtin-package
                                           use-package
                                           elpaca))
        ;; Remove straight
        (when (plist-member effective-args :straight)
          (setq effective-args (lightemacs-use-package--plist-delete
                                effective-args
                                :straight)))

        ;; Force :ensure t at compile time if it is not explicitly provided
        (when (and (eq lightemacs-package-manager 'elpaca)
                   (not ensure-is-member))
          (setq effective-args (append effective-args (list :ensure t)))
          (setq ensure-is-member t)
          (setq ensure-value t))

        ;; Remove :vc
        (let ((vc-is-member (plist-member effective-args :vc)))
          (when (and vc-is-member
                     (or noninteractive
                         (bound-and-true-p byte-compile-current-file)))
            (setq effective-args
                  (lightemacs-use-package--plist-delete effective-args :vc)))

          ;; (when (and (not vc-is-member)
          ;;            (not (eq lightemacs-package-manager 'elpaca))
          ;;            use-package-always-ensure)
          ;;    ;; Always replace :ensure with :ensure nil to prevent the native
          ;;    ;; compiler from downloading from repositories such as MELPA
          ;;    (lightemacs-debug-message
          ;;      "[lightemacs] Added ':ensure nil' to the %s package" name)
          ;;    (setq effective-args
          ;;          (lightemacs-use-package--plist-delete effective-args :ensure))
          ;;    (setq effective-args (append (list :ensure nil) effective-args)))
          ))))
    ;; Return the 3 elements as a list
    (list effective-args
          nil ; removed: normalized-args
          (if ensure-value
              t
            nil))))

(defmacro lightemacs-use-package (name &rest args)
  "Provide a formal interface for package configuration via `use-package'.
NAME and ARGS are the same arguments as the `use-package' macro.
Normalization and manager selection occur at macro-expansion time."
  (declare (indent defun))
  ;; (message
  ;;  "(Before) PACKAGE MANAGER for %s: %s" name lightemacs-package-manager)
  (let* ((normalized-result (lightemacs-use-package--normalize name args))
         (effective-args (nth 0 normalized-result))
         (_normalized-args (nth 1 normalized-result))
         (_ensure-value (nth 2 normalized-result)))
    `(progn
       ;; TODO remove
       ;; (message "(After) PACKAGE MANAGER for %s: %s"
       ;;          ',name lightemacs-package-manager)
       ;;
       ;; (message "[DEBUG] Ensure:%S effective:%S normalized-args:%S"
       ;;          ',ensure-value ',effective-args ',normalized-args)
       ;; This block is compiled into the .elc.
       ;; During batch compilation, the condition is true (it is compiling),
       ;; so the `unless' body is skipped and NOT expanded or executed.
       ;; TODO Replace with with an advice
       ;; (lightemacs-use-package--before-package ',name ',effective-args
       ;;                                         ',normalized-args
       ;;                                         ',ensure-value)
       ;; TODO should I use this one instead?
       ;; (unless (or noninteractive
       ;;             (bound-and-true-p byte-compile-current-file))
       ;;   (lightemacs-use-package--before-package ',name ',effective-args
       ;;                                           ',normalized-args
       ;;                                           ',ensure-value))

       (use-package ,name ,@effective-args))))

;;; Provide

(provide 'lightemacs-use-package)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lightemacs-use-package.el ends here
