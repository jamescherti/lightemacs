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

;;; Configuration

;; The init.el file serializes the package manager state into a dedicated,
;; loadable file (le-autogen-config.el). This file is injected it via an
;; environment variable or path discovery to ensure that macros from
;; use-package, straight, or elpaca expand correctly during asynchronous or
;; batch compilation without forcing a full, heavy framework initialization.
;;
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
(eval-and-compile
  (require 'use-package)
  ;; `use-package-normalize-keywords'
  (require 'use-package-core)

  (defun lightemacs-use-package--detect-autogen-config ()
    "Dynamically locate the autogen configuration file.
Returns the absolute path to the configuration file, or nil if not found."
    ;; (let ((file (expand-file-name "var/le-autogen-config.el" lightemacs-user-directory)))
    ;;   (if (file-regular-p file)
    ;;       file
    ;;     (error "The autogen config file doesn't exist: %s" file)))

    ;; TODO find another solution
    (getenv "LIGHTEMACS__INTERNAL_LOAD_CONFIG")

    ;; (let* ((framework-file (locate-library "lightemacs-use-package.el" t))
    ;;        (current-file
    ;;         (or
    ;;          ;; (and (fboundp 'macroexp-file-name) (macroexp-file-name))
    ;;          ;; (bound-and-true-p byte-compile-current-file)
    ;;          ;; load-file-name
    ;;          framework-file))
    ;;        ;; Resolve symlinks to prevent incorrect directory traversal
    ;;        (true-current (and current-file (file-truename current-file)))
    ;;        (search-dir (and true-current (file-name-directory true-current)))
    ;;        (root-dir (and search-dir (locate-dominating-file search-dir
    ;;                                                          "init.el"))))
    ;;
    ;;   (cond
    ;;    (root-dir
    ;;     (if (file-regular-p framework-file)
    ;;         (expand-file-name "var/le-autogen-config.el" root-dir)
    ;;       (error "The autogen config file does not exist: %s"
    ;;              (expand-file-name "var/le-autogen-config.el" root-dir))))
    ;;    ;; (t
    ;;    ;;  ;; Fallback: user-emacs-directory is globally inherited by default
    ;;    ;;  (expand-file-name "le-autogen-config.el" user-emacs-directory))
    ;;    ))
    )

  (when (and (not (bound-and-true-p lightemacs-use-package--compiler-env-loaded))
             (or (bound-and-true-p byte-compile-current-file)
                 (bound-and-true-p comp-compiling)))
    (let* (;; root-dir
           (config-file
            (lightemacs-use-package--detect-autogen-config)))
      (if (and config-file (file-regular-p config-file))
          (progn
            (message "[lightemacs] Loading the compiler configuration from: %s"
                     config-file)
            (load config-file nil 'nomessage nil t))
        (error "[lightemacs] Could not locate le-autogen-config.el")
        ;; (error
        ;;  (concat
        ;;   "[lightemacs] Could not locate le-autogen-config.el"
        ;;   " config:%s"
        ;;   " macroexp-file-name:%s"
        ;;   " byte-compile-current-file:%s"
        ;;   " load-file-name:%s"
        ;;   " lib:%s")
        ;;  config-file
        ;;  (and (fboundp 'macroexp-file-name) (macroexp-file-name))
        ;;  (bound-and-true-p byte-compile-current-file)
        ;;  load-file-name
        ;;  (locate-library "lightemacs-use-package.el" t))
        )))

  ;; Conditionally declare the package manager function for the compiler. At
  ;; this point, `lightemacs-package-manager' is guaranteed to be set correctly
  ;; for this compilation process.
  ;;
  ;; This silences warnings such as:
  ;; Warning (native-compiler): file.el: Warning: the function
  ;; `straight-use-package' might not be defined at runtime.
  (cond
   ((eq lightemacs-package-manager 'straight)
    (unless (fboundp 'straight-use-package)
      (autoload 'straight-use-package "straight")))
   ((eq lightemacs-package-manager 'elpaca)
    (unless (fboundp 'elpaca)
      (autoload 'elpaca "elpaca")))))

;;; Require

(require 'lightemacs) ; `lightemacs-verbose-message'

;;; Variables

(defvar lightemacs-use-package-refresh-contents t
  "If non-nil, `lightemacs-use-package' may refresh package contents once.")

(defvar lightemacs-use-package-modifiers nil
  "An alist of modifiers injected into `lightemacs-use-package' at expansion.
Elements should be structured as (PACKAGE-NAME :KEYWORD VALUE...).
For example: \\='((some-package :straight t :defer t)).")

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

(defun lightemacs-use-package--normalize (_name args)
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

  (let* ((ensure-is-member (memq :ensure args))
         (ensure-value (when ensure-is-member
                         (let ((val (cadr ensure-is-member)))
                           (if (keywordp val) t val))))
         (straight-is-member (memq :straight args)))

    (when (and ensure-is-member
               ensure-value)
      ;; To avoid conflict with straight
      (error
       "`lightemacs-use-package': The only supported value for :ensure is nil"))

    ;; (when straight-is-member
    ;;   ;; :straight is not supported by elpaca and the built-in use-package
    ;;   (error "`lightemacs-use-package': Modifying :straight is not allowed"))

    (cond
     ;; Straight
     ((eq lightemacs-package-manager 'straight)
      (setq args (copy-sequence args))
      (let ((vc-is-member (memq :vc args)))
        (if vc-is-member
            ;; `:vc' takes precedence over `:straight'
            ;; (setq args (append (list :straight nil)
            ;;                    args))
            ;; (error "`lightemacs-use-package': %s: :vc is not allowed when the package manager is straight" name)
            t
          ;; This part adds support for `:ensure nil`
          ;;
          ;; Straight mode
          ;; ensure is non-nil: straight = ensure-value
          ;; ensure is nil: straight is nil??
          (let ((straight-value (if ensure-is-member
                                    (if ensure-value t nil)
                                  (bound-and-true-p
                                   straight-use-package-by-default))))

            ;; Add straight
            (when (and (not straight-is-member) (not straight-value))
              (setq args (append (list :straight straight-value)
                                 args)))

            ;; (setq ensure-value straight-value)
            ;; Explicitly append :ensure nil so package.el never attempts an
            ;; install
            (setq args (append (list :ensure nil)
                               args))))))

     ;; Builtin package or Elpaca
     ;; -------------------------
     ;; ((or (eq lightemacs-package-manager 'builtin-package)
     ;;      (eq lightemacs-package-manager 'use-package)
     ;;      (eq lightemacs-package-manager 'elpaca-package))
     ;;
     ;;  ;; Remove straight
     ;;  ;; (when (memq :straight args)
     ;;  ;;   (setq args (lightemacs-use-package--plist-delete
     ;;  ;;                         args
     ;;  ;;                         :straight)))
     ;;  ;; Force :ensure t at compile time if it is not explicitly provided
     ;;  ;; (when (and (eq lightemacs-package-manager 'elpaca)
     ;;  ;;            (not ensure-is-member))
     ;;  ;;   (setq args (append args (list :ensure t)))
     ;;  ;;   (setq ensure-is-member t)
     ;;  ;;   (setq ensure-value t))
     ;;
     ;;  ;; Edge case: When Emacs runs async native compilation in an isolated,
     ;;  ;; noninteractive background worker process (emacs -Q -batch).
     ;;  ;;
     ;;  ;; In this isolated background environment, packages are not recognized
     ;;  ;; as installed because the main session's package state (like
     ;;  ;; package-alist or package-vc data) is not fully loaded. When the
     ;;  ;; compiler evaluates top-level forms or expands the use-package macro
     ;;  ;; for tmp-diff-hl.el, it sees the :vc and :ensure keywords. Thinking
     ;;  ;; the package is missing, it generates and executes the code to install
     ;;  ;; it (via package-vc-install).
     ;;  ;;
     ;;  ;; This installation attempt triggers two things that cause the
     ;;  ;; compilation log errors:
     ;;  ;;
     ;;  ;; It attempts to fetch archive contents to resolve dependencies,
     ;;  ;; causing the proxy authentication error.
     ;;  ;;
     ;;  ;; It attempts to clone the git repository, which prompts for user input
     ;;  ;; (Overwrite previous checkout...) and causes the background process to
     ;;  ;; hang or fail since it is running noninteractively.
     ;;  ;;
     ;;  ;; To fix this, we need to prevent use-package from attempting to
     ;;  ;; install packages during compilation.
     ;;  ;; (when (memq lightemacs-package-manager '(builtin-package
     ;;  ;;                                          use-package))
     ;;  ;;   ;; (let ((vc-is-member (plist-member args :vc)))
     ;;  ;;   ;;   (when (or
     ;;  ;;   ;;          ;; Async native JIT compilation always spawns an isolated
     ;;  ;;   ;;          ;; background worker process using emacs -batch. Because it
     ;;  ;;   ;;          ;; runs in batch mode, the noninteractive variable is
     ;;  ;;   ;;          ;; automatically set to t. This catches all async native
     ;;  ;;   ;;          ;; compilation.
     ;;  ;;   ;;          noninteractive
     ;;  ;;   ;;          ;; byte-compile-current-file: Compiling a file
     ;;  ;;   ;;          ;; synchronously/interactively (e.g., using M-x
     ;;  ;;   ;;          ;; emacs-lisp-native-compile or M-x byte-compile-file), Emacs
     ;;  ;;   ;;          ;; doesn't run in batch mode. However, native compilation
     ;;  ;;   ;;          ;; always runs the byte-compiler as its first pass to
     ;;  ;;   ;;          ;; generate the initial representation of the code.
     ;;  ;;   ;;          ;; Therefore, byte-compile-current-file will always be
     ;;  ;;   ;;          ;; non-nil during this phase.
     ;;  ;;   ;;          (bound-and-true-p byte-compile-current-file))
     ;;  ;;   ;;     (when vc-is-member
     ;;  ;;   ;;       (setq args
     ;;  ;;   ;;             (lightemacs-use-package--plist-delete args :vc)))))
     ;;  ;;
     ;;  ;;   ;; Replace :ensure with :ensure nil to prevent the native compiler
     ;;  ;;   ;; from downloading from repositories
     ;;  ;;   ;; (when (or
     ;;  ;;   ;;        ;; Async native JIT compilation always spawns an isolated
     ;;  ;;   ;;        ;; background worker process using emacs -batch. Because it
     ;;  ;;   ;;        ;; runs in batch mode, the noninteractive variable is
     ;;  ;;   ;;        ;; automatically set to t. This catches all async native
     ;;  ;;   ;;        ;; compilation.
     ;;  ;;   ;;        noninteractive
     ;;  ;;   ;;        ;; byte-compile-current-file: Compiling a file
     ;;  ;;   ;;        ;; synchronously/interactively (e.g., using M-x
     ;;  ;;   ;;        ;; emacs-lisp-native-compile or M-x byte-compile-file), Emacs
     ;;  ;;   ;;        ;; doesn't run in batch mode. However, native compilation
     ;;  ;;   ;;        ;; always runs the byte-compiler as its first pass to
     ;;  ;;   ;;        ;; generate the initial representation of the code.
     ;;  ;;   ;;        ;; Therefore, byte-compile-current-file will always be
     ;;  ;;   ;;        ;; non-nil during this phase.
     ;;  ;;   ;;        (bound-and-true-p byte-compile-current-file))
     ;;  ;;   ;;   (lightemacs-debug-message
     ;;  ;;   ;;     "[lightemacs] Added ':ensure nil' to the %s package for compilation"
     ;;  ;;   ;;     name)
     ;;  ;;   ;;   (setq args
     ;;  ;;   ;;         (lightemacs-use-package--plist-delete args :ensure))
     ;;  ;;   ;;   (setq args (append (list :ensure nil) args)))
     ;;  ;;   )
     ;;  )
     )
    args))

(defmacro lightemacs-use-package (name &rest args)
  "Provide a formal interface for package configuration via `use-package'.
NAME and ARGS are the same arguments as the `use-package' macro.
Normalization and manager selection occur at macro-expansion time."
  (declare (indent defun))
  (let* ((injected-args (cdr (assq name lightemacs-use-package-modifiers)))
         (combined-args (append injected-args args))
         (effective-args (lightemacs-use-package--normalize name combined-args)))
    `(progn
       (use-package ,name ,@effective-args))))

;;; Provide

(provide 'lightemacs-use-package)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lightemacs-use-package.el ends here
