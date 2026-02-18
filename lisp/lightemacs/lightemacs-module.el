;;; lightemacs-module.el --- lightemacs-module -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Initialize the package manager.

;;; Code:

;;; Require
(require 'le-core-defaults)

;;; Choose the package manager

(when (bound-and-true-p lightemacs-package-manager)
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
    (error
     (concat "[lightemacs]"
             "Invalid value for `lightemacs-package-manager': '%S'. Valid "
             "choices are: 'straight, 'elpaca, or 'use-package.")
     lightemacs-package-manager))))

;;; Misc macros

(defmacro lightemacs-module-hooks (name func hooks)
  "Register hooks where FUNC should be enabled for the package NAME.

This defines a variable named `lightemacs-NAME-target-hooks' (if it does not
already exist) initialized with HOOKS. It then iterates over that variable,
adding FUNC to each hook found.

NAME should be the package symbol (e.g., \='flycheck).
FUNC should be the function symbol (e.g., \='flycheck-mode).
HOOKS should be a list of hook symbols (e.g., \='(prog-mode-hook))."
  (declare (indent 1) (debug t))
  (let ((var (intern (format "lightemacs-%s-target-hooks" name)))
        (docstring (format "List of hooks where `%s' is enabled." func)))
    `(progn
       (defvar ,var ,hooks ,docstring)
       ;; Ensure we treat the variable as a list, even if the user set it to a
       ;; single symbol
       (dolist (hook (if (listp ,var) ,var (list ,var)))
         (add-hook hook ',func)))))

(defmacro lightemacs-module-bind (module &rest body)
  "Define key bindings for MODULE with BODY, unless inhibited.

This macro introduces an inhibition variable named:
`lightemacs-MODULE-inhibit-keybindings'.

When non-nil, BODY will not be evaluated, thereby preventing the installation of
the specified key bindings."
  (declare (indent 1) (debug t))
  (let ((inhibit-var (intern (format "lightemacs-%s-inhibit-keybindings"
                                     module))))
    `(progn
       (defvar ,inhibit-var nil
         ,(format "Prevent configuring `%s' keybindings.

When this variable is set to a non-nil value, any key bindings that would
normally be defined through `lightemacs-define-*' macros are skipped
for `%s'.

This allows users to disable or override the default Lightemacs key
configuration for that func without modifying the macro definition itself."
                  module module))
       (unless ,inhibit-var
         ,@body))))

(defmacro lightemacs-module-setq-maybe (name &rest args)
  "Set default values for variables in module NAME.

ARGS is a list of alternating variable-value pairs.

If a variable is already bound, its value is preserved (ignored).
If it is unbound, it is set to the provided value.

This macro checks `lightemacs-NAME-inhibit-defaults'. If non-nil,
all settings in this block are skipped."
  (declare (indent 1) (debug t))
  (let ((inhibit-var (intern (format "lightemacs-%s-inhibit-defaults" name)))
        (forms nil)
        var val)

    ;; Compile-time: Unroll the loop into individual checks
    (while args
      (setq var (pop args))
      (setq val (pop args))
      ;; Use `set-default` to ensure we set the global value (like defvar)
      (push `(unless (boundp ',var) (set-default ',var ,val)) forms))

    ;; Runtime: Single optimized check
    `(unless (bound-and-true-p ,inhibit-var)
       ,@(nreverse forms))))

;;; lightemacs-module macro

(defvar lightemacs-module--packages-refreshed nil
  "Non-nil if package contents have been refreshed during the current session.
Used by `lightemacs-module--before-package' to ensure that
`package-refresh-contents' is invoked at most once per Emacs session, avoiding
redundant network calls when installing multiple packages.")

(defvar lightemacs-module--installed nil
  "List of package symbols that have been installed during this session.
Used as a cache by `lightemacs-module--before-package' to skip re-checking
`package-installed-p' for packages that were already installed, improving
startup performance when configuring multiple packages.")

(defmacro lightemacs-module--verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  (declare (indent 0) (debug t))
  `(progn
     (when (or lightemacs-verbose lightemacs-debug)
       (message (concat "[lightemacs] " ,(car args)) ,@(cdr args)))))

(define-obsolete-function-alias
  'lightemacs--before-use-package
  'lightemacs-module--before-package
  "2026-02-17")

;; move this to a defvar function
(defun lightemacs-module--before-package (name plist)
  "Ensure a package is installed before `lightemacs-module' expands.

NAME is the symbol identifying the package to install or configure.
PLIST is the property list of keyword arguments supplied to `use-package'.

This function performs the following steps when the package manager
is `use-package' and the :ensure property is non-nil."
  ;; TODO Support load-path and make it install packages
  (when (and (bound-and-true-p lightemacs-package-manager)
             (eq lightemacs-package-manager 'use-package))
    (let* ((ensure-member (plist-member plist :ensure))
           (ensure-value (if ensure-member
                             (plist-get plist :ensure)
                           (bound-and-true-p use-package-always-ensure))))
      (when (and ensure-value
                 (not (memq name lightemacs-module--installed))
                 (not (package-installed-p name)))
        ;; Refresh packages
        (when (and (not lightemacs-module--packages-refreshed)
                   lightemacs-module-refresh-contents)
          (lightemacs-module--verbose-message
            "Refreshing package contents before installing %s"
            name)
          (setq lightemacs-module--packages-refreshed t)
          (condition-case err
              (package-refresh-contents)
            (error
             (display-warning 'lightemacs
                              (format "Failed to install package %s: %s"
                                      name (error-message-string err))
                              :error))))

        ;; Install the package
        (lightemacs-module--verbose-message "Installing %s" name)
        (when (fboundp 'use-package-ensure-function)
          (funcall use-package-ensure-function name (list ensure-value) nil))
        (push name lightemacs-module--installed)))))

(defmacro lightemacs-module-package (name &rest args)
  "Provide a formal interface for package configuration via `use-package'.

NAME designates the package symbol.
ARGS represents the property list of configuration parameters.

If :ensure is explicitly nil and no :straight declaration exists,
append (:straight nil) to ARGS. Invokes `lightemacs-module--before-package`
with the resulting arguments prior to expanding `use-package`."
  (declare (indent 1) (debug t))
  (let ((effective-args (copy-sequence args)))
    (when (and (bound-and-true-p lightemacs-package-manager)
               (eq lightemacs-package-manager 'straight)
               (not (plist-member effective-args :straight)))
      (when (and (plist-member effective-args :ensure)
                 (null (plist-get effective-args :ensure)))
        (when (bound-and-true-p lightemacs-debug)
          (message "[lightemacs] Added `:straight nil' to the %s package" name))
        (setq effective-args (append '(:straight nil) effective-args))))
    `(progn
       (lightemacs-module--before-package ',name ',effective-args)
       (use-package ,name ,@effective-args))))

(provide 'lightemacs-module)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lightemacs-module.el ends here
