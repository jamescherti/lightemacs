;;; lightemacs-package.el --- lightemacs-package -*- lexical-binding: t -*-

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

(defmacro lightemacs-package-add-hook (mode hooks)
  "Define a variable listing hooks where MODE should be enabled.

This defines a variable named `lightemacs-MODE-target-hooks' (if it does not
already exist) initialized with HOOKS. It then iterates over that variable,
adding MODE to each hook found.

MODE should be a quoted symbol (e.g., \='flycheck-mode).
HOOKS should be a list of hook symbols (e.g., \='(prog-mode-hook))."
  (declare (indent 1) (debug t))
  (let ((var (intern (format "lightemacs-%s-target-hooks" mode)))
        (docstring (format "List of hooks where `%s' is enabled." mode)))
    `(progn
       (defvar ,var ,hooks ,docstring)
       ;; Ensure we treat the variable as a list, even if the user set it to a
       ;; single symbol
       (dolist (hook (if (listp ,var) ,var (list ,var)))
         (add-hook hook ',mode)))))

(defmacro lightemacs-package-bind (module &rest body)
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
configuration for that mode without modifying the macro definition itself."
                  module module))
       (unless ,inhibit-var
         ,@body))))

;;; lightemacs-package macro

(defvar lightemacs-package--refreshed nil
  "Non-nil if package contents have been refreshed during the current session.
Used by `lightemacs-package--before-package' to ensure that
`package-refresh-contents' is invoked at most once per Emacs session, avoiding
redundant network calls when installing multiple packages.")

(defvar lightemacs-package--installed-p nil
  "List of package symbols that have been installed during this session.
Used as a cache by `lightemacs-package--before-package' to skip re-checking
`package-installed-p' for packages that were already installed, improving
startup performance when configuring multiple packages.")

(defmacro lightemacs-package--verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  (declare (indent 0) (debug t))
  `(progn
     (when (or lightemacs-verbose lightemacs-debug)
       (message (concat "[lightemacs] " ,(car args)) ,@(cdr args)))))

(define-obsolete-function-alias
  'lightemacs--before-use-package
  'lightemacs-package--before-package
  "2026-02-17")

(defun lightemacs-package--before-package (name plist)
  "Ensure a package is installed before `lightemacs-package' expands.

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
                           use-package-always-ensure)))
      (when (and ensure-value
                 (not (memq name lightemacs-package--installed-p))
                 (not (package-installed-p name)))
        ;; Refresh packages
        (when (and (not lightemacs-package--refreshed)
                   lightemacs-package-refresh-contents)
          (lightemacs-package--verbose-message
            "Refreshing package contents before installing %s"
            name)
          (setq lightemacs-package--refreshed t)
          (condition-case err
              (package-refresh-contents)
            (error
             (display-warning 'lightemacs
                              (format "Failed to install package %s: %s"
                                      name (error-message-string err))
                              :error))))

        ;; Install the package
        (lightemacs-package--verbose-message "Installing %s" name)
        (funcall use-package-ensure-function name (list ensure-value) nil)
        (push name lightemacs-package--installed-p)))))

(defmacro lightemacs-package (name &rest args)
  "Provide a formal interface for package configuration via `use-package'.

NAME designates the package symbol.
ARGS represents the property list of configuration parameters.

If :ensure is explicitly nil and no :straight declaration exists,
append (:straight nil) to ARGS. Invokes `lightemacs-package--before-package`
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
       (lightemacs-package--before-package ',name ',effective-args)
       (use-package ,name ,@effective-args))))

(provide 'lightemacs-package)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lightemacs-package.el ends here
