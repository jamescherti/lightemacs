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
(require 'le-core-defun)
(require 'lightemacs-use-package)

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

;;; Function: `lightemacs-module-load'

(defvar lightemacs-module--loaded nil)

(defun lightemacs-module-load (modules)
  "Load all modules listed in MODULES.
If a module fails to load, an error warning is displayed and the module
is not added to the loaded list."
  (dolist (feature-symbol modules)
    (unless (memq feature-symbol lightemacs-module--loaded)
      (lightemacs-verbose-message "Load module: %s" feature-symbol)
      (condition-case err
          (progn
            (require feature-symbol)
            (push feature-symbol lightemacs-module--loaded))
        (error
         (if (or (eq lightemacs-optional-modules t)
                 (and (listp lightemacs-optional-modules)
                      (memq feature-symbol lightemacs-optional-modules)))
             (display-warning 'lightemacs
                              (format "Failed to load module '%s': %s"
                                      feature-symbol
                                      (error-message-string err))
                              :error)
           (error "Failed to load module '%s': %s"
                  feature-symbol
                  (error-message-string err))))))))

;;; Provide

(provide 'lightemacs-module)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lightemacs-module.el ends here
