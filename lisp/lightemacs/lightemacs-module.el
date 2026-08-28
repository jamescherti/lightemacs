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

(require 'lightemacs) ; `lightemacs-verbose-message'

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

;; (defun lightemacs-slow-module-load (modules)
;;   "Load all modules listed in MODULES.
;; If a module fails to load, an error warning is displayed and the module
;; is not added to the loaded list."
;;   (dolist (feature-symbol modules)
;;     (lightemacs-verbose-message "Load module: %s" feature-symbol)
;;     (require feature-symbol)))

(defun lightemacs--remove-el-file-suffix (filename)
  "Remove the Elisp file suffix from FILENAME and return it (.el, .el.gz...)."
  (let ((suffixes (mapcar (lambda (ext) (concat ".el" ext))
                          load-file-rep-suffixes)))
    (catch 'done
      (dolist (suffix suffixes filename)
        (when (string-suffix-p suffix filename)
          (setq filename (substring filename 0 (- (length suffix))))
          (throw 'done t))))
    filename))

(defun lightemacs-module-load (modules)
  "Load all modules listed in MODULES.
If a module fails to load, an error warning is displayed and the module
is not added to the loaded list."
  (let ((priority-path (cons (expand-file-name lightemacs-modules-directory)
                             load-path)))
    (dolist (feature-symbol modules)
      (lightemacs-verbose-message "Load module: %s" feature-symbol)

      (condition-case err
          (unless (featurep feature-symbol)
            (let ((exact-path (locate-library (symbol-name feature-symbol)
                                              nil
                                              priority-path)))
              (if exact-path
                  ;; Pass base path (e.g. "/path/to/le-modname") so `require'
                  ;; searches for .elc first (which also triggers the .eln
                  ;; native-compilation swap if available), gracefully falling
                  ;; back to .el if uncompiled.
                  (let ((base-path (lightemacs--remove-el-file-suffix exact-path)))
                    (lightemacs-verbose-message "Load module: %s (%s)"
                                                feature-symbol
                                                base-path)
                    (require feature-symbol base-path))

                (error "Cannot find module '%s' in priority path" feature-symbol))))

        (error
         (display-warning 'lightemacs
                          (format "Failed to load module '%s': %s"
                                  feature-symbol (error-message-string err))
                          :warning))))))

;;; Provide

(provide 'lightemacs-module)

;;; lightemacs-module.el ends here
