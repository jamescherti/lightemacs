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

;;; Customization

;; TODO Change this to t by default
(defvar lightemacs-module-auto-compile nil
  "Automatically byte and natively compile modules before loading.
If non-nil, modules are checked and compiled if their source files are
newer than their compiled counterparts. It defaults to nil.")

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

(defun lightemacs--remove-el-file-suffix (filename)
  "Remove the Elisp suffix from FILENAME and return it (.el, .elc, .el.gz...).
FILENAME is the full or relative path of the file as a string."
  (let ((suffixes (apply #'append
                         (mapcar (lambda (ext)
                                   (list (concat ".elc" ext)
                                         (concat ".el" ext)))
                                 load-file-rep-suffixes))))
    (catch 'done
      (dolist (suffix suffixes filename)
        (when (string-suffix-p suffix filename)
          (setq filename (substring filename 0 (- (length suffix))))
          (throw 'done t))))
    filename))

;; Byte compilation check
(defun lightemacs--compile-module-maybe (base-path)
  "Byte and natively compile the module at BASE-PATH when needed.
This function verifies whether the source file (.el) is newer than its
corresponding byte-compiled (.elc) or natively compiled (.eln) artifacts.
If the source file is newer, byte compilation is performed synchronously,
and native compilation is dispatched asynchronously.
BASE-PATH is the base path of the module without its file extension."
  (when lightemacs-module-auto-compile
    (let ((el-file (concat base-path ".el")))
      (when (file-exists-p el-file)
        (let* ((elc-file (funcall
                          (if (bound-and-true-p byte-compile-dest-file-function)
                              byte-compile-dest-file-function
                            #'byte-compile-dest-file)
                          el-file))
               (elc-file-exists (file-exists-p elc-file)))
          (when (or (not elc-file-exists)
                    (file-newer-than-file-p el-file elc-file))
            ;; Clean up stale .elc files before recompiling
            (when (and elc-file-exists
                       (file-writable-p elc-file))
              (lightemacs-verbose-message "[DELETE] %s" elc-file)
              (delete-file elc-file))
            (lightemacs-verbose-message "[BYTE COMPILE] %s" el-file)
            (byte-compile-file el-file)))

        ;; Native compilation check (Async)
        (when (and (featurep 'native-compile)
                   (fboundp 'native-comp-available-p)
                   (native-comp-available-p)
                   (fboundp 'comp-el-to-eln-filename)
                   (fboundp 'native-compile-async))
          (let* ((eln-file (comp-el-to-eln-filename el-file)))
            (when (or (not (file-exists-p eln-file))
                      (file-newer-than-file-p el-file eln-file))
              ;; Suppress background compiler warnings/errors to avoid popups
              (lightemacs-verbose-message "[ASYNC NATIVE COMPILE] %s" el-file)
              (native-compile-async el-file))))))))

(defun lightemacs-module-load (modules)
  "Load all modules listed in MODULES.
If a module fails to load, an error warning is displayed and the module
is not added to the loaded list."
  (when lightemacs-module-auto-compile
    (require 'bytecomp)
    (when (and (featurep 'native-compile)
               (fboundp 'native-comp-available-p)
               (native-comp-available-p))
      (require 'comp nil t)))

  (let ((priority-path (cons lightemacs-modules-directory
                             (cons lightemacs-local-modules-directory
                                   load-path))))
    (dolist (feature-symbol modules)
      (lightemacs-verbose-message "Load module: %s" feature-symbol)

      (unless (featurep feature-symbol)
        (let ((exact-path (locate-library (symbol-name feature-symbol)
                                          nil
                                          priority-path)))
          (if exact-path
              (progn
                ;; Pass base path (e.g. "/path/to/le-modname") so `require'
                ;; searches for .elc first (which also triggers the .eln
                ;; native-compilation swap if available), gracefully falling
                ;; back to .el if uncompiled.
                (let ((base-path (lightemacs--remove-el-file-suffix exact-path)))
                  (lightemacs-debug-message "Load module path: %s (%s)"
                                            feature-symbol
                                            base-path)

                  ;; Load before compiling to prevent race conditions such
                  ;; as: Debugger entered--Lisp error: (file-missing "Cannot
                  ;; open load file" "No such file or directory"
                  ;; "orderless") load("orderless" nil t)
                  (require feature-symbol base-path)

                  ;; Compile
                  (lightemacs--compile-module-maybe base-path)))
            (error "Cannot find module '%s'" feature-symbol)))))))

;;; Provide

(provide 'lightemacs-module)

;;; lightemacs-module.el ends here
