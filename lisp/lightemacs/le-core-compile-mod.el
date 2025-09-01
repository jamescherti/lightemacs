;; le-core-compile-mod.el --- le-core-compile-mod -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Compile the Lightemacs core and modules.

;;; Code:

(require 'bytecomp)

(defmacro lightemacs-verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  (declare (indent 0) (debug t))
  `(progn
     (when lightemacs-verbose
       (message (concat "[lightemacs] " ,(car args)) ,@(cdr args)))))

(defun lightemacs--byte-compile-if-outdated (file-or-symbol &optional no-error)
  "Byte-compile FILE-OR-SYMBOL into a .elc if it is missing or outdated.

If the corresponding .elc file does not exist or is older than FILE-OR-SYMBOL,
this function attempts to compile FILE-OR-SYMBOL. Compilation warnings and
errors are suppressed, and verbose messages are displayed if
`lightemacs-verbose` is non-nil.

NO-ERROR, if non-nil, suppresses raising an error when FILE-OR-SYMBOL cannot be
found. Returns t if the file is up to date or successfully compiled, nil or the
error object otherwise."
  (let ((el-file (if (symbolp file-or-symbol)
                     (locate-library (symbol-name file-or-symbol))
                   file-or-symbol)))
    (cond
     ((and (stringp el-file)
           (or (string-suffix-p ".eln" el-file)
               (string-suffix-p ".elc" el-file)))
      (lightemacs-verbose-message
        "[lightemacs] IGNORE: Byte-compile: Already compiled: %s"
        el-file))

     ((not (file-exists-p el-file))
      (unless no-error
        (error "[lightemacs] Byte-compile: Cannot detect the .elc path for: %s"
               el-file)))

     (t
      (let* ((elc-file (funcall
                        (if (bound-and-true-p byte-compile-dest-file-function)
                            byte-compile-dest-file-function
                          #'byte-compile-dest-file)
                        el-file)))
        (cond
         ((not elc-file)
          (message "WARNING: Byte-compile: Cannot detect the .elc path for: %s"
                   el-file))

         ((and (file-exists-p elc-file)
               (not (file-newer-than-file-p el-file elc-file)))
          ;; Up to date
          t)

         ((and lightemacs-verbose
               (not (file-writable-p elc-file)))
          (lightemacs-verbose-message
            (concat "IGNORE: Byte-compile: Destination .elc is read-only: %S. "
                    "Ensure you have write permissions to allow recompilation.")
            elc-file))

         (t
          (let* ((noninteractive t)
                 (byte-compile-warnings nil)
                 (cond-error nil)
                 (success (condition-case err
                              (progn
                                (lightemacs-verbose-message
                                  "Byte-compile: %S" el-file)
                                (byte-compile-file el-file))
                            (error
                             ;; Return error
                             (setq cond-error err)))))
            (when (or (not success)
                      cond-error)
              (let ((delete-by-moving-to-trash nil))
                (lightemacs-verbose-message "Delete .elc file: %S" elc-file)
                (ignore-errors
                  (delete-file elc-file))))

            (when (or cond-error
                      (not success))
              (lightemacs-verbose-message
                "Error: Byte-compile: %S (Result: %s)"
                el-file (cond (cond-error
                               (format "Error: %s"
                                       cond-error))
                              ((eq success t)
                               "success")
                              ((not success)
                               "error")
                              (t
                               success))))))))))))

(provide 'le-core-compile-mod)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-core-compile-mod.el ends here
