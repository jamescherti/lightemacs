;;; le-core-compile.el --- le-core-compile -*- lexical-binding: t -*-

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

(defun lightemacs--byte-compile-if-outdated (el-file)
  "Byte-compile EL-FILE into .elc if the .elc is missing or outdated."
  (let* ((elc-file (funcall
                    (if (bound-and-true-p byte-compile-dest-file-function)
                        byte-compile-dest-file-function
                      #'byte-compile-dest-file)
                    el-file)))
    (cond
     ((not elc-file)
      (message "WARNING: Byte-compile: Cannot detect the .elc path for: %S"
               el-file))

     ((and (file-exists-p elc-file)
           (not (file-newer-than-file-p el-file elc-file)))
      ;; Up to date
      t)

     ((and lightemacs-verbose
           (not (file-writable-p elc-file)))
      (lightemacs-verbose-message
        (concat "IGNORED: Byte-compile: Destination .elc is read-only: %S. "
                "Ensure you have write permissions to allow recompilation.")
        elc-file))

     (t
      (let* ((noninteractive t)
             (byte-compile-warnings nil)
             (cond-error nil)
             (success (condition-case err
                          (byte-compile-file el-file)
                        (error
                         ;; Return error
                         (setq cond-error err)))))
        (when (or (not success)
                  cond-error)
          (let ((delete-by-moving-to-trash nil))
            (lightemacs-verbose-message "Delete .elc file: %S" elc-file)
            (ignore-errors
              (delete-file elc-file))))

        (when (and (not cond-error)
                   (not (eq success 'no-byte-compile)))
          (lightemacs-verbose-message
            "Byte-compile: %S (Result: %s)" el-file (cond
                                                     (cond-error
                                                      (format "Error: %s"
                                                              cond-error))
                                                     ((eq success t)
                                                      "success")
                                                     ((not success)
                                                      "error")
                                                     (t
                                                      success)))))))))

(provide 'le-core-compile)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-core-compile.el ends here
