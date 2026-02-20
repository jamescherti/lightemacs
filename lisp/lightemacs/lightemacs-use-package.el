;;; lightemacs-use-package.el --- lightemacs-use-package -*- lexical-binding: t -*-

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

;;; lightemacs-use-package macro

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

;; TODO duplicate
(defmacro lightemacs-use-package--verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  (declare (indent 0) (debug t))
  `(progn
     (when (or lightemacs-verbose lightemacs-debug)
       (message (concat "[lightemacs] " ,(car args)) ,@(cdr args)))))

;; move this to a defvar function
(defun lightemacs-use-package--before-package (name plist)
  "Ensure a package is installed before `lightemacs-use-package' expands.

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
                 (not (memq name lightemacs-use-package--installed))
                 (not (package-installed-p name)))
        ;; Refresh packages
        (when (and (not lightemacs-use-package--packages-refreshed)
                   lightemacs-use-package-refresh-contents)
          (lightemacs-use-package--verbose-message
           "Refreshing package contents before installing %s"
           name)
          (setq lightemacs-use-package--packages-refreshed t)
          (condition-case err
              (package-refresh-contents)
            (error
             (display-warning 'lightemacs
                              (format "Failed to install package %s: %s"
                                      name (error-message-string err))
                              :error))))

        ;; Install the package
        (lightemacs-use-package--verbose-message "Installing %s" name)
        (when (fboundp 'use-package-ensure-function)
          (funcall use-package-ensure-function name (list ensure-value) nil))
        (push name lightemacs-use-package--installed)))))

(defmacro lightemacs-use-package (name &rest args)
  "Provide a formal interface for package configuration via `use-package'.

NAME designates the package symbol.
ARGS represents the property list of configuration parameters.

If :ensure is explicitly nil and no :straight declaration exists,
append (:straight nil) to ARGS. Invokes `lightemacs-use-package--before-package`
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
       (lightemacs-use-package--before-package ',name ',effective-args)
       (use-package ,name ,@effective-args))))

(provide 'lightemacs-use-package)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lightemacs-use-package.el ends here
