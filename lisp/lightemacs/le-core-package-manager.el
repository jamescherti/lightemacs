;;; le-core-package-manager.el --- le-core-package-manager -*- lexical-binding: t -*-

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

(require 'lightemacs)

;;; Choose the package manager

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
  (error (concat "[lightemacs]"
                 "Invalid value for `lightemacs-package-manager': '%S'. Valid "
                 "choices are: 'straight, 'elpaca, or 'use-package.")
         lightemacs-package-manager)))

;;; lightemacs-use-package macro

(defvar lightemacs--use-package-refreshed nil
  "Non-nil if package contents have been refreshed during the current session.
Used by `lightemacs--before-use-package' to ensure that
`package-refresh-contents' is invoked at most once per Emacs session, avoiding
redundant network calls when installing multiple packages.")

(defvar lightemacs--installed-packages nil
  "List of package symbols that have been installed during this session.
Used as a cache by `lightemacs--before-use-package' to skip re-checking
`package-installed-p' for packages that were already installed, improving
startup performance when configuring multiple packages.")

(defun lightemacs--before-use-package (name plist)
  "Ensure a package is installed before `lightemacs-use-package' expands.

NAME is the symbol identifying the package to install or configure.
PLIST is the property list of keyword arguments supplied to `use-package'.

This function performs the following steps when the package manager
is `use-package' and the :ensure property is non-nil."
  ;; TODO Support load-path and make it install packages
  (when (and (eq lightemacs-package-manager 'use-package))
    (let* ((ensure-member (plist-member plist :ensure))
           (ensure-value (if ensure-member
                             (plist-get plist :ensure)
                           use-package-always-ensure))
           ;; (load-dir (plist-get plist :load-path))
           )
      (when (and ensure-value
                 (not (memq name lightemacs--installed-packages))
                 ;; TODO handle load-path? Remove load-dir?
                 ;; (or (not load-dir)
                 ;;     (not (locate-library (symbol-name name) nil load-dir)))
                 (not (package-installed-p name)))
        ;; Refresh packages
        (when (and (not lightemacs--use-package-refreshed)
                   lightemacs-use-package-refresh-contents)
          (lightemacs-verbose-message
            "[USE-PACKAGE] Refreshing package contents before installing %s"
            name)
          (setq lightemacs--use-package-refreshed t)
          (condition-case err
              (package-refresh-contents)
            (error
             (display-warning 'lightemacs
                              (format "Failed to install package %s: %s"
                                      name (error-message-string err))
                              :error))))

        ;; Install the package
        (lightemacs-verbose-message "[USE-PACKAGE] Installing %s" name)
        (funcall use-package-ensure-function name (list ensure-value) nil)
        (push name lightemacs--installed-packages)))))

(defmacro lightemacs-use-package (name &rest args)
  "Provide a formal interface for package configuration via `use-package'.

NAME designates the package symbol.
ARGS represents the property list of configuration parameters.

If :ensure is explicitly nil and no :straight declaration exists,
append (:straight nil) to ARGS. Invokes `lightemacs--before-use-package`
with the resulting arguments prior to expanding `use-package`."
  (declare (indent 1) (debug t))
  (let ((effective-args (copy-sequence args)))
    (when (and (eq lightemacs-package-manager 'straight)
               (not (plist-member effective-args :straight)))
      (when (and (plist-member effective-args :ensure)
                 (null (plist-get effective-args :ensure)))
        (lightemacs-debug-message
          "lightemacs-use-package: Added `:straight nil' to the %s package" name)
        (setq effective-args (append '(:straight nil) effective-args))))
    ;; `(progn
    ;;    (lightemacs--before-use-package ',name ',effective-args)
    ;;    (use-package ,name ,@effective-args))
    `(progn
       (lightemacs--before-use-package ',name ',effective-args)
       (eval (cons 'use-package (cons ',name ',effective-args)))
       ;; (use-package ,name ,@effective-args)
       ;; (lightemacs-shield-macros
       ;;   (use-package ,name ,@effective-args))
       )
    ))


(provide 'le-core-package-manager)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-core-package-manager.el ends here
