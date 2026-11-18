;;; le-core-pm-straight.el --- le-core-pm-straight -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Boostrap the `straight' package manager.

;;; Code:

;;; Require

(require 'lightemacs)

;;; Bootstrap

(defvar bootstrap-version)

(defvar lightemacs-straight-bootstrap-url)

(let ((bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       (or (bound-and-true-p straight-base-dir)
                           lightemacs-var-directory)))
      (bootstrap-version 7))
  (when (and (not (bound-and-true-p lightemacs--no-package-manager-bootstrap))
             (not (file-exists-p bootstrap-file)))
    (with-current-buffer
        (url-retrieve-synchronously
         (or (bound-and-true-p lightemacs-straight-bootstrap-url)
             "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))

  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

;;; Map ':ensure nil' to ':straight nil' during keyword normalization.

(defun lightemacs--straight-use-package-ensure-disabled-p (_name args)
  "Return non-nil when ARGS contains a normalized `:ensure nil'.
_NAME is the package name supplied by `use-package'."
  (equal (plist-get args :ensure) '(nil)))

(defun lightemacs--straight-use-package-disabled-value (name _args)
  "Return the normalized value of `:straight nil' for package NAME.
_ARGS is the normalized declaration supplied by `use-package'."
  (use-package-normalize/:straight name :straight '(nil)))

(require 'use-package-core)
(add-to-list 'use-package-defaults
             '(:straight lightemacs--straight-use-package-disabled-value
                         lightemacs--straight-use-package-ensure-disabled-p))

;; TODO remove
;; (defun lightemacs--straight-use-package-map-ensure (fn-args)
;;   "Add `:straight nil' for a single explicit `:ensure nil' group.
;; FN-ARGS contains the arguments to `use-package-normalize-keywords'.
;; An explicit `:straight' takes precedence regardless of its position."
;;   (let* ((declaration (cadr fn-args))
;;          (ensure-group (memq :ensure declaration))
;;          (ensure-args (cdr ensure-group))
;;          (remaining (cdr ensure-args)))
;;     (if
;;         (and (consp ensure-args)
;;              (null (car ensure-args))
;;              (or (null remaining) (keywordp (car remaining)))
;;              (not (memq :ensure remaining))
;;              (not (memq :straight declaration)))
;;         (cons (car fn-args)
;;               (cons (cons :straight (cons nil declaration))
;;                     (cddr fn-args)))
;;       fn-args)))
;;
;; (advice-add 'use-package-normalize-keywords :filter-args
;;             #'lightemacs--straight-use-package-map-ensure)

;;; Provide

(provide 'le-core-pm-straight)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-core-pm-straight.el ends here
