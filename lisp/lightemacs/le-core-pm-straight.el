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
  (when (and (not (bound-and-true-p lightemacs--no-bootstrap))
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

(defun my/use-package-map-ensure-to-straight (fn-args)
  "Map `:ensure nil' to `:straight nil' during keyword normalization.
FN-ARGS is a list containing two elements: the NAME of the package (a symbol),
and the ARGS property list provided to the `use-package' declaration."
  (let ((name (car fn-args))
        (keywords (cadr fn-args)))
    (if (and (plist-member keywords :ensure)
             (not (plist-get keywords :ensure)))
        (list name (append (list :straight nil) keywords))
      fn-args)))

(advice-add 'use-package-normalize-keywords :filter-args #'my/use-package-map-ensure-to-straight)

;;; Require

(provide 'le-core-pm-straight)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-core-pm-straight.el ends here
