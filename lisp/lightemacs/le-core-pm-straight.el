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

(require 'lightemacs)
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

(provide 'le-core-pm-straight)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-core-pm-straight.el ends here
