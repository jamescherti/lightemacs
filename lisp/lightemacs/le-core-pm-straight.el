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

(when (bound-and-true-p lightemacs--no-bootstrap)
  ;; Force the correct directory to avoid ~/.emacs.d/ leaks
  (setq straight-base-dir lightemacs-var-directory)
  ;; Disable all straight.el modification checks and builds
  (setq straight-check-for-modifications nil)
  (setq straight-disable-compile t)
  (setq straight-disable-native-compile t))

(let ((bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       (or (bound-and-true-p straight-base-dir)
                           lightemacs-var-directory)))
      (bootstrap-version 7))
  (when (and (not (bound-and-true-p lightemacs--no-bootstrap))
             (not (file-exists-p bootstrap-file)))
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))

  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(provide 'le-core-pm-straight)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars callargs)
;; End:

;;; le-core-pm-straight.el ends here
