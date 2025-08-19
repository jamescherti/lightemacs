;;; init.el --- Init -*- lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The Lightemacs project is an Emacs framework.

;;; Code:

;;; Load minimal-emacs.d init.el

(require 'cl-lib)

(if (fboundp 'lightemacs-load-init-file)
    (progn
      (if (fboundp 'minimal-emacs-load-user-init)
          (minimal-emacs-load-user-init "pre-init.el")
        (error "The early-init.el file failed to loaded"))

      (cl-letf (((symbol-function 'minimal-emacs-load-user-init)
                 (lambda (&rest _)
                   nil)))
        (lightemacs-load-init-file "init.el")))
  (error "The early-init.el file was not loaded"))

;;; Load modules

;; Require lightemacs
(add-to-list 'load-path lightemacs--modules-dir)
(require 'lightemacs)

;; Load config.el
(if (fboundp 'minimal-emacs-load-user-init)
    (minimal-emacs-load-user-init "config.el")
  (error "Undefined function: minimal-emacs-load-user-init"))

;; Load all modules
(if (fboundp 'lightemacs-load-modules)
    (lightemacs-load-modules lightemacs-modules)
  (error "Undefined function: lightemacs-load-modules"))

;; Load post-init.el
(when (fboundp 'minimal-emacs-load-user-init)
  (minimal-emacs-load-user-init "post-init.el"))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; init.el ends here
