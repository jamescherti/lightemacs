;;; init.el --- Init -*- lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The Light Emacs project is an Emacs framework.
;; Do not modify this file; instead, modify pre-init.el or post-init.el.

;;; Code:

;; Load minimal-emacs.d init.el
(lightemacs-load-init-file "init.el")

;;; Load modules

(defun lightemacs-load-modules ()
  "Load all modules listed in `lightemacs-modules'."
  (let ((modules-dir (expand-file-name "modules" lightemacs-user-emacs-directory)))
    (dolist (mod lightemacs-modules)
      (let ((mod-file (expand-file-name (format "mod-%s.el" mod) modules-dir)))
        (load-file mod-file)))))

(lightemacs-load-modules)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; init.el ends here
