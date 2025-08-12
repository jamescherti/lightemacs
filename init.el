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

;; Load minimal-emacs.d init.el
(if (fboundp 'lightemacs-load-init-file)
    (lightemacs-load-init-file "init.el")
  (error "The early-init.el file was not loaded"))

;;; Load modules

(defun lightemacs-load-modules ()
  "Load all modules listed in `lightemacs-modules'."
  (let ((modules-dir (expand-file-name "modules"
                                       lightemacs-user-emacs-directory)))
    (dolist (mod lightemacs-modules)
      (let* ((feature-str (format "mod-%s" mod))
             (feature-symbol (intern feature-str))
             (module-file (expand-file-name (format "%s.el" feature-str)
                                            modules-dir)))
        (when init-file-debug
          (message "[LOAD MODULE] %s" module-file))
        (require feature-symbol module-file)))))

(let ((lightemacs-modules '(lightemacs)))
  (lightemacs-load-modules))
(lightemacs-load-modules)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; init.el ends here
