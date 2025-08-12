;;; mod-lightemacs.el --- mod-lightemacs -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Useful functions. This is always loaded.

;;; Code:

;;; On first input/file/buffer

(defvar lightemacs-on-first-input-hook nil
  "Transient hooks run before the first user input.")
(defvar lightemacs-on-first-file-hook nil
  "Transient hooks run before the first interactively opened file.")
(defvar lightemacs-on-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")

;; `permanent-local' ensures their values persist across major mode changes.
(put 'lightemacs-on-first-input-hook 'permanent-local t)
(put 'lightemacs-on-first-file-hook 'permanent-local t)
(put 'lightemacs-on-first-buffer-hook 'permanent-local t)

(defun on--run-first-input-hook (&rest _)
  "Run `lightemacs-on-first-input-hook' once upon the first user input."
  (when after-init-time
    (remove-hook 'pre-command-hook #'on--run-first-input-hook)
    (run-hooks 'lightemacs-on-first-input-hook)))

(defun on--run-first-file-hook (&rest _)
  "Run `lightemacs-on-first-file-hook' once upon the first opened file."
  (when after-init-time
    (remove-hook 'find-file-hook #'on--run-first-file-hook)
    (remove-hook 'dired-initial-position-hook #'on--run-first-file-hook)
    (run-hooks 'lightemacs-on-first-file-hook)))

(defun on--run-first-buffer-hook (&rest _)
  "Run `lightemacs-on-first-buffer-hook' once upon the first visible buffer."
  (when after-init-time
    (remove-hook 'find-file-hook #'on--run-first-buffer-hook)
    (remove-hook 'window-buffer-change-functions #'on--run-first-buffer-hook)
    (remove-hook 'server-visit-hook #'on--run-first-buffer-hook)
    (run-hooks 'lightemacs-on-first-buffer-hook)))

(unless noninteractive
  (add-hook 'pre-command-hook #'on--run-first-input-hook)

  (add-hook 'find-file-hook #'on--run-first-file-hook)
  (add-hook 'dired-initial-position-hook #'on--run-first-file-hook)

  (add-hook 'find-file-hook #'on--run-first-buffer-hook)
  (add-hook 'window-buffer-change-functions #'on--run-first-buffer-hook)
  (add-hook 'server-visit-hook #'on--run-first-buffer-hook))


(provide 'mod-lightemacs)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-lightemacs.el ends here
