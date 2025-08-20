;;; le-stripspace.el --- le-stripspace -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The stripspace Emacs package provides stripspace-local-mode, a minor mode
;; that automatically removes trailing whitespace and blank lines at the end of
;; the buffer when saving.

;;; Code:

(defvar lightemacs-stripspace-local-mode-hook-list '(;; Programming
                                                     prog-mode-hook
                                                     ;; Text files
                                                     text-mode-hook
                                                     ;; Configuration files
                                                     conf-mode-hook)
  "The modes where `easy-escape-minor-mode' is activated.")

(use-package stripspace
  :commands stripspace-local-mode

  :init
  (dolist (hook lightemacs-stripspace-local-mode-hook-list)
    (add-hook hook #'easy-escape-minor-mode))

  ;; The `stripspace-only-if-initially-clean' option:
  ;; - nil to always delete trailing whitespace.
  ;; - Non-nil to only delete whitespace when the buffer is clean initially.
  ;; (The initial cleanliness check is performed when `stripspace-local-mode' is
  ;; enabled.)
  (setq stripspace-only-if-initially-clean nil)

  ;; Enabling `stripspace-restore-column' preserves the cursor's column position
  ;; even after stripping spaces. This is useful in scenarios where you add
  ;; extra spaces and then save the file. Although the spaces are removed in the
  ;; saved file, the cursor remains in the same position, ensuring a consistent
  ;; editing experience without affecting cursor placement.
  (setq stripspace-restore-column t))

(provide 'le-stripspace)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-stripspace.el ends here
