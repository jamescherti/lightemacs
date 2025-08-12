;;; mod-autorevert.el --- mod-autorevert -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Auto-revert is a feature that automatically updates the contents of a buffer
;; to reflect changes made to the underlying file on disk.
;;
;; URL: https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html

;;; Code:

(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode
             auto-revert-handler
             global-auto-revert-mode)

  :hook
  (after-init . global-auto-revert-mode)
  ;; TODO: Use lightemacs-on-first-file
  ;; (lightemacs-on-first-file . global-auto-revert-mode)

  :custom
  (auto-revert-interval 3)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling t))

(provide 'mod-autorevert)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-autorevert.el ends here
