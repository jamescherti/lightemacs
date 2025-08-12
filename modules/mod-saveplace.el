;;; mod-saveplace.el --- mod-saveplace -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Saveplace mode enables Emacs to remember the last location within a file upon
;; reopening. This is beneficial for resuming work at the precise point where
;; you previously left off.

;;; Code:

(use-package saveplace
  :ensure nil
  :commands save-place-mode

  :hook
  (after-init . save-place-mode)
  ;; TODO: change it to lightemacs-on-first-file-hook
  ;; (lightemacs-on-first-file . save-place-mode)

  :init
  (setq save-place-limit 500))

(provide 'mod-saveplace)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-saveplace.el ends here
