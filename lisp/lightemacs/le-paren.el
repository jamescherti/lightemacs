;;; le-paren.el --- le-paren -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `show-paren-mode' highlights matching pairs of parentheses and other paired
;; characters, improving code readability and helping to quickly identify
;; unbalanced expressions.

;;; Code:

(use-package paren
  :ensure nil
  :commands (show-paren-mode
             show-paren-local-mode)
  :hook
  (lightemacs-on-first-buffer . show-paren-mode))

;;; Provide
(provide 'le-paren)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-paren.el ends here
