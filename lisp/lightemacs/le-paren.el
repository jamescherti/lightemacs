;;; le-paren.el --- le-paren -*- no-byte-compile: t; lexical-binding: t -*-

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
  :init
  (add-hook 'lightemacs-on-first-buffer-hook #'show-paren-mode)
  (setq show-paren-delay 0.08))

;;; Provide
(provide 'le-paren)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-paren.el ends here
