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

(eval-and-compile
  (require 'lightemacs)
  (require 'use-package))

(lightemacs-use-package
  paren
  :ensure nil
  :commands (show-paren-mode
             show-paren-local-mode)
  :init
  (setq show-paren-delay 0.08)

  (lightemacs-define-mode-add-hook-to show-paren-mode
                                      '(lightemacs-on-first-buffer-hook)))

;;; Provide
(provide 'le-paren)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-paren.el ends here
