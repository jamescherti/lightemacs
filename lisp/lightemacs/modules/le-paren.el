;;; le-paren.el --- le-paren -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package paren
  :ensure nil
  :commands (show-paren-mode
             show-paren-local-mode)
  :init
  (lightemacs-module-hooks paren
    show-paren-mode
    '(lightemacs-on-first-buffer-hook))

  (lightemacs-module-setq-maybe paren
    show-paren-delay 0.08))

(provide 'le-paren)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-paren.el ends here
