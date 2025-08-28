;;; le-display-line-numbers.el --- le-display-line-numbers -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Show line numbers on the left side of the buffer. The numbers update
;; automatically as you add, remove, or scroll lines, but they don’t change the
;; actual text.

;;; Code:

(eval-and-compile
  (require 'lightemacs)
  (require 'use-package))

(lightemacs-use-package
  display-line-numbers
  :ensure nil
  :commands (global-display-line-numbers-mode
             display-line-numbers-mode)
  :init
  (lightemacs-define-mode-add-hook-to
    display-line-numbers-mode
    '(prog-mode-hook text-mode-hook conf-mode-hook)))

(provide 'le-display-line-numbers)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-display-line-numbers.el ends here
