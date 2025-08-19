;;; le-paredit.el --- le-paredit -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Paredit assists in editing Lisp code by enforcing the structural integrity of
;; s-expressions. Instead of treating parentheses as ordinary characters,
;; Paredit ensures that every edit operation, such as inserting, deleting, or
;; moving expressions, preserves balanced parentheses and valid Lisp syntax. It
;; provides structured editing commands for navigating, wrapping, splicing, or
;; reformatting code, making it significantly easier to manipulate nested
;; expressions without introducing syntactic errors.
;;
;; URL: https://paredit.org/

;;; Code:

(use-package paredit
  :commands paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "M-s") nil)  ; Conflict with Consult
  ;; (define-key paredit-mode-map (kbd "RET") nil)
  )

(provide 'le-paredit)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-paredit.el ends here
