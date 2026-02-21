;;; le-enhanced-evil-paredit.el --- le-enhanced-evil-paredit -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module configures the `enhanced-evil-paredit' package, which prevents
;; parenthesis imbalance when using evil-mode with paredit. It intercepts
;; evil-mode commands such as delete, change, and paste, blocking any operation
;; that would break the parenthetical structure. This ensures Lisp code remains
;; syntactically correct while retaining the editing capabilities of evil-mode.
;;
;; This module automatically enables `enhanced-evil-paredit-mode' whenever
;; paredit-mode is activated.
;;
;; URL: https://github.com/jamescherti/enhanced-evil-paredit.el

;;; Code:

(require 'lightemacs-module)
(require 'le-evil)

(lightemacs-use-package enhanced-evil-paredit
  :after evil
  :hook (paredit-mode . enhanced-evil-paredit-mode)
  :init
  (with-eval-after-load 'evil-snipe
    (when (and (fboundp 'evil-define-key)
               (fboundp 'evil-define-key*))
      (evil-define-key 'normal enhanced-evil-paredit-mode-map (kbd "S") nil))))

(provide 'le-enhanced-evil-paredit)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-enhanced-evil-paredit.el ends here
