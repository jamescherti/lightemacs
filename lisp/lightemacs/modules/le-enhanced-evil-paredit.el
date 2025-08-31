;;; le-enhanced-evil-paredit.el --- le-enhanced-evil-paredit -*- lexical-binding: t -*-

;; Author: James Cherti
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

(eval-and-compile
  (require 'lightemacs)
  (require 'le-diminish)
  (require 'le-paredit)
  (require 'le-evil))

(lightemacs-use-package
  enhanced-evil-paredit
  :after evil
  :diminish enhanced-evil-paredit-mode
  :commands enhanced-evil-paredit-mode
  :hook (paredit-mode . enhanced-evil-paredit-mode)
  :preface
  (defun lightemacs--fix-enhanced-evil-paredit-evil-snipe-keybinding ()
    "Resolve keybinding conflicts between `enhanced-evil-paredit'/`evil-snipe'.
Disables the S key in `enhanced-evil-paredit-mode-map' when `evil-snipe-mode'
is active to prevent interference with `evil-snipe' commands."
    (with-eval-after-load 'enhanced-evil-paredit
      (with-eval-after-load 'evil
        (evil-define-key 'normal enhanced-evil-paredit-mode-map (kbd "S") nil))))

  :init
  (if (bound-and-true-p evil-snipe-mode)
      (lightemacs--fix-enhanced-evil-paredit-evil-snipe-keybinding)
    (add-hook 'evil-snipe-mode-hook
              #'lightemacs--fix-enhanced-evil-paredit-evil-snipe-keybinding)))

(provide 'le-enhanced-evil-paredit)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-enhanced-evil-paredit.el ends here
