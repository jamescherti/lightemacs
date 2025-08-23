;;; le-enhanced-evil-paredit.el --- le-enhanced-evil-paredit -*- no-byte-compile: t; lexical-binding: t -*-

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
;; paredit-mode is activated. (This behavior can be customized using the
;; `lightemacs-enhanced-evil-paredit-mode-hook-list' variable.)
;;
;; URL: https://github.com/jamescherti/enhanced-evil-paredit.el

;;; Code:

(require 'lightemacs)
(require 'le-diminish)
(require 'le-paredit)

(lightemacs-use-package
  enhanced-evil-paredit
  :commands enhanced-evil-paredit-mode

  :init
  (defun lightemacs--fix-enhanced-evil-paredit-evil-snipe-keybinding ()
    "Resolve keybinding conflicts between `enhanced-evil-paredit'/`evil-snipe'.
Disables the S key in `enhanced-evil-paredit-mode-map' when `evil-snipe-mode'
is active to prevent interference with `evil-snipe' commands."
    (with-eval-after-load 'enhanced-evil-paredit
      (evil-define-key 'normal enhanced-evil-paredit-mode-map (kbd "S") nil)))

  (if (bound-and-true-p evil-snipe-mode)
      (lightemacs--fix-enhanced-evil-paredit-evil-snipe-keybinding)
    (add-hook 'evil-snipe-mode-hook
              #'lightemacs--fix-enhanced-evil-paredit-evil-snipe-keybinding)))

;; Define the global variable `lightemacs-enhanced-evil-paredit-mode-hook-list'
(lightemacs-define-mode-hook-list enhanced-evil-paredit-mode
                                  '(paredit-mode-hook))

(provide 'le-enhanced-evil-paredit)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-enhanced-evil-paredit.el ends here
