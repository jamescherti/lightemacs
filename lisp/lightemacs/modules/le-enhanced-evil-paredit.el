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
(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package enhanced-evil-paredit
  :after evil
  :preface
  (defun enhanced-evil-paredit--evil-snipe-setup ()
    "Remove the S keybinding for `evil-snipe'."
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal enhanced-evil-paredit-mode-map (kbd "S") nil)))

  :init
  ;; Remove the S keybinding from `enhanced-evil-paredit-mode-map' for
  ;; `evil-snipe'.
  (add-hook 'evil-snipe-mode-hook #'enhanced-evil-paredit--evil-snipe-setup)

  (lightemacs-module-hooks enhanced-evil-paredit
    enhanced-evil-paredit-mode
    '(paredit-mode-hook)))

(provide 'le-enhanced-evil-paredit)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-enhanced-evil-paredit.el ends here
