;;; le-aggressive-indent.el --- le-aggressive-indent -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `electric-indent-mode' is sufficient to maintain proper code alignment during
;; straightforward typing. However, when you begin shifting code blocks,
;; transposing lines, or manipulating s-expressions, indentation inconsistencies
;; are likely to occur.
;;
;; `aggressive-indent-mode' is a minor mode that ensures code remains
;; consistently indented. It automatically reindents after every modification,
;; providing greater reliability than `electric-indent-mode'.
;;
;; URL: https://github.com/Malabarba/aggressive-indent-mode

;;; Code:

(defvar lightemacs-aggressive-indent-hook-list '(emacs-lisp-mode-hook
                                                 scheme-mode-hook)
  "The modes where `aggressive-indent-mode' is activated.")

(use-package aggressive-indent
  :commands (aggressive-indent-mode
             global-aggressive-indent-mode)
  :init
  (dolist (hook lightemacs-aggressive-indent-hook-list)
    (add-hook hook #'aggressive-indent-mode))

  :config
  (add-to-list 'aggressive-indent-protected-commands 'evil-redo)
  (add-to-list 'aggressive-indent-protected-commands 'evil-undo)
  (add-to-list 'aggressive-indent-protected-commands 'comment-dwim)
  (add-to-list 'aggressive-indent-protected-commands 'comment-box)

  (setq aggressive-indent-excluded-modes 'html-mode))

(provide 'le-aggressive-indent)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-aggressive-indent.el ends here
