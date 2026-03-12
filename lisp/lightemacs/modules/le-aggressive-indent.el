;;; le-aggressive-indent.el --- le-aggressive-indent -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package aggressive-indent
  :commands (aggressive-indent-mode
             global-aggressive-indent-mode)

  :init
  (lightemacs-module-hooks aggressive-indent
    aggressive-indent-mode
    '(emacs-lisp-mode-hook
      scheme-mode-hook))

  :config
  ;; Protected commands
  (push 'evil-redo aggressive-indent-protected-commands)
  (push 'evil-undo aggressive-indent-protected-commands)
  (push 'evil-commentary aggressive-indent-protected-commands)
  (push 'comment-or-uncomment-region aggressive-indent-protected-commands)
  (push 'comment-dwim aggressive-indent-protected-commands)
  (push 'comment-box aggressive-indent-protected-commands)

  ;; Exclude modes
  (setq aggressive-indent-excluded-modes 'html-mode))

(provide 'le-aggressive-indent)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-aggressive-indent.el ends here
