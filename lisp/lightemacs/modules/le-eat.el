;;; le-eat.el --- le-eat -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Eat (Emulate A Terminal) is a terminal emulator implemented entirely in Emacs
;; Lisp. It supports full-screen terminal applications such as Emacs itself and
;; offers high performance, operating over three times faster than Term. Eat
;; provides advanced features like Sixel graphics, complete mouse support, shell
;; integration, and reduced screen flicker for smoother performance. Configuring
;; shell integration is recommended to fully utilize its capabilities.
;;
;; URL: https://codeberg.org/akib/emacs-eat

;;; Code:

(require 'lightemacs-module)

(lightemacs-use-package eat
  :commands (eat
             eat-eshell-mode
             eat-eshell-visual-command-mode
             eat-other-window
             eat-project
             eat-project-other-window
             eat-term-make)
  :functions eat-self-input

  :preface
  (defun lightemacs-eat--setup ()
    ;; Hide the mode-line
    (setq mode-line-format nil)

    ;; Inhibit early horizontal scrolling
    (setq-local hscroll-margin 0)

    ;; Suppress prompts for terminating active processes when closing eat
    (setq-local confirm-kill-processes nil))

  :init
  (add-hook 'eat-mode-hook #'lightemacs-eat--setup)
  (setq eat-kill-buffer-on-exit t))

;;; Provide
(provide 'le-eat)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-eat.el ends here
