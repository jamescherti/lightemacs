;;; mod-vterm.el --- mod-vterm -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `vterm' is an Emacs terminal emulator that provides a fully interactive shell
;; experience within Emacs, supporting features such as color, cursor movement,
;; and advanced terminal capabilities.
;;
;; Unlike simpler Emacs terminal modes, `vterm' leverages the underlying
;; libvterm C library for high-performance, accurate terminal emulation,
;; allowing users to run shell programs, text-based applications, and REPLs
;; seamlessly.

;;; Code:

(use-package vterm
  :if (bound-and-true-p module-file-suffix)
  :commands (vterm
             vterm-send-string
             vterm-send-return
             vterm-module-compile)
  :functions (vterm--self-insert
              vterm-send-key)

  :hook
  (vterm-mode . mod-vterm--setup)

  :init
  (defun mod-vterm--setup ()
    ;; Hide the mode-line
    (setq mode-line-format nil)

    ;; Inhibit early horizontal scrolling
    (setq-local hscroll-margin 0)

    ;; Suppress prompts for terminating active processes when closing vterm
    (setq-local confirm-kill-processes nil))

  (setq vterm-timer-delay 0.05)
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000))

;;; Provide
(provide 'mod-vterm)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-vterm.el ends here
