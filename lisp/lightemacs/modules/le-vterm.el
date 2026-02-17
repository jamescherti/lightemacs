;;; le-vterm.el --- le-vterm -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(require 'lightemacs-use-package)

;; Vterm needs `vterm-module' to work.  Compile it now? (y or n) y
;; Compilation of ‘emacs-libvterm’ module succeeded
;; Debugger entered--Lisp error: (error "Loading file
;; /home/work/.emacs.d/lisp/lightemacs/modules/le-vterm.elc failed to provide
;; feature ‘le-vterm’")
(provide 'le-vterm)

(lightemacs-use-package vterm
  :if (bound-and-true-p module-file-suffix)
  :commands (vterm
             vterm-send-string
             vterm-send-return
             vterm-send-key
             vterm-module-compile)
  :functions vterm--self-insert

  :preface
  (when noninteractive
    ;; vterm unnecessarily triggers compilation of vterm-module.so upon loading.
    ;; This prevents that during byte-compilation (`use-package' eagerly loads
    ;; packages when compiling).
    (advice-add #'vterm-module-compile :override #'ignore))

  (defun lightemacs-vterm--setup ()
    ;; Hide the mode-line
    (setq mode-line-format nil)

    ;; Inhibit early horizontal scrolling
    (setq-local hscroll-margin 0)

    ;; Suppress prompts for terminating active processes when closing vterm
    (setq-local confirm-kill-processes nil))

  :init
  (add-hook 'vterm-mode-hook #'lightemacs-vterm--setup)

  (setq vterm-timer-delay 0.05)
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000))

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-vterm.el ends here
