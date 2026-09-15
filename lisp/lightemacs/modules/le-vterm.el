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
;; Unlike standard Emacs terminal modes, `vterm' utilizes the libvterm C library
;; for high-performance emulation. This ensures accurate terminal behavior when
;; running shell programs, text-based applications, and REPLs.
;;
;; URL: https://github.com/akermu/emacs-libvterm

;;; Code:

(require 'lightemacs)
(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

;; Vterm needs `vterm-module' to work.  Compile it now? (y or n) y
;; Compilation of 'emacs-libvterm' module succeeded
;; Debugger entered--Lisp error: (error "Loading file
;; ~/.emacs.d/lisp/lightemacs/modules/le-vterm.elc failed to provide
;; feature 'le-vterm'")
(provide 'le-vterm)

(defvar lightemacs-vterm-optimize t
  "Non-nil means apply performance optimizations to `vterm-mode' buffers.
When non-nil, `lightemacs--all-terminals-optimize' runs in `vterm-mode-hook'.")

(when noninteractive
  (with-eval-after-load 'eval
    ;; vterm unnecessarily triggers compilation of vterm-module.so upon loading.
    ;; This prevents that during byte-compilation (`use-package' eagerly loads
    ;; packages when compiling).
    (when (fboundp 'vterm-module-compile)
      (advice-add #'vterm-module-compile :override #'ignore)
      (provide 'vterm-module))))

(lightemacs-use-package vterm
  :if (bound-and-true-p module-file-suffix)
  :commands (vterm
             vterm-send-string
             vterm-send-return
             vterm-send-key
             vterm-module-compile)
  :functions vterm--self-insert
  :init
  (lightemacs-module-setq-maybe vterm
    vterm-timer-delay 0.01
    vterm-kill-buffer-on-exit t
    ;; Set the amount of lines retained by `vterm'.
    vterm-max-scrollback 5000)

  (add-hook 'vterm-mode-hook #'lightemacs--all-terminals-disable-kill-prompt)
  (when lightemacs-vterm-optimize
    (add-hook 'vterm-mode-hook #'lightemacs--all-terminals-optimize)))

;;; le-vterm.el ends here
