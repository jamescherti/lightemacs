;;; le-term.el --- le-term -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `term' and `ansi-term' are built-in Emacs terminal emulators that allow
;; interaction with a shell as if it were running in a standalone terminal
;; application. The `term' mode provides a general terminal interface with two
;; submodes, line mode for typical Emacs editing and char mode for direct
;; terminal input, enabling a balance between editor features and raw terminal
;; behavior. The `ansi-term' mode is a specialized variant that uses a real
;; terminal program such as `/bin/bash' and offers more accurate handling of
;; ANSI escape sequences, resulting in better compatibility with interactive
;; command-line programs like `top', `vim', or `htop'.

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))

(defun lightemacs-term--setup ()
  "Configuration for term and `ansi-term' buffers."
  ;; Suppress prompts for terminating active processes when closing vterm
  (setq-local confirm-kill-processes nil)

  ;; Prevent Emacs from prompting "Buffer has a running process; kill it?" when
  ;; closing the buffer or exiting the editor by silently disabling the
  ;; query-on-exit flag for the underlying shell process.
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (set-process-query-on-exit-flag proc nil)))

  ;; Disable `hscroll-margin' in shell buffers to prevent visual jumping when
  ;; the cursor approaches the left or right edges of the window.
  (setq-local hscroll-margin 0)

  ;; Disable the `mode-line'
  (setq-local mode-line-format nil))

(add-hook 'term-mode-hook #'lightemacs-term--setup t)

(provide 'le-term)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-term.el ends here
