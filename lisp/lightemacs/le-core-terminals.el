;;; le-core-terminals.el --- le-core-terminals -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Useful functions for terminals.

;;; Code:

(defun lightemacs--all-terminals-optimize ()
  "Configure `vterm', `eat', or `term'/`ansi-term'."
  (let ((eat-p (derived-mode-p 'eat-mode))
        (vterm-p (derived-mode-p 'vterm-mode))
        (term-p (derived-mode-p 'term-mode))
        (inhibit-redisplay t)
        (inhibit-message t))
    (when eat-p
      (setq-local font-lock-defaults '(nil t)))

    (when (or term-p eat-p)
      ;; Stops long lines from wrapping
      (setq-local truncate-lines t)
      ;; Setting this variable to `most-positive-fixnum' makes Emacs scrolls
      ;; only enough to bring point into view rather than recentering.
      (setq-local scroll-conservatively most-positive-fixnum))

    (when (or term-p eat-p vterm-p)
      (setq-local line-spacing 0))

    ;; Disallows automatic horizontal scrolling of windows.
    (setq-local auto-hscroll-mode nil)

    ;; Prevents Emacs from drawing highlight boxes over non-breaking spaces and
    ;; soft hyphens.
    (setq-local nobreak-char-display nil)

    ;; Disable modes
    (dolist (mode lightemacs-all-terminals-disabled-modes)
      (when (and (boundp mode)
                 (fboundp mode))
        ;; Using (symbol-value mode) doesn't work. What works is to
        ;; unconditionally disable the mode if the function exists. Explicitly
        ;; calling (funcall mode -1) is the standard practice, as it sets a
        ;; local state that causes globalized modes not to reactivate in that
        ;; specific buffer.
        (ignore-errors
          (funcall mode -1))))

    ;; Prevent globalized minor modes from reactivating the mode by setting
    ;; their global variable to nil locally. This fixes modes such as
    ;; `global-hl-line-mode' that activate anyway.
    (dolist (global-mode lightemacs-all-terminals-disabled-global-modes)
      (when (boundp global-mode)
        (set (make-local-variable global-mode) nil)))))

(defun lightemacs--all-terminals-disable-process-query-on-exit ()
  "Disable the query-on-exit flag for the process in the current buffer."
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (set-process-query-on-exit-flag proc nil))))

(defun lightemacs--all-terminals-disable-kill-prompt ()
  "Prevent Emacs from prompting: Buffer has a running process; kill it?"
  ;; Prevent Emacs from prompting "Buffer has a running process; kill it?" when
  ;; closing the buffer or exiting the editor by silently disabling the
  ;; query-on-exit flag for the underlying shell process.
  (cond
   ;; Vterm creates the process during mode init, so it exists now.
   ((derived-mode-p 'vterm-mode)
    (lightemacs--all-terminals-disable-process-query-on-exit))
   ;; Term create the process AFTER the mode hook runs. We must defer
   ;; the flag change using their respective exec hooks.
   ((derived-mode-p 'term-mode)
    (add-hook 'term-exec-hook #'lightemacs--all-terminals-disable-process-query-on-exit nil t))))

(provide 'le-core-terminals)

;;; le-core-terminals.el ends here
