;;; le-core-process-buffers.el --- le-core-process-buffers -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Useful functions for process buffers such as terminals or `compilation-mode'
;; buffers.

;;; Code:

(defvar lightemacs-process-buffers-modes
  '(vterm-mode
    eat-mode
    term-mode
    ghostel-mode
    compilation-mode)
  "List of terminal emulator major modes.")

(defvar lightemacs-process-buffers-compilation-modes
  '(compilation-mode)
  "List of compilation modes.")

(defvar lightemacs-process-buffers-compilation-modes-never-disable
  '(;; Build logs often contain long file paths, compiler flags, linker
    ;; commands, and stack traces. Keeping visual-line-mode active wraps long
    ;; lines within the window so output stays readable without forcing
    ;; horizontal scrolling or cutting off text at the right edge.
    visual-line-mode

    ;; Highlighting the current line helps you track which diagnostic, warning,
    ;; or error line you are inspecting as you step through build output or jump
    ;; between errors.
    hl-line-mode)
  "List of minor modes to keep enabled in compilation buffers.
These modes are exempt from being disabled in buffers matching
`lightemacs-process-buffers-compilation-modes'.")

(defvar lightemacs-process-buffers-disabled-modes
  '(;; Highlights the current line, which is generally unnecessary in a
    ;; terminal buffer and can alter the appearance of terminal output.
    ;; NOTE: This was disabled by le-default-settings
    ;; hl-line-mode

    ;; Displays line numbers in the window margin, which are not part of the
    ;; terminal's character grid.
    display-line-numbers-mode

    ;; Draws a vertical indicator in the window, which is unrelated to the
    ;; terminal's character grid.
    ;; NOTE: This was disabled by le-default-settings
    ;; display-fill-column-indicator-mode

    ;; Enables Emacs visual-line wrapping rather than the terminal emulator's
    ;; own line wrapping.
    visual-line-mode

    ;; Displays whitespace characters and related visual indicators, which are
    ;; generally unnecessary in terminal buffers and can alter their appearance.
    ;; NOTE: This was disabled by le-default-settings
    ;; whitespace-mode

    ;; Adds faces to matching delimiters, which is generally unnecessary in
    ;; terminal buffers and can alter terminal output's appearance.
    rainbow-delimiters-mode
    ;; Requests contextual documentation, which is generally not useful in
    ;; terminal emulator buffers.
    eldoc-mode
    ;; These modes perform Emacs editing actions such as inserting matching
    ;; delimiters or indentation. Such behavior is generally undesirable when
    ;; terminal input should be handled by the terminal application.
    electric-pair-local-mode
    electric-indent-local-mode
    show-paren-local-mode
    smartparens-mode
    ;; These packages provide Emacs-side completion or expansion features that
    ;; are generally unnecessary when completion is handled by the terminal
    ;; application or shell.
    yas-minor-mode
    yas/minor-mode
    company-mode
    ;; NOTE: This was disabled by le-corfu
    ;; corfu-mode
    )
  "List of minor modes to disable in terminal emulator buffers.")

(defvar lightemacs-process-buffers-disabled-global-modes
  '(;; Highlights the current line, which is generally unnecessary in a
    ;; terminal buffer and can alter the appearance of terminal output.
    ;; NOTE: This was disabled by le-default-settings
    ;; global-hl-line-mode

    ;; Displays line numbers in the window margin, which are not part of the
    ;; terminal's character grid.
    global-display-line-numbers-mode

    ;; Draws a vertical indicator in the window, which is unrelated to the
    ;; terminal's character grid.
    ;; NOTE: This was disabled by le-default-settings
    ;; global-display-fill-column-indicator-mode

    ;; Enables Emacs visual-line wrapping rather than the terminal emulator's
    ;; own line wrapping.
    global-visual-line-mode

    ;; Displays whitespace characters and related visual indicators, which are
    ;; generally unnecessary in terminal buffers and can alter their appearance.
    ;; NOTE: This was disabled by le-default-settings
    ;; global-whitespace-mode

    ;; These modes perform Emacs editing actions such as inserting matching
    ;; delimiters or indentation. Such behavior is generally undesirable when
    ;; terminal input should be handled by the terminal application.
    electric-pair-mode
    electric-indent-mode
    show-paren-mode
    smartparens-global-mode
    ;; These packages provide Emacs-side completion or expansion features that
    ;; are generally unnecessary when completion is handled by the terminal
    ;; application or shell.
    yas-global-mode
    yas/global-mode
    global-company-mode
    ;; NOTE: This was disabled by le-corfu
    ;; global-corfu-mode
    )
  "List of global minor modes to disable locally in terminal emulator buffers.

Disabling a local minor mode directly is often insufficient because globalized
minor modes may reactivate their local counterparts via hooks during buffer
initialization. Listing the global mode variables here ensures the terminal
setup function can make them buffer-local and set them to nil, which prevents
the global modes from overriding the local configuration.

This prevents, for example, `global-hl-line-mode' from automatically
reactivating the local mode via hooks. Setting the global minor mode variable to
nil locally acts as a shield for this specific buffer.")

(defun lightemacs--lightemacs-process-buffers-optimize ()
  "Configure terminal and compilation buffers."
  (let ((eat-p (derived-mode-p 'eat-mode))
        (vterm-p (derived-mode-p 'vterm-mode))
        (term-p (derived-mode-p 'term-mode))
        (comp-p (apply #'derived-mode-p lightemacs-process-buffers-compilation-modes))
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

    (unless comp-p
      (setq-local auto-hscroll-mode nil))

    (when comp-p
      (visual-line-mode 1))

    (setq-local nobreak-char-display nil)

    (dolist (mode-fn lightemacs-process-buffers-disabled-modes)
      (unless (and comp-p
                   (memq mode-fn lightemacs-process-buffers-compilation-modes-never-disable))
        (when (and (boundp mode-fn)
                   (fboundp mode-fn))
          ;; Using (symbol-value mode) doesn't work. What works is to
          ;; unconditionally disable the mode if the function exists. Explicitly
          ;; calling (funcall mode -1) is the standard practice, as it sets a
          ;; local state that causes globalized modes not to reactivate in that
          ;; specific buffer.
          (ignore-errors
            (funcall mode-fn -1)))))

    ;; Prevent globalized minor modes from reactivating the mode by setting
    ;; their global variable to nil locally. This fixes modes such as
    ;; `global-hl-line-mode' that activate anyway.
    (dolist (global-var lightemacs-process-buffers-disabled-global-modes)
      (when (boundp global-var)
        (set (make-local-variable global-var) nil)))))

(defun lightemacs--process-buffers-disable-process-query-on-exit ()
  "Disable the query-on-exit flag for the process in the current buffer."
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (set-process-query-on-exit-flag proc nil))))

(defun lightemacs--process-buffers-disable-kill-prompt ()
  "Prevent Emacs from prompting: Buffer has a running process; kill it?"
  ;; Prevent Emacs from prompting "Buffer has a running process; kill it?" when
  ;; closing the buffer or exiting the editor by silently disabling the
  ;; query-on-exit flag for the underlying shell process.
  (cond
   ;; Vterm creates the process during mode init, so it exists now.
   ((derived-mode-p 'vterm-mode)
    (lightemacs--process-buffers-disable-process-query-on-exit))
   ;; Term create the process AFTER the mode hook runs. We must defer
   ;; the flag change using their respective exec hooks.
   ((derived-mode-p 'term-mode)
    (add-hook 'term-exec-hook #'lightemacs--process-buffers-disable-process-query-on-exit nil t))))

(provide 'le-core-process-buffers)

;;; le-core-process-buffers.el ends here
