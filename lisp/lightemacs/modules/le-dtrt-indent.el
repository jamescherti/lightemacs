;;; le-dtrt-indent.el --- le-dtrt-indent -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This dtrt-indent package provides functions to detects the indentation offset
;; used in existing source code files and automatically adjusts Emacs settings
;; accordingly, thereby simplifying the editing of files created in external
;; environments.
;;
;; URL: https://github.com/jscheid/dtrt-indent

;;; Code:

(require 'lightemacs-package)

(defvar lightemacs-dtrt-indent-inhibit nil
  "Non-nil inhibits automatic indentation detection via `dtrt-indent'.
This variable can be set buffer-locally to prevent `dtrt-indent' from adjusting
the indentation settings automatically in the current buffer.")

(defvar lightemacs-dtrt-indent-excluded-modes '()
  "List of major modes excluded from automatic indentation detection.
Modes in this list will not trigger `dtrt-indent' when buffers of those types
are opened or their major mode changes.")

(defun le-dtrt-indent--detect-indentation ()
  "Automatically enable `dtrt-indent-mode' unless inhibited.
This function checks several conditions before enabling indentation detection:
1. Emacs has completed initialization (`after-init-time').
2. `lightemacs-dtrt-indent-inhibit' is nil.
3. The buffer is not in `fundamental-mode'.
4. The buffer name does not start with `*' or a space.
5. The current major mode is not in `lightemacs-dtrt-indent-excluded-modes'.
If all conditions are satisfied, `dtrt-indent-mode' is enabled silently."
  (unless (or (not after-init-time)
              lightemacs-dtrt-indent-inhibit
              (eq major-mode 'fundamental-mode)
              (member (substring (buffer-name) 0 1) '("*" " "))
              (apply #'derived-mode-p lightemacs-dtrt-indent-excluded-modes))
    (let ((inhibit-message (not init-file-debug)))
      (when (fboundp 'dtrt-indent-mode)
        (funcall 'dtrt-indent-mode +1)))))

(lightemacs-package dtrt-indent
  :commands (dtrt-indent-global-mode
             dtrt-indent-mode
             dtrt-indent-adapt
             dtrt-indent-undo
             dtrt-indent-diagnosis
             dtrt-indent-highlight)

  :init
  (add-hook 'change-major-mode-after-body-hook #'le-dtrt-indent--detect-indentation)
  (setq dtrt-indent-verbosity (if init-file-debug 1 0))
  (setq dtrt-indent-max-lines 1900)  ; Faster

  ;; By default, `dtrt-indent' detects SMIE-based modes and lets
  ;; `smie-config-guess' handle indentation. However, `dtrt-indent' also sets
  ;; additional variables that SMIE does not (for example,
  ;; `dtrt-indent-hook-generic-mapping-list'). Setting this option to non-nil
  ;; forces `dtrt-indent' to run in SMIE-based modes as well, ensuring these
  ;; extra settings are applied.
  (setq dtrt-indent-run-after-smie t))

(provide 'le-dtrt-indent)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-dtrt-indent.el ends here
