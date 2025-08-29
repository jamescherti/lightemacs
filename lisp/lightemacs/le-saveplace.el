;;; le-saveplace.el --- le-saveplace -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Saveplace mode enables Emacs to remember the last location within a file upon
;; reopening. This is beneficial for resuming work at the precise point where
;; you previously left off.

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(defvar lightemacs-saveplace-recenter-after-find-file t
  "If non-nil, recenter the buffer after restoring the cursor position.")

(lightemacs-use-package
  saveplace
  :ensure nil
  :commands save-place-mode

  ;; TODO use on first file?
  :hook (after-init . save-place-mode)

  :init
  (setq save-place-limit 500)

  :config
  (defun lightemacs-saveplace--recenter ()
    "Recenter the current window."
    (when (and (get-buffer-window)
               (not (minibufferp)))
      (ignore-errors
        (recenter))))

  (defun lightemacs-saveplace--after-find-file ()
    "Recenter the current window when `scroll-conservatively' >= 101.
This function is called by `save-place-after-find-file-hook'.
It avoids recentering while an EasySession session is in progress."
    (when (and (or (>= scroll-conservatively 101)
                   lightemacs-saveplace-recenter-after-find-file)
               (> (point) (point-min)))
      ;; Use a timer to ensure a window exists when recenter is called
      (run-with-timer 0 nil #'lightemacs-saveplace--recenter)))

  (add-hook 'save-place-after-find-file-hook
            'lightemacs-saveplace--after-find-file))

(provide 'le-saveplace)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-saveplace.el ends here
