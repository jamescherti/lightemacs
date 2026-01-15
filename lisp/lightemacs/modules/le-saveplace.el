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

(require 'le-shut-up)

(defvar lightemacs-saveplace-recenter-after-find-file t
  "If non-nil, recenter the buffer after restoring the cursor position.")

(defvar lightemacs-saveplace-quiet t
  "If non-nil, suppress saveplace messages when Emacs is exiting.
This affects the messages shown during the `save-place-kill-emacs-hook',
preventing output in the echo area while saving buffer positions.")

(lightemacs-use-package saveplace
  :ensure nil
  :commands save-place-mode

  ;; TODO use on first file?
  :hook (lightemacs-after-init . save-place-mode)

  :preface
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

  :init
  (setq save-place-limit 500)

  :config
  (add-hook 'save-place-after-find-file-hook
            'lightemacs-saveplace--after-find-file)

  (defun lightemacs--around-save-place-kill-emacs-hook (fn &rest args)
    "FN is the advised function. ARGS are the function arguments."
    (if lightemacs-saveplace-quiet
        (apply fn args)
      (shut-up
        (apply fn args))))

  (advice-add 'save-place-kill-emacs-hook :around
              #'lightemacs--around-save-place-kill-emacs-hook))

(provide 'le-saveplace)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-saveplace.el ends here
