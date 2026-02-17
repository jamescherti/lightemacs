;;; le-saveplace.el --- le-saveplace -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(require 'lightemacs-package)

(require 'le-shut-up)

;; (defvar lightemacs-saveplace-recenter-after-find-file t
;;   "If non-nil, recenter the buffer after restoring the cursor position.")

(defvar lightemacs-saveplace-quiet t
  "If non-nil, suppress saveplace messages when Emacs is exiting.
This affects the messages shown during the `save-place-kill-emacs-hook',
preventing output in the echo area while saving buffer positions.")

(lightemacs-package saveplace
  :ensure nil
  :commands save-place-mode

  ;; TODO use on first file?
  :hook (lightemacs-after-init . save-place-mode)

  ;; TODO: Fix embark-collect-mode recentering when selecting .el files
  ;;   :preface
  ;;   ;; Fix embark collect recentering
  ;;   (defun lightemacs-saveplace--recenter ()
  ;;     "Recenter the current window."
  ;;     (let ((win (get-buffer-window (current-buffer))))
  ;;       (when (and win
  ;;                  (not (minibufferp)))
  ;;         (with-selected-window win
  ;;           (recenter)))))
  ;;
  ;;   (defun lightemacs-saveplace--after-find-file ()
  ;;     "Recenter the current window when `scroll-conservatively' >= 101.
  ;; This function is called by `save-place-after-find-file-hook'.
  ;; It avoids recentering while an EasySession session is in progress."
  ;;     (when (and (or (>= scroll-conservatively 101)
  ;;                    lightemacs-saveplace-recenter-after-find-file)
  ;;                (> (point) (point-min)))
  ;;       ;; Use a timer to ensure a window exists when recenter is called
  ;;       (run-with-timer 0 nil #'lightemacs-saveplace--recenter)))

  :init
  (setq save-place-limit 500)

  :config
  ;; (add-hook 'save-place-after-find-file-hook
  ;;           'lightemacs-saveplace--after-find-file)

  (defun lightemacs--around-save-place-kill-emacs-hook (fn &rest args)
    "Advice around `save-place-kill-emacs-hook' to optionally suppress messages.

FN is the original function being advised.
ARGS are the arguments passed to FN.

If `lightemacs-saveplace-quiet' is non-nil, output generated during execution is
silenced using `shut-up'Otherwise,the function executes normally."
    (if lightemacs-saveplace-quiet
        (shut-up
          (apply fn args))
      (apply fn args)))

  (advice-add 'save-place-kill-emacs-hook :around
              'lightemacs--around-save-place-kill-emacs-hook))

(provide 'le-saveplace)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-saveplace.el ends here
