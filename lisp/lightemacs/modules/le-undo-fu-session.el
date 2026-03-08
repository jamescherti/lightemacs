;;; le-undo-fu-session.el --- le-undo-fu-session -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The undo-fu package is a lightweight wrapper around Emacs' built-in undo
;; system, providing more convenient undo/redo functionality while preserving
;; access to the full undo history.
;;
;;The undo-fu-session package complements undo-fu by enabling the saving and
;; restoration of undo history across Emacs sessions, even after restarting.

;;; Code:

(eval-when-compile
  (require 'lightemacs-use-package))

;;; Variables

(defvar lightemacs-undo-fu-session-quiet lightemacs-reduce-messages
  "If non-nil, inhibit noisy `undo-fu-session' messages.
When non-nil, warnings such as \"Undo-Fu-Session cannot recover undo data\"
will only be logged to the *Messages* buffer, keeping the echo area clean.")

;;; `use-package'

(lightemacs-use-package undo-fu-session
  :commands undo-fu-session-global-mode
  :init
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'"
                                             "/git-rebase-todo\\'"
                                             "\\.gpg$"))
  (add-hook 'lightemacs-after-init-hook #'undo-fu-session-global-mode)

  :config
  (when (executable-find "zstd")
    ;; zstd is used due to its superior performance, as execution speed is the
    ;; primary objective within the Emacs environment.
    (setq undo-fu-session-compression 'zst))
  (undo-fu-session-global-mode))

;;; Quiet

(defun lightemacs--undo-fu-session-silence-a (orig-fn &rest args)
  "Advice to selectively inhibit `undo-fu-session' messages.
If `lightemacs-undo-fu-session-quiet' is non-nil, `inhibit-message' is set to t.
ORIG-FN is the original function and ARGS are its arguments."
  (let ((inhibit-message (or inhibit-message lightemacs-undo-fu-session-quiet)))
    (apply orig-fn args)))

(with-eval-after-load 'undo-fu-session
  ;; Use a loop to easily apply the generic advice to multiple noisy functions
  (dolist (fn '(undo-fu-session--file-limit-enforce
                undo-fu-session--recover-safe))
    (when (fboundp fn)
      (advice-add fn :around #'lightemacs--undo-fu-session-silence-a))))

(provide 'le-undo-fu-session)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-undo-fu-session.el ends here
