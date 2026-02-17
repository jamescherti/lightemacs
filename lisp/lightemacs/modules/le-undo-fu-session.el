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

(require 'lightemacs-use-package)

(lightemacs-use-package undo-fu-session
  :commands undo-fu-session-global-mode
  :init
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'"
                                             "/git-rebase-todo\\'"
                                             "\\.gpg$"))

  (when (executable-find "zstd")
    ;; zstd is used due to its superior performance, as execution speed is the
    ;; primary objective within the Emacs environment.
    (setq undo-fu-session-compression 'zst))

  (add-hook 'lightemacs-after-init-hook #'undo-fu-session-global-mode))

(provide 'le-undo-fu-session)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-undo-fu-session.el ends here
