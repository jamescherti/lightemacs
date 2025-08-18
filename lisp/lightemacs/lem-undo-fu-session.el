;;; lem-undo-fu-session.el --- lem-undo-fu-session -*- lexical-binding: t -*-

;; Author: James Cherti
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

(use-package undo-fu-session
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))

(provide 'lem-undo-fu-session)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lem-undo-fu-session.el ends here
