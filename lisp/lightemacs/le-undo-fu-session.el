;;; le-undo-fu-session.el --- le-undo-fu-session -*- no-byte-compile: t; lexical-binding: t -*-

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

(eval-and-compile
  (require 'lightemacs)
  (require 'use-package))

(lightemacs-use-package
  undo-fu-session
  :demand t
  :commands undo-fu-session-global-mode
  :init
  (add-hook 'after-init-hook #'undo-fu-session-global-mode))

(provide 'le-undo-fu-session)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-undo-fu-session.el ends here
