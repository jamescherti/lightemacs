;;; le-buffer-guardian.el --- le-buffer-guardian -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The buffer-guardian Emacs package provides buffer-guardian-mode, a global
;; mode that automatically saves buffers without requiring manual intervention.
;;
;; By default, buffer-guardian-mode saves file-visiting buffers when:
;; - Switching to another buffer.
;; - Switching to another window or frame.
;; - The window configuration changes (e.g., window splits).
;; - The minibuffer is opened.
;; - Emacs loses focus.
;;
;; URL: https://github.com/jamescherti/buffer-guardian.el

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package buffer-guardian
  :commands buffer-guardian-mode
  :hook
  (lightemacs-on-first-buffer . buffer-guardian-mode))

(provide 'le-buffer-guardian)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-buffer-guardian.el ends here
