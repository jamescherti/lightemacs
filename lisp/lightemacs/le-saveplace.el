;;; le-saveplace.el --- le-saveplace -*- no-byte-compile: t; lexical-binding: t -*-

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

(require 'lightemacs)

(lightemacs-use-package
  saveplace
  :ensure nil
  :commands save-place-mode

  :init
  (setq save-place-limit 500))

(lightemacs-define-mode-hook-list save-place-mode
                                  '(lightemacs-on-first-file-hook))

(provide 'le-saveplace)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-saveplace.el ends here
