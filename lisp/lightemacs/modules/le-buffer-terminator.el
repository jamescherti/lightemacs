;;; le-buffer-terminator.el --- le-buffer-terminator -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The buffer-terminator package automatically kills inactive buffers to
;; maintain a clean workspace and optimize performance. While the defaults work
;; for most, behavior is customizable via `buffer-terminator-rules-alist'.
;;
;; `buffer-terminator-mode' kills buffers inactive for
;; `buffer-terminator-inactivity-timeout' (default: 30 minutes) every
;; `buffer-terminator-interval' (default: 10 minutes).
;;
;; It safely preserves:
;; - Special buffers (non-file, *starred*, or special-mode derived).
;; - Unsaved modified files.
;; - Visible buffers (including indirect ones like org-src or markdown-mode).
;; - Buffers with running processes.
;;
;; URL: https://github.com/jamescherti/buffer-terminator.el

;;; Code:

(require 'lightemacs-module)

(lightemacs-use-package buffer-terminator
  :commands (buffer-terminator-mode
             buffer-terminator-apply-rules)

  :init
  (setq buffer-terminator-debug init-file-debug)

  (lightemacs-module-hooks buffer-terminator
    buffer-terminator-mode
    '(emacs-startup-hook)))

(provide 'le-buffer-terminator)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-buffer-terminator.el ends here
