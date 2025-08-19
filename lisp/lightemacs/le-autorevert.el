;;; le-autorevert.el --- le-autorevert -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Auto-revert is a feature that automatically updates the contents of a buffer
;; to reflect changes made to the underlying file on disk.
;;
;; URL: https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html

;;; Code:

(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode
             global-auto-revert-mode
             auto-revert-handler)

  :init
  (add-hook 'lightemacs-on-first-file-hook #'global-auto-revert-mode)
  (setq auto-revert-interval 4)
  (setq auto-revert-use-notify t))

(provide 'le-autorevert)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-autorevert.el ends here
