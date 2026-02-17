;;; le-autorevert.el --- le-autorevert -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(eval-and-compile
  (require 'le-core-package-manager))

(lightemacs-use-package autorevert
  :ensure nil
  :commands (auto-revert-mode
             global-auto-revert-mode
             auto-revert-handler)
  :hook (lightemacs-on-first-file . global-auto-revert-mode)
  :init
  (setq auto-revert-interval 4)
  (setq auto-revert-use-notify t))

(provide 'le-autorevert)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-autorevert.el ends here
