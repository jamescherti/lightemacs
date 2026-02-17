;;; le-magit.el --- le-magit -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Magit provides a comprehensive interface to the Git version control system.
;; It aims to serve as a full-featured Git porcelain. Although it does not yet
;; cover every Git command, it is sufficiently complete to enable even
;; experienced Git users to perform nearly all their routine version control
;; tasks entirely within Emacs.
;;
;; URL: https://github.com/magit/magit

;;; Code:

(require 'lightemacs-use-package)

(lightemacs-use-package magit
  :commands (magit-status
             magit-commit
             magit-commit-create
             magit-diff
             magit-load-config-extensions
             magit-log
             magit-process-git)

  :functions (magit-restore-window-configuration
              magit-mode-get-buffers)

  :bind ("C-x g" . magit-status)

  :init
  (setq magit-diff-refine-hunk t))

(provide 'le-magit)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-magit.el ends here
