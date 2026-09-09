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

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(defvar lightemacs-autorevert-disable-dired-verbose t
  "Set to non-nil to disable `auto-revert-verbose' in Dired buffers.")

(lightemacs-use-package autorevert
  :ensure nil
  :commands (auto-revert-mode
             global-auto-revert-mode
             auto-revert-handler)
  :init
  (lightemacs-module-setq-maybe autorevert
    auto-revert-interval 4
    auto-revert-use-notify t
    auto-revert-verbose (not lightemacs-reduce-messages))

  (lightemacs-module-hooks autorevert-global
    global-auto-revert-mode
    '(lightemacs-on-first-file-hook))

  :preface
  (defun le-autorevert-disable-verbose-in-dired ()
    "Disable `auto-revert-verbose' in the current Dired buffer.
This prevents unnecessary noise caused by displaying a message each time a file
is updated. Additionally, the specific messages displayed by Dired in these
instances are often misleading. This behavior is controlled by the variable
`lightemacs-autorevert-disable-dired-verbose'."
    (when lightemacs-autorevert-disable-dired-verbose
      (setq-local auto-revert-verbose nil)))

  :config
  (add-hook 'dired-mode-hook #'le-autorevert-disable-verbose-in-dired))

(provide 'le-autorevert)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-autorevert.el ends here
