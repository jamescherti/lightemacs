;;; le-org-appear.el --- le-org-appear -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configure `org-mode' and `org-agenda'.

;;; Code:

(use-package org-appear
  :commands org-appear-mode
  :init
  (add-hook 'org-mode-hook #'org-appear-mode))

(provide 'le-org-appear)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-org-appear.el ends here
