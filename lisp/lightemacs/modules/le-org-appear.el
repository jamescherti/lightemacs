;;; le-org-appear.el --- le-org-appear -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `org-appear' temporarily reveals normally hidden `org-mode' elements (such as
;; emphasis markers, links, or entities) when the cursor enters them, and hides
;; them again when the cursor leaves.

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package org-appear
  :commands org-appear-mode
  :hook (org-mode . org-appear-mode))

(provide 'le-org-appear)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-org-appear.el ends here
