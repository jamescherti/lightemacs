;;; le-org.el --- le-org -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configure `org-mode' and `org-agenda'.

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package org
  :ensure nil
  :commands (org-mode
             org-indent-mode
             org-set-tags-command
             org-version
             org-agenda
             org-capture
             org-schedule
             org-agenda-filter
             org-agenda-todo
             org-agenda-set-tags
             org-agenda-filter-remove-all
             org-agenda-goto)

  :mode
  ("\\.org\\'" . org-mode))

(provide 'le-org)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-org.el ends here
