;;; le-nerd-icons-dired.el --- le-nerd-icons-dired -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Requires: nerd-icons, dired
;;
;; `nerd-icons-dired' adds Nerd Font icons to file and folder listings in Dired.
;; It displays icons beside their names to help identify file types at a
;; glance.
;;
;; URL: https://github.com/rainstormstudio/nerd-icons-dired

;;; Code:

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-module-load '(nerd-icons))

(lightemacs-use-package nerd-icons-dired
  :commands nerd-icons-dired-mode
  :init
  (lightemacs-module-hooks nerd-icons-dired
    nerd-icons-dired-mode
    '(dired-mode-hook)))

(provide 'le-nerd-icons-dired)

;;; le-nerd-icons-dired.el ends here
