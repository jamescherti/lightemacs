;;; le-evil-commentary.el --- le-evil-commentary -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Comment or uncomment text in Normal or Visual mode by pressing 'gc'.

;;; Code:

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package evil-commentary
  :commands evil-commentary-mode
  :init
  (lightemacs-module-hooks evil-commentary
    evil-commentary-mode
    '(lightemacs-after-init-hook)))

(provide 'le-evil-commentary)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-evil-commentary.el ends here
