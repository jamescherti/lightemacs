;;; le-evil-commentary.el --- le-evil-commentary -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Comment or uncomment text in Normal or Visual mode by pressing 'gc'.

;;; Code:

(require 'le-diminish)

(use-package evil-commentary
  :diminish evil-commentary-mode
  :commands evil-commentary-mode
  :init
  (add-hook 'evil-mode-hook #'evil-commentary-mode))

(provide 'le-evil-commentary)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-evil-commentary.el ends here
