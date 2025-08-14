;;; mod-evil-commentary.el --- mod-evil-commentary -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Comment or uncomment text in Normal or Visual mode by pressing gc.

;;; Code:

(use-package evil-commentary
  :after evil
  :commands evil-commentary-mode
  :hook (evil-mode . evil-commentary-mode))

(provide 'mod-evil-commentary)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-evil-commentary.el ends here
