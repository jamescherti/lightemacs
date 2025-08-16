;;; mod-elec-pair.el --- mod-elec-pair -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Automatically insert matching delimiters (), {}...

;;; Code:

(use-package elec-pair
  :ensure nil
  :commands (electric-pair-mode
             electric-pair-local-mode
             electric-pair-delete-pair)
  :hook
  ;; TODO on-first-buffer?
  (after-init . electric-pair-mode))

;;; Provide
(provide 'mod-elec-pair)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-elec-pair.el ends here
