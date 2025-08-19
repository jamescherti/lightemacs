;;; le-marginalia.el --- le-marginalia -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Marginalia enriches minibuffer completions with contextual annotations.
;; It Enhances Vertico by adding rich annotations to completion candidates, such
;; as file sizes, documentation, or metadata.

;;; Code:

(use-package marginalia
  :commands (marginalia-mode
             marginalia-cycle)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  :init
  (add-hook 'lightemacs-on-first-input-hook #'marginalia-mode)
  (setq marginalia-max-relative-age 0)
  (setq marginalia-align 'right))

(provide 'le-marginalia)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-marginalia.el ends here
