;;; mod-marginalia.el --- mod-marginalia -*- lexical-binding: t -*-

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

  :hook
  (after-init . marginalia-mode)
  ;; TODO: Use on-first-input
  ;; (on-first-input . marginalia-mode)

  :init
  (setq marginalia-max-relative-age 0)
  (setq marginalia-align 'right))

(provide 'mod-marginalia)

;;; mod-marginalia.el ends here
