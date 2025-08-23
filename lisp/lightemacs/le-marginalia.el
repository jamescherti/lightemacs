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

(require 'lightemacs)

(lightemacs-use-package
  marginalia
  :commands (marginalia-mode
             marginalia-cycle)

  :init
  (setq marginalia-max-relative-age 0)
  (setq marginalia-align 'right))

(lightemacs-define-keybindings marginalia
  (define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle))

(lightemacs-define-mode-hook-list marginalia-mode
                                  '(lightemacs-on-first-input-hook))

(provide 'le-marginalia)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-marginalia.el ends here
