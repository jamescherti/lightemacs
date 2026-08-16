;;; le-marginalia.el --- le-marginalia -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package marginalia
  :commands (marginalia-mode
             marginalia-cycle)

  :init
  (lightemacs-module-hooks marginalia
    marginalia-mode
    '(lightemacs-on-first-input-hook))

  (lightemacs-module-bind marginalia
    (with-eval-after-load 'marginalia
      (define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle)))

  (lightemacs-module-setq-maybe marginalia
    marginalia-max-relative-age 0
    marginalia-align 'right))

;;; Provide

(provide 'le-marginalia)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-marginalia.el ends here
