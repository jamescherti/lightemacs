;;; le-elec-pair.el --- Configure Electric Pair -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Automatically insert matching delimiters (), {}...

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))
(require 'lightemacs-module)

(lightemacs-use-package elec-pair
  :ensure nil
  :commands (electric-pair-mode
             electric-pair-local-mode
             electric-pair-delete-pair)
  :init
  (lightemacs-module-hooks electric-pair-global
    electric-pair-mode
    '(lightemacs-on-first-buffer-hook))

  (lightemacs-module-hooks electric-pair-local
    electric-pair-local-mode
    nil))

;;; Provide
(provide 'le-elec-pair)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-elec-pair.el ends here
