;;; le-elec-pair.el --- le-elec-pair -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Automatically insert matching delimiters (), {}...

;;; Code:

(eval-and-compile
  (require 'lightemacs)
  (require 'use-package))

(lightemacs-use-package
  elec-pair
  :ensure nil
  :commands (electric-pair-mode
             electric-pair-local-mode
             electric-pair-delete-pair)
  :init
  (lightemacs-define-mode-add-hook-to electric-pair-mode
                                      '(lightemacs-on-first-buffer-hook)))

;;; Provide
(provide 'le-elec-pair)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-elec-pair.el ends here
