;;; le-elec-pair.el --- le-elec-pair -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Automatically insert matching delimiters (), {}...

;;; Code:

(require 'lightemacs)

(lightemacs-use-package
  elec-pair
  :ensure nil
  :commands (electric-pair-mode
             electric-pair-local-mode
             electric-pair-delete-pair))

(lightemacs-define-mode-hook-list electric-pair-mode
                                  '(lightemacs-on-first-buffer-hook))

;;; Provide
(provide 'le-elec-pair)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-elec-pair.el ends here
