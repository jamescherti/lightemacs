;;; le-core-package-manager.el --- le-core-package-manager -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Initialize the package manager.

;;; Code:

(cond
 ;; Straight
 ((eq lightemacs-package-manager 'straight)
  (require 'le-core-straight))

 ;; Elpaca
 ((eq lightemacs-package-manager 'elpaca)
  (require 'le-core-elpaca))

 ;; use-package (built-in)
 ((eq lightemacs-package-manager 'use-package)
  (require 'le-core-use-package))

 (t
  (error (concat "[lightemacs]"
                 "Invalid value for `lightemacs-package-manager': '%S'. Valid "
                 "choices are: 'straight, 'elpaca, or 'use-package.")
         lightemacs-package-manager)))

(provide 'le-core-package-manager)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-core-package-manager.el ends here
