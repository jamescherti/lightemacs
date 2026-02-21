;;; le-core-pm-use-package.el --- le-core-pm-use-package -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Initialize the `use-package' package manager.

;;; Code:

(unless (bound-and-true-p byte-compile-current-file)
  ;; Install use-package if necessary
  (when (version< emacs-version "29.1")
    (unless (package-installed-p 'use-package)
      (unless package-archive-contents
        (package-refresh-contents))
      (package-install 'use-package)))

  (eval-when-compile
    (require 'use-package))

  ;; Initialize
  (package-initialize))

(provide 'le-core-pm-use-package)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-core-pm-use-package.el ends here
