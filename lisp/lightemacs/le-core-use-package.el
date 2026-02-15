;;; le-core-use-package.el --- le-core-use-package -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Initialize the `use-package' package manager.

;;; Code:

;; Initialize
(package-initialize)

;; Install use-package if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (setq lightemacs--use-package-refreshed t)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(provide 'le-core-use-package)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-core-use-package.el ends here
