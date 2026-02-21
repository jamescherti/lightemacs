;; le-package-lint-flymake.el --- le-package-lint-flymake -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configures package-lint to integrate with Flymake, providing real-time
;; evaluation of Emacs Lisp package metadata and formatting. It assists in
;; identifying packaging errors, verifying required headers, and ensuring
;; adherence to standard archive guidelines directly within the buffer.
;;
;; The le-package-lint-flymake module activates automatically for
;; emacs-lisp-mode. You must also add the le-flymake module to your config.el to
;; ensure Flymake starts when editing source code.
;;
;; URL: https://github.com/purcell/package-lint-flymake

;;; Code:

(require 'lightemacs-module)
(require 'le-package-lint)

(lightemacs-use-package package-lint-flymake
  :commands (package-lint-flymake-setup
             package-lint-flymake)

  :init
  (lightemacs-module-hooks package-lint-flymake
    package-lint-flymake-setup
    '(emacs-lisp-mode-hook)))

(provide 'le-package-lint-flymake)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-package-lint-flymake.el ends here
