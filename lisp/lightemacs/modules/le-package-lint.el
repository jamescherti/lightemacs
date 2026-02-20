;;; le-package-lint.el --- le-package-lint -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The `package-lint' package is a static analysis tool for validating the
;; metadata and structural conventions of Emacs Lisp files intended for
;; distribution as packages.
;;
;; It inspects package headers, dependency declarations, naming conventions,
;; keybindings, and other metadata to detect issues that could prevent
;; installation or cause incompatibilities for users.
;;
;; The tool also reports deviations from established Elisp packaging guidelines,
;; helping maintain consistency with community standards. It can be integrated
;; into automated build workflows and is commonly required for submission to
;; public archives such as MELPA.
;;
;; Integration packages such as flycheck-package and package-lint-flymake
;; provide real-time feedback within buffers during development.
;;
;; URL: https://github.com/purcell/package-lint

;;; Code:

(require 'lightemacs-module)

(lightemacs-use-package package-lint
  :commands (package-lint-batch-and-exit
             package-lint-current-buffer
             package-lint-buffer))

(provide 'le-package-lint)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-package-lint.el ends here
