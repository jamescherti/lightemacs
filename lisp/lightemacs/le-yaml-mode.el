;;; le-yaml-mode.el --- le-yaml-mode -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `yaml-mode' is a major mode for Emacs that provides specialized editing
;; support for YAML files. It offers syntax highlighting, indentation rules, and
;; basic editing conveniences, making it easier to read and write YAML
;; documents.
;;
;; NOTE: Tree-sitter's `yaml-ts-mode' offers more advanced syntax support than
;; `yaml-mode'.
;;
;; URL: https://github.com/yoshiki/yaml-mode

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(eval-and-compile
  (require 'use-package))

(lightemacs-use-package
  yaml-mode
  :commands yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(provide 'le-yaml-mode)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-yaml-mode.el ends here
