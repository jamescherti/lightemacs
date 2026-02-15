;;; le-yaml-ts-mode.el --- le-yaml-ts-mode -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

;;; Fix `yaml-ts-mode' comment-start-skip

(defun lightemacs-yaml-ts-mode--fix-comment-start-skip ()
  "Fix `comment-start-skip' in `yaml-ts-mode'.
Adjust `yaml-ts-mode' `comment-start-skip' to resolve an issue in certain Emacs
versions where `comment-or-uncomment-region' fails to correctly handle YAML
blocks. The previous `comment-start-skip' pattern left some - characters
uncommented after repeatedly commenting and uncommenting indented YAML
sections (bug#78892).
Versions affected by this bug: Emacs 30.1, 30.2, and <= 29.*."
  (setq-local comment-start-skip "#+ *"))

(add-hook 'yaml-ts-mode-hook #'lightemacs-yaml-ts-mode--fix-comment-start-skip)

;;; Setup `auto-mode-alist'

(defun lightemacs-yaml-ts-mode--setup ()
  "Configure `yaml-ts-mode'."
  (when (and (fboundp 'treesit-ready-p)
             (funcall 'treesit-ready-p 'yaml))
    (add-to-list 'auto-mode-alist '("\.[Yy][Aa][Mm]?[Ll]\\'" . yaml-ts-mode))))

(add-hook 'lightemacs-after-init-hook #'lightemacs-yaml-ts-mode--setup)

;;; Enhance `yaml-ts-mode' indentation

(defun lightemacs-yaml-ts-mode--setup-indentation ()
  "Configure `yaml-ts-mode'."
  (setq-local tab-width 2))

(add-hook 'yaml-ts-mode-hook #'lightemacs-yaml-ts-mode--setup-indentation)

(provide 'le-yaml-ts-mode)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-yaml-ts-mode.el ends here
