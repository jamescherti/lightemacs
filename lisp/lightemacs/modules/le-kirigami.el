;;; le-kirigami.el --- le-kirigami -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The kirigami Emacs package provides a unified method to fold and unfold text
;; in Emacs across a diverse set of Emacs modes.
;;
;; Supported modes include: `outline-mode', `outline-minor-mode',
;; `outline-indent-minor-mode', `org-mode', `markdown-mode', `gfm-mode',
;; `vdiff-mode', `vdiff-3way-mode', `hide-ifdef-mode', `vimish-fold-mode',
;; `TeX-fold-mode' (AUCTeX), `fold-this-mode', `origami-mode', `yafolding-mode',
;; `folding-mode', `ts-fold-mode', `treesit-fold-mode', and `hs-minor-mode'
;; (hideshow).
;;
;; With Kirigami, folding key bindings only need to be configured once. After
;; that, the same keys work consistently across all supported major and minor
;; modes, providing a unified and predictable experience for opening and closing
;; folds. The available commands include:
;; * `kirigami-open-fold': Open the fold at point.
;; * `kirigami-open-fold-rec': Open the fold at point recursively.
;; * `kirigami-close-fold': Close the fold at point.
;; * `kirigami-open-folds': Open all folds in the buffer.
;; * `kirigami-close-folds': Close all folds in the buffer.
;; * `kirigami-toggle-fold': Toggle the fold at point.
;;
;; In addition to unified interface, the kirigami package enhances folding
;; behavior in `outline', `markdown-mode', and `org-mode' packages. It ensures
;; that deep folds open reliably, allows folds to be closed even when the cursor
;; is positioned inside the content, and ensures that sibling folds at the same
;; level are visible when a sub-fold is expanded. When Kirigami closes outline
;; folds, it preserves the visibility of folded headings in the window.
;; Additionally, it resolves upstream Emacs issues, such as
;; bug#79286.
;;
;; URL: https://github.com/jamescherti/kirigami.el

;;; Code:

(require 'lightemacs-module)

(lightemacs-use-package kirigami
  :commands (kirigami-open-fold
             kirigami-open-fold-rec
             kirigami-close-fold
             kirigami-toggle-fold
             kirigami-open-folds
             kirigami-close-folds-except-current
             kirigami-close-folds))

(lightemacs-module-bind kirigami
  (global-set-key (kbd "C-c z o") 'kirigami-open-fold)
  (global-set-key (kbd "C-c z c") 'kirigami-close-fold)
  (global-set-key (kbd "C-c z m") 'kirigami-close-folds)
  (global-set-key (kbd "C-c z r") 'kirigami-open-folds)
  (global-set-key (kbd "C-c z O") 'kirigami-open-fold-rec)
  (global-set-key (kbd "C-c z TAB") 'kirigami-toggle-fold))

(provide 'le-kirigami)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-kirigami.el ends here
