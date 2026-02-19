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

(lightemacs-module-package kirigami
  :commands (kirigami-open-fold
             kirigami-open-fold-rec
             kirigami-close-fold
             kirigami-toggle-fold
             kirigami-open-folds
             kirigami-close-folds-except-current
             kirigami-close-folds)

  :init
  (global-set-key (kbd "C-c k o") 'kirigami-open-fold)      ; Open fold at point
  (global-set-key (kbd "C-c k c") 'kirigami-close-fold)     ; Close fold at point
  (global-set-key (kbd "C-c k m") 'kirigami-close-folds)    ; Close all folds
  (global-set-key (kbd "C-c k r") 'kirigami-open-folds)     ; Open all folds
  (global-set-key (kbd "C-c k O") 'kirigami-open-fold-rec)  ; Open fold recursively
  (global-set-key (kbd "C-c k TAB") 'kirigami-toggle-fold)) ; Toggle fold at point

(provide 'le-kirigami)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-kirigami.el ends here
