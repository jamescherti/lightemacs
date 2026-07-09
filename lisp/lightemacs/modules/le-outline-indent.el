;;; le-outline-indent.el --- le-outline-indent -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The outline-indent Emacs package provides a minor mode that enables code
;; folding based on indentation levels.
;;
;; In addition to code folding, *outline-indent* allows:
;; - Moving indented blocks up and down
;; - Indenting/unindenting to adjust indentation levels
;; - Inserting a new line with the same indentation level as the current line
;; - Move backward/forward to the indentation level of the current line
;; - and other features.
;;
;; This mode is enabled by default for YAML and Python files.

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))
(require 'lightemacs-module)

(lightemacs-use-package outline-indent
  :commands (outline-indent-minor-mode
             outline-indent-shift-left
             outline-indent-shift-right
             outline-indent-move-subtree-up
             outline-indent-move-subtree-down
             outline-indent-forward-same-level
             outline-indent-backward-same-level
             outline-indent-select
             outline-indent-narrow
             outline-indent-insert-heading
             outline-indent-toggle-fold
             outline-indent-open-fold-rec
             outline-indent-close-fold
             outline-indent-open-fold
             outline-indent-open-folds
             outline-indent-close-folds)
  :init
  (setq outline-indent-ellipsis lightemacs-ellipsis)

  (lightemacs-module-hooks outline-indent-minor
    outline-indent-minor-mode '())

  :config
  ;; Fold management
  (define-key outline-indent-minor-mode-map (kbd "C-c o o") 'outline-indent-open-fold)
  (define-key outline-indent-minor-mode-map (kbd "C-c o c") 'outline-indent-close-fold)
  (define-key outline-indent-minor-mode-map (kbd "C-c o m") 'outline-indent-close-folds)
  (define-key outline-indent-minor-mode-map (kbd "C-c o r") 'outline-indent-open-folds)
  (define-key outline-indent-minor-mode-map (kbd "C-c o O") 'outline-indent-open-fold-rec)
  (define-key outline-indent-minor-mode-map (kbd "C-c o TAB") 'outline-indent-toggle-fold)
  (define-key outline-indent-minor-mode-map (kbd "C-c o t") 'outline-indent-toggle-level-at-point)

  ;; Select and narrow
  (define-key outline-indent-minor-mode-map (kbd "C-c o v") 'outline-indent-select)
  (define-key outline-indent-minor-mode-map (kbd "C-c o n") 'outline-indent-narrow)

  ;; Navigation at same indentation level (matches native outline-mode layout)
  (define-key outline-indent-minor-mode-map (kbd "C-c o f") 'outline-indent-forward-same-level)
  (define-key outline-indent-minor-mode-map (kbd "C-c o b") 'outline-indent-backward-same-level)

  ;; Shift left or right
  (define-key outline-indent-minor-mode-map (kbd "C-c o <right>") 'outline-indent-shift-right)
  (define-key outline-indent-minor-mode-map (kbd "C-c o <left>") 'outline-indent-shift-left)

  ;; Insert heading
  (define-key outline-indent-minor-mode-map (kbd "C-c o i") 'outline-indent-insert-heading))

(provide 'le-outline-indent)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-outline-indent.el ends here
