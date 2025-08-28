;;; le-evil.el --- le-evil -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Evil is an extensible vi layer for Emacs. It emulates the main features of
;; Vim, and provides facilities for writing custom extensions.
;;
;; This configures the evil package.
;;
;; URL: https://github.com/emacs-evil/evil

;;; Code:

(eval-and-compile
  (require 'lightemacs)
  (require 'use-package))

;;; Variables

(eval-and-compile
  ;; This has to be defined before evil
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-collection-setup-minibuffer t))

;;; Use-package evil

(lightemacs-use-package
  evil
  :commands (evil-mode
             evil-select-search-module)

  :init
  (setq evil-search-wrap lightemacs-cycle)

  ;; Use evil-search instead of isearch
  (setq evil-search-module 'evil-search)

  ;; Time in seconds of idle before updating search highlighting.
  (setq evil-ex-hl-update-delay 0.05)

  ;; Make :s in visual mode operate only on the actual visual selection
  ;; (character or block), instead of the full lines covered by the selection
  (setq evil-ex-visual-char-range t)

  ;; Use Vim-style regular expressions in search and substitute commands,
  ;; allowing features like \v (very magic), \zs, and \ze for precise matches
  (setq evil-ex-search-vim-style-regexp t)

  ;; Disable copying the selection to the clipboard on every cursor move in
  ;; visual mode to increase performance
  (setq evil-visual-update-x-selection-p nil)

  ;; Do not modify the mode line to show Evil state
  (setq evil-mode-line-format nil)

  ;; Highlight only in the selected window to reduce Emacs' workload
  (setq evil-ex-interactive-search-highlight 'selected-window)

  ;; Suppress motion errors during keyboard macro execution in Evil
  (setq evil-kbd-macro-suppress-motion-error t)

  ;; Better Vim emulation
  ;; (setq evil-symbol-word-search t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq evil-respect-visual-line-mode nil)
  (setq evil-want-C-u-scroll nil)  ; Conflicts with evil-want-C-u-delete
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-g-bindings t)
  (setq evil-want-C-h-delete t)
  (setq evil-want-C-u-delete t)
  (setq evil-want-C-w-delete t)

  (lightemacs-define-mode-add-hook-to evil-mode '(after-init-hook))

  (lightemacs-define-keybindings evil
    (with-eval-after-load 'evil
      ;; Pressing '-' opens a `dired' buffer for the directory containing the
      ;; current file, automatically selecting that file. This provides a fast way
      ;; to navigate and manage files without manually switching to the directory.
      (define-key evil-normal-state-map (kbd "-") #'lightemacs-find-parent-directory)))

  :config
  (with-eval-after-load 'evil
    (evil-select-search-module 'evil-search-module evil-search-module))

  ;; Prevent ElDoc help from disappearing in the minibuffer when executing
  ;; certain Evil commands in Emacs.
  ;; Fixes: https://github.com/emacs-evil/evil/pull/1980
  (eval-after-load 'eldoc
    '(when (fboundp 'eldoc-add-command-completions)
       ;; `evil-delete-back-to-indentation', `evil-delete-backward-word',
       ;; `evil-insert', `evil-insert-line', `evil-append', `evil-append-line'...

       ;; Add evil-delete commands to ElDoc to display help while deleting using:
       ;; - evil-delete-backward-word (C-w)
       ;; - evil-delete-back-to-indentation (C-u)
       ;; - evil-delete-backward-char-and-join (C-h)
       ;; - And other evil-delete-* commands.
       (eldoc-add-command-completions "evil-delete-")

       ;; Add insert and append commands to ElDoc to display help after switching
       ;; to insert mode.
       (eldoc-add-command-completions "evil-insert-")
       (eldoc-add-command-completions "evil-append-"))))

;;; Keybindings

;;; Synchronize `evil-shift-width' with `tab-width'.

(defun lightemacs-evil--update-shift-width ()
  "Synchronize `evil-shift-width' with `tab-width'.
Org mode is excluded, since `tab-width' is conventionally fixed at 8 there."
  (unless (derived-mode-p 'org-mode)
    (setq-local evil-shift-width tab-width)))

(add-hook 'after-change-major-mode-hook #'lightemacs-evil--update-shift-width)

;;; Provide

(provide 'le-evil)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-evil.el ends here
