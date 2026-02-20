;;; le-evil.el --- le-evil -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(require 'lightemacs-module)

;;; Variables

(eval-and-compile
  ;; This has to be defined before evil
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-collection-setup-minibuffer t))

;;; Use-package evil

(defun lightemacs-evil-delete-backward-C-h ()
  "In Evil insert state, make `C-h' behave like the `DEL' key.

Correct `C-h' behavior to ensure `electric-pair' deletes adjacent pairs When
using `electric-pair-mode', the expected behavior when pressing `C-h' near a
pair of adjacent delimiters (e.g., () [] {}) is that both the opening and
closing delimiters should be deleted together if they were inserted as a pair.
This function corrects `C-h' behavior to ensure `electric-pair' deletes adjacent
pairs.

This function also prevents ElDoc help from disappearing in the minibuffer when
pressing `C-h', since it is prefixed with `evil-delete'."
  (interactive)
  (when-let* ((del-binding (key-binding (kbd "DEL"))))
    (call-interactively del-binding)))

(lightemacs-use-package evil
  :commands (evil-mode
             evil-select-search-module)

  ;; Pressing '-' opens a `dired' buffer for the directory containing the
  ;; current file, automatically selecting that file. This provides a fast way
  ;; to navigate and manage files without manually switching to the directory.
  :bind (:map evil-normal-state-map
              ("-" . lightemacs-find-parent-directory))

  :hook (lightemacs-after-init . evil-mode)

  :init
  (setq evil-search-wrap lightemacs-cycle)

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

  ;; Suppress motion errors during keyboard macro execution in Evil
  (setq evil-kbd-macro-suppress-motion-error t)

  ;; Better Vim emulation
  ;; (setq evil-symbol-word-search t)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq evil-respect-visual-line-mode nil)
  (setq evil-want-C-g-bindings t)

  (setq evil-search-module 'evil-search)

  :custom
  ;; (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-C-i-jump t)
  (evil-want-C-h-delete t)
  (evil-want-C-w-delete t)
  (evil-want-C-u-delete t)

  :config
  ;; Occasionally, `evil' fails to respect the `evil-search-module'
  ;; customization, causing search behavior to diverge from the configured
  ;; value.
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; NOTE: This patch has been merged into the Emacs master branch but has not
  ;; been officially released yet.
  ;; URL: https://github.com/emacs-evil/evil/pull/1975
  ;; commit 3b80eb5c4496c21d72e233159b9698a73321afc5
  ;; Author: James Cherti <https://www.jamescherti.com/contact/>
  ;; Date:   2025-08-07 09:11:23 -0400
  ;;
  ;; Fixes #1974: Correct C-h behavior to ensure electric-pair deletes adjacent
  ;; pairs When using electric-pair-mode, the expected behavior when pressing
  ;; C-h near a pair of adjacent delimiters (e.g., () [] {} "") is that both the
  ;; opening and closing delimiters should be deleted together if they were
  ;; inserted as a pair. This pull request fixes #1974. This pull request
  ;; corrects C-h behavior to ensure electric-pair deletes adjacent pairs. It
  ;; calls the same function as DEL (code 127) in electric-pair-mode-map:
  (when (bound-and-true-p evil-want-C-h-delete)
    (define-key evil-insert-state-map (kbd "C-h")
                #'lightemacs-evil-delete-backward-C-h)

    ;; Remove C-h from `evil-insert-state-bindings' and replace it
    (setq evil-insert-state-bindings
          (seq-filter
           (lambda (binding)
             (not (and (stringp (car binding))
                       (string= (car binding) "\C-h"))))
           evil-insert-state-bindings))

    (add-to-list
     'evil-insert-state-bindings
     '("\C-h" . lightemacs-evil-delete-backward-C-h)))

  ;; Eldoc
  (with-eval-after-load 'eldoc
    (eldoc-add-command 'evil-normal-state
                       'lightemacs-evil-delete-backward-C-h
                       'evil-delete-backward-C-h  ;; Patch pending TODO
                       'evil-change
                       'evil-replace)

    ;; TODO pull request submitted.
    ;; Author: James Cherti <https://www.jamescherti.com/contact/>
    ;; URL: https://github.com/emacs-evil/evil/pull/1980
    ;; Prevent ElDoc help from disappearing in the minibuffer when executing
    ;; certain Evil commands in Emacs.
    (when (fboundp 'eldoc-add-command-completions)
      ;; Add evil delete, substitute, and change commands
      ;; - `evil-delete'
      ;; - `evil-delete-backward-word' (C-w)
      ;; - `evil-delete-back-to-indentation' (C-u)
      ;; - `evil-delete-backward-char-and-join' (C-h)
      ;; - And others.
      (eldoc-add-command-completions "evil-delete")
      (eldoc-add-command-completions "evil-substitute")
      (eldoc-add-command-completions "evil-change")

      ;; Add insert and append commands (`evil-insert', `evil-insert-resume',
      ;; `evil-append'...) to ElDoc to display help after switching to insert
      ;; mode.
      (eldoc-add-command-completions "evil-insert")
      (eldoc-add-command-completions "evil-append")

      ;; Add yank commands (`evil-yank' and `evil-yank-line')
      (eldoc-add-command-completions "evil-yank"))))

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
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-evil.el ends here
