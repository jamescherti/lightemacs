;;; mod-evil.el --- mod-evil -*- lexical-binding: t -*-

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
;; This configures the packages evil and evil-collection.
;;
;; URL: https://github.com/emacs-evil/evil

;;; Code:

(require 'lightemacs)

;;; Evil

;; This has to be defined before evil
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(use-package evil
  :hook
  (after-init . evil-mode)
  :functions define-key
  :init
  ;; Delete selected text on paste in visual state
  (setq evil-kill-on-visual-paste t)

  ;; Enable C-u to delete in insert state
  (setq evil-want-C-u-delete t)

  ;; Use evil-search instead of isearch
  (setq evil-search-module 'evil-search)

  ;; Enable C-h delete
  (setq evil-want-C-h-delete t)

  ;; Make `v$` exclude the final newline
  (setq evil-v$-excludes-newline t)

  ;; Prevent Evil state from being echoed, preserving Eldoc display in the
  ;; minibuffer (If set to t, Eldoc output in the minibuffer will be overridden)
  (setq evil-echo-state nil)

  ;; Use Vim-style regular expressions in search and substitute commands,
  ;; allowing features like \v (very magic), \zs, and \ze for precise matches
  (setq evil-ex-search-vim-style-regexp t)

  ;; Make :s in visual mode operate only on the actual visual selection
  ;; (character or block), instead of the full lines covered by the selection
  (setq evil-ex-visual-char-range t)

  ;; Enable automatic horizontal split below
  (setq evil-split-window-below t)

  ;; Enable automatic vertical split to the right
  (setq evil-vsplit-window-right t)

  ;; Enable fine-grained undo behavior
  (setq evil-want-fine-undo t)

  ;; Enable C-w to delete in insert state
  (setq evil-want-C-w-delete t)

  ;; 'Y' yanks to the end of the line.
  (setq evil-want-Y-yank-to-eol t)

  ;; Disable wrapping of search around buffer
  (setq evil-search-wrap nil)

  :config
  (with-eval-after-load 'aggressive-indent
    (add-to-list 'aggressive-indent-protected-commands 'evil-delete)
    (add-to-list 'aggressive-indent-protected-commands 'evil-redo)
    (add-to-list 'aggressive-indent-protected-commands 'evil-undo)
    (add-to-list 'aggressive-indent-protected-commands 'evil-delete-char)
    (add-to-list 'aggressive-indent-protected-commands 'evil-delete-line)))

;;; Press '-' to open dired

;; Pressing '-' opens a `dired' buffer for the directory containing the current
;; file, automatically selecting that file. This provides a fast way to navigate
;; and manage files without manually switching to the directory.

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "-") #'lightemacs-find-dired-parent))

(provide 'mod-evil)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-evil.el ends here
