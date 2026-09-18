;;; le-so-long.el --- le-so-long -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The built-in so-long module helps Emacs reduce performance degradation and
;; unresponsiveness when opening files with exceptionally long lines, such as
;; minified source code or large datasets. It detects such files and disables
;; various minor modes and font-locking features that are known to cause
;; performance issues in these situations.

;;; Code:

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package so-long
  :ensure nil
  :commands (global-so-long-mode
             so-long-mode)
  :init
  ;; By default, when so-long detects a file with abnormally long lines, its
  ;; standard behavior is to replace the buffer's major mode with so-long-mode.
  ;; This is undesirable if you want to edit files using their original major
  ;; mode, which allows you to retain useful minor-mode behavior and tooling.
  ;; Setting so-long-action to so-long-minor-mode retains the original major
  ;; mode while applying the configured so-long performance mitigations.
  (setq so-long-action 'so-long-minor-mode)

  (lightemacs-module-hooks so-long-global
    global-so-long-mode
    '(lightemacs-on-first-file-hook))

  (lightemacs-module-hooks so-long-local
    so-long-mode
    '())

  :config
  ;; Apply so-long to configuration and plain text files
  (add-to-list 'so-long-target-modes 'conf-mode)
  (add-to-list 'so-long-target-modes 'text-mode)

  ;; Prevent so-long from attempting to restore the cursor position
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))

  ;; Ensure the buffer remains writable when so-long triggers, overriding the
  ;; default behavior that locks the buffer as read-only.
  (setf (alist-get 'buffer-read-only so-long-variable-overrides nil t) nil)

  ;; Retain line numbers for usability
  (setq so-long-minor-modes (delq 'display-line-numbers-mode so-long-minor-modes))

  ;; Keep syntax highlighting and reduce it.
  (setq so-long-minor-modes (delq 'font-lock-mode so-long-minor-modes))

  ;; Reduce the Tree-sitter decoration level
  (add-to-list 'so-long-variable-overrides '(treesit-font-lock-level . 1))

  ;; Limit font-lock to the minimum decoration level to save CPU cycles
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))

  ;; Many third-party packages can become expensive on buffers containing very
  ;; long lines. This block adds commonly used minor modes to
  ;; so-long-minor-modes. When so-long triggers, active modes in this list are
  ;; disabled for the current buffer.
  (dolist (mode '(;; Structural Editing and Parenthesis Matching
                  paredit-mode
                  enhanced-evil-paredit-mode
                  rainbow-delimiters-mode
                  smartparens-mode
                  smartparens-strict-mode

                  ;; Regex and Custom Highlighting
                  easy-escape-minor-mode
                  highlight-defined-mode
                  highlight-indent-guides-mode
                  auto-composition-mode

                  ;; Outline Scanning / Text folding
                  outline-minor-mode
                  treesit-fold-mode
                  ts-fold-mode
                  ts-fold-indicators-mode

                  ;; State & History Persistence / I/O
                  undo-fu-session-mode
                  undo-tree-mode
                  better-jumper-local-mode
                  auto-revert-mode

                  ;; Formatters & Whitespace Managers
                  aggressive-indent-mode
                  stripspace-local-mode
                  ws-butler-mode

                  ;; Linters & Language Servers
                  eglot--managed-mode
                  eldoc-mode
                  flycheck-mode
                  flymake-mode

                  ;; Spell Checkers
                  jinx-mode
                  spell-fu-mode

                  ;; UI Overlays & Margins
                  indent-bars-mode
                  highlight-numbers-mode
                  diff-hl-mode
                  git-gutter-mode
                  line-reminder-mode
                  page-break-lines-mode
                  hl-fill-column-mode))
    (add-to-list 'so-long-minor-modes mode)))

(provide 'le-so-long)

;;; le-so-long.el ends here
