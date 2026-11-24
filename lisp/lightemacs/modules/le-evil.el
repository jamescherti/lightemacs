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

;;; Require

(require 'lightemacs)
(require 'use-package)

;;; Variables

(defvar lightemacs-evil-setup-undo-redo t)
(defvar lightemacs-evil-setup-evil-search t)

(defvar lightemacs-evil-suppress-search-failed lightemacs-reduce-messages
  "Non-nil means suppress \='search failed\=' messages from Evil search commands.
When non-nil, failed search motions like `evil-ex-search-next' (n) and
`evil-ex-search-previous' (N) fail silently.
Key benefits:
- Prevents overwriting informative messages in the echo area, such as
  Eldoc documentation signatures, Flymake diagnostics, or which-key hints.
- Avoids visual noise during rapid repeat motions.
- Keyboard macros will still correctly abort on a failed search.")

(eval-and-compile
  ;; This has to be defined before evil
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-collection-setup-minibuffer t))

;;; Undo-redo

(when lightemacs-evil-setup-undo-redo
  (with-eval-after-load 'evil
    (let ((undo-system (if (< emacs-major-version 31)
                           (progn
                             (require 'le-undo-fu)
                             'undo-fu)
                         'undo-redo)))
      (if (>= emacs-major-version 29)
          (setopt evil-undo-system undo-system)
        (customize-set-variable 'evil-undo-system undo-system)))))

;;; Functions

(defun lightemacs-evil-find-parent-directory ()
  "Go to the parent directory or superior archive buffer of the current buffer.
If the current buffer is a subfile of an archive or tar file, switch to the
superior archive buffer. Otherwise, open a `dired' buffer for the current
file's directory and select the file. If the buffer is not visiting a file,
open `default-directory'."
  (interactive)
  (let ((superior-buffer
         (let ((archive (bound-and-true-p archive-superior-buffer))
               (tar (bound-and-true-p tar-superior-buffer)))
           (cond ((and archive (buffer-live-p archive)) archive)
                 ((and tar (buffer-live-p tar)) tar)))))
    (cond
     ;; Archive and tar superior buffer
     (superior-buffer
      (set-window-buffer nil superior-buffer))

     ;; Files
     (t
      (let* ((buf (or (buffer-base-buffer) (current-buffer)))
             (file (buffer-file-name buf))
             (dir (if file (file-name-directory file) default-directory)))
        (when dir
          (when-let* ((dired-buf (find-file-noselect dir)))
            (set-window-buffer nil dired-buf)
            (when file
              (with-current-buffer dired-buf
                (when (derived-mode-p 'dired-mode)
                  (when (fboundp 'dired-goto-file)
                    (dired-goto-file file))
                  (lightemacs-recenter-maybe)))))))))))

;;; Use-package evil

(use-package evil
  :commands (evil-mode
             evil-select-search-module
             evil-define-key*
             evil-set-undo-system)

  :init
  (lightemacs-module-hooks evil
    evil-mode
    '(lightemacs-after-init-hook))

  (lightemacs-module-bind evil
    ;; Pressing '-' opens a `dired' buffer for the directory containing the
    ;; current file, automatically selecting that file. This provides a fast way
    ;; to navigate and manage files without manually switching to the directory.
    (with-eval-after-load 'evil
      (define-key evil-normal-state-map (kbd "-") #'lightemacs-evil-find-parent-directory)))

  (lightemacs-module-setq-maybe evil
    evil-search-wrap lightemacs-cycle
    ;; Enabling `evil-want-fine-undo' provides more granular tracking of text
    ;; changes. This allows you to revert small typos without losing an entire
    ;; insertion block, making the undo system behave more like vanilla Emacs
    ;; than standard Vim. While this clutters the history, the increased control
    ;; makes the tradeoff worth it.
    evil-want-fine-undo t
    ;; Time in seconds of idle before updating search highlighting.
    evil-ex-hl-update-delay 0.01
    ;; Prevent Evil state from being echoed, preserving Eldoc display in the
    ;; minibuffer (If set to t, Eldoc output in the minibuffer will be
    ;; overridden)
    evil-echo-state nil
    ;; Disable copying the selection to the clipboard on every cursor move in
    ;; visual mode to increase performance
    evil-visual-update-x-selection-p nil
    ;; This modifies Y to yank text from the cursor to the end of the line. This
    ;; is a adopted modern default (also used by Neovim) because it makes Y
    ;; consistent with C (change to end of line) and D (delete to end of line).
    ;; Traditional Vim behavior makes Y duplicate yy (yank whole line), which is
    ;; redundant.
    evil-want-Y-yank-to-eol t
    ;; Make :s/text/text2/g in visual mode operate only on the actual visual
    ;; selection (character or block), instead of the full lines covered by the
    ;; selection
    evil-ex-visual-char-range t
    ;; Enabling these provides standard readline and Vim insert mode behavior.
    ;; This allows users to delete characters, words, and lines using standard
    ;; terminal keybindings while in insert mode.
    evil-want-C-h-delete t
    evil-want-C-w-delete t
    evil-want-C-u-delete t
    ;; Enable automatic horizontal split below and automatic vertical split to
    ;; the right.
    ;;
    ;; Forcing horizontal splits to open below and vertical splits to open on
    ;; the right aligns window management with modern IDEs and terminal
    ;; multiplexers.
    evil-split-window-below t
    evil-vsplit-window-right t)
  :config
  (define-key evil-insert-state-map (kbd "C-g") #'lightemacs-keyboard-quit)
  (define-key evil-normal-state-map (kbd "C-g") #'lightemacs-keyboard-quit)
  (define-key evil-visual-state-map (kbd "C-g") #'lightemacs-keyboard-quit)

  (when lightemacs-evil-setup-evil-search
    ;; Occasionally, `evil' fails to respect the `evil-search-module' when
    ;; `evil-search-module' is in :custom, causing search behavior to diverge
    ;; from the configured value.
    (if (>= emacs-major-version 29)
        (setopt evil-search-module 'evil-search)
      (customize-set-variable 'evil-search-module 'evil-search))))

;;; Synchronize `evil-shift-width' with `tab-width'.

(defun lightemacs-evil--update-shift-width ()
  "Synchronize `evil-shift-width' with `tab-width'.
Org mode is excluded, since `tab-width' is conventionally fixed at 8 there."
  (unless (derived-mode-p 'org-mode)
    (setq-local evil-shift-width tab-width)))

(add-hook 'after-change-major-mode-hook #'lightemacs-evil--update-shift-width)

;;; Patch: Correct C-h to ensure electric-pair deletes adjacent pairs

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

(with-eval-after-load 'evil
  (when (bound-and-true-p evil-want-C-h-delete)
    ;; Alternative, but it makes eldoc messages disappear
    ;; (define-key evil-insert-state-map (kbd "C-h") (kbd "DEL"))
    (define-key evil-insert-state-map (kbd "C-h")
                #'lightemacs-evil-delete-backward-C-h)

    ;; Remove C-h from `evil-insert-state-bindings' and replace it
    (setq evil-insert-state-bindings
          (seq-filter
           (lambda (binding)
             (not (and (stringp (car binding))
                       (string= (car binding) "\C-h"))))
           evil-insert-state-bindings))

    ;; Alternative, but it makes eldoc messages disappear
    ;; (add-to-list 'evil-insert-state-bindings `("\C-h" . ,(kbd "DEL")))

    (add-to-list 'evil-insert-state-bindings
                 '("\C-h" . lightemacs-evil-delete-backward-C-h))))

;;; Patch: Fix eldoc

;; To improve the ElDoc experience for Evil mode users, this pull request fixes
;; #1969 by adding the following commands to the ElDoc system:
;;
;; evil-delete-*
;; evil-insert-*
;; evil-append-*
;;
;; This resolves the issue where ElDoc help disappears from the minibuffer when
;; using Evil mode after executing certain commands, such as deleting words or
;; characters, or switching to insert mode.
;;
;; URL: https://github.com/emacs-evil/evil/pull/1980
(with-eval-after-load 'evil
  (with-eval-after-load 'eldoc
    (eldoc-add-command 'evil-normal-state
                       'lightemacs-evil-delete-backward-C-h
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

;;; Enhance `evil-goto-definition'

(setq evil-goto-definition-functions
      '(;; Delegates the jump to the Emacs 'xref' API. This is the modern
        ;; standard that can cross file boundaries and hooking directly into
        ;; Language Server Protocols (eglot/lsp-mode) or dumb-jump.
        evil-goto-definition-xref

        ;; Uses the standard Emacs 'imenu' index. It relies on regular
        ;; expressions defined by the current major mode to find definitions
        ;; within the active buffer.
        evil-goto-definition-imenu

        ;; DISABLED: evil-goto-definition-semantic
        ;; Hooks into the CEDET/Semantic framework. This is an older parsing
        ;; engine that attempts to understand code syntax trees. Disabled. This
        ;; belongs to a legacy era of Emacs (the CEDET/Semantic project) which
        ;; attempted to parse C/C++ and other languages entirely in Elisp. In
        ;; the modern landscape of Language Server Protocols (LSP) and
        ;; Tree-sitter, CEDET is obsolete, slow, and abandoned.
        ;; evil-goto-definition-semantic

        ;; The fallback mechanism. If all else fails, it executes a primitive
        ;; text search inside the current buffer for the exact string matching
        ;; the symbol under your cursor.
        evil-goto-definition-search))

;; TODO evil patch
;; Fix: C-t when jumping using imenu. `evil-goto-definition-search' and
;; `evil-goto-definition-imenu' does not push the pre-jump location onto the
;; xref marker stack, so C-t (xref-go-back) cannot return here after a jump.

(defun lightemacs-push-xref-on-imenu-jump (orig-fn &rest args)
  "Push pre-jump location to `xref' stack if goto definition function moves.

ORIG-FN is the original goto definition function.
ARGS are the arguments passed to ORIG-FN."
  (let ((start-buffer (current-buffer))
        (start-point (point))
        (start-marker (point-marker))
        result)
    (setq result (apply orig-fn args))
    (when (or (not (eq (current-buffer) start-buffer))
              (not (eql (point) start-point)))
      (require 'xref)
      (if (fboundp 'xref-push-marker-stack)
          (xref-push-marker-stack start-marker)
        (error "Undefined: `xref-push-marker-stack'")))
    result))

(with-eval-after-load 'evil
  (advice-add 'evil-goto-definition-search :around
              #'lightemacs-push-xref-on-imenu-jump)
  (advice-add 'evil-goto-definition-imenu :around
              #'lightemacs-push-xref-on-imenu-jump))

;;; Provide

(provide 'le-evil)

;;; le-evil.el ends here
