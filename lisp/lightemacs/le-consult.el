;;; le-consult.el --- le-consult -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Consult offers a suite of commands for efficient searching, previewing, and
;; interacting with buffers, file contents, and more, improving various tasks.
;;
;; Vertico, Consult, and Embark collectively enhance Emacs' completion and
;; navigation capabilities.
;;
;; This module configures: consult, consult-xref, and consult-imenu.
;;
;; URL: https://github.com/minad/consult

;;; Code:

;; Require

;; Load `lightemacs--ripgrep-executable' and `lightemacs--fdfind-executable'
(require 'le-core-cli-tools)

(require 'lightemacs)

;;; Use-package consult

(lightemacs-use-package
  consult
  :commands (consult-fd
             consult-register-window
             consult-ripgrep
             consult-bookmark
             consult-buffer
             consult-buffer-other-frame
             consult-buffer-other-window
             consult-compile-error
             consult-complex-command
             consult-find
             consult-flymake
             consult-focus-lines
             consult-global-mark
             consult-goto-line
             consult-grep
             consult-history
             consult-isearch-history
             consult-keep-lines
             consult-kmacro
             consult-line
             consult-line-multi
             consult-locate
             consult-man
             consult-mark
             consult-mode-command
             consult-org-agenda
             consult-outline
             consult-preview-at-point-mode
             consult-project-buffer
             consult-recent-file
             consult-register
             consult-register-load
             consult-register-store
             consult-register-format
             consult-theme
             consult-yank-pop
             consult-imenu
             consult-xref)
  :functions (consult--customize-put
              consult-narrow-help)

  :init
  ;; Enable automatic preview at point in the *Completions* buffer.
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window. This adds thin lines, sorting
  ;; and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  :config
  (require 'consult-imenu)
  (require 'consult-xref)
  (require 'consult-info)

  ;; TODO defer
  (require 'consult-register)
  (require 'consult-compile)
  (require 'consult-kmacro)

  (consult-customize
   consult-theme :preview-key '(:debounce 0.1 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.1 any))

  ;; Configure the narrowing key. Both < and C-+ work reasonably well. "C-+"
  (setq consult-narrow-key "<")

  (setq consult-async-min-input 3
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  ;; (setq consult--gc-threshold (* 2 64 1024 1024))
  ;; (setq consult--process-chunk (* 2 1024 1024))

  (setq consult-fd-args
        (concat (if lightemacs--fdfind-executable
                    lightemacs--fdfind-executable
                  "fd")
                ;; Lightemacs
                " --hidden --exclude .git --absolute-path"
                (if (memq system-type '(cygwin windows-nt ms-dos))
                    " --path-separator=/"
                  "")

                ;; Default
                " --full-path --color=never"))

  (setq consult-ripgrep-args
        (concat (if lightemacs--ripgrep-executable
                    lightemacs--ripgrep-executable
                  "rg")
                ;; Lightemacs
                " --hidden -g !.git -g !.svn -g !.hg"

                ;; Default
                " --null --line-buffered --color=never --max-columns=1000"
                " --path-separator / --smart-case --no-heading"
                " --with-filename --line-number --search-zip"))

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

;;; Key bindings

(lightemacs-define-keybindings consult
  ;; Global bindings
  (global-set-key (kbd "C-c M-x") #'consult-mode-command)
  (global-set-key (kbd "C-c h") #'consult-history)
  (global-set-key (kbd "C-c m") #'consult-man)
  (global-set-key (kbd "C-c i") #'consult-info)
  (global-set-key [remap Info-search] #'consult-info)

  (global-set-key (kbd "C-x M-:") #'consult-complex-command)
  (global-set-key (kbd "C-x b") #'consult-buffer)
  (global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window)
  (global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)
  (global-set-key (kbd "C-x t b") #'consult-buffer-other-tab)
  (global-set-key (kbd "C-x r b") #'consult-bookmark)
  (global-set-key (kbd "C-x p b") #'consult-project-buffer)

  (global-set-key (kbd "M-#") #'consult-register-load)
  (global-set-key (kbd "M-'") #'consult-register-store)
  (global-set-key (kbd "C-M-#") #'consult-register)

  (global-set-key (kbd "M-y") #'consult-yank-pop)

  (global-set-key (kbd "M-g e") #'consult-compile-error)
  (global-set-key (kbd "M-g f") #'consult-flymake)
  (global-set-key (kbd "M-g g") #'consult-goto-line)
  (global-set-key (kbd "M-g M-g") #'consult-goto-line)
  (global-set-key (kbd "M-g o") #'consult-outline)
  (global-set-key (kbd "M-g m") #'consult-mark)
  (global-set-key (kbd "M-g k") #'consult-global-mark)
  (global-set-key (kbd "M-g i") #'consult-imenu)
  (global-set-key (kbd "M-g I") #'consult-imenu-multi)

  (global-set-key (kbd "M-s d") #'consult-find)
  (global-set-key (kbd "M-s c") #'consult-locate)
  (global-set-key (kbd "M-s g") #'consult-grep)
  (global-set-key (kbd "M-s G") #'consult-git-grep)
  (global-set-key (kbd "M-s r") #'consult-ripgrep)
  (global-set-key (kbd "M-s l") #'consult-line)
  (global-set-key (kbd "M-s L") #'consult-line-multi)
  (global-set-key (kbd "M-s k") #'consult-keep-lines)
  (global-set-key (kbd "M-s u") #'consult-focus-lines)
  (global-set-key (kbd "M-s e") #'consult-isearch-history)

  ;; Isearch-mode bindings
  (with-eval-after-load 'isearch
    (define-key isearch-mode-map (kbd "M-e") #'consult-isearch-history)
    (define-key isearch-mode-map (kbd "M-s e") #'consult-isearch-history)
    (define-key isearch-mode-map (kbd "M-s l") #'consult-line)
    (define-key isearch-mode-map (kbd "M-s L") #'consult-line-multi))

  ;; Minibuffer-local-map bindings
  (with-eval-after-load 'minibuffer
    (define-key minibuffer-local-map (kbd "M-s") #'consult-history)
    (define-key minibuffer-local-map (kbd "M-r") #'consult-history)))

;;; Completing indicator

(defun lightemacs-consult--crm-indicator (args)
  "Add a prompt indicator for `completing-read-multiple' when using Consult.
Displays `[CRM<separator>]` in the minibuffer to clarify multi-selection.

ARGS are the arguments.

This helps users recognize that multiple inputs are allowed and how to separate
them. Ensures this runs only when `crm` is loaded and Consult is in use."
  (when (boundp 'crm-separator)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args))))

(when (fboundp 'lightemacs-consult--crm-indicator)
  (advice-add
   #'completing-read-multiple :filter-args #'lightemacs-consult--crm-indicator))

;;; Provide
(provide 'le-consult)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-consult.el ends here
