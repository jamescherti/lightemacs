;;; le-consult.el --- le-consult -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(eval-and-compile
  (require 'le-core-package-manager))

;; Load `lightemacs--ripgrep-executable' and `lightemacs--fdfind-executable'
(require 'le-core-cli-tools)

;;; Use-package consult

(lightemacs-use-package consult
  :commands (consult-fd
             consult-register-window
             consult-ripgrep
             consult-bookmark
             consult-buffer
             consult-buffer-other-frame
             consult-buffer-other-window
             ;; consult-compile-error
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
             consult-project-buffer
             consult-recent-file
             consult-register
             consult-register-load
             consult-register-store
             consult-register-format
             consult-theme
             consult-yank-pop
             consult-imenu
             consult-info
             consult-xref)
  :functions (consult--customize-put
              consult-narrow-help)

  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c r" . consult-recent-file)
         ("M-s p" . consult-fd)

         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ;; ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ;; ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :init
  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; Obsolete (TODO)
  ;; (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
  ;; (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  ;; (setq register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window. This adds thin lines, sorting
  ;; and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  :config
  (require 'consult-imenu)
  (require 'consult-xref)
  (require 'consult-info)

  ;; TODO defer
  ;; (require 'consult-register)
  ;; (require 'consult-compile)
  ;; (require 'consult-kmacro)

  (consult-customize
   consult-theme :preview-key '(:debounce 0.1 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.1 any))

  ;; Configure the narrowing key. Both < and C-+ work reasonably well. "C-+"
  (setq consult-narrow-key "<")

  (setq consult-async-min-input 3
        consult-async-refresh-delay 0.1
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  (setq consult--gc-threshold (* 64 1024 1024))
  (setq consult--process-chunk (* 2 1024 1024))

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

(advice-add
 #'completing-read-multiple :filter-args 'lightemacs-consult--crm-indicator)

;;; Provide
(provide 'le-consult)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-consult.el ends here
