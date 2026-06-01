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
  (require 'lightemacs-use-package))
(require 'lightemacs-module)

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
             consult-buffer-other-tab
             consult-buffer-other-window
             ;; consult-compile-error
             consult-complex-command
             consult-find
             consult-flymake
             consult-focus-lines
             consult-global-mark
             consult-goto-line
             consult-grep
             consult-grep-match
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
             consult-org-heading
             consult-outline
             consult-project-buffer
             consult-recent-file
             consult-register
             consult-register-load
             consult-register-store
             consult-register-format
             consult-theme
             consult-yank-pop
             consult-yank-from-kill-ring
             consult-yank-replace
             consult-imenu
             consult-info
             consult-xref)
  :functions (consult--customize-put
              consult-narrow-help)

  :preface
  (defun lightemacs-consult--crm-indicator (args)
    "Add a prompt indicator for `completing-read-multiple' when using Consult.
Displays `[CRM<separator>]` in the minibuffer to clarify multi-selection.
ARGS are the arguments.
This helps users recognize that multiple inputs are allowed and how to separate
them. Ensures this runs only when `crm` is loaded and Consult is in use."
    (if (boundp 'crm-separator)
        (cons (format "[CRM%s] %s"
                      (replace-regexp-in-string
                       "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                       crm-separator)
                      (car args))
              (cdr args))
      ;; Fallback: return the original arguments untouched
      args))

  (advice-add #'completing-read-multiple
              :filter-args
              'lightemacs-consult--crm-indicator)

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

  ;; Use Consult to select completion in region if not using Corfu
  ;; (setq completion-in-region-function #'consult-completion-in-region)

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

  (setq consult--gc-threshold (* 128 1024 1024))
  (setq consult--process-chunk (* 2 1024 1024))

  ;; Fix `elpaca' with `with-eval-after-load'
  (with-eval-after-load 'le-core-cli-tools
    (if (fboundp 'lightemacs-core--load-cli-tools)
        (lightemacs-core--load-cli-tools)
      (error "Undefined: lightemacs-core--load-cli-tools"))

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
                  " --with-filename --line-number --search-zip")))

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

;;; Keybindings

(lightemacs-module-bind consult
  ;; C-c bindings in `mode-specific-map'
  (keymap-global-set "C-c r" #'consult-recent-file)
  (keymap-global-set "M-s p" #'consult-fd)

  (keymap-global-set "C-c M-x" #'consult-mode-command)
  (keymap-global-set "C-c h" #'consult-history)
  ;; (keymap-global-set "C-c k" #'consult-kmacro)
  (keymap-global-set "C-c m" #'consult-man)
  (keymap-global-set "C-c i" #'consult-info)
  (keymap-global-set "<remap> <Info-search>" #'consult-info)

  ;; C-x bindings in `ctl-x-map'
  (keymap-global-set "C-x M-:" #'consult-complex-command)
  (keymap-global-set "C-x b" #'consult-buffer)
  (keymap-global-set "C-x 4 b" #'consult-buffer-other-window)
  (keymap-global-set "C-x 5 b" #'consult-buffer-other-frame)
  (keymap-global-set "C-x t b" #'consult-buffer-other-tab)
  (keymap-global-set "C-x r b" #'consult-bookmark)
  (keymap-global-set "C-x p b" #'consult-project-buffer)

  ;; Custom M-# bindings for fast register access
  (keymap-global-set "M-#" #'consult-register-load)
  (keymap-global-set "M-'" #'consult-register-store)
  (keymap-global-set "C-M-#" #'consult-register)

  ;; Other custom bindings
  (keymap-global-set "M-y" #'consult-yank-pop)

  ;; M-g bindings in `goto-map'
  ;; (keymap-global-set "M-g e" #'consult-compile-error)
  (keymap-global-set "M-g r" #'consult-grep-match)
  (keymap-global-set "M-g f" #'consult-flymake)
  (keymap-global-set "M-g g" #'consult-goto-line)
  (keymap-global-set "M-g M-g" #'consult-goto-line)
  (keymap-global-set "M-g o" #'consult-outline)
  (keymap-global-set "M-g m" #'consult-mark)
  (keymap-global-set "M-g k" #'consult-global-mark)
  (keymap-global-set "M-g i" #'consult-imenu)
  (keymap-global-set "M-g I" #'consult-imenu-multi)

  ;; M-s bindings in `search-map'
  (keymap-global-set "M-s d" #'consult-find)
  (keymap-global-set "M-s c" #'consult-locate)
  (keymap-global-set "M-s g" #'consult-grep)
  (keymap-global-set "M-s G" #'consult-git-grep)
  (keymap-global-set "M-s r" #'consult-ripgrep)
  (keymap-global-set "M-s l" #'consult-line)
  (keymap-global-set "M-s L" #'consult-line-multi)
  (keymap-global-set "M-s k" #'consult-keep-lines)
  (keymap-global-set "M-s u" #'consult-focus-lines)

  ;; Isearch integration
  (keymap-global-set "M-s e" #'consult-isearch-history)

  (with-eval-after-load 'isearch
    (keymap-set isearch-mode-map "M-e" #'consult-isearch-history)
    (keymap-set isearch-mode-map "M-s e" #'consult-isearch-history)
    (keymap-set isearch-mode-map "M-s l" #'consult-line)
    (keymap-set isearch-mode-map "M-s L" #'consult-line-multi))

  ;; Minibuffer history
  (keymap-set minibuffer-local-map "M-s" #'consult-history)
  (keymap-set minibuffer-local-map "M-r" #'consult-history))

;;; Provide

(provide 'le-consult)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-consult.el ends here
