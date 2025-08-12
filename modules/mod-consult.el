;;; mod-consult.el --- mod-consult -*- no-byte-compile: t; lexical-binding: t -*-

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
;; URL: https://github.com/minad/consult

;;; Code:

;;; Consult

(use-package consult
  :commands (consult-fd
             consult-register-window
             consult-ripgrep
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
             consult-info
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
             consult-yank-pop)

  :hook
  ;; Enable automatic preview at point in the *Completions* buffer.
  (completion-list-mode . consult-preview-at-point-mode)

  :init
  (with-eval-after-load 'embark
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window. This adds thin lines, sorting
  ;; and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  (consult-customize consult-ripgrep
                     ;; consult-xref
                     consult-fd :preview-key '(:debounce 0.1 any)
                     consult-recent-file
                     consult--source-file-register
                     consult--source-recent-file
                     consult--source-project-recent-file
                     consult-git-grep consult-grep consult-bookmark
                     consult-theme :preview-key '(:debounce 0.1 any)
                     consult--source-bookmark
                     :preview-key '(:debounce 0.1 any)))

;;; Consult extensions

(use-package consult-xref
  :after consult
  :ensure nil
  :commands consult-xref
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package consult-info
  :after consult
  :ensure nil
  :commands consult-info
  :init
  (defun consult-info-emacs ()
    "Search through Emacs info pages."
    (interactive)
    (consult-info "emacs" "efaq" "elisp" "cl"))
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global (kbd "<leader>ci") #'consult-info))

  (defun consult-info-org ()
    "Search through the Org info page."
    (interactive)
    (consult-info "org"))

  (defun consult-info-completion ()
    "Search through completion info pages."
    (interactive)
    (consult-info "vertico" "consult" "marginalia" "orderless" "embark"
                  "corfu" "cape" "tempel")))

(use-package consult-imenu
  :after consult
  :ensure nil
  :commands (consult-imenu
             consult-imenu-multi))

;;; Completing indicator

(defun crm-indicator (args)
  "Add a prompt indicator for `completing-read-multiple` when using Consult.
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

(when (fboundp 'crm-indicator)
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;;; Provide
(provide 'mod-consult)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-consult.el ends here
