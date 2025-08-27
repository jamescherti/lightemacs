;;; le-paredit.el --- le-paredit -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Paredit assists in editing Lisp code by enforcing the structural integrity of
;; s-expressions. Instead of treating parentheses as ordinary characters,
;; Paredit ensures that every edit operation, such as inserting, deleting, or
;; moving expressions, preserves balanced parentheses and valid Lisp syntax. It
;; provides structured editing commands for navigating, wrapping, splicing, or
;; reformatting code, making it significantly easier to manipulate nested
;; expressions without introducing syntactic errors.
;;
;; URL: https://paredit.org/

;;; Code:

(eval-and-compile
  (require 'lightemacs)
  (require 'use-package))

(lightemacs-use-package
  paredit
  :commands paredit-mode

  :init
  (lightemacs-define-keybindings paredit
    (with-eval-after-load 'paredit
      (unbind-key "M-?" paredit-mode-map)  ; Conflict with xref-find-references
      (unbind-key "M-;" paredit-mode-map)  ; Conflict with comment-dwim
      (unbind-key "M-s" paredit-mode-map)  ; Conflict with Consult
      (unbind-key "RET" paredit-mode-map)))

  ;; This defines the global variable `lightemacs-paredit-mode-hook-list'
  (lightemacs-define-mode-hook-list paredit-mode
                                    '(emacs-lisp-mode-hook
                                      lisp-interaction-mode-hook
                                      lisp-mode-hook
                                      eval-expression-minibuffer-setup-hook
                                      scheme-mode-hook
                                      ielm-mode-hook
                                      cider-repl-mode-hook
                                      clojure-mode-hook
                                      geiser-repl-mode-hook
                                      racket-mode-hook
                                      racket-repl-mode-hook
                                      slime-repl-mode-hook))

  :config
  ;; Prevent ElDoc help from disappearing in the minibuffer when executing
  ;; certain Evil commands in Emacs.
  (with-eval-after-load 'eldoc
    (eldoc-add-command-completions "paredit-")))

(provide 'le-paredit)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-paredit.el ends here
