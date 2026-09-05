;;; le-paredit.el --- le-paredit -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package paredit
  :commands paredit-mode

  :init
  (lightemacs-module-hooks paredit
    paredit-mode
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

  (lightemacs-module-bind paredit
    (with-eval-after-load 'paredit
      (define-key paredit-mode-map (kbd "M-?") nil)   ;; conflict with xref-find-references
      (define-key paredit-mode-map (kbd "M-;") nil)   ;; conflict with comment-dwim
      (define-key paredit-mode-map (kbd "M-s") nil)   ;; conflict with Consult
      (define-key paredit-mode-map (kbd "RET") nil)))

  :config
  ;; Prevent ElDoc help from disappearing in the minibuffer when executing
  ;; certain Paredit commands (e.g., `paredit-backward-delete').
  (with-eval-after-load 'eldoc
    (when (fboundp 'eldoc-add-command-completions)
      (eldoc-add-command-completions "paredit-"))))

(provide 'le-paredit)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-paredit.el ends here
