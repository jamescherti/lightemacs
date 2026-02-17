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

(eval-and-compile
  (require 'le-core-package-manager))

(lightemacs-use-package paredit
  :commands paredit-mode

  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)
         (scheme-mode . paredit-mode)
         (ielm-mode . paredit-mode)
         (cider-repl-mode . paredit-mode)
         (clojure-mode . paredit-mode)
         (geiser-repl-mode . paredit-mode)
         (racket-mode . paredit-mode)
         (racket-repl-mode . paredit-mode)
         (slime-repl-mode . paredit-mode))

  :bind (:map paredit-mode-map
              ("M-?" . nil)   ;; conflict with xref-find-references
              ("M-;" . nil)   ;; conflict with comment-dwim
              ("M-s" . nil)   ;; conflict with Consult
              ("RET" . nil))

  :config
  ;; Prevent ElDoc help from disappearing in the minibuffer when executing
  ;; certain Paredit commands (e.g., `paredit-backward-delete').
  (with-eval-after-load 'eldoc
    (eldoc-add-command-completions "paredit-")))

(provide 'le-paredit)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-paredit.el ends here
