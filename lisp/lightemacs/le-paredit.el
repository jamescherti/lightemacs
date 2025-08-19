;;; le-paredit.el --- le-paredit -*- lexical-binding: t -*-

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

(defvar lightemacs-paredit-hook-list '(emacs-lisp-mode-hook
                                       scheme-mode-hook
                                       lisp-interaction-mode-hook
                                       ielm-mode-hook
                                       lisp-mode-hook
                                       eval-expression-minibuffer-setup-hook
                                       cider-repl-mode-hook
                                       clojure-mode-hook
                                       geiser-repl-mode-hook
                                       racket-mode-hook
                                       racket-repl-mode-hook
                                       slime-repl-mode-hook)
  "The modes where `paredit-mode' is activated.")

(use-package paredit
  :commands paredit-mode
  ;; :bind (:map paredit-mode-map
  ;;             ("RET" . nil)
  ;;             ("M-s" . nil))

  :init
  (dolist (hook lightemacs-paredit-hook-list)
    (add-hook hook #'paredit-mode))

  :config
  (unbind-key "M-?" paredit-mode-map)  ; Conflict with xref-find-references
  (unbind-key "M-;" paredit-mode-map)  ; Conflict with comment-dwim
  (unbind-key "M-s" paredit-mode-map)  ; Conflict with Consult
  (unbind-key "RET" paredit-mode-map))

(provide 'le-paredit)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-paredit.el ends here
