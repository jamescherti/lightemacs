;;; mod-helpful.el --- mod-helpful -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Helpful enhances the built-in help system by providing richer, more
;; contextual information about symbols, functions, variables, and macros.
;; Unlike the default `describe-*` commands, Helpful integrates additional
;; metadata such as source code, documentation strings, keybindings, references,
;; and interactive examples, all within a unified and easily navigable buffer.
;;
;; URL: https://github.com/Wilfred/helpful

;;; Code:

(use-package helpful
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)

  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)

  :preface
  (defun mod-helpful--emacs-lisp-setup ()
    "Setup `helpful' for ELisp files."
    (setq-local evil-lookup-func #'helpful-at-point))

  :init
  (setq helpful-max-buffers 7)
  (add-hook 'emacs-lisp-mode-hook #'mod-helpful--emacs-lisp-setup))

(provide 'mod-helpful)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-helpful.el ends here
