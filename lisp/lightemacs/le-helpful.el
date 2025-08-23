;;; le-helpful.el --- le-helpful -*- no-byte-compile: t; lexical-binding: t -*-

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

(require 'lightemacs)

(lightemacs-use-package
  helpful
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)

  :preface
  (defun le-helpful--emacs-lisp-setup ()
    "Setup `helpful' for ELisp files."
    (setq-local evil-lookup-func #'helpful-at-point))

  :init
  (setq helpful-max-buffers 7)
  (add-hook 'emacs-lisp-mode-hook #'le-helpful--emacs-lisp-setup))

(lightemacs-define-keybindings helpful
  (global-set-key [remap describe-command] #'helpful-command)
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-key] #'helpful-key)
  (global-set-key [remap describe-symbol] #'helpful-symbol)
  (global-set-key [remap describe-variable] #'helpful-variable))

(provide 'le-helpful)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-helpful.el ends here
