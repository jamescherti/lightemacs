;;; le-flymake.el --- le-flymake -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Flymake is a built-in on-the-fly syntax checking tool that analyzes source
;; code buffers in the background and highlights errors or warnings as you type.
;; It operates by invoking external syntax checkers or compilers asynchronously
;; and then annotates the buffer with diagnostic messages, which can be
;; navigated using dedicated commands. Unlike language servers, Flymake itself
;; does not perform analysis but provides a generic framework that integrates
;; with various backends, making it lightweight, extensible, and adaptable
;; across different programming languages.

;;; Code:

;;; Use-package `flymake'

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package flymake
  :ensure nil
  :commands (flymake-mode
             flymake-show-buffer-diagnostics
             flymake-goto-next-error
             flymake-goto-prev-error)

  :hook ((prog-mode . flymake-mode)
         (text-mode . flymake-mode))

  :bind
  (:map flymake-mode-map
        ("C-c e d" . flymake-show-buffer-diagnostics)
        ("C-c e p" . flymake-goto-prev-error)
        ("C-c e n" . flymake-goto-next-error)
        ("M-g p"   . flymake-goto-prev-error)
        ("M-g n"   . flymake-goto-next-error))

  :init
  (setq flymake-wrap-around nil))

;;; `elisp-mode': Append `load-path' to `elisp-flymake-byte-compile-load-path'

(defvar lightemacs-flymake--setup-elisp-done nil
  "Non-nil once `elisp-flymake-byte-compile-load-path' has been extended.")

;; Ensure `elisp-flymake-byte-compile-load-path' includes `load-path' once
(with-eval-after-load 'elisp-mode
  (unless lightemacs-flymake--setup-elisp-done
    (setq elisp-flymake-byte-compile-load-path
          (append elisp-flymake-byte-compile-load-path load-path))
    ;; Idempotence
    (setq lightemacs-flymake--setup-elisp-done t)))

(provide 'le-flymake)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-flymake.el ends here
