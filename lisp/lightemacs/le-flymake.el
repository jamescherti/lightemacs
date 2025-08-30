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
  (require 'lightemacs)
  (require 'use-package))

(lightemacs-use-package
  flymake
  :ensure nil
  :commands (flymake-mode
             flymake-show-buffer-diagnostics
             flymake-goto-next-error
             flymake-goto-prev-error)
  :init
  (setq flymake-wrap-around nil)

  (lightemacs-define-keybindings flymake-mode
    (with-eval-after-load 'flymake
      (define-key flymake-mode-map (kbd "C-c e p") #'flymake-goto-prev-error)
      (define-key flymake-mode-map (kbd "C-c e n") #'flymake-goto-next-error)
      (define-key flymake-mode-map (kbd "C-c e d") #'flymake-show-buffer-diagnostics)

      (define-key flymake-mode-map (kbd "M-g p") #'flymake-goto-prev-error)
      (define-key flymake-mode-map (kbd "M-g n") #'flymake-goto-next-error)))

  (lightemacs-define-mode-add-hook-to
    flymake-mode
    '(prog-mode-hook
      text-mode-hook)))

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
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-flymake.el ends here
