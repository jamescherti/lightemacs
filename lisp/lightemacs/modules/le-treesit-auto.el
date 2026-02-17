;;; le-treesit-auto.el --- le-treesit-auto -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The le-treesit-auto module automatically installs and enables Tree-sitter
;; major modes in Emacs 29 and later. If the Tree-sitter parser is unavailable
;; or incompatible, it falls back to the original major mode.
;;
;; Tree-sitter is an incremental parsing system introduced in Emacs 29 that
;; delivers precise, high-performance syntax highlighting. It supports a wide
;; range of programming languages, including Bash, C, C++, C#, CMake, CSS,
;; Dockerfile, Go, Java, JavaScript, JSON, Python, Rust, TOML, TypeScript, YAML,
;; Elisp, Lua, and many others.
;;
;; URL: https://github.com/renzmann/treesit-auto

;;; Code:

(eval-and-compile
  (require 'le-core-package-manager))

(lightemacs-use-package treesit-auto
  :commands (global-treesit-auto-mode
             treesit-auto-add-to-auto-mode-alist)
  :hook (lightemacs-after-init . lightemacs-treesit-auto--setup)
  :preface
  (defun lightemacs-treesit-auto--setup ()
    "Setup `treesit-auto'."
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode))
  :init
  (setq treesit-auto-install 'prompt))

(provide 'le-treesit-auto)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-treesit-auto.el ends here
