;;; mod-treesit-auto.el --- mod-treesit-auto -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The mod-treesit-auto module automatically installs and enables Tree-sitter
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

(use-package treesit-auto
  :hook
  (after-init . global-treesit-auto-mode)
  :init
  (setq treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(provide 'mod-treesit-auto)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-treesit-auto.el ends here
