;;; le-treesit-fold.el --- le-treesit-fold -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `treesit-fold' is an Emacs package that provides intelligent code folding by
;; leveraging the structural understanding of the built-in tree-sitter parser
;; (available in Emacs 29+). Unlike traditional folding methods that rely on
;; regular expressions or indentation, treesit-fold uses the actual syntax tree
;; of the code to accurately identify foldable regions such as functions,
;; classes, comments, and documentation strings. This allows for faster and more
;; precise folding behavior that respects the grammar of the programming
;; language, ensuring that fold boundaries are always syntactically correct even
;; in complex or nested code structures.
;;
;; https://github.com/emacs-tree-sitter/treesit-fold

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))
(require 'lightemacs-module)

(lightemacs-use-package treesit-fold
  :if (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  :commands (treesit-fold-close
             treesit-fold-close-all
             treesit-fold-open
             treesit-fold-toggle
             treesit-fold-open-all
             treesit-fold-mode
             global-treesit-fold-mode
             treesit-fold-open-recursively
             treesit-fold-line-comment-mode)
  :init
  (lightemacs-module-setq-maybe treesit-fold
    treesit-fold-line-count-show t
    treesit-fold-line-count-format lightemacs-ellipsis)

  (lightemacs-module-hooks treesit-fold-global
    global-treesit-fold-mode
    '())

  (lightemacs-module-hooks treesit-fold-local
    treesit-fold-mode
    '(;; Systems and General Purpose
      c-ts-mode-hook
      c++-ts-mode-hook
      java-ts-mode-hook
      rust-ts-mode-hook
      go-ts-mode-hook
      ruby-ts-mode-hook
      php-ts-mode-hook
      csharp-ts-mode-hook
      go-mod-ts-mode-hook
      lua-ts-mode-hook

      ;; Web and Frontend
      js-ts-mode-hook
      typescript-ts-mode-hook
      tsx-ts-mode-hook
      css-ts-mode-hook
      html-ts-mode-hook
      heex-ts-mode-hook
      xml-ts-mode-hook

      ;; Scripting and Infrastructure
      bash-ts-mode-hook
      cmake-ts-mode-hook
      dockerfile-ts-mode-hook
      awk-ts-mode-hook
      vimscript-ts-mode-hook
      nix-ts-mode-hook

      ;; Data and Configuration
      json-ts-mode-hook
      toml-ts-mode-hook

      ;; Build Systems and Makefiles
      makefile-ts-mode-hook

      ;; Hardware Description and Shaders
      verilog-ts-mode-hook
      vhdl-ts-mode-hook
      hlsl-ts-mode-hook

      ;; Scientific, Data Science, and Academic
      latex-ts-mode-hook
      beancount-ts-mode-hook

      ;; Documentation and Diagrams
      markdown-ts-mode-hook
      mermaid-ts-mode-hook

      ;; Other
      gdscript-ts-mode-hook
      clojure-ts-mode-hook
      caml-ts-mode-hook
      ocaml-ts-mode-hook
      erlang-ts-mode-hook
      elixir-ts-mode-hook
      scala-ts-mode-hook
      dart-ts-mode-hook
      haskell-ts-mode-hook
      julia-ts-mode-hook
      kotlin-ts-mode-hook
      gleam-ts-mode-hook
      noir-ts-mode-hook
      swift-ts-mode-hook
      zig-ts-mode-hook))

  :config
  (set-face-attribute 'treesit-fold-replacement-face nil
                      :foreground "#808080"
                      :box nil
                      :weight 'bold))

(provide 'le-treesit-fold)

;;; le-treesit-fold.el ends here
