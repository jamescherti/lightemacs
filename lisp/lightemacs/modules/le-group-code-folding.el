;;; le-group-code-folding.el --- group-code-folding -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configure Emacs code folding.
;;
;; This configuration is similar to:
;; https://www.jamescherti.com/emacs-the-definitive-guide-to-code-folding/

;;; Code:

;;; Require

(require 'lightemacs)

;;; Variables

(defvar lightemacs-code-folding-enable-treesit-fold nil)

;;;  Outline indent

(declare-function outline-indent-minor-mode "outline-indent" (&optional arg))

(lightemacs-module-hooks outline-indent-minor
  outline-indent-minor-mode
  (append
   (list
    ;; Python
    'python-mode-hook
    ;; Yaml
    'yaml-mode-hook
    'yaml-ts-mode-hook
    ;; Haskell
    'haskell-mode-hook)
   (when (not lightemacs-code-folding-enable-treesit-fold)
     ;; Use indentation-based folding as a fallback in these modes.
     (list 'python-ts-mode-hook
           'haskell-ts-mode-hook))))

;; Declare this after `lightemacs-module-hooks' to ensure defvar contains the
;; default values above
(lightemacs-module-load '(le-outline-indent))

;;; Outline

(lightemacs-module-hooks outline-minor
  outline-minor-mode
  '(emacs-lisp-mode-hook
    lisp-interaction-mode-hook ; scratch
    lisp-mode-hook
    conf-mode-hook
    markdown-mode-hook
    markdown-ts-mode-hook
    diff-mode-hook))

;;; Hideshow

(lightemacs-module-hooks hideshow
  hs-minor-mode
  (append
   (list
    ;; Systems and General Purpose
    'c-mode-hook
    'c++-mode-hook
    'java-mode-hook
    'rust-mode-hook
    'go-mode-hook
    'ruby-mode-hook
    'php-mode-hook
    'perl-mode-hook
    'ultisnips-mode-hook

    ;; Web and Frontend
    'js-mode-hook
    'typescript-mode-hook
    'css-mode-hook

    ;; Scripting, Data, and Infrastructure
    'sh-mode-hook
    'json-mode-hook
    'lua-mode-hook
    'nxml-mode-hook

    ;; mhtml and html
    'html-mode-hook)
   (unless lightemacs-code-folding-enable-treesit-fold
     (list
      'bash-ts-mode-hook))))

;; Declare this after `lightemacs-module-hooks' to ensure defvar contains the
;; default values above
(lightemacs-module-load '(le-hideshow))

;;; treesit-fold

(declare-function treesit-fold-mode "treesit-fold" (&optional arg))

(when lightemacs-code-folding-enable-treesit-fold
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
      python-ts-mode-hook

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
      yaml-ts-mode-hook

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

  ;; Declare this after `lightemacs-module-hooks' to ensure defvar contains the
  ;; default values above
  (lightemacs-module-load '(le-treesit-fold)))

;;; Kirigami

(lightemacs-module-load '(le-kirigami))

;;; Provide

(provide 'le-group-code-folding)

;;; le-group-code-folding.el ends here
