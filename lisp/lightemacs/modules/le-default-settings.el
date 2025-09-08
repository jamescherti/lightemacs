;;; le-default-settings.el --- le-default-settings -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Miscellaneous Lightemacs default settings.

;;; Code:

;;; Require

(eval-and-compile
  (require 'lightemacs))

;; Load `lightemacs--ripgrep-executable' and `lightemacs--fdfind-executable'
(require 'le-core-cli-tools)

;;; Misc

;; TODO: move to m.e.?
(setq eldoc-documentation-strategy #'eldoc-documentation-compose)

(setq byte-compile-warnings
      (if init-file-debug
          t
        ;; free-vars: warns about the use of free (unbound) variables. This is
        ;; almost always a sign of a typo or a bug.
        ;;
        ;; unresolved: warns when a function or variable cannot be found at
        ;; compile time. This can indicate a missing require or an undefined
        ;; symbol.
        ;;
        ;; noruntime – warns when runtime evaluation of code is disabled, e.g.,
        ;; using defvar or defun incorrectly in a macro context.
        ;;
        ;; obsolete – optional, but useful if you want to catch usage of
        ;; deprecated functions or variables.
        ;; '(not free-vars unresolved noruntime)
        t

        ;; (byte-compile-warnings . (not free-vars unresolved mapcar constants))
        ;; (setq byte-compile-warnings '(not nresolved free-vars callargs redefine obsolete noruntime cl-functions interactive-only))
        ;; (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local obsolete))
        ;; (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
        ;; (setq byte-compile-warnings '(not lexical))
        ;; (setq warning-suppress-types '((lexical-binding)))
        ;; (setq warning-minimum-level :error)

        ))
(setq warning-minimum-level (if init-file-debug :warning :error))

;; Alternative: (setq-default display-fill-column-indicator-character ?┊)
(setq-default display-fill-column-indicator-character ?\N{U+2502})

;;; treesit

(setq treesit-font-lock-level 4) ; Max: 4

;; 54 languages
(setq treesit-language-source-alist
      '((bash . "https://github.com/tree-sitter/tree-sitter-bash")
        (python . "https://github.com/tree-sitter/tree-sitter-python")
        (yaml . "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
        (json . "https://github.com/tree-sitter/tree-sitter-json")
        (html . "https://github.com/tree-sitter/tree-sitter-html")
        (lua . "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (dockerfile . "https://github.com/camdencheek/tree-sitter-dockerfile")
        (java . "https://github.com/tree-sitter/tree-sitter-java")
        (javascript . "https://github.com/tree-sitter/tree-sitter-javascript")
        ;; TODO: add markdown to treesit auto
        (markdown
         ;; For split parsers like Markdown, the extra two fields are required:
         ;; 1. "split_parser" indicates that this language uses a parser split
         ;;    into multiple components.
         ;; 2. The directory path (e.g., "tree-sitter-markdown/src") points to
         ;;    the location of the parser source within the repository. Without
         ;;    these, treesit would not be able to find and compile the parser
         ;;    correctly.
         ;;
         ;; A split parser is a Tree-sitter parser that is divided into multiple
         ;; smaller parsers instead of being a single file or module. Each
         ;; smaller parser handles a part of the language, such as different
         ;; syntaxes or embedded languages, and together they form the complete
         ;; parser. This approach makes it easier to manage complex languages,
         ;; like Markdown, which can contain code blocks, inline formatting, and
         ;; other embedded languages. In Emacs, specifying "split_parser" and
         ;; the source directory tells treesit how to find and build all the
         ;; pieces correctly.
         "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
         "split_parser"
         "tree-sitter-markdown/src")
        ;; TODO: add markdown-inline to treesit auto
        (markdown-inline
         "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
         "split_parser"
         "tree-sitter-markdown-inline/src")
        ;; TODO: add php to treesit auto
        (php
         "https://github.com/tree-sitter/tree-sitter-php"
         "master"
         "php/src")
        (c . "https://github.com/tree-sitter/tree-sitter-c")
        (cpp . "https://github.com/tree-sitter/tree-sitter-cpp")
        (c-sharp . "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (commonlisp . "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
        (css . "https://github.com/tree-sitter/tree-sitter-css")
        (glsl . "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
        (go . "https://github.com/tree-sitter/tree-sitter-go")
        (julia . "https://github.com/tree-sitter/tree-sitter-julia")
        (make . "https://github.com/tree-sitter-grammars/tree-sitter-make")
        (ruby . "https://github.com/tree-sitter/tree-sitter-ruby")
        (rust . "https://github.com/tree-sitter/tree-sitter-rust")
        (scala . "https://github.com/tree-sitter/tree-sitter-scala")
        (toml . "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx . "https://github.com/tree-sitter/tree-sitter-typescript")
        (typescript . "https://github.com/tree-sitter/tree-sitter-typescript")
        (vue . "https://github.com/tree-sitter-grammars/tree-sitter-vue")

        (heex . "https://github.com/phoenixframework/tree-sitter-heex")
        (janet . "https://github.com/sogaiu/tree-sitter-janet-simple")
        (kotlin . "https://github.com/fwcd/tree-sitter-kotlin")
        (latex . "https://github.com/latex-lsp/tree-sitter-latex")
        (magik . "https://github.com/krn-robin/tree-sitter-magik")
        (nix . "https://github.com/nix-community/tree-sitter-nix")
        (nu . "https://github.com/nushell/tree-sitter-nu")
        (org . "https://github.com/milisims/tree-sitter-org")
        (perl . "https://github.com/ganezdragon/tree-sitter-perl")
        (proto . "https://github.com/mitchellh/tree-sitter-proto")
        (r . "https://github.com/r-lib/tree-sitter-r")
        (sql . "https://github.com/DerekStride/tree-sitter-sql")
        (surface . "https://github.com/connorlay/tree-sitter-surface")
        (typst . "https://github.com/uben0/tree-sitter-typst")
        (verilog . "https://github.com/gmlarumbe/tree-sitter-verilog")
        (vhdl . "https://github.com/alemuller/tree-sitter-vhdl")
        (wast . "https://github.com/wasm-lsp/tree-sitter-wasm")
        (wat . "https://github.com/wasm-lsp/tree-sitter-wasm")
        (wgsl . "https://github.com/mehmetoguzderin/tree-sitter-wgsl")
        (awk . "https://github.com/Beaglefoot/tree-sitter-awk")
        (bibtex . "https://github.com/latex-lsp/tree-sitter-bibtex")
        (blueprint . "https://github.com/huanie/tree-sitter-blueprint")
        (clojure . "https://github.com/sogaiu/tree-sitter-clojure")
        (cmake . "https://github.com/uyha/tree-sitter-cmake")
        (dart . "https://github.com/ast-grep/tree-sitter-dart")
        (elixir . "https://github.com/elixir-lang/tree-sitter-elixir")
        (gomod . "https://github.com/camdencheek/tree-sitter-go-mod")))

;;; Autosave

;;; Tools (ripgrep and fd)

(when lightemacs--ripgrep-executable
  (setq xref-search-program 'ripgrep))

;;; VC

(setq vc-git-print-log-follow t)

;;; Minibuffer

(setq minibuffer-default-prompt-format " [default %s]")
;; TODO use macro?
(add-hook 'lightemacs-on-first-input-hook #'minibuffer-depth-indicate-mode)

;;; Mode line

(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

;;; Frame

;; TODO use macro?
(add-hook 'lightemacs-after-init-hook #'window-divider-mode)

;;; auto-mode-alist

(nconc auto-mode-alist
       '(;; Gentoo ebuilds
         ("\\.ebuild\\'" . sh-mode)

         ;; Git
         ("/COMMIT_EDITMSG\\'" . diff-mode)
         ("/git-rebase-todo\\'" . diff-mode)

         ;; Arch Linux
         ("\\.install\\'" . sh-mode)  ; PKGBUILD
         ("\\.hook\\'" . conf-unix-mode)  ; /usr/share/libalpm/hooks/

         ;; Linux
         ("/etc/hosts\\'" . conf-unix-mode)
         ("/\\.ssh/known_hosts\\'" . conf-space-mode)

         ("/Eask\\'" . emacs-lisp-mode)
         ("/Cask\\'" . emacs-lisp-mode)))

;;; Patches

;; NOTE: This patch has been merged into the Emacs master branch but has not
;; been officially released yet.
;;
;; commit 4e37a99c20ad35a4e46ee9291c94940ec00fb77a
;; Author: James Cherti
;; Date:   2025-03-19 11:56:11 -0400
;;
;; ElDoc: Add more commands using 'eldoc-add-command-completions'
;;
;; Add more commands to 'eldoc-add-command-completions' to fix disappearing
;; ElDoc help in the minibuffer for the following cases:
;; - All modes: Added "comment-indent-new-line".
;; - All modes: Added "delete-char" for handling when the user presses delete.
;; - Python mode: Added "python-indent-dedent-line-backspace" for handling when
;; the user presses backspace.
;;
;; * lisp/emacs-lisp/eldoc.el (eldoc-remove-command-completions):
;; * lisp/progmodes/python.el (python-base-mode): Add more commands to
;; 'eldoc-add-command-completions'.
(with-eval-after-load 'eldoc
  (eldoc-add-command-completions
   "python-indent-dedent-line-backspace"
   "comment-indent-new-line"
   "delete-char"))

;; TODO: Send patch to Emacs
(with-eval-after-load 'eldoc
  (eldoc-add-command-completions
   "electric-pair-delete-pair"))

;; NOTE: This patch has been merged into the Emacs master branch but has not
;; been officially released yet.
;; commit cf6c365d5cf8ee5f460e59393e76b934a1a432b2
;; Author: James Cherti
;; Date:   2025-04-11 10:18:19 -0400
;;
;; Mark !%:.^~, as punctuation rather than symbol constituents
;;
;; In Bash, the characters !%:.^~, are not valid in variable names. In sh, they
;; are not permitted in either function or variable names. Treating them as
;; punctuation is convenient, as they are rarely used in function names and
;; never in variable names. Even among commands, their usage is uncommon. The
;; only character among these that is commonly seen in command names is '.',
;; although it is rarely used in function names.
;;
;; Marking these characters as punctuation, rather than symbol constituents,
;; enhances the accuracy of symbol detection.
;;
;; * lisp/progmodes/sh-script.el: Mark !%:.^~, as punctuation in the
;; sh-mode-syntax-table syntax table.
(defun lightemacs-default-settings--sh-syntax-table ()
  "Enhance `sh-mode' and `bash-ts-mode' syntax table."
  (modify-syntax-entry ?! ".")
  (modify-syntax-entry ?% ".")
  (modify-syntax-entry ?: ".")
  (modify-syntax-entry ?. ".")
  (modify-syntax-entry ?^ ".")
  (modify-syntax-entry ?~ ".")
  (modify-syntax-entry ?, "."))

(add-hook 'sh-mode-hook #'lightemacs-default-settings--sh-syntax-table)
(add-hook 'bash-ts-mode-hook #'lightemacs-default-settings--sh-syntax-table)

(with-eval-after-load 'sh-script
  ;; NOTE: This patch has been merged into the Emacs master branch but has not
  ;; been officially released yet.
  ;; commit 2ea0919550366babfea1de6468ef9e8b1857b478
  ;; Author: James Cherti
  ;; Date:   2024-11-24 12:09:33 -0500
  ;;
  ;; Support hyphen in Bash function names
  ;;
  ;; * lisp/progmodes/sh-script.el (sh-imenu-generic-expression): Add
  ;; hyphen to function-name regexp.
  (setq sh-imenu-generic-expression
        `((sh
           . ((nil
               ;; function FOO
               ;; function FOO()
               "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(?:()\\)?"
               1)
              ;; FOO()
              (nil
               "^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()"
               1)))
          ;; The difference between the Bash regular expression and the sh regular
          ;; expression is that Bash also allows hyphens (-) in function names.
          (bash
           . ((nil
               ;; function FOO
               ;; function FOO()
               "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?"
               1)
              ;; FOO()
              (nil
               "^\\s-*\\([[:alpha:]_][[:alnum:]_-]*\\)\\s-*()"
               1)))
          (mksh
           . ((nil
               ;; function FOO
               ;; function FOO()
               ,(rx bol (* (syntax whitespace)) "function" (+ (syntax whitespace))
                    (group (1+ (not (any "\0\t\n \"$&'();<=>\\`|#*?[]/"))))
                    (* (syntax whitespace)) (? "()"))
               1)
              (nil
               ;; FOO()
               ,(rx bol (* (syntax whitespace))
                    (group (1+ (not (any "\0\t\n \"$&'();<=>\\`|#*?[]/"))))
                    (* (syntax whitespace)) "()")
               1))))))

;;; Python

;; We're using `dtrt-indent'
(setq python-indent-guess-indent-offset nil)

;;; Provide

(provide 'le-default-settings)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-default-settings.el ends here
