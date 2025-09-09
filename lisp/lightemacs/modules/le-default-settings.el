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

(with-eval-after-load 'eldoc
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
  (eldoc-add-command-completions
   "python-indent-dedent-line-backspace"
   "comment-indent-new-line"
   "delete-char")

  ;; TODO: Send patch to Emacs
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
(defvar python-indent-offset 4)

;;; Provide

(provide 'le-default-settings)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-default-settings.el ends here
