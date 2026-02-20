;;; le-default-settings.el --- le-default-settings -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Miscellaneous Lightemacs default settings.

;;; Code:

;;; Require

(require 'lightemacs-module)

;; Load `lightemacs--ripgrep-executable' and `lightemacs--fdfind-executable'
(require 'le-core-cli-tools)

;;; Misc

(setq auto-revert-verbose (not lightemacs-reduce-messages))
(setq next-error-verbose (not lightemacs-reduce-messages))

;; TODO: move to m.e.?
(setq eldoc-documentation-strategy #'eldoc-documentation-compose)

(setq warning-minimum-level (if init-file-debug :warning :error))

;; Alternative: (setq-default display-fill-column-indicator-character ?┊)
(setq-default display-fill-column-indicator-character ?\N{U+2502})

;; By default, Emacs stores authinfo credentials as plain text in the home
;; directory, which exposes authentication data to any process or user with file
;; access. The configuration below enables GPG encryption for the authinfo file
;; so that credentials remain encrypted at rest and are only decrypted on
;; demand.
;;
;; The auth-sources file follows a standard netrc-like format where each entry
;; defines authentication parameters for a specific host:
;;
;; machine github.com login username password your-personal-access-token
;;
;; When a package requests credentials, Emacs invokes gpg-agent to decrypt the
;; encrypted authinfo file. If the associated private key is locked, the
;; configured pinentry program prompts for the passphrase. The decrypted content
;; is parsed in memory and is never persisted to disk in plain text form.
;;
;; The configuration below prioritizes an encrypted authinfo file stored in
;; `lightemacs-var-directory', with a fallback to ~/.authinfo.gpg if present.
(setq auth-sources (list
                    (file-name-concat lightemacs-var-directory "authinfo.gpg")
                    "~/.authinfo.gpg"))

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
       '(("\\.ebuild\\'" . sh-mode) ; Gentoo ebuilds

         ;; Linux
         ("\\.service\\'" . conf-mode) ; PKGBUILD / systemd service
         ("\\.socket\\'" . conf-mode)
         ("\\.timer\\'" . conf-mode)
         ("\\.target\\'" . conf-mode)
         ("\\.mount\\'" . conf-mode)
         ("\\.path\\'" . conf-mode)
         ("\\.info\\(\\.gz\\)?\\'" . Info-on-current-buffer)
         ("/hosts\\'" . conf-space-mode)
         ("/\\.ssh/known_hosts\\'" . conf-space-mode)

         ;; Arch Linux
         ("\\.install\\'" . sh-mode)     ; PKGBUILD
         ("\\.hook\\'" . conf-mode) ; /usr/share/libalpm/hooks/

         ;; Python
         ("/\\.pylintrc\\'" . conf-mode)

         ("/Eask\\'" . emacs-lisp-mode)
         ("/Cask\\'" . emacs-lisp-mode)))

;;; proced

;; You can launch proced
(add-hook 'proced-mode-hook 'proced-toggle-auto-update)
(setq proced-tree-flag t)
(setq proced-auto-update-flag 'visible)
(setq proced-enable-color-flag t)
(setq proced-auto-update-interval 1)
(setq proced-filter 'user) ; Change interactively with `s'

;;; Patches

(with-eval-after-load 'eldoc
  ;; NOTE: This patch has been merged into the Emacs master branch but has not
  ;; been officially released yet.
  ;;
  ;; commit 4e37a99c20ad35a4e46ee9291c94940ec00fb77a
  ;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(defun lightemacs-default-settings--sh-syntax-table ()
  "Enhance `sh-mode' and `bash-ts-mode' syntax table."
  ;; NOTE: This patch has been submitted to the Emacs developers.
  ;; sh-script: Mark + and * as punctuation rather than a symbol constituent
  ;;
  ;; In Bash, the characters + and * are not valid in variable names. In sh,
  ;; they are also disallowed in both function and variable names. Treating
  ;; them as punctuation is appropriate, as they most commonly appear as
  ;; operators.
  ;;
  ;; Examples:
  ;; * Arithmetic: $((var1*var2))
  ;; * Compound assignment: list_name+=("item")
  ;;
  ;; * lisp/progmodes/sh-script.el: Mark + and * as punctuation in the
  ;; sh-mode-syntax-table syntax table.
  (modify-syntax-entry ?+ ".")
  (modify-syntax-entry ?* ".")

  ;; NOTE: This patch has been merged into the Emacs master branch but has not
  ;; been officially released yet.
  ;; commit cf6c365d5cf8ee5f460e59393e76b934a1a432b2
  ;; Author: James Cherti <https://www.jamescherti.com/contact/>
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
  ;; Author: James Cherti <https://www.jamescherti.com/contact/>
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
