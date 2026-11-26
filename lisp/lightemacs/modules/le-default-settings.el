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

(require 'lightemacs)

;;; Variables

;;; Default keybindingsare easily shadowed by major modes, minor modes, or
;; keymap managers like Evil mode. By injecting our custom keymap directly into
;; emulation-mode-map-alists, we place these specific bindings at the absolute
;; highest precedence level possible within the Emacs input system.
;;
;; This ensures that core shortcuts, such as text scaling, will always execute
;; reliably regardless of the current buffer's mode or the active Evil state. It
;; prevents the frustration of having newly installed packages silently hijack
;; your preferred shortcuts, ensuring a consistently predictable editing
;; experience.

;; Define a dummy variable that is always true so the map stays active
(defvar lightemacs-keymap-override-mode t
  "Always non-nil to ensure the override keymap remains active.")

(defvar lightemacs-keymap-override-map (make-sparse-keymap)
  "Keymap with highest precedence for text scaling.")

;; Create the alist required by emulation-mode-map-alists
(defvar lightemacs--keymap-override-alist
  `((lightemacs-keymap-override-mode . ,lightemacs-keymap-override-map))
  "Alist to be added to `emulation-mode-map-alists'.")

(unless noninteractive
  (dolist (binding '(("C--" . text-scale-decrease)
                     ("C-+" . text-scale-increase)
                     ("C-0" . text-scale-adjust)))
    (let ((key (car binding))
          (cmd (cdr binding)))
      (if (fboundp 'keymap-set)
          (keymap-set lightemacs-keymap-override-map key cmd)
        (define-key lightemacs-keymap-override-map (kbd key) cmd))))

  ;; Insert the alist into the core Emacs input resolution system
  (add-to-list 'emulation-mode-map-alists 'lightemacs--keymap-override-alist))

;;; Misc

(setq package-native-compile t)

(setq enable-local-variables :safe
      next-error-verbose (not lightemacs-reduce-messages)
      warning-minimum-level (if init-file-debug :warning :error))

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

(defvar lightemacs-auto-calculate-read-process-output-max t)

;; Increase single chunk bytes to read from subprocess
(when lightemacs-auto-calculate-read-process-output-max
  (setq read-process-output-max
        (or (and (eq system-type 'gnu/linux)
                 (condition-case nil
                     ;; On GNU/Linux systems, the value should not exceed
                     ;; /proc/sys/fs/pipe-max-size
                     (with-temp-buffer
                       (insert-file-contents "/proc/sys/fs/pipe-max-size")
                       (string-to-number (buffer-string)))
                   (error nil)))
            (* 1024 1024))))

;;; treesit

(setq treesit-font-lock-level 4) ; Max: 4

;;; Autosave

(with-eval-after-load 'xref
  (if (fboundp 'lightemacs--load-cli-tools)
      (lightemacs--load-cli-tools)
    (error "Undefined: lightemacs--load-cli-tools"))

  (when lightemacs--ripgrep-executable
    (setq xref-search-program 'ripgrep)))

;;; VC

(setq vc-git-print-log-follow t)

;;; UI

(unless noninteractive
  (setq minibuffer-default-prompt-format " [default %s]"
        line-number-mode t
        column-number-mode t
        mode-line-position-column-line-format '("%l:%C"))
  (add-hook 'lightemacs-on-first-input-hook #'minibuffer-depth-indicate-mode))

;;; auto-mode-alist

(nconc auto-mode-alist
       '(("\\.ebuild\\'" . sh-mode) ; Gentoo ebuilds

         ("/LICENSE\\'" . text-mode)
         ("rc\\'" . conf-mode)

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

;;; `proced'

(unless noninteractive
  (setq proced-tree-flag t
        proced-auto-update-flag 'visible
        proced-enable-color-flag t
        proced-filter 'user ; Change interactively with `s'
        proced-auto-update-interval 1))

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
  ;; - Python mode: Added "python-indent-dedent-line-backspace" for handling
  ;;   when the user presses backspace.
  ;;
  ;; * lisp/emacs-lisp/eldoc.el (eldoc-remove-command-completions):
  ;; * lisp/progmodes/python.el (python-base-mode): Add more commands to
  ;; 'eldoc-add-command-completions'.
  (when (fboundp 'eldoc-add-command-completions)
    (eldoc-add-command-completions
     "python-indent-dedent-line-backspace"
     "comment-indent-new-line"
     "delete-char"
     ;; TODO: Send patch to Emacs
     "electric-pair-delete-pair")))

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
  ;; In Bash, the characters !%:.^~, are not valid in variable names. In sh,
  ;; they are not permitted in either function or variable names. Treating them
  ;; as punctuation is convenient, as they are rarely used in function names and
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
          ;; The difference between the Bash regular expression and the sh
          ;; regular expression is that Bash also allows hyphens (-) in function
          ;; names.
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

;;; Empty the minibuffer after `y-or-n-p'

(defun lightemacs--empty-minibuffer (orig-fun &rest args)
  "Empty the minibuffer after the original function has finished executing.
This advice is intended to be used with `y-or-n-p' to clear the minibuffer after
the user has responded to a yes-or-no prompt.
ORIG-FUN is the original function being advised.
ARGS are the arguments passed to the original function."
  (unwind-protect
      (apply orig-fun args)
    (message nil)))

(unless noninteractive
  (advice-add 'yes-or-no-p :around 'lightemacs--empty-minibuffer)
  (advice-add 'y-or-n-p :around 'lightemacs--empty-minibuffer))

;;; `term' and `ansi-term'

;; `term' and `ansi-term' are built-in Emacs terminal emulators that allow
;; interaction with a shell as if it were running in a standalone terminal
;; application. The `term' mode provides a general terminal interface with two
;; submodes, line mode for typical Emacs editing and char mode for direct
;; terminal input, enabling a balance between editor features and raw terminal
;; behavior. The `ansi-term' mode is a specialized variant that uses a real
;; terminal program such as `/bin/bash' and offers more accurate handling of
;; ANSI escape sequences, resulting in better compatibility with interactive
;; command-line programs like `top', `vim', or `htop'.

(defvar lightemacs-term-optimize t
  "Non-nil means apply performance optimizations to `term-mode' buffers.
When non-nil, `lightemacs--lightemacs-process-buffers-optimize' runs in
`term-mode-hook'.")

(add-hook 'term-mode-hook #'lightemacs--process-buffers-disable-kill-prompt)

(when lightemacs-term-optimize
  (add-hook 'term-mode-hook #'lightemacs--lightemacs-process-buffers-optimize t))

;;; Disable terminal in some global modes

(setq whitespace-global-modes `(not ,@lightemacs-process-buffers-modes))

(with-eval-after-load 'display-fill-column-indicator
  (add-to-list 'global-display-fill-column-indicator-modes
               (cons 'not lightemacs-process-buffers-modes)))

(with-eval-after-load 'loaddefs
  (add-to-list 'global-completion-preview-modes
               (cons 'not lightemacs-process-buffers-modes)))

;;; Python

;; By default, Emacs `python-mode' and `python-ts-mode' set tab-width to 8. PEP
;; 8 specifies a standard indentation level of 4 spaces for Python code. Setting
;; `tab-width' to `python-indent-offset' ensures correct visual alignment and
;; consistency with standard Python indentation rules.
;; TODO Send a patch to Emacs
(defun lightemacs--python-tab-width-setup ()
  "Set the buffer-local `tab-width` to 4 for Python files.
This function takes no arguments."
  (when (boundp 'python-indent-offset)
    (setq tab-width python-indent-offset))
  ;; PEP 8 - Style Guide for Python Code:
  ;; https://peps.python.org/pep-0008/#maximum-line-length
  (setq-local fill-column 79))

(add-hook 'python-mode-hook #'lightemacs--python-tab-width-setup)
(add-hook 'python-ts-mode-hook #'lightemacs--python-tab-width-setup)

;;; hl-line

(defun lightemacs-process-buffers--buffer-p (buf)
  "Return non-nil if BUF is a terminal emulator buffer."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (apply #'derived-mode-p lightemacs-process-buffers-modes))))

;; Configure `global-hl-line-buffers' with a predicate evaluated by
;; `buffer-match-p' to control where global line highlighting is active.
(setq global-hl-line-buffers
      '(not (or
             ;; `cursor-face-highlight-mode': Excludes buffers using specialized
             ;; face highlighting to prevent visual overlap and conflicts.
             (lambda (b) (buffer-local-value 'cursor-face-highlight-mode b))
             ;; Leading space names: Excludes internal or temporary buffers
             ;; starting with a space character.
             (lambda (b) (string-match-p "\\` " (buffer-name b)))
             ;; `minibufferp': Prevents line highlighting inside the minibuffer.
             minibufferp
             ;; `lightemacs-process-buffers--buffer-p': Excludes terminal emulators
             ;; defined in lightemacs-process-buffers-modes (vterm, eat, term, and
             ;; ghostel). Terminal buffers manage their own display and cursor
             ;; rendering; disabling highlighting prevents visual artifacts and
             ;; performance overhead during rapid output updates.
             lightemacs-process-buffers--buffer-p)))

;;; Flymake

;; Make `next-error' and `previous-error' behavior predictable.
;; When multiple compilation or search buffers exist, Emacs can sometimes guess
;; the wrong one. This setting forces Emacs to prioritize the current buffer or
;; the most recently generated unnavigated error buffer.
(setq-default next-error-find-buffer-function 'next-error-buffer-unnavigated-current)

;;; Flyspell

(setq ispell-quietly lightemacs-reduce-messages)

(setq
 ;; Performance tweak: Only check words that are actively typed or edited,
 ;; rather than checking words as the point moves across them. This reduces
 ;; spell-checking performance cost when navigating existing text. The tradeoff
 ;; is that pre-existing misspellings remain undetected until the text is
 ;; modified or explicitly checked.
 flyspell-check-changes t

 ;; Performance tweak: Force Flyspell to run spell-checks using idle timers
 ;; rather than blocking via sit-for. This prevents brief input delays while
 ;; typing.
 flyspell-delay-use-timer t)

;;; Remove stale early-init.elc

;; At shutdown, remove early-init.elc if its source is newer so the next session
;; picks up the edits. A current early-init.elc is kept.
;;
;; Tools that byte-compile an entire Emacs configuration can leave
;; early-init.elc behind when the user edits early-init.el. By default, Emacs
;; can load that older bytecode even when the source is newer. Setting
;; `load-prefer-newer' inside early-init.el is too late to affect this choice
;; because Emacs selects the file before evaluating its contents.
;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2026-06/msg01711.html
;;
;; For example, if you edit early-init.el during an Emacs session, the next
;; startup can still run the older early-init.elc. Emacs chooses which file to
;; load before it evaluates load-prefer-newer or any stale-file check inside it.
;; This is useful when the .elc file predates the early-init.el check, because
;; it has no code to detect your edit. When registered on kill-emacs-hook, this
;; function removes the .elc at normal shutdown only if the source is newer. The
;; next session then loads your edited source, while a current .elc stays
;; available for compiled startup.

(defun lightemacs--delete-stale-early-init-elc ()
  "Delete early-init.elc when early-init.el has a newer modification time."
  (let* ((early-init-el (expand-file-name "early-init.el"
                                          lightemacs-user-directory))
         (early-init-elc (concat early-init-el "c")))
    (condition-case err
        (when
            (and (file-regular-p early-init-elc)
                 (file-newer-than-file-p early-init-el early-init-elc))
          (delete-file early-init-elc)
          (message "[Lightemacs] Deleted stale %s" early-init-elc))
      (file-error
       (message "[Lightemacs] Could not clean up %s: %s"
                early-init-elc (error-message-string err))))))

(add-hook 'kill-emacs-hook #'lightemacs--delete-stale-early-init-elc)

;;; Update the outline ellipsis

(defun le-outline--set-buffer-local-ellipsis (ellipsis)
  "Apply the ellipsis ELLIPSIS to outline mode locally to a buffer."
  (let* ((display-table (or buffer-display-table (make-display-table)))
         (face-offset (* (face-id 'shadow) (ash 1 22)))
         (value (vconcat (mapcar (lambda (c) (+ face-offset c))
                                 ;; Trim trailing whitespace after the
                                 ;; ellipsis, as it can be misleading when the
                                 ;; line is not truncated. Wrapping may
                                 ;; display only the space after the ellipsis
                                 ;; on the next line, creating the illusion of
                                 ;; a new line. Deleting that apparent "new
                                 ;; line" may delete the entire logical line
                                 ;; containing the ellipsis.
                                 (string-trim-right ellipsis)))))
    (set-display-table-slot display-table 'selective-display value)
    (setq buffer-display-table display-table)))

(defun le-outline--update-ellipsis ()
  "Update `outline-mode' ellipsis."
  ;; Do not modify the ellipsis in `org-mode' or `outline-indent-minor-mode' as
  ;; it is managed internally by their respective modes
  (unless (or (bound-and-true-p outline-indent-minor-mode)
              (derived-mode-p 'org-mode))
    (le-outline--set-buffer-local-ellipsis lightemacs-ellipsis)))

(add-hook 'outline-minor-mode-hook #'le-outline--update-ellipsis)
(add-hook 'outline-mode-hook #'le-outline--update-ellipsis)

;;; Bug fix: This bug fix was sent to the Emacs developers

;; Issue report:
;;   FYI: Merged into Emacs 32: 8decb653ffd8b046ffcccb5e9aa2e8a820ce6ab0
;;
;;   [PATCH] outline-end-of-subtree misses final newline when at end of buffer
;;   When using outline-mode (or outline-minor-mode), making a
;;   subtree invisible that is located at the very end of
;;   the buffer causes the final newline to be incorrectly
;;   flagged as invisible.
;;
;;   This occurs because outline-end-of-subtree relies on
;;   (and (bolp) (not (eolp))) to decide whether to call
;;   outline--end-of-previous. At the end of a buffer terminating
;;   with a newline, point is at an empty line, making
;;   (not (eolp)) evaluate to nil, which makes the function skips
;;   stepping back over the final newline before the visibility
;;   is flagged. This results in the ellipsis swallowing the
;;   end-of-buffer marker.
;;
;;   Steps to reproduce:
;;   1. emacs -Q
;;   2. Set require-final-newline to t
;;   2. Switch to *scratch* and evaluate: (outline-mode)
;;   3. Insert the following text:
;;   * Heading 1
;;   * Heading 2
;;   4. Move point to "Heading 2" and type: M-x outline-hide-subtree
;;   5. Observe that the end of the buffer is swallowed into the
;;   ellipsis.
;;
;;   Expected behavior:
;;   The invisible region should terminate before the final
;;   newline, leaving the end-of-buffer reachable and visible
;;   outside the hidden subtree.
;;
;;   The attached patch fixes this by explicitly checking for
;;   (eobp) before deciding to step backward.

(defun le-outline--advice-backtrack-whitespace (&rest _args)
  "Backtrack over whitespace-only lines to prevent them from being folded.
This advice ensures that empty lines between headings or at the end of
the buffer remain visible when a fold is applied."
  (if (eobp)
      (if (bolp)
          (forward-char -1))))

;; Merged into Emacs 32: 8decb653ffd8b046ffcccb5e9aa2e8a820ce6ab0
(when (< emacs-major-version 32)
  (with-eval-after-load 'outline
    (advice-add 'outline-end-of-subtree :after
                #'le-outline--advice-backtrack-whitespace)))

;;; ibuffer

;; If non-nil, then forward and backwards movement commands cycle.
(setq ibuffer-movement-cycle lightemacs-cycle)

(with-eval-after-load 'ibuffer
  (setq ibuffer-filter-group-name-face '(:inherit (success bold))))

;;; Elisp

;; TODO Setting use-package-enable-imenu-support to t doesn't work. Fix it.
;; (with-eval-after-load 'use-package-core
;;   (with-eval-after-load 'lisp-mode
;;     (when (and (boundp 'use-package-form-regexp-eval)
;;                (boundp 'lisp-imenu-generic-expression))
;;       (add-to-list 'lisp-imenu-generic-expression
;;                    (list "Packages" (eval use-package-form-regexp-eval) 2)))))

(defcustom lightemacs-use-package-form-regexp-eval
  `(concat ,(eval-when-compile
              (concat "^\\s-*("
                      (regexp-opt '("use-package" "require") t)
                      "\\s-+\\("))
           (or (bound-and-true-p lisp-mode-symbol-regexp)
               "\\(?:\\sw\\|\\s_\\|\\\\.\\)+") "\\)")
  "Sexp providing regexp for finding `use-package' forms in user files.
This is used by `use-package-jump-to-package-form' and
`use-package-enable-imenu-support'."
  :type 'sexp
  :group 'use-package
  :version "29.1")

(defvar lightemacs-use-package-enable-imenu-support t
  "When non-nil, add a Packages section to Imenu in Emacs Lisp buffers.")

(defun my-elisp-imenu-add-packages ()
  "Index package declarations when Lightemacs Imenu support is enabled."
  (when lightemacs-use-package-enable-imenu-support
    (add-to-list 'imenu-generic-expression
                 (list "Packages"
                       (eval lightemacs-use-package-form-regexp-eval t) 2))
    (setq imenu--index-alist nil)))

(add-hook 'emacs-lisp-mode-hook #'my-elisp-imenu-add-packages 90)

;;; Provide

(provide 'le-default-settings)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-default-settings.el ends here
