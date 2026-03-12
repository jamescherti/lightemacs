;;; le-core-defaults.el --- le-core-defaults -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Default customizations.

;;; Code:

;;; Variables

(defvar lightemacs-user-directory user-emacs-directory
  "Directory beneath Lightemacs files are placed.
Note that this should end with a directory separator.")

(defvar lightemacs-local-directory
  (expand-file-name "lisp/local/" lightemacs-user-directory))

(defvar lightemacs-local-modules-directory
  (expand-file-name "modules/" lightemacs-local-directory))

(defvar lightemacs-core-directory
  (expand-file-name "lisp/lightemacs/" lightemacs-user-directory))

(defvar lightemacs-modules-directory
  (expand-file-name "modules/" lightemacs-core-directory))

(defvar lightemacs-var-directory
  (expand-file-name "var/" lightemacs-user-directory))

(defvar lightemacs-modules '(;; Default modules
                             le-flavor-essential

                             ;; Vim keybindings (DISABLED)
                             ;; --------------------------
                             ;; le-group-evil
                             )
  "Modules that are enabled by default.

Lightemacs provides a range of modules that can be selectively enabled or
disabled according to your preferences, with all modules ensuring packages are
loaded only when needed, enabling exceptionally fast, deferred startup.")

(defvar lightemacs-core-modules '()
  "Core modules that are enabled by default.
Do not modify this variable.")

(defvar lightemacs-excluded-packages nil
  "List of package symbols that should be excluded from initialization.
Each element must be a symbol naming a package that would otherwise be
initialized by Lightemacs. Packages listed here are skipped during the
initialization process. Only packages declared via the `lightemacs-module'
macro are affected by this variable.")

(defvar lightemacs-verbose nil
  "Enable displaying verbose messages.")

(defvar lightemacs-debug nil
  "Enable displaying debug messages.")

(defvar lightemacs-reduce-messages nil
  "When non-nil, reduce the verbosity of various Emacs packages.")

(defvar lightemacs-ellipsis " ▼"
  "String used to indicate folded sections in Org-mode and Outline-mode.
This ellipsis appears at the end of a heading or section that has been
collapsed. It provides a visual cue that more content is hidden. You can
customize this variable to use a different character or string (such as '…',
'▶', or other Unicode symbols) to match your visual preference or theme. This
variable is buffer-local in Org-mode and Outline-mode, affecting only the
display of folded text.")

(defvar lightemacs-cycle t
  "If non-nil, enables cycling through candidates in supported plugins.
This enabled or disable cycling in plugins such as Vertico and Consult.
When nil, cycling is disabled, so selection stops at the first or last candidate
instead of wrapping around.")

(defvar lightemacs-package-manager nil
  "Specifies which package manager to use in Lightemacs.

Choices are:
- \='builtin-package: Use Emacs' built-in package.el and `use-package'.
- \='straight: Use `straight.el' for package management.
- \='elpaca: Use `elpaca'.

This variable controls how the `lightemacs-module' macro handles
installation and configuration of packages.")

(defvar lightemacs-native-comp-excluded-cpus nil
  "Number of CPUs to reserve and not use for `native-compile'.
Set this to nil to disable this feature.")

(defvar lightemacs-load-compiled-init-files t
  "If non-nil, attempt to load byte-compiled .elc for init files.
This will enable Lightemacs to load byte-compiled or possibly native-compiled
init files for the following initialization files: init.el, pre-init.el,
post-init.el, pre-early-init.el, and post-early-init.el.")

;;; Hook: `lightemacs-after-init-hook'

(defvar lightemacs-after-init-hook nil
  "Hook run after LightEmacs initialization is complete.
If `lightemacs-package-manager' is elpaca, this hook runs after
`elpaca-after-init-hook'. Otherwise, it runs after `after-init-hook', similar to
Emacs standard behavior.")

(defvar lightemacs-emacs-startup-hook nil
  "Hook run after Emacs startup processes are complete.
If `lightemacs-package-manager' is elpaca, this hook runs after
`elpaca-after-init-hook'. Otherwise, it runs after `emacs-startup-hook', similar
to Emacs standard behavior.")

;;; Packages

(defvar lightemacs--le-core-default-done nil)

(unless lightemacs--le-core-default-done
  (setq lightemacs--le-core-default-done t)
  (setq package-archive-priorities '(("gnu"          . 90)
                                     ("nongnu"       . 80)
                                     ("melpa"        . 70)
                                     ("melpa-stable" . 50)))
  (setq package-archives
        '(("melpa"        . "https://melpa.org/packages/")
          ("gnu"          . "https://elpa.gnu.org/packages/")
          ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")))

  (setq package-pinned-packages
        '((dired-filter                  . "melpa")
          (dired-hacks-utils             . "melpa")
          (elisp-autofmt                 . "melpa")
          (evil                          . "melpa")
          (evil-surround                 . "melpa")  ;; nongnu is outdated
          (evil-collection               . "melpa")
          (flymake-yamllint              . "melpa")
          (flyspell-lazy                 . "melpa")  ;; Unmaintained
          (helpful                       . "melpa")
          (highlight-numbers             . "melpa")
          (jenkinsfile-mode              . "melpa")
          (vimrc-mode                    . "melpa")
          (vterm                         . "melpa")
          (undo-fu                       . "melpa") ; nongnu is outdated
          (undo-fu-session               . "melpa") ; nongnu is outdated

          (flymake-ansible-lint          . "melpa")
          (flymake-bashate               . "melpa")
          (diff-hl                       . "melpa") ; gnu is outdated
          (yasnippet                     . "melpa")  ; gnu is outdated
          (elisp-refs                    . "melpa")
          (aggressive-indent             . "melpa")
          (package-lint                  . "melpa")
          (package-lint-flymake          . "melpa")
          (yaml-mode                     . "melpa")
          (page-break-lines              . "melpa") ; nongnu is outdated
          (edit-indirect                 . "melpa") ; nongnu is outdated
          (markdown-mode                 . "melpa") ; nongnu is outdated
          (lsp-mode                      . "melpa")
          (lsp-ui                        . "melpa")
          (modus-themes                  . "melpa") ; gnu is outdated
          (ace-window                    . "melpa") ; gnu is outdated
          (avy                           . "melpa") ; gnu is outdated

          (annalist                      . "melpa-stable")
          (ansible-doc                   . "melpa-stable")
          (apheleia                      . "melpa-stable")
          (basic-mode                    . "melpa-stable")
          (consult-dir                   . "melpa-stable")
          (dtrt-indent                   . "melpa-stable")
          (dumb-jump                     . "melpa-stable")
          (f                             . "melpa-stable")
          (flymake-quickdef              . "melpa-stable")
          (groovy-mode                   . "melpa-stable")
          (highlight-defined             . "melpa-stable")
          (markdown-toc                  . "melpa-stable")
          (org-appear                    . "melpa-stable")
          (parent-mode                   . "melpa-stable")
          (php-mode                      . "melpa-stable")  ; nongnu is outdated
          (prescient                     . "melpa-stable")
          (vertico-prescient             . "melpa-stable")
          (corfu-prescient               . "melpa-stable")
          (s                             . "melpa-stable")
          (focus                         . "melpa-stable")
          (treesit-auto                  . "melpa-stable")
          (visual-fill-column            . "melpa-stable") ; nongnu is outdated
          (yasnippet-snippets            . "melpa-stable") ; nongnu is outdated

          (corfu                         . "gnu")
          (cape                          . "gnu")
          (compat                        . "gnu")
          (csv-mode                      . "gnu")
          (dash                          . "gnu")
          (diminish                      . "gnu")
          (easy-escape                   . "gnu")
          (expand-region                 . "gnu")
          (gcmh                          . "gnu")
          (indent-bars                   . "gnu")
          (embark                        . "gnu")
          (embark-consult                . "gnu")
          (consult                       . "gnu")
          (vertico                       . "gnu")
          (marginalia                    . "gnu")
          (orderless                     . "gnu")
          (org                           . "gnu")
          (rainbow-mode                  . "gnu")
          (transient                     . "gnu")
          (ztree                         . "gnu")

          (eat                           . "nongnu")
          (evil-visualstar               . "nongnu")
          (exec-path-from-shell          . "nongnu")
          (git-modes                     . "nongnu")
          (golden-ratio                  . "nongnu")
          (goto-chg                      . "nongnu")
          (gptel                         . "nongnu")
          (lua-mode                      . "nongnu")
          (magit                         . "nongnu")
          (paredit                       . "nongnu")
          (popup                         . "nongnu")
          (rainbow-delimiters            . "nongnu")
          (wgrep                         . "nongnu")
          (with-editor                   . "nongnu")
          (ws-butler                     . "nongnu"))))

;;; Provide

(provide 'le-core-defaults)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-core-defaults.el ends here
