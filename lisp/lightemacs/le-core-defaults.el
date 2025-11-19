;;; le-core-defaults.el --- le-core-defaults -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Default customizations.

;;; Code:

;;; Defaults

(setq load-prefer-newer t)
(setq package-enable-at-startup nil)

(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(setq package-archive-priorities '(("gnu"          . 90)
                                   ("nongnu"       . 80)
                                   ("melpa"        . 70)
                                   ("melpa-stable" . 50)))

(setq package-pinned-packages
      '((dired-filter                  . "melpa")
        (dired-hacks-utils             . "melpa")
        (elisp-autofmt                 . "melpa")
        (evil                          . "melpa")
        (evil-surround                 . "melpa")  ;; nongnu is outdated
        (flymake-yamllint              . "melpa")
        (flyspell-lazy                 . "melpa")  ;; Unmaintained
        (helpful                       . "melpa")
        (highlight-numbers             . "melpa")
        (jenkinsfile-mode              . "melpa")
        (vimrc-mode                    . "melpa")
        (vterm                         . "melpa")

        (bufferfile                    . "melpa")
        (dir-config                    . "melpa")
        (enhanced-evil-paredit         . "melpa")
        (flymake-ansible-lint          . "melpa")
        (flymake-bashate               . "melpa")
        (inhibit-mouse                 . "melpa")
        (quick-sdcv                    . "melpa")
        (stripspace                    . "melpa")
        (buffer-terminator             . "melpa")
        (outline-indent                . "melpa-stable")
        (vim-tab-bar                   . "melpa-stable")
        (tomorrow-night-deepblue-theme . "melpa-stable")
        (easysession                   . "melpa-stable")
        (compile-angel                 . "melpa-stable")
        (persist-text-scale            . "melpa-stable")

        (annalist                      . "melpa-stable")
        (ansible-doc                   . "melpa-stable")
        (apheleia                      . "melpa-stable")
        (basic-mode                    . "melpa-stable")
        (consult-dir                   . "melpa-stable")
        (corfu-prescient               . "melpa-stable")
        (dtrt-indent                   . "melpa-stable")
        (dumb-jump                     . "melpa-stable")
        (elisp-refs                    . "melpa-stable")
        (evil-collection               . "melpa-stable")
        (f                             . "melpa-stable")
        (flymake-quickdef              . "melpa-stable")
        (groovy-mode                   . "melpa-stable")
        (highlight-defined             . "melpa-stable")
        (markdown-toc                  . "melpa-stable")
        (org-appear                    . "melpa-stable")
        (package-lint-flymake          . "melpa-stable")
        (parent-mode                   . "melpa-stable")
        (php-mode                      . "melpa-stable")  ; nongnu is outdated
        (prescient                     . "melpa-stable")
        (s                             . "melpa-stable")
        (tocus                         . "melpa-stable")
        (treesit-auto                  . "melpa-stable")
        (vertico-prescient             . "melpa-stable")
        (visual-fill-column            . "melpa-stable")  ; nongnu is outdated
        (yasnippet-snippets            . "melpa-stable")   ; nongnu is outdated

        (ace-window                    . "gnu")
        (aggressive-indent             . "gnu")
        (avy                           . "gnu")
        (cape                          . "gnu")
        (compat                        . "gnu")
        (consult                       . "gnu")
        (corfu                         . "gnu")
        (csv-mode                      . "gnu")
        (dash                          . "gnu")
        (diff-hl                       . "gnu")
        (diminish                      . "gnu")
        (easy-escape                   . "gnu")
        (embark                        . "gnu")
        (embark-consult                . "gnu")
        (expand-region                 . "gnu")
        (gcmh                          . "gnu")
        (indent-bars                   . "gnu")
        (marginalia                    . "gnu")
        (modus-themes                  . "gnu")
        (orderless                     . "gnu")
        (org                           . "gnu")
        (rainbow-mode                  . "gnu")
        (transient                     . "gnu")
        (vertico                       . "gnu")
        (yasnippet                     . "gnu")
        (ztree                         . "gnu")

        (eat                           . "nongnu")
        (edit-indirect                 . "nongnu")
        (evil-visualstar               . "nongnu")
        (exec-path-from-shell          . "nongnu")
        (git-modes                     . "nongnu")
        (golden-ratio                  . "nongnu")
        (goto-chg                      . "nongnu")
        (gptel                         . "nongnu")
        (lua-mode                      . "nongnu")
        (magit                         . "nongnu")
        (markdown-mode                 . "nongnu")
        (package-lint                  . "nongnu")
        (page-break-lines              . "nongnu")
        (paredit                       . "nongnu")
        (popup                         . "nongnu")
        (rainbow-delimiters            . "nongnu")
        (undo-fu                       . "nongnu")
        (undo-fu-session               . "nongnu")
        (wgrep                         . "nongnu")
        (with-editor                   . "nongnu")
        (ws-butler                     . "nongnu")
        (yaml-mode                     . "nongnu")))

;; Minimal-emacs.d defaults
(setq minimal-emacs-frame-title-format "%b – Lightemacs")
(setq minimal-emacs-package-initialize-and-refresh nil)  ; Managed by Lightemacs
(setq minimal-emacs-gc-cons-percentage 0.1)
(setq minimal-emacs-gc-cons-threshold (* 40 1024 1024))
(setq minimal-emacs-gc-cons-threshold-restore-delay 3)
(setq minimal-emacs-ui-features '(context-menu tooltips))

;;; Variables

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

(defvar lightemacs-core-modules '(;; This loaded the default theme specified
                                  ;; in the `lightemacs-theme-name' variable.
                                  ;; The default theme tomorrow-night-deepblue
                                  ;; is a beautiful deep blue variant of the
                                  ;; Tomorrow Night theme, which is renowned
                                  ;; for its elegant color palette that is
                                  ;; pleasing to the eyes.
                                  le-theme

                                  ;; Miscellaneous Lightemacs default settings.
                                  le-default-settings)
  "Core modules that are enabled by default.
Do not modify this variable.")

(defvar lightemacs-excluded-packages nil
  "List of package symbols that should be excluded from initialization.
Each element must be a symbol naming a package that would otherwise be
initialized by Lightemacs. Packages listed here are skipped during the
initialization process. Only packages declared via the `lightemacs-use-package'
macro are affected by this variable.")

(defcustom lightemacs-verbose nil
  "Enable displaying verbose messages."
  :type 'boolean
  :group 'lightemacs)

(defcustom lightemacs-debug nil
  "Enable displaying debug messages."
  :type 'boolean
  :group 'lightemacs)

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

(defvar lightemacs-use-package-refresh-contents t
  "If non-nil, `lightemacs-use-package' may refresh package contents once.
Refresh package contents when `lightemacs-use-package-refresh-contents' is
non-nil and the package is not installed.")

(defvar lightemacs-package-manager 'use-package
  "Specifies which package manager to use in Lightemacs.

Choices are:
- \='use-package: Use Emacs' built-in package.el and `use-package'.
- \='straight: Use `straight.el' for package management.
- \='elpaca: Use `elpaca'.

This variable controls how the `lightemacs-use-package' macro handles
installation and configuration of packages.")

(defvar lightemacs-native-comp-excluded-cpus nil
  "Number of CPUs to reserve and not use for `native-compile'.
Set this to nil to disable this feature.")

(defvar lightemacs-load-compiled-init-files t
  "If non-nil, attempt to load byte-compiled .elc for init files.
This will enable Lightemacs to load byte-compiled or possibly native-compiled
init files for the following initialization files: init.el, pre-init.el,
post-init.el, pre-early-init.el, and post-early-init.el.")

(provide 'le-core-defaults)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-core-defaults.el ends here
