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

(defvar lightemacs-package-manager 'use-package
  "Specifies which package manager to use in Lightemacs.

Choices are:
- \='use-package: Use Emacs' built-in package.el and `use-package'.
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

(defvar lightemacs-optional-modules nil
  "Control which module load errors are ignored.
If t, ignore errors on all modules.
If nil, always fail when a module fails to load.
If a list of symbols, ignore errors for the modules in the list.")

;;; Provide

(provide 'le-core-defaults)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-core-defaults.el ends here
