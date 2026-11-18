;;; le-core-pm-use-package.el --- le-core-pm-use-package -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Initialize the `use-package' package manager.

;;; Code:

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
        (evil-collection               . "melpa")
        (evil-surround                 . "melpa")  ;; nongnu is outdated
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
        (lsp-mode                      . "melpa-stable")
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
        (ws-butler                     . "nongnu")))

;; Initialize
(package-initialize)

(defvar lightemacs-use-package-init-refresh t)

(when (and (not (bound-and-true-p package-archive-contents))
           lightemacs-use-package-init-refresh)
  (package-refresh-contents))

;; Install use-package if necessary
(when (and (version< emacs-version "29.1")
           (not (package-installed-p 'use-package)))
  (package-install 'use-package))

(require 'use-package)

(provide 'le-core-pm-use-package)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-core-pm-use-package.el ends here
