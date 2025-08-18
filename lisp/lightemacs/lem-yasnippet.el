;;; lem-yasnippet.el --- lem-yasnippet -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The yasnippet package provides a template system that enhances text editing
;; by enabling users to define and use snippets, which are predefined templates
;; of code or text. The user triggers snippet expansion by pressing the Tab key
;; after typing an abbreviation, such as if. Upon pressing Tab, YASnippet
;; replaces the abbreviation with the corresponding full template, allowing the
;; user to fill in placeholders or fields within the expanded snippet.

;; URL: https://github.com/joaotavora/yasnippet

;;; Code:

(use-package yasnippet
  :commands (yas-minor-mode
             yas-minor-mode-on
             yas-global-mode
             yas-load-directory
             yas-reload-all
             yas-expand-snippet)
  :functions yas-filtered-definition

  :hook
  (after-init . yas-global-mode)

  :init
  (setq yas-verbosity 0)
  (setq yas-indent-line 'fixed)
  (setq yas-triggers-in-field nil)  ; Snippet inside of snippets
  (setq yas-also-auto-indent-first-line t)
  (setq yas-also-indent-empty-lines t)
  (setq yas-snippet-revival nil)  ; Sometimes, undo loops when this is activated
  (setq yas-wrap-around-region nil))

(provide 'lem-yasnippet)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lem-yasnippet.el ends here
