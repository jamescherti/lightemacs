;;; le-org.el --- le-org -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configure `org-mode' and `org-agenda'.

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package org
  :commands (org-mode
             org-indent-mode
             org-set-tags-command
             org-version
             org-agenda
             org-capture
             org-schedule
             org-agenda-filter
             org-agenda-todo
             org-agenda-set-tags
             org-agenda-filter-remove-all
             org-agenda-goto)

  :mode
  ("\\.org\\'" . org-mode)

  :init
  ;; Log completion time; provides audit trail but adds automatic notes that may
  ;; clutter logs.
  (setq org-log-done 'time)

  ;; Enable modules on an opt-in basis to reduce initial Org load latency.
  (setq org-modules nil)

  ;; Enable Speed Keys at the beginning of a heading (press '?' to list
  ;; available keys).
  ;; (setq org-use-speed-commands t)

  ;; Ctrl-A/E moves to beginning/end of heading instead of line; improves
  ;; navigation.
  (setq org-special-ctrl-a/e t)

  (setq org-todo-keywords '((sequence "TODO" "MAYBE" "CANCELED" "DONE")))

  ;; Fall back file for org-capture.el.
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))

  ;; Prevent accidental edits inside hidden or folded text.
  (setq org-fold-catch-invisible-edits 'smart)
  (with-no-warnings
    ;; Obsolete version of: `org-fold-catch-invisible-edits'
    (setq org-catch-invisible-edits 'smart))

  ;; Disable automatic footnote renumbering and sorting on edit.
    (setq org-footnote-auto-adjust nil)

  ;; Do not insert empty lines between collapsed sections; makes folded view
  ;; denser but reduces visual separation between headings. This keeps your
  ;; files compact by removing empty lines between folded headings.
  (setq org-cycle-separator-lines 2)

  ;; Prevent marking a parent to do as done if its child tasks are incomplete;
  ;; ensures task consistency but may slow task completion when some subtasks
  ;; are still pending.
  (setq org-enforce-todo-dependencies t)

  ;; Turn on org-indent-mode on startup.
  (setq org-startup-indented t)

  (with-eval-after-load 'org-indent
    (setq org-indent-indentation-per-level 1))

  ;; Hide leading stars
  (setq org-hide-leading-stars t)

  ;; Prevent inserting blank lines before new headings and list items.
  (setq org-blank-before-new-entry '((heading)
                                     (plain-list-item)))

  ;; Require braces for sub/superscripts (e.g., _{sub} or ^{super}).
  (setq org-use-sub-superscripts '{})

  ;; Render Org entities (e.g., \tilde) as UTF-8 characters.
  (setq org-pretty-entities t)

  ;; Pretty entity display includes formatting sub/superscripts
  (setq org-pretty-entities-include-sub-superscripts t)

  ;; Fontify code in code blocks
  (setq org-src-fontify-natively t)

  ;; Disable opening links on left click by preventing accidental clicks on
  ;; inline images in Org buffers from opening them in new buffers.
  (setq org-mouse-1-follows-link nil)

  (setq org-export-backends '(html texinfo md))

  (setq org-src-content-indentation 0)
  (with-no-warnings
    ;; The `with-no-warnings' macro maintains compatibility with older Org
    ;; versions where the variable was named `org-edit-src-content-indentation'.
    (setq org-edit-src-content-indentation 0))

  ;; Prevent table cells starting with "=" from auto-evaluating as formulas on
  ;; TAB. Avoids unintended evaluation when using "=" for verbatim formatting.
  ;; (Formulas can still be manually evaluated with C-c =.)
  (setq org-table-formula-evaluate-inline nil))

(provide 'le-org)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-org.el ends here
