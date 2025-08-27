;;; le-org.el --- le-org -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configure `org-mode' and `org-agenda'.

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(eval-and-compile
  (require 'use-package))

(lightemacs-use-package
  org
  :ensure nil
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
  ;; Disable saving a bookmark when capturing; avoids cluttering the bookmark
  ;; list but loses the ability to quickly return to the capture location.
  (setq org-capture-bookmark nil)

  ;; Define refile targets up to maxlevel in the current file and agenda files;
  ;; allows flexible refiling but may slow completion in very large files and
  ;; requires remembering hierarchy.
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))

  ;; Allow creating new parent nodes when refiling, but ask for confirmation;
  ;; provides flexibility in organization but adds an extra prompt that may
  ;; interrupt workflow.
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Use the full outline path when refiling; makes it easier to select the
  ;; correct target but requires remembering or seeing the full path.
  (setq org-refile-use-outline-path t)

  ;; Ctrl-A/E moves to beginning/end of heading instead of line; improves
  ;; navigation.
  (setq org-special-ctrl-a/e t)

  ;; Indent text according to heading level; makes outline visually clearer but
  ;; can misalign code blocks or tables.
  (setq org-startup-indented t)

  ;; Hide markers like * / _ = ~; cleaner view but markers are not visible for
  ;; editing emphasis.
  (setq org-hide-emphasis-markers t)

  ;; Do not truncate long lines on startup; easier reading for wide text but may
  ;; wrap long lines visually.
  (setq org-startup-truncated t)

  ;; Color DONE headlines; quickly identifies completed tasks
  (setq org-fontify-done-headline t)

  ;; Color TODO headlines; improves task visibility
  (setq org-fontify-todo-headline t)

  ;; Fontify the whole heading line; improves readability but may affect
  ;; alignment of inline content.
  ;; - Benefit: The whole heading line looks consistent and visually distinct.
  ;; - Drawback: Inline elements may be harder to read or lose their usual
  ;;             highlighting, especially if the heading face color is very dark
  ;;             or clashes with your theme.
  (setq org-fontify-whole-heading-line t)

  ;; Fontify quote and verse blocks; highlights these blocks but may interfere
  ;; with other syntax highlighting.
  (setq org-fontify-quote-and-verse-blocks t)

  ;; Do not fontify the entire block delimiter line; prevents color bleeding
  ;; when folding headings or blocks.
  (setq org-fontify-whole-block-delimiter-line nil)

  ;; Disable sub/superscript interpretation (_ and ^)
  (setq org-use-sub-superscripts '{})

  ;; Indentation per heading level; controls visual hierarchy but tight spacing
  ;; may feel cramped.
  (setq org-indent-indentation-per-level 2)

  ;; No extra indentation for source blocks. It keeps code aligned with text.
  (setq org-edit-src-content-indentation 0)

  ;; Set ellipsis for folded sections; improves folding visibility but may not
  ;; suit all fonts.
  (setq org-ellipsis lightemacs-ellipsis)

  ;; Allow alphabetical lists; flexible list styles but may confuse automatic
  ;; numbering.
  (setq org-list-allow-alphabetical t)

  ;; Log completion time; provides audit trail but adds automatic notes that may
  ;; clutter logs.
  (setq org-log-done 'time)

  ;; RET follows links; intuitive navigation but may conflict with normal line
  ;; breaks.
  (setq org-return-follows-link t)

  ;; More comprehensive imenu
  (setq org-imenu-depth 6)

  ;; Disable stepwise path completion; direct path completion but may be harder
  ;; to navigate long hierarchies.
  (setq org-outline-path-complete-in-steps nil)

  ;; Do not automatically adjust indentation based on outline structure;
  ;; preserves original formatting.
  (setq org-adapt-indentation nil)

  ;; Do not insert empty lines between collapsed sections; makes folded view
  ;; denser but reduces visual separation between headings.
  (setq org-cycle-separator-lines 0)

  ;; Prevent marking a parent TODO as DONE if its child tasks are incomplete;
  ;; ensures task consistency but may slow task completion when some subtasks
  ;; are still pending.
  (setq org-enforce-todo-dependencies t)

  ;; Prevent marking a TODO with checkboxes as DONE if any checkboxes are
  ;; incomplete; preserves logical consistency but may frustrate users if
  ;; subtasks are partially complete.
  (setq org-enforce-todo-checkbox-dependencies t)

  ;; Insert new headings after the current subtree instead of at point;
  ;; maintains logical structure
  (setq org-insert-heading-respect-content t)

  ;; When nil, it will go to the end of the line before making a new line.
  (setq org-M-RET-may-split-line nil)

  ;; Display descriptive text for links instead of raw URLs; improves
  ;; readability
  (setq org-link-descriptive t)

  ;; Use native major-mode indentation
  (setq org-src-preserve-indentation t)

  ;; Make TAB behave according to the language mode inside source blocks;
  ;; consistent editing experience
  (setq org-src-tab-acts-natively t)

  ;; No need to ask. Just exercise caution.
  (setq org-confirm-babel-evaluate nil
        ;; Do not ask for confirmation before executing Emacs Lisp links.
        org-link-elisp-confirm-function nil)

  (setq org-hide-leading-stars t
        ;; showeverything is Org's default, but it ignores
        ;; org-hide-block-startup (#+startup: hideblocks), archived trees,
        ;; hidden drawers, and VISIBILITY properties. Setting it to nil has the
        ;; same effect functionally, but respects these settings.
        org-startup-folded nil
        org-image-actual-width nil
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . shadow)))

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "PROJ(p)"  ; A project
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))

  ;; Show src buffer in popup
  (setq org-src-window-setup 'other-window)

  ;; Display indirect tree buffers in the current window
  (setq org-indirect-buffer-display 'current-window)

  ;; Show agenda in the current window, keeping all other windows.
  (setq org-agenda-window-setup 'current-window)

  ;; To prevent agenda commands to honor startup options when visiting an agenda
  ;; file for the first time, use this:
  ;; https://orgmode.org/worg/agenda-optimization.html
  (setq org-agenda-inhibit-startup t)

  (setq org-agenda-skip-unavailable-files t
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d"
        org-agenda-span 10)

  ;; Prevents clutter in agenda by skipping already done scheduled tasks.
  (setq org-agenda-skip-scheduled-if-done t)

  ;; Reduces clutter for completed tasks with deadlines.
  (setq org-agenda-skip-deadline-if-done t))

(provide 'le-org)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-org.el ends here
