;;; mod-org.el --- mod-org -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configure `org-mode'.

;;; Code:

(use-package org
  :commands (org-mode
             org-indent-mode
             org-set-tags-command
             org-version)

  :mode
  ("\\.org\\'" . org-mode)
  ("\\.[Oo][Rr][Gg]\\.[aA][sS][cC]\\'" . org-mode)  ;; .org.asc (GPG Encrypted)

  :init
  ;; Disable saving a bookmark when capturing; avoids cluttering the bookmark
  ;; list but loses the ability to quickly return to the capture location.
  (setq org-capture-bookmark nil)

  ;; Define refile targets up to maxlevel in the current file and agenda files;
  ;; allows flexible refiling but may slow completion in very large files and
  ;; requires remembering hierarchy.
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 5))
          (nil . (:maxlevel . 5))))

  ;; Allow creating new parent nodes when refiling, but ask for confirmation;
  ;; provides flexibility in organization but adds an extra prompt that may
  ;; interrupt workflow.
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Use the full outline path when refiling; makes it easier to select the
  ;; correct target but requires remembering or seeing the full path.
  (setq org-refile-use-outline-path t)

  ;; Disable automatic alignment of tags; keeps tags where typed but may result
  ;; in uneven or inconsistent tag placement across headings.
  (setq org-auto-align-tags nil)

  ;; Set tag column to 0 (tags appear immediately after heading); simplifies
  ;; layout but may make long headings with tags harder to read.
  (setq org-tags-column 0)

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

  ;; Do not fontify the whole block delimiter line; prevents color bleeding
  (setq org-fontify-whole-block-delimiter-line nil)

  ;; Disable sub/superscript interpretation (_ and ^); avoids accidental
  ;; formatting but loses math-style notation.
  (setq org-use-sub-superscripts nil)

  ;; Indentation per heading level; controls visual hierarchy but tight spacing
  ;; may feel cramped.
  (setq org-indent-indentation-per-level 2)

  ;; No extra indentation for source blocks; keeps code aligned with text but
  ;; may reduce readability.
  (setq org-edit-src-content-indentation 0)

  ;; Set ellipsis for folded sections; improves folding visibility but may not
  ;; suit all fonts.
  (setq org-ellipsis lightemacs-ellipsis)

  ;; Allow alphabetical lists; flexible list styles but may confuse automatic
  ;; numbering.
  (setq org-list-allow-alphabetical t)  ; allow alphabetical list

  ;; Set sub-item indent; improves nested list readability but reduces spacing
  ;; flexibility.
  (setq org-list-indent-offset 1)

  ;; Prevent editing invisible content; avoids accidental changes but may block
  ;; some advanced edits.
  (setq org-catch-invisible-edits 'smart)  ; Obsolete
  (setq org-fold-catch-invisible-edits 'smart)

  ;; Log completion time; provides audit trail but adds automatic notes that may
  ;; clutter logs.
  (setq org-log-done 'time)

  ;; Do not add notes when clocking out; keeps logs cleaner but loses contextual
  ;; info.
  (setq org-log-note-clock-out nil)

  ;; Skip logging for rescheduled tasks; reduces unnecessary log entries but may
  ;; hide changes.
  (setq org-log-redeadline nil)

  ;; RET follows links; intuitive navigation but may conflict with normal line
  ;; breaks.
  (setq org-return-follows-link t)

  ;; More comprehensive imenu
  (setq org-imenu-depth 5)

  ;; Fast todo selection without popup; efficient for experts but hides guidance
  ;; for beginners.
  (setq org-use-fast-todo-selection 'expert)

  ;; Prettify entities without sub/superscripts; cleaner symbols but loses
  ;; fine-grained formatting.
  (setq org-pretty-entities-include-sub-superscripts nil)

  ;; Loop only over headlines in active region; faster processing but ignores
  ;; headlines outside region.
  (setq org-loop-over-headlines-in-active-region 'start-level)

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

  ;; When non-nil, adjust the indentation level of yanked (pasted) Org subtrees
  ;; to match the context. This ensures that subtrees are inserted at the
  ;; correct depth, relative to the surrounding content.
  ;;
  ;; Benefit:
  ;; - Maintains structural consistency in Org documents when pasting subtrees
  ;; into different outline levels.
  ;; - Prevents malformed hierarchies by adapting subtree heading levels
  ;; automatically.
  (setq org-yank-adjusted-subtrees t)

  ;; When non-nil, automatically fold (hide) yanked Org subtrees after insertion.
  ;;
  ;; Benefit:
  ;; - Reduces visual clutter by collapsing inserted content immediately.
  ;; - Preserves the folding state of the document and improves readability
  ;; after yanking.
  (setq org-yank-folded-subtrees t)

  ;; To prevent agenda commands to honor startup options when visiting an agenda
  ;; file for the first time, use this:
  ;; https://orgmode.org/worg/agenda-optimization.html
  (setq org-agenda-inhibit-startup t)

  ;; Insert new headings after the current subtree instead of at point;
  ;; maintains logical structure
  (setq org-insert-heading-respect-content t)

  ;; Prefer future dates when entering incomplete dates; useful for planning
  ;; upcoming tasks
  (setq org-read-date-prefer-future 'time)

  ;; Display descriptive text for links instead of raw URLs; improves
  ;; readability
  (setq org-link-descriptive t)

  ;; Do not preserve leading indentation in source blocks; normalizes code
  ;; indentation
  (setq org-src-preserve-indentation nil)

  ;; Make TAB behave according to the language mode inside source blocks;
  ;; consistent editing experience
  (setq org-src-tab-acts-natively t))

(provide 'mod-org)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-org.el ends here
