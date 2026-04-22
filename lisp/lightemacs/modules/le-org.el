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
  (setq org-fold-show-context-detail
        '(;; 'local' reveals the current heading but keeps children folded.
          ;; Useful to focus strictly on the agenda item without visual clutter.
          (agenda . local)

          ;; This fixes:
          ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-08/msg01128.html
          ;; TODO patch org?
          ;;
          ;; 'canonical' reveals the current headline, its direct ancestors, and
          ;; its immediate children. This is ideal for searching. It gives you
          ;; enough structural context to know exactly where you are in the
          ;; document hierarchy without unfolding the entire tree.
          (isearch . canonical)

          ;; when exposing a bookmark location 'canonical' is highly useful for
          ;; bookmarks that point to project roots or major category headers,
          ;; allowing you to see the immediate contents upon jumping.
          (bookmark-jump . canonical)

          ;; when using the command org-occur (C-c / /)
          ;; 'canonical' is useful here because it shows the immediate children
          ;; of the matched headings, providing a broader overview of the
          ;; matched section in your sparse tree rather than just an isolated
          ;; line.
          (occur-tree . canonical)

          ;; When using the command org-goto (C-c C-j)
          ;; 'canonical' is useful here if you frequently jump to parent
          ;; headings and immediately need to see their sub-headings to navigate
          ;; further.
          ;; (org-goto . canonical)

          ;; when constructing a sparse tree based on tags matches 'canonical'
          ;; is useful if your tags are applied to high-level categories and you
          ;; want the sparse tree to automatically reveal the specific items
          ;; underneath them.
          ;; (tags-tree . canonical)

          ;; when exposing search matches associated with a link 'canonical' is
          ;; useful if your internal links frequently point to index or parent
          ;; nodes and you want to see the associated subcategories immediately
          ;; upon arrival.
          ;; (link-search . canonical)

          ;; when exposing the jump goal of a mark 'canonical' helps re-orient
          ;; you by showing the immediate children of the location you just
          ;; popped back to via the mark ring.
          ;; (mark-goto . canonical)

          ;; The fallback for any context not explicitly defined above.
          ;; 'ancestors' keeps the buffer as tidy as possible by only unfolding
          ;; the direct path from the top level down to your target, leaving all
          ;; other sibling and child trees completely folded.
          (default . ancestors)))

  (setq org-tags-column 0)

  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("md" . markdown)))

  ;; Disable saving a bookmark when capturing; avoids cluttering the bookmark
  ;; list but loses the ability to quickly return to the capture location.
  (if (boundp 'org-bookmark-names-plist)
      (setf (plist-get org-bookmark-names-plist :last-capture) nil)
    (setq org-bookmark-names-plist
          '(:last-capture nil
                          :last-refile "org-refile-last-stored"
                          :last-capture-marker "org-capture-last-stored-marker")))
  (with-suppressed-warnings ((obsolete org-capture-bookmark))
    (setq org-capture-bookmark nil))

  ;; Define refile targets up to maxlevel in the current file and agenda files;
  ;; allows flexible refiling but may slow completion in very large files and
  ;; requires remembering hierarchy.
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))

  ;; Use the full outline path when refiling; makes it easier to select the
  ;; correct target but requires remembering or seeing the full path.
  (setq org-refile-use-outline-path t)

  ;; Ctrl-A/E moves to beginning/end of heading instead of line; improves
  ;; navigation.
  (setq org-special-ctrl-a/e t)

  ;; Indent text according to heading level; makes outline visually clearer but
  ;; can misalign code blocks or tables.
  (setq org-startup-indented t)

  ;; Color DONE headlines; quickly identifies completed tasks
  (setq org-fontify-done-headline t)

  ;; Color to do headlines; improves task visibility
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

  ;; Disable sub/superscript interpretation (_ and ^)
  (setq org-use-sub-superscripts '{})

  ;; Indentation per heading level; controls visual hierarchy but tight spacing
  ;; may feel cramped.
  ;; NOTE: Same as default
  ;; (setq org-indent-indentation-per-level 2)

  ;; Set ellipsis for folded sections; improves folding visibility but may not
  ;; suit all fonts.
  (setq org-ellipsis lightemacs-ellipsis)

  ;; Allow alphabetical lists; flexible list styles but may confuse automatic
  ;; numbering.
  (setq org-list-allow-alphabetical t)

  ;; More comprehensive imenu
  (setq org-imenu-depth 6)

  ;; Disable stepwise path completion; direct path completion but may be harder
  ;; to navigate long hierarchies.
  (setq org-outline-path-complete-in-steps nil)

  ;; Prevent marking a parent to do as done if its child tasks are incomplete;
  ;; ensures task consistency but may slow task completion when some subtasks
  ;; are still pending.
  (setq org-enforce-todo-dependencies t)

  ;; Insert new headings after the current subtree instead of at point;
  ;; maintains logical structure
  (setq org-insert-heading-respect-content t)

  ;; When nil, it will go to the end of the line before making a new line.
  (setq org-M-RET-may-split-line nil)

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
        org-agenda-span 10))

(provide 'le-org)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-org.el ends here
