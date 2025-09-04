;;; le-which-key.el --- le-which-key -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The `which-key' mode dynamically displays available keybindings in a popup or
;; dedicated buffer as a key sequence is entered. It facilitates discovery and
;; retention of key combinations by presenting context-sensitive completions,
;; thereby enhancing navigation through complex or highly customized keymaps.

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package which-key
  :ensure nil
  :commands which-key-mode

  ;; TODO Fix first key press
  ;; :hook (lightemacs-on-first-input-hook . which-key-mode)
  :hook (after-init . which-key-mode)

  :init
  (setq
   ;; Maximum number of columns displayed. nil lets which-key choose
   ;; automatically. Benefit: Adjusts to available space dynamically. Drawback:
   ;; Can result in inconsistent layout across sessions.
   which-key-max-display-columns nil  ; nil or 3

   ;; Minimum number of lines to display in the which-key buffer.
   which-key-min-display-lines 6

   ;; Sort uppercase keys after lowercase.
   which-key-sort-uppercase-first nil

   ;; Sorting strategy for key display. Alphabetical order makes keybindings
   ;; predictable.
   which-key-sort-order 'which-key-key-order-alpha

   ;; which-key-sort-order 'which-key-prefix-then-key-order

   ;; Slot for side window display. `which-key' uses a side window to show key
   ;; suggestions.
   ;;
   ;; This determines the layering order of this window relative to other side
   ;; windows (like compilation buffers, help buffers, or other popup windows).
   ;; Negative values, such as -10, push the which-key window behind most other
   ;; side windows, preventing it from covering or overlapping important
   ;; content.
   ;;
   ;; Setting a highly negative slot can cause the which-key window to
   ;; appear "below" other side windows, which may make it less immediately
   ;; noticeable. If another window pops up with a higher slot, which-key may be
   ;; partially hidden or layered awkwardly.
   which-key-side-window-slot -10

   ;; Padding between columns in the which-key display. Improves readability.
   which-key-add-column-padding 1

   ;; Allow multiple replacement rules for keys. Provides flexible key display
   ;; customization.
   which-key-allow-multiple-replacements t

   ;; which-key-echo-keystrokes 0.02

   ;; Time to wait before displaying which-key after the first key press.
   ;; Reduces delay, improving responsiveness.
   which-key-idle-delay 1.3

   ;; Secondary delay for updating which-key after each subsequent key press.
   ;; Makes updates faster and more responsive after the first key.
   which-key-idle-secondary-delay 0.1

   ;; Maximum length of key descriptions. Prevents overly long lines in the
   ;; which-key popup.
   which-key-max-description-length 32  ; Alternative: 40

   ;; which-key-allow-evil-operators t

   ;; which-key-prevent-C-h-from-cycling t
   ;; which-key-special-keys nil
   ;; which-key-use-C-h-for-paging t

   ;; String separator between key and description.
   ;; which-key-separator "  "
   which-key-separator " "

   ;; Prefix string indicating the start of a key sequence. Visually
   ;; distinguishes prefix keys. Extra characters may slightly increase clutter.
   ;; which-key-prefix-prefix "+"
   ;; which-key-prefix-prefix (if (display-graphic-p) "… " "... ")
   )

  :config
  (with-eval-after-load 'which-key  ; Reason: no-require: t
    ;; Set up side-window that opens on bottom.
    (which-key-setup-side-window-bottom)))

(provide 'le-which-key)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-which-key.el ends here
