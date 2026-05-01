;;; le-outline.el --- le-outline -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Update the ellipsis in `outline-mode' and `outline-minor-mode' using
;; the `lightemacs-ellipsis' variable.
;;
;; Bug fix: outline-end-of-subtree misses final newline when at end of buffer

;;; Code:

;;; Hooks

(require 'lightemacs-module)

;;; lightemacs-module-hooks

(lightemacs-module-hooks outline-minor
  outline-minor-mode
  '())

;;; Update the outline ellipsis

(defun le-outline--set-buffer-local-ellipsis (ellipsis)
  "Apply the ellipsis ELLIPSIS to outline mode locally to a buffer."
  (let* ((display-table (or buffer-display-table (make-display-table)))
         (face-offset (* (face-id 'shadow) (ash 1 22)))
         (value (vconcat (mapcar (lambda (c) (+ face-offset c))
                                 ;; Trim trailing whitespace after the
                                 ;; ellipsis, as it can be misleading when the
                                 ;; line is not truncated. Wrapping may
                                 ;; display only the space after the ellipsis
                                 ;; on the next line, creating the illusion of
                                 ;; a new line. Deleting that apparent "new
                                 ;; line" may delete the entire logical line
                                 ;; containing the ellipsis.
                                 (string-trim-right ellipsis)))))
    (set-display-table-slot display-table 'selective-display value)
    (setq buffer-display-table display-table)))

(defun le-outline--update-ellipsis ()
  "Update `outline-mode' ellipsis."
  ;; Do not modify the ellipsis in `org-mode' or `outline-indent-minor-mode' as
  ;; it is managed internally by their respective modes
  (unless (or (bound-and-true-p outline-indent-minor-mode)
              (derived-mode-p 'org-mode))
    (le-outline--set-buffer-local-ellipsis lightemacs-ellipsis)))

(add-hook 'outline-minor-mode-hook #'le-outline--update-ellipsis)
(add-hook 'outline-mode-hook #'le-outline--update-ellipsis)

;;; Bug fix: TODO: This bug fix was sent to the Emacs developers

;; Issue report:
;;   [PATCH] outline-end-of-subtree misses final newline when at end of buffer
;;   When using outline-mode (or outline-minor-mode), making a
;;   subtree invisible that is located at the very end of
;;   the buffer causes the final newline to be incorrectly
;;   flagged as invisible.
;;
;;   This occurs because outline-end-of-subtree relies on
;;   (and (bolp) (not (eolp))) to decide whether to call
;;   outline--end-of-previous. At the end of a buffer terminating
;;   with a newline, point is at an empty line, making
;;   (not (eolp)) evaluate to nil, which makes the function skips
;;   stepping back over the final newline before the visibility
;;   is flagged. This results in the ellipsis swallowing the
;;   end-of-buffer marker.
;;
;;   Steps to reproduce:
;;   1. emacs -Q
;;   2. Set require-final-newline to t
;;   2. Switch to *scratch* and evaluate: (outline-mode)
;;   3. Insert the following text:
;;   * Heading 1
;;   * Heading 2
;;   4. Move point to "Heading 2" and type: M-x outline-hide-subtree
;;   5. Observe that the end of the buffer is swallowed into the
;;   ellipsis.
;;
;;   Expected behavior:
;;   The invisible region should terminate before the final
;;   newline, leaving the end-of-buffer reachable and visible
;;   outside the hidden subtree.
;;
;;   The attached patch fixes this by explicitly checking for
;;   (eobp) before deciding to step backward.

(defun le-outline--advice-backtrack-whitespace (&rest _args)
  "Backtrack over whitespace-only lines to prevent them from being folded.
This advice ensures that empty lines between headings or at the end of
the buffer remain visible when a fold is applied."
  (if (eobp)
      (if (bolp)
          (forward-char -1))))

(with-eval-after-load 'outline
  (advice-add 'outline-end-of-subtree :after
              #'le-outline--advice-backtrack-whitespace))

(provide 'le-outline)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-outline.el ends here
