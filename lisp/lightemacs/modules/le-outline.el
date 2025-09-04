;;; le-outline.el --- le-outline -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Update the ellipsis in `outline-mode' and `outline-minor-mode' using
;; the `lightemacs-ellipsis' variable.

;;; Code:

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

(provide 'le-outline)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-outline.el ends here
