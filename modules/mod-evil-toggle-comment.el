;;; mod-evil-toggle-comment.el --- mod-evil-toggle-comment -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(with-eval-after-load 'evil
  (evil-define-operator mod-evil-toggle-comment-visual (beg end)
    "Toggle comment from BEG to END."
    (interactive "<r>")
    (unless (derived-mode-p 'org-mode)
      (comment-or-uncomment-region beg end)))

  (defun mod-evil-toggle-comment-line ()
    "Toggle comment in the current line."
    (interactive)
    (unless (derived-mode-p 'org-mode)
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))))

  (evil-define-key 'normal 'global (kbd "gc") 'mod-evil-toggle-comment-line)
  (evil-define-key 'visual 'global (kbd "gc") 'mod-evil-toggle-comment-visual))

(provide 'mod-evil-toggle-comment)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-evil-toggle-comment.el ends here
