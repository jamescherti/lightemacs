;;; le-diff-hl.el --- le-diff-hl -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configures the `diff-hl' package, which highlights uncommitted changes in the
;; window margin, enabling navigation between them. Also known as source control
;; gutter indicators, it displays added, modified, and deleted lines in real
;; time. In Git-controlled buffers, changes can be staged and unstaged directly,
;; providing a clear view of version-control changes without running 'git diff'.
;; By default, the module does not start `diff-hl-mode' automatically.
;;
;; URL: https://github.com/dgutov/diff-hl

;;; Code:

;;; Require

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

;;; use-package diff-hl

(lightemacs-use-package diff-hl
  :commands (diff-hl-mode
             global-diff-hl-mode
             diff-hl-flydiff
             diff-hl-update)
  :init
  (lightemacs-module-hooks diff-hl-global
    global-diff-hl-mode
    '(lightemacs-on-first-file-hook))

  (lightemacs-module-hooks diff-hl-local
    diff-hl-mode
    nil)

  (lightemacs-module-hooks diff-hl-flydiff-global
    diff-hl-flydiff-mode
    '(global-diff-hl-mode-hook))

  (lightemacs-module-setq-maybe diff-hl
    diff-hl-flydiff-delay 0.4  ; Faster
    diff-hl-draw-borders nil
    diff-hl-disable-on-remote t
    diff-hl-show-staged-changes nil  ; Realtime feedback
    diff-hl-update-async t  ; Do not block Emacs
    diff-hl-global-modes '(not pdf-view-mode image-mode)))


;;; Bug fix: Update diff-hl after switching to a buffer

;; TODO: Send a patch to diff-hl?

(defun lightemacs--diff-hl-update-hook (window)
  "Update diff-hl when WINDOW buffer changes, if `diff-hl-mode' is enabled."
  (let ((buffer (window-buffer window)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (bound-and-true-p diff-hl-mode)
                   (fboundp 'diff-hl-update))
          (diff-hl-update))))))

(defun lightemacs--diff-hl-update-on-buffer-change ()
  "Add or remove the diff-hl update hook based on the state of `diff-hl-mode'."
  (if diff-hl-mode
      (add-hook 'window-buffer-change-functions #'lightemacs--diff-hl-update-hook nil t)
    (remove-hook 'window-buffer-change-functions #'lightemacs--diff-hl-update-hook t)))

(add-hook 'diff-hl-mode-hook #'lightemacs--diff-hl-update-on-buffer-change)

;;; Provide

(provide 'le-diff-hl)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-diff-hl.el ends here
