;;; le-winner.el --- le-winner -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Track changes in the window configuration, allowing undoing actions such as
;; closing windows using `winner-undo'.

;;; Code:

(require 'lightemacs-use-package)

(lightemacs-use-package winner
  :ensure nil
  :commands (winner-mode
             winner-undo
             winner-redo)

  :hook
  (lightemacs-on-first-buffer . winner-mode)

  :init
  ;; (setq winner-ring-size 40)
  ;; (setq winner-dont-bind-my-keys t)
  (setq winner-boring-buffers '("*Completions*"
                                "*Minibuf-0*"
                                "*Minibuf-1*"
                                "*Minibuf-2*"
                                "*Minibuf-3*"
                                "*Minibuf-4*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*")))

(provide 'le-winner)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-winner.el ends here
