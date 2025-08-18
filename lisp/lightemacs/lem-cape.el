;;; lem-cape.el --- lem-cape -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Cape, or Completion At Point Extensions, extends the capabilities of
;; in-buffer completion. It integrates with Corfu or the default completion UI,
;; by providing additional backends through completion-at-point-functions.
;;
;; URL: https://github.com/minad/cape

;;; Code:

(use-package cape
  :bind ("C-c p" . cape-prefix-map)
  :commands (cape-dabbrev
             cape-file)

  :init
  (setq cape-dabbrev-min-length 1)
  (setq cape-dabbrev-check-other-buffers t)

  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)

  ;; `sh-mode'
  (defun lem-cape--setup-cape-sh-mode ()
    "Dabbrev is better than the default configuration for `sh-mode'."
    (setq-local completion-at-point-functions '(cape-dabbrev cape-file)))

  (when (fboundp 'lem-cape--setup-cape-sh-mode)
    (add-hook 'bash-ts-mode-hook #'lem-cape--setup-cape-sh-mode)
    (add-hook 'sh-mode-hook #'lem-cape--setup-cape-sh-mode)))

(provide 'lem-cape)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lem-cape.el ends here
