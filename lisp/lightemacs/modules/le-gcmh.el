;;; le-gcmh.el --- le-gcmh -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Gcmh (Garbage Collector Magic Hack) optimizes Emacs’ garbage collection
;; behavior by adjusting the garbage collection threshold dynamically. Instead
;; of collecting memory frequently during normal editing, gcmh increases the
;; threshold while Emacs is idle, reducing interruptions and improving perceived
;; performance. It also restores the threshold during active usage to prevent
;; excessive memory use. In essence, it makes Emacs feel more responsive by
;; tuning garbage collection automatically.
;;
;; URL: https://gitlab.com/koral/gcmh
;;      https://github.com/emacsmirror/gcmh

;;; Code:

(require 'lightemacs-module)

(lightemacs-use-package gcmh
  :commands (gcmh-mode
             gcmh-idle-garbage-collect)
  :hook
  (lightemacs-on-first-buffer . gcmh-mode)

  :init
  (setq gcmh-verbose init-file-debug
        gcmh-auto-idle-delay-factor 10

        gcmh-high-cons-threshold (* 100 1024 1024)
        gcmh-low-cons-threshold minimal-emacs-gc-cons-threshold

        ;; This variable determines how long Emacs should wait (in seconds) while
        ;; being idle before triggering garbage collection. "Idle" here means no
        ;; keyboard or mouse input is received for the specified period.
        gcmh-idle-delay 'auto))

(provide 'le-gcmh)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-gcmh.el ends here
