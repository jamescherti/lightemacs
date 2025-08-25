;;; le-ace-window.el --- le-ace-window -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Ace Window provides a fast and efficient method for switching between windows
;; in a frame. Instead of cycling through windows sequentially or using more
;; cumbersome key sequences, Ace Window displays a single-letter label on each
;; visible window, allowing the user to jump directly to a target window by
;; pressing the corresponding key.
;;
;; URL: https://github.com/abo-abo/ace-window

;;; Code:

(require 'lightemacs)

(lightemacs-use-package
  ace-window
  :commands ace-window
  :init
  (setq aw-background nil  ; t is not compatible with all themes
        aw-scope 'frame))

(lightemacs-define-keybindings ace-window
  ;; Remap 'C-x o'
  (global-set-key [remap other-window] #'ace-window))

(provide 'le-ace-window)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-ace-window.el ends here
